# The repositories of the ices-taf and ices-advice organisations, and what is
# found in their files, read with the GitHub CLI.
#
# Most ices-taf repositories are private, so `gh` must be authenticated as
# someone who can read them (in GitHub Actions: the TAF_STATUS_TOKEN secret),
# otherwise only the public repositories are counted. What is written here is
# what is found in the repositories, never their code: the names of the
# private repositories are published in the tables, but nothing of what is in
# them.
#
# ices-taf.csv, ices-advice.csv: the repositories of each organisation.
# model-scripts.csv: the size of the model scripts of each repository named
#   for a year, and of the management strategy evaluations.
# data-sources.csv: the other ices-taf repositories that each repository
#   named for a year takes data from.
# rdbes-code.csv: the connections to the RDBES found in the code of the
#   repositories of the RDBES workflow (../rdbes.json), and the size of
#   their code.
# templates.csv: the TAF templates of ../templates.csv, with the files in the
#   repository of each and where each marker of progress is found.
#
# Looking at the files of each repository is slow, so the model scripts and
# the data sources are only read again for the repositories pushed to since
# the previous build (../previous).

# the repositories of the management strategy evaluations, which are not named
# for a year: wk_WKMSE..., wk_WKNSMSE... and WGHANSA_...
mse_reg <- "^(wk_wk(ns)?mse|wghansa_)"

# a new TAF repository starts with R scripts of about 100 bytes: one larger
# than this has some code in it
min_script_size <- 150

# ---- helpers ---------------------------------------------------------------

gh_api <- function(endpoint) {
  json <- suppressWarnings(
    system2("gh", c("api", shQuote(endpoint)), stdout = TRUE, stderr = FALSE)
  )
  if (!is.null(attr(json, "status"))) {
    return(NULL)
  }
  jsonlite::fromJSON(paste(json, collapse = "\n"), simplifyVector = FALSE)
}

org_repos <- function(org) {
  all <- list()
  page <- 1
  repeat {
    res <- gh_api(sprintf("orgs/%s/repos?type=all&per_page=100&page=%d", org, page))
    if (is.null(res)) stop("could not list the repositories of ", org)
    if (length(res) == 0) break
    all <- c(all, res)
    page <- page + 1
  }
  chr <- function(field) {
    vapply(all, function(r) if (is.null(r[[field]])) NA_character_ else r[[field]], "")
  }
  data.frame(
    name = chr("name"),
    visibility = chr("visibility"),
    url = chr("html_url"),
    size = vapply(all, function(r) as.numeric(r$size), 1),
    branch = chr("default_branch"),
    pushed = chr("pushed_at"),
    stringsAsFactors = FALSE
  )
}

# ---- model scripts ---------------------------------------------------------

# size in bytes of the model scripts at the top level of each repository:
# model.R itself (NA if there is none), and together all the R scripts and
# R Markdown files with model or method in their name, such as model_run1.R,
# 03_model.R, method.R and model01.Rmd. This is to tell the repositories with
# some modelling in them from those with none: whether they follow the TAF
# layout is for the QC test.
model_scripts <- function(names, owner = "ices-taf") {
  out <- data.frame(model_r = rep(NA_real_, length(names)), model_size = 0)
  # a few repositories per request: GitHub times out on large queries
  for (batch in split(seq_along(names), ceiling(seq_along(names) / 15))) {
    query <- paste0(
      "{",
      paste0(
        sprintf(
          "r%d: repository(owner: \"%s\", name: \"%s\") { object(expression: \"HEAD:\") { ... on Tree { entries { name type object { ... on Blob { byteSize } } } } } }",
          batch, owner, names[batch]
        ),
        collapse = " "
      ),
      "}"
    )
    for (attempt in 1:3) {
      json <- suppressWarnings(
        system2("gh", c("api", "graphql", "-f", shQuote(paste0("query=", query))), stdout = TRUE, stderr = FALSE)
      )
      if (is.null(attr(json, "status"))) break
    }
    if (!is.null(attr(json, "status"))) stop("could not read the files of the repositories")
    res <- jsonlite::fromJSON(paste(json, collapse = "\n"), simplifyVector = FALSE)$data
    for (i in batch) {
      # an empty repository has no files
      files <- Filter(function(f) f$type == "blob", res[[paste0("r", i)]]$object$entries)
      file <- vapply(files, function(f) f$name, "")
      size <- vapply(files, function(f) as.numeric(f$object$byteSize), 1)
      main <- tolower(file) %in% c("model.r", "method.r")
      if (any(main)) out$model_r[i] <- sum(size[main])
      out$model_size[i] <- sum(size[grepl("(model|method).*[.](r|rmd)$", tolower(file))])
    }
  }
  out
}

# the model scripts of the repositories, read from GitHub only for those that
# are new or have been pushed to since they were last read. Reading them is
# most of the time this script takes.
cached_model_scripts <- function(repos) {
  known <- if (file.exists("../previous/model-scripts.csv")) {
    read.csv("../previous/model-scripts.csv", stringsAsFactors = FALSE)
  } else {
    data.frame(name = character(), pushed = character(), model_r = numeric(), model_size = numeric())
  }
  current <- merge(repos[c("name", "pushed")], known, all.x = TRUE, sort = FALSE)
  # nothing is recorded for a repository that has not been read
  stale <- !paste(current$name, current$pushed) %in% paste(known$name, known$pushed)
  message(sum(stale), " of ", nrow(current), " repositories to read the model scripts of")
  if (any(stale)) {
    current[stale, c("model_r", "model_size")] <- model_scripts(current$name[stale])
  }
  # keep what is known of repositories that cannot be seen this time, as
  # when run without access to the private ones
  kept <- rbind(current, known[!known$name %in% current$name, names(current)])
  write.csv(kept[order(kept$name), ], "model-scripts.csv", row.names = FALSE)
}

# ---- data sources ----------------------------------------------------------

# the text of a file in each of several repositories, NA where there is no
# such file
file_texts <- function(names, paths, owner = "ices-taf") {
  out <- rep(NA_character_, length(names))
  # a few files per request: GitHub times out on large queries
  for (batch in split(seq_along(names), ceiling(seq_along(names) / 20))) {
    query <- paste0(
      "{",
      paste0(
        sprintf(
          "r%d: repository(owner: \"%s\", name: \"%s\") { object(expression: \"HEAD:%s\") { ... on Blob { text } } }",
          batch, owner, names[batch], gsub("\"", "\\\\\"", paths[batch])
        ),
        collapse = " "
      ),
      "}"
    )
    for (attempt in 1:3) {
      json <- suppressWarnings(
        system2("gh", c("api", "graphql", "-f", shQuote(paste0("query=", query))), stdout = TRUE, stderr = FALSE)
      )
      if (is.null(attr(json, "status"))) break
    }
    if (!is.null(attr(json, "status"))) stop("could not read the files of the repositories")
    res <- jsonlite::fromJSON(paste(json, collapse = "\n"), simplifyVector = FALSE)$data
    for (i in batch) {
      text <- res[[paste0("r", i)]]$object$text
      if (!is.null(text)) out[i] <- text
    }
  }
  out
}

# the repositories that each repository takes data from: the other ices-taf
# repositories named in its DATA.bib, in boot/ or in the older bootstrap/, or
# in the scripts of the entries in it with source = {script}. Read from
# GitHub only for the repositories that are new or have been pushed to since
# they were last read.
data_sources <- function(repos) {
  known <- if (file.exists("../previous/data-sources.csv")) {
    read.csv("../previous/data-sources.csv", stringsAsFactors = FALSE, na.strings = "")
  } else {
    data.frame(name = character(), pushed = character(), sources = character())
  }
  current <- merge(repos[c("name", "pushed")], known, all.x = TRUE, sort = FALSE)
  stale <- !paste(current$name, current$pushed) %in% paste(known$name, known$pushed)
  message(sum(stale), " of ", nrow(current), " repositories to read the data sources of")
  if (any(stale)) {
    names <- current$name[stale]
    n <- length(names)
    bibs <- file_texts(rep(names, 2), rep(c("boot/DATA.bib", "bootstrap/DATA.bib"), each = n))
    folder <- ifelse(is.na(bibs[1:n]), "bootstrap/", "boot/")
    bib <- ifelse(is.na(bibs[1:n]), bibs[n + 1:n], bibs[1:n])
    # the scripts of the entries with source = {script}
    scripts <- lapply(seq_len(n), function(i) {
      if (is.na(bib[i])) {
        return(character())
      }
      entries <- strsplit(bib[i], "@")[[1]]
      script <- grepl("source\\s*=\\s*\\{\\s*script\\s*\\}", entries)
      keys <- regmatches(entries[script], regexec("^\\w+\\s*\\{\\s*([^,\\s]+)\\s*,", entries[script]))
      keys <- vapply(keys, function(x) if (length(x) == 2) x[2] else NA_character_, "")
      paste0(folder[i], keys[!is.na(keys)], ".R")
    })
    texts <- file_texts(rep(names, lengths(scripts)), unlist(scripts))
    text <- split(texts, factor(rep(seq_len(n), lengths(scripts)), seq_len(n)))
    current$sources[stale] <- vapply(seq_len(n), function(i) {
      words <- unlist(regmatches(c(bib[i], text[[i]]), gregexpr("[0-9]{4}_[A-Za-z0-9._-]+", c(bib[i], text[[i]]))))
      words <- unique(c(words, sub("[.][A-Za-z]+$", "", words)))
      found <- sort(setdiff(intersect(words, repos$name), names[i]))
      if (length(found)) paste(found, collapse = " ") else NA_character_
    }, "")
  }
  kept <- rbind(current, known[!known$name %in% current$name, names(current)])
  write.csv(kept[order(kept$name), ], "data-sources.csv", row.names = FALSE, na = "")
  message(sum(!is.na(current$sources)), " repositories take data from another")
}

# ---- the RDBES workflow ----------------------------------------------------

# a file in a repository, as text, or NULL if it cannot be read
gh_file <- function(repo, path, owner = "ices-taf") {
  text <- suppressWarnings(system2(
    "gh",
    c("api", "-H", shQuote("Accept: application/vnd.github.raw"), shQuote(sprintf("repos/%s/%s/contents/%s", owner, repo, utils::URLencode(path)))),
    stdout = TRUE, stderr = FALSE
  ))
  if (!is.null(attr(text, "status"))) NULL else text
}

# whether the code of a repository calls a function of icesRDBES, outside
# comments
calls <- function(text, fun) {
  any(grepl(paste0("\\b", fun, "\\s*\\("), sub("#.*$", "", text)))
}

# the connections of a repository to the RDBES, found in its code. The paths
# are those of its files: a repository can hold several TAF projects in
# folders, as by expert group, each with its own boot/DATA.bib, output.R and
# report.R.
#
# - download: the scripts of the data in boot/DATA.bib with source = {script}
#   that call icesRDBES::rdbes_download_data()
# - upload: output.R and report.R, or the scripts they source, that call
#   icesRDBES::rdbes_upload_data()
# - sandbox: any of these that call icesRDBES::use_sboxrdbes(), so that they
#   download from or upload to the RDBES sandbox
rdbes_ports <- function(repo, paths) {
  found <- list(download = character(), upload = character(), sandbox = FALSE)
  read <- function(path) {
    text <- gh_file(repo, path)
    if (calls(text, "use_sboxrdbes")) found$sandbox <<- TRUE
    text
  }

  for (bib in paths[grepl("(^|/)boot/DATA[.]bib$", paths)]) {
    base <- sub("boot/DATA[.]bib$", "", bib)
    entries <- strsplit(paste(read(bib), collapse = "\n"), "@")[[1]]
    for (entry in entries) {
      key <- regmatches(entry, regexec("^\\w+\\s*\\{\\s*([^,\\s]+)\\s*,", entry))[[1]][2]
      source <- regmatches(entry, regexec("source\\s*=\\s*\\{\\s*([^}]*?)\\s*\\}", entry))[[1]][2]
      script <- paste0(base, "boot/", key, ".R")
      if (!is.na(key) && source %in% "script" && script %in% paths && calls(read(script), "rdbes_download_data")) {
        found$download <- c(found$download, script)
      }
    }
  }

  # output.R and report.R, and the scripts they source, a few levels down
  todo <- paths[grepl("(^|/)(output|report)[.]R$", paths)]
  bases <- setNames(sub("[^/]*$", "", todo), todo)
  seen <- character()
  while (length(todo) && length(seen) < 50) {
    path <- todo[1]
    todo <- todo[-1]
    if (path %in% seen) next
    seen <- c(seen, path)
    text <- read(path)
    if (calls(text, "rdbes_upload_data")) found$upload <- c(found$upload, path)
    # sourced from the folder of the TAF project, which is where it runs
    sourced <- regmatches(text, regexpr("source\\(\\s*[\"'][^\"']+[\"']", text))
    sourced <- sub("^source\\(\\s*[\"']", "", sub("[\"']$", "", sourced))
    sourced <- sub("^[.]/", "", paste0(bases[[path]], sourced))
    sourced <- sourced[sourced %in% paths & !sourced %in% seen]
    bases[sourced] <- bases[[path]]
    todo <- c(todo, sourced)
  }
  found
}

# the countries with data for each stock of the RDBES workflow: one row for
# each stock, with the countries separated by commas
read_stock_countries <- function(file) {
  x <- read.csv(file, stringsAsFactors = FALSE)
  countries <- strsplit(x$CountriesWithData, "\\s*,\\s*")
  data.frame(stock = rep(x$StockCode, lengths(countries)), country = trimws(unlist(countries)))
}

# the code of the repositories that can be in the RDBES workflow: the national
# estimation, stock coordination and stock assessment repositories, those
# that exist. The code of the national estimation and stock coordination
# repositories is in folders, by expert group or by stock, so all the R
# scripts in them are counted, not only those at the top level with model or
# method in their name. data.R chooses the assessment of each stock.
rdbes_code <- function(repos) {
  rdbes <- jsonlite::fromJSON("../rdbes.json")
  # the countries named, and those with data for a stock
  countries <- sort(union(rdbes$countries, read_stock_countries("../rdbes-stock-countries.csv")$country))
  names <- c(
    sprintf("%d_all_RDBES_%s", rdbes$year, countries),
    sprintf("%d_%s_RDBES_combined", rdbes$year, rdbes$stocks),
    sprintf("%d_%s%s", rdbes$year, rep(rdbes$stocks, each = 2), c("_assessment", ""))
  )
  out <- data.frame(name = intersect(names, repos$name))
  out$code_size <- NA_real_
  out$download <- out$upload <- ""
  out$sandbox <- FALSE
  for (j in seq_len(nrow(out))) {
    # an empty repository has no tree
    tree <- gh_api(sprintf("repos/ices-taf/%s/git/trees/HEAD?recursive=1", out$name[j]))$tree
    blobs <- Filter(function(f) f$type == "blob", tree)
    paths <- vapply(blobs, function(f) f$path, "")
    size <- vapply(blobs, function(f) as.numeric(f$size), 1)
    out$code_size[j] <- sum(size[grepl("[.](r|rmd)$", tolower(paths)) & size > min_script_size])
    ports <- rdbes_ports(out$name[j], paths)
    out$download[j] <- paste(ports$download, collapse = " ")
    out$upload[j] <- paste(ports$upload, collapse = " ")
    out$sandbox[j] <- ports$sandbox
  }
  write.csv(out, "rdbes-code.csv", row.names = FALSE)
}

# ---- templates -------------------------------------------------------------

# the markers of progress of a template, shown on status/templates.qmd. Each
# is looked for in the R scripts whose names match stage: in the name, or in
# the code outside comments. A pattern for the code can be a function of the
# lines of code instead.
template_markers <- list(
  inputs = list(stage = "^(data|utilities)", name = "valid|check", code = "\\bstopifnot\\s*\\(|\\bvalidat\\w*\\s*\\("),
  # more than loading packages, making folders and running other scripts
  model = list(stage = "^model", code = function(code) {
    sum(nzchar(trimws(code)) & !grepl("^\\s*(library|require|mkdir|source)\\s*\\(", code)) >= 3
  }),
  # manage() is the forecast of SPiCT, its management scenarios
  forecast = list(stage = "^model", name = "forecast|stf", code = "\\b(stf|forecast|manage)\\s*\\("),
  plots = list(stage = "^(output|report)", code = "\\b(taf\\.png|png|jpeg|pdf|svg|ggsave)\\s*\\("),
  # plots drawn, whether or not they are saved to a file
  drawn = list(stage = "^(output|report)", code = "\\b(ggplot|plot\\w*|barplot|hist|matplot|image)\\s*\\("),
  tables = list(stage = "^(output|report)", code = "\\b(write\\.taf|write\\.csv|write\\.table)\\s*\\("),
  diagnostics = list(stage = "^(model|output|report)", code = "(?i)retro|residual|mohn|leaveout|hindcast|jitter|runs?test|\\bmase\\b"),
  # a write-up made by the report scripts: Word, or rendered R Markdown or Quarto
  report = list(stage = "^report", code = "\\b(read_docx|render|quarto_render)\\s*\\("),
  sag = list(stage = "", code = "(?i)icesSAG|createSAGxml|sag[^\"']*\\.xml"),
  asd = list(stage = "", code = "(?i)icesASD|asd[^\"']*\\.xml"),
  # an FLStock made as an output, not one the data are read into
  flstock = list(stage = "^output", code = "\\bFLStock\\b|SAM2FLStock|readFLSss3")
)

# the TAF scripts at the top of a repository
taf_scripts <- c("data.R", "model.R", "output.R", "report.R")

# the code of the R scripts of a repository, outside comments. All the
# scripts are looked at, not only those the TAF scripts source(): a template
# can offer scripts for several methods, for the user to choose from.
template_code <- function(repo, paths) {
  r_files <- paths[grepl("\\.R$", paths)]
  owner <- sub("/.*", "", repo)
  name <- sub(".*/", "", repo)
  sapply(r_files, function(path) sub("#.*$", "", gh_file(name, path, owner)), simplify = FALSE)
}

# the first script a marker is found in, or "" if it is in none
find_marker <- function(marker, code) {
  for (path in names(code)) {
    if (!grepl(marker$stage, basename(path))) next
    if (!is.null(marker$name) && grepl(marker$name, basename(path), ignore.case = TRUE)) return(path)
    found <- if (is.function(marker$code)) marker$code(code[[path]]) else any(grepl(marker$code, code[[path]], perl = TRUE))
    if (found) return(path)
  }
  ""
}

# the latest run of the QC workflow of a repository, one with qc in its name
qc_run <- function(repo) {
  runs <- gh_api(sprintf("repos/%s/actions/runs?per_page=50", repo))$workflow_runs
  runs <- Filter(function(r) grepl("qc", basename(r$path), ignore.case = TRUE), runs)
  if (length(runs) == 0) return(list(conclusion = NA_character_, url = NA_character_))
  list(conclusion = if (is.null(runs[[1]]$conclusion)) "in progress" else runs[[1]]$conclusion, url = runs[[1]]$html_url)
}

# the markers of progress of the code of a template, or of one of its methods:
# the markers found, and the level. Level 1: the TAF scripts (for a method,
# its own data, model, output and report scripts, such as model_chr.R), with
# some modelling. Level 2: also passes the QC workflow of the repository.
template_progress <- function(code, paths, qc, scripts = taf_scripts) {
  markers <- vapply(template_markers, find_marker, "", code = code)
  level <- if (all(scripts %in% paths) && nzchar(markers[["model"]])) {
    if (qc$conclusion %in% "success") "2" else "1"
  } else ""
  c(level = level, markers)
}

# the templates of ../templates.csv, with the files in the repository of
# each, separated by semicolons, and where each marker of progress is found. A
# template with no repository yet is planned. A template with several methods
# (methods, separated by semicolons) is followed by a row for each, with the
# markers found in the scripts of that method, those with _<method> in their
# name.
template_repos <- function() {
  x <- read.csv("../templates.csv", stringsAsFactors = FALSE, na.strings = "")
  info <- lapply(x$repo, function(repo) if (is.na(repo)) NULL else gh_api(paste0("repos/", repo)))
  field <- function(name) {
    vapply(info, function(r) if (is.null(r)) NA_character_ else r[[name]], "")
  }
  x$url <- field("html_url")
  x$branch <- field("default_branch")
  x$pushed <- field("pushed_at")
  rows <- lapply(seq_len(nrow(x)), function(i) {
    row <- x[i, setdiff(names(x), "methods")]
    row$method <- NA_character_
    none <- c(files = NA_character_, qc = NA_character_, qc_url = NA_character_, level = NA_character_,
      vapply(template_markers, function(m) NA_character_, ""))
    if (is.na(x$branch[i])) return(cbind(row, t(none)))
    tree <- gh_api(sprintf("repos/%s/git/trees/%s?recursive=1", x$repo[i], x$branch[i]))
    if (is.null(tree)) return(cbind(row, t(none)))
    paths <- vapply(Filter(function(f) f$type == "blob", tree$tree), function(f) f$path, "")
    code <- template_code(x$repo[i], paths)
    qc <- qc_run(x$repo[i])
    common <- c(files = paste(paths, collapse = ";"), qc = qc$conclusion, qc_url = qc$url)
    template <- cbind(row, t(c(common, template_progress(code, paths, qc))))
    methods <- if (is.na(x$methods[i])) character() else strsplit(x$methods[i], ";")[[1]]
    by_method <- lapply(methods, function(method) {
      own <- grepl(paste0("_", method, "\\.R$"), basename(names(code)))
      scripts <- paste0(sub("\\.R$", "", taf_scripts), "_", method, ".R")
      row$method <- method
      cbind(row, t(c(common, template_progress(code[own], paths, qc, scripts))))
    })
    do.call(rbind, c(list(template), by_method))
  })
  write.csv(do.call(rbind, rows), "templates.csv", row.names = FALSE, na = "")
}

# ---- read ------------------------------------------------------------------

result <- tryCatch({
  repos <- org_repos("ices-taf")
  write.csv(repos, "ices-taf.csv", row.names = FALSE)
  write.csv(org_repos("ices-advice"), "ices-advice.csv", row.names = FALSE)

  # the model scripts are needed for the repositories named for a year and
  # the management strategy evaluations; data.R chooses among them
  cached_model_scripts(repos[grepl("^[0-9]{4}_", repos$name) | grepl(mse_reg, repos$name, ignore.case = TRUE), ])
  # the repositories named for a year, and what they take data from
  data_sources(repos[grepl("^[0-9]{4}_", repos$name), ])
  rdbes_code(repos)
  template_repos()
}, error = function(e) e)

if (inherits(result, "error")) {
  # never publish a site without status data
  if (nzchar(Sys.getenv("CI"))) stop(result)
  warning("Could not read the repositories from GitHub: ", conditionMessage(result))
  unlink(dir())
}
