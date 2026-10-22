# Build the fragments included by the pages in status/.
#
# Run from the docs/ folder before `quarto render`, like progress.R.
#
# Reads the repository lists of the ices-taf and ices-advice organisations
# with the GitHub CLI, the stock codes from the ICES vocabulary (icesVocab),
# the QC results from the taf-qc-dashboard repository, the stock list from
# the ICES Stock Database and the advice sheets from _data/advice-sheets.csv.
#
# Looking at the files of each repository is slow, so what is found is kept in
# _data/model-scripts.csv, which is committed, and only the repositories
# pushed to since are looked at again. The UpdateAdvice workflow commits it once
# a week; in between, a build reads the few that have changed.
#
# Most
# ices-taf repositories are private, so `gh` must be authenticated as someone
# who can read them (in GitHub Actions: the TAF_STATUS_TOKEN secret), otherwise
# only the public repositories are counted.
#
# The names of the private repositories are published in the tables, but
# nothing of what is in them.
#
# The stock assessment statistics follow model.R in
# https://github.com/ices-taf/shiny_taf-dashboard

out_dir <- "_generated"
first_year <- 2018
sd_url <- paste0(
  "https://sd.ices.dk/services/odata4/StockListDWs4",
  "?$filter=ActiveYear%20ge%20", first_year,
  "&$select=StockKeyLabel,ActiveYear,ExpertGroup,DataCategory"
)

# <year>_<stock code>, e.g. 2024_cod.27.47d20
tafreg <- "^[0-9]{4}_[a-z]{3}[.]"

# a TAF stock assessment repository: <year>_<stock code>, optionally followed
# by a part of the stock (e.g. ane.27.9a_west) and by _assessment
assessment_reg <- "^([0-9]{4})_(.+?)(_assessment)?$"
stock_parts <- c("west", "south")

# an untouched skeleton model.R is 94 bytes of comments and setup
min_model_size <- 100

# the repositories on the repositories page: named for a year, or wk_WK...
listed_reg <- "^[0-9]{4}_|^wk_wk"

# stock codes as they are written in the names of some repositories, and the
# stock code that is meant
stock_aliases <- c("cod.27.1-2coastN" = "cod.27.1-2.coastN")

# the repositories of the management strategy evaluations, which are not named
# for a year: wk_WKMSE..., wk_WKNSMSE... and WGHANSA_... They are given the
# type mse and the year they were last pushed to.
mse_reg <- "^(wk_wk(ns)?mse|wghansa_)"

# other types of work to give a TAF readiness level to, in the groups they are
# shown in. A repository named <year>_<name>_<type> is in a group by its type,
# by the last part of its name, or by all of its name after the year.
other_types <- list(
  "Mixed fisheries advice" = c("MixedFisheriesAdvice", "wgmixfish_accessions", "wgmixfish_methods"),
  "Benchmarks" = "benchmark",
  "Reference points" = c("newref", "wkmfoa", "rebuild", "refpts", "mse"),
  "Surveys" = c("survey", "acousticIndex", "stox")
)

# the model scripts of each repository, kept from one run to the next so that
# only the repositories pushed to since are read again
models_file <- "_data/model-scripts.csv"

# the advice sheets and the GitHub links in them, updated by _scripts/advice.R
sheets_file <- "_data/advice-sheets.csv"
sheets_first_year <- 2024
# what can be said of the link in an advice sheet, the most serious first
link_statuses <- c(
  "Links to ices-taf", "Links elsewhere", "Wrong repository",
  "Repository not public", "Malformed link", "OK"
)

# QC results, one entry for each repository tested, named ices-taf_<name>
qc_url <- "https://raw.githubusercontent.com/ices-tools-prod/taf-qc-dashboard/main/json/master.json"
# a repository without a SOFTWARE.bib file fails these two checks together,
# which still counts as passing the QC test. Failing only the second means
# the file is there but not valid, which does not.
qc_no_software_bib <- c("qc.software.bib.exists", "qc.software.bib.valid")

# the repositories that each repository takes data from, kept from one run to
# the next like the model scripts. Read by status/stock-repos.qmd
sources_file <- "_data/data-sources.csv"

# the countries and stocks of the RDBES workflow, see status/rdbes.qmd
rdbes_file <- "_data/rdbes.json"
stock_countries_file <- "_data/rdbes-stock-countries.csv"

# the TAF templates being developed, with their repositories in ices-tools-dev
# or ices-tools-prod and their issues on the project board, see
# status/templates.qmd
templates_file <- "_data/templates.csv"
# a new TAF repository starts with R scripts of about 100 bytes: one larger
# than this has some code in it
min_script_size <- 150

dir.create(out_dir, showWarnings = FALSE)

fragments <- c("status-summary")

# ---- helpers ---------------------------------------------------------------

write_html <- function(name, html) {
  writeLines(
    c("```{=html}", html, "```"),
    file.path(out_dir, paste0(name, ".md"))
  )
}

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

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

# year and stock code from a repository name
add_name_parts <- function(repos) {
  parts <- strsplit(repos$name, "_")
  dated <- grepl("^[0-9]{4}_", repos$name)
  repos$year <- ifelse(dated, suppressWarnings(as.numeric(substring(repos$name, 1, 4))), NA)
  repos$stock <- ifelse(
    grepl(tafreg, repos$name),
    vapply(parts, function(x) if (length(x) > 1) x[2] else NA_character_, ""),
    NA
  )
  repos$type <- vapply(parts, function(x) {
    # the type follows the part of the stock, if there is one
    if (length(x) > 2 && x[3] %in% stock_parts) x <- x[-3]
    if (length(x) > 2) sub("-.*$", "", x[3]) else NA_character_
  }, "")
  repos$type[is.na(repos$type) & !is.na(repos$stock)] <- "assessment"
  repos$type[!dated] <- NA
  repos
}

# the TAF stock assessment repositories (see assessment_reg), where the stock
# code is in the ICES_StockCode vocabulary
taf_assessments <- function(repos, stock_codes) {
  parts <- regmatches(repos$name, regexec(assessment_reg, repos$name, perl = TRUE))
  shaped <- lengths(parts) == 4
  repos <- repos[shaped, ]
  repos$year <- as.numeric(vapply(parts[shaped], "[", "", 2))
  repos$stock <- vapply(parts[shaped], "[", "", 3)
  repos$type <- "assessment"
  # a stock code that is written differently in the name of a repository
  alias <- repos$stock %in% names(stock_aliases)
  repos$stock[alias] <- stock_aliases[repos$stock[alias]]

  # split off the part of the stock, if that leaves a valid stock code
  part_reg <- paste0("_(", paste(stock_parts, collapse = "|"), ")$")
  has_part <- !repos$stock %in% stock_codes &
    sub(part_reg, "", repos$stock) %in% stock_codes
  repos$part <- ifelse(has_part, sub("^.*_", "", repos$stock), NA)
  repos$stock[has_part] <- sub(part_reg, "", repos$stock[has_part])

  repos <- repos[repos$stock %in% stock_codes, ]
  message(nrow(repos), " TAF stock assessment repositories")
  repos
}

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
  known <- if (file.exists(models_file)) {
    read.csv(models_file, stringsAsFactors = FALSE)
  } else {
    data.frame(name = character(), pushed = character(), model_r = numeric(), model_size = numeric())
  }
  current <- merge(repos[c("name", "pushed")], known, all.x = TRUE, sort = FALSE)
  # nothing is recorded for a repository that has not been read
  stale <- !paste(current$name, current$pushed) %in% paste(known$name, known$pushed)
  message(sum(stale), " of ", nrow(current), " repositories to read the model scripts of")
  if (any(stale)) {
    current[stale, c("model_r", "model_size")] <- model_scripts(current$name[stale])
    dir.create(dirname(models_file), showWarnings = FALSE)
    # keep what is known of repositories that cannot be seen this time, as
    # when run without access to the private ones
    kept <- rbind(current, known[!known$name %in% current$name, names(current)])
    write.csv(kept[order(kept$name), ], models_file, row.names = FALSE)
  }
  current[match(repos$name, current$name), c("model_r", "model_size")]
}

# the QC test result for each repository: whether it was tested, and whether
# it passed, either outright or failing only for having no SOFTWARE.bib file
qc_results <- function(names) {
  qc <- jsonlite::fromJSON(qc_url, simplifyVector = FALSE)
  # a failed check with no name is still a failure
  failed <- lapply(qc, function(x) {
    vapply(x$failed_checks, function(check) if (is.null(check)) NA_character_ else check, "")
  })
  passed <- mapply(function(x, failed) {
    isTRUE(x$passed) || setequal(failed, qc_no_software_bib)
  }, qc, failed)
  i <- match(names, sub("^ices-taf_", "", vapply(qc, function(x) x$repository, "")))
  data.frame(
    qc_tested = !is.na(i),
    qc_passed = passed[i],
    # the checks that failed, without the qc. they all start with. Having no
    # SOFTWARE.bib file is not counted as a failure, so its two checks are
    # left out; the second on its own is of a file that is there but not valid.
    qc_failed = vapply(failed, function(x) {
      if ("qc.software.bib.exists" %in% x) x <- x[!x %in% qc_no_software_bib]
      paste(sub("^qc[.]", "", ifelse(is.na(x), "unnamed check", x)), collapse = ", ")
    }, "")[i],
    qc_date = substr(vapply(qc, function(x) x$timestamp, ""), 1, 10)[i]
  )
}

# the current advice sheet of each stock and year, with what can be said of
# the links to GitHub in it. The published code of a stock is in a public
# repository named <year>_<stock code> in ices-advice, so that is where the
# sheet should link to: ices-taf is where the work is done, and is private.
advice_sheets <- function(advice_repos) {
  sheets <- read.csv(sheets_file, colClasses = "character", na.strings = "NA")
  sheets$year <- as.numeric(sheets$year)
  sheets <- sheets[sheets$year %in% sheets_first_year:9999 & !is.na(sheets$stock), ]
  # a sheet can be replaced during the year: keep the latest
  sheets <- sheets[order(sheets$published, sheets$id), ]
  sheets <- sheets[!duplicated(sheets[c("stock", "year")], fromLast = TRUE), ]

  advice_repos <- advice_repos[advice_repos$visibility == "public", ]
  public <- tolower(advice_repos$name)
  # the repository of a stock and year: the one named for them, or failing
  # that the first that starts with them
  repository <- function(stock, year) {
    name <- tolower(paste0(year, "_", stock))
    i <- c(which(public == name), which(startsWith(public, name)))
    if (length(i)) advice_repos$url[i[1]] else NA_character_
  }

  link_status <- function(link, stock, year) {
    # the organisation, and the name of the repository as it is written
    part <- regmatches(link, regexec("github[.]com/(ices-[A-Za-z-]+)/?([^/?#]*)", link, ignore.case = TRUE))[[1]]
    org <- tolower(part[2])
    repo <- tolower(sub("[.]git$", "", part[3]))
    expected <- tolower(paste0(year, "_", stock))
    if (org %in% "ices-taf") {
      "Links to ices-taf"
    } else if (!org %in% "ices-advice") {
      "Links elsewhere"
    } else if (!grepl("^[a-z0-9_.-]+$", repo)) {
      # no repository named, or characters that cannot be in a name
      "Malformed link"
    } else if (!startsWith(repo, expected)) {
      # another year, another stock, or a mistyped name
      "Wrong repository"
    } else if (!repo %in% public) {
      # the right start to the name, but not a repository that can be seen:
      # mistyped if the stock and year do have one, otherwise private or
      # not there
      if (is.na(repository(stock, year))) "Repository not public" else "Wrong repository"
    } else if (!grepl("^https?://(www[.])?github[.]com/", link, ignore.case = TRUE)) {
      "Malformed link"
    } else {
      "OK"
    }
  }

  sheets$status <- vapply(seq_len(nrow(sheets)), function(i) {
    if (is.na(sheets$links[i])) {
      return("Not checked")
    }
    links <- strsplit(sheets$links[i], " | ", fixed = TRUE)[[1]]
    if (!length(links)) {
      # nothing linked: is there something that should have been?
      has_repo <- !is.na(repository(sheets$stock[i], sheets$year[i]))
      return(if (has_repo) "Repository not linked" else "No link")
    }
    status <- vapply(links, link_status, "", stock = sheets$stock[i], year = sheets$year[i])
    link_statuses[min(match(status, link_statuses))]
  }, "")
  # the repository that a sheet should link to, where it does not
  sheets$repository <- NA_character_
  unlinked <- sheets$status %in% c("Repository not linked", "Wrong repository")
  sheets$repository[unlinked] <- mapply(repository, sheets$stock[unlinked], sheets$year[unlinked])
  sheets
}

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

# the repositories of the RDBES workflow: the national estimation, stock
# coordination and stock assessment repositories, with what is found of
# their connections to the RDBES in their code, and the TAF readiness level
# of the first two. The code of the national estimation and stock
# coordination repositories is in folders, by expert group or by stock, so
# all the R scripts in them are looked at for the level, not only those at
# the top level with model or method in their name.
# the countries with data for each stock of the RDBES workflow: one row for
# each stock, with the countries separated by commas
read_stock_countries <- function(file) {
  x <- read.csv(file, stringsAsFactors = FALSE)
  countries <- strsplit(x$CountriesWithData, "\\s*,\\s*")
  data.frame(stock = rep(x$StockCode, lengths(countries)), country = trimws(unlist(countries)))
}

rdbes_repos <- function(repos) {
  rdbes <- jsonlite::fromJSON(rdbes_file)
  # the countries named, and those with data for a stock
  rdbes$countries <- sort(union(rdbes$countries, read_stock_countries(stock_countries_file)$country))
  # the assessment of a stock: <year>_<stock>_assessment, or failing that
  # <year>_<stock>, as on the readiness page
  assessment <- vapply(rdbes$stocks, function(stock) {
    candidates <- sprintf("%d_%s%s", rdbes$year, stock, c("_assessment", ""))
    c(candidates[candidates %in% repos$name], candidates[1])[1]
  }, "")
  out <- data.frame(
    name = c(
      sprintf("%d_all_RDBES_%s", rdbes$year, rdbes$countries),
      sprintf("%d_%s_RDBES_combined", rdbes$year, rdbes$stocks),
      assessment
    ),
    stage = rep(c("national", "coordination", "assessment"), c(length(rdbes$countries), length(rdbes$stocks), length(rdbes$stocks)))
  )
  i <- match(out$name, repos$name)
  out$exists <- !is.na(i)
  out$visibility <- repos$visibility[i]
  out$pushed <- repos$pushed[i]
  out$code_size <- NA_real_
  out$download <- out$upload <- ""
  out$sandbox <- FALSE
  for (j in which(out$exists)) {
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
  out$has_code <- !is.na(out$code_size) & out$code_size > 0
  out$trl <- ifelse(out$has_code, 1, NA)
  out <- cbind(out, qc_results(out$name))
  out$trl[out$has_code & out$qc_passed %in% TRUE] <- 2
  message(
    sum(out$exists), " of ", nrow(out), " RDBES workflow repositories, ",
    sum(nzchar(out$download)), " downloading from the RDBES, ",
    sum(nzchar(out$upload)), " uploading to it"
  )
  out
}

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
  known <- if (file.exists(sources_file)) {
    read.csv(sources_file, stringsAsFactors = FALSE, na.strings = "")
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
    kept <- rbind(current, known[!known$name %in% current$name, names(current)])
    write.csv(kept[order(kept$name), ], sources_file, row.names = FALSE, na = "")
  }
  message(sum(!is.na(current$sources)), " repositories take data from another")
}

category <- function(data_category) {
  x <- floor(suppressWarnings(as.numeric(data_category)))
  ifelse(x < 2, "Category 1", "Categories 2-6")
}

tally <- function(data, by, name) {
  if (nrow(data) == 0) {
    out <- data[by]
    out[[name]] <- integer(0)
    return(out)
  }
  out <- aggregate(rep(1L, nrow(data)), data[by], sum)
  names(out)[ncol(out)] <- name
  out
}

# the templates of templates_file, with the files in the repository of each,
# separated by semicolons. A template with no repository yet is planned.
template_repos <- function() {
  x <- read.csv(templates_file, stringsAsFactors = FALSE, na.strings = "")
  info <- lapply(x$repo, function(repo) if (is.na(repo)) NULL else gh_api(paste0("repos/", repo)))
  field <- function(name) {
    vapply(info, function(r) if (is.null(r)) NA_character_ else r[[name]], "")
  }
  files <- function(repo, branch) {
    if (is.na(branch)) return(NA_character_)
    tree <- gh_api(sprintf("repos/%s/git/trees/%s?recursive=1", repo, branch))
    if (is.null(tree)) return(NA_character_)
    blobs <- Filter(function(f) f$type == "blob", tree$tree)
    paste(vapply(blobs, function(f) f$path, ""), collapse = ";")
  }
  x$url <- field("html_url")
  x$pushed <- field("pushed_at")
  x$files <- unname(mapply(files, x$repo, field("default_branch")))
  x
}

# ---- build -----------------------------------------------------------------

build <- function() {
  # valid stock codes
  stock_codes <- icesVocab::getCodeList("ICES_StockCode")$Key

  # all repositories
  repos <- add_name_parts(org_repos("ices-taf"))
  mse <- grepl(mse_reg, repos$name, ignore.case = TRUE)
  repos$type[mse] <- "mse"
  repos$year[mse] <- as.numeric(substr(repos$pushed[mse], 1, 4))
  advice <- add_name_parts(org_repos("ices-advice"))

  # TAF readiness level, see system/readiness-level.qmd
  with_level <- function(x) {
    # level 1: the code is on GitHub, with more in the model scripts than the
    # TAF skeleton
    x <- cbind(x, cached_model_scripts(x))
    x$has_model <- x$model_size > min_model_size
    x$trl <- ifelse(x$has_model, 1, NA)
    # level 2: at level 1 and passes the QC test. No repository has been
    # tested for level 3 yet.
    x <- cbind(x, qc_results(x$name))
    x$trl[x$has_model & x$qc_passed %in% TRUE] <- 2
    x
  }

  # the TAF stock assessment repositories
  taf_repos <- with_level(taf_assessments(repos, stock_codes))
  # they are all of type assessment, whatever their name: the older stock
  # codes, such as had-iceg, are not seen as a stock by add_name_parts()
  repos$type[repos$name %in% taf_repos$name] <- "assessment"

  # the repositories of some other types of work, by the type in their name.
  # Read by status/readiness.qmd
  group_of <- function(name, type) {
    known_as <- c(type, sub("^.*_", "", name), sub("^[0-9]{4}_", "", name))
    group <- names(other_types)[vapply(other_types, function(x) any(known_as %in% x), TRUE)]
    if (length(group)) group[1] else NA_character_
  }
  dated <- !is.na(repos$year)
  repos$group[dated] <- mapply(group_of, repos$name[dated], repos$type[dated])
  other_repos <- repos[!is.na(repos$group), ]
  # the assessments are not other work, whatever follows in their name
  other_repos <- other_repos[!other_repos$name %in% taf_repos$name, ]
  other_repos <- with_level(other_repos)
  write.csv(
    other_repos[
      order(other_repos$group, other_repos$year, other_repos$name),
      c("name", "year", "type", "group", "visibility", "pushed", "model_size", "has_model", "qc_tested", "qc_passed", "qc_failed", "trl")
    ],
    file.path(out_dir, "other-repos.csv"),
    row.names = FALSE
  )
  # the RDBES workflow. Read by status/rdbes.qmd
  write.csv(rdbes_repos(repos), file.path(out_dir, "rdbes-repos.csv"), row.names = FALSE)

  # published: the stock has a public repository for the year in ices-advice.
  # A private one is not counted: the readers of the advice cannot open it,
  # and it can only be seen by a run with access to it. This is recorded
  # separately from the level, as level 4 can only follow level 3.
  public <- advice$name[advice$visibility == "public"]
  taf_repos$published <-
    sub("_assessment$", "", taf_repos$name) %in% public |
    paste(taf_repos$year, taf_repos$stock, sep = "_") %in% public

  # the advice sheet of each stock and year. Read by status/advice-sheets.qmd
  sheets <- advice_sheets(advice)
  write.csv(
    sheets[order(sheets$year, sheets$stock), c("year", "stock", "title", "published", "doi", "url", "links", "status", "repository")],
    file.path(out_dir, "advice-sheets.csv"),
    row.names = FALSE
  )
  i <- match(paste(taf_repos$stock, taf_repos$year), paste(sheets$stock, sheets$year))
  taf_repos$advice_url <- sheets$url[i]
  taf_repos$advice_status <- sheets$status[i]
  message(nrow(sheets), " advice sheets, ", sum(sheets$status == "OK"), " with a link that is OK")

  # read by status/readiness.qmd, and for checking locally: _generated/ is
  # not committed or published
  write.csv(
    taf_repos[
      order(taf_repos$year, taf_repos$stock),
      c("name", "year", "stock", "part", "visibility", "size", "pushed", "model_r", "model_size", "has_model", "qc_tested", "qc_passed", "qc_failed", "qc_date", "trl", "published", "advice_url", "advice_status")
    ],
    file.path(out_dir, "taf-repos.csv"),
    row.names = FALSE
  )
  message(
    sum(taf_repos$has_model), " at readiness level 1 or above, ",
    sum(taf_repos$trl %in% 2), " at level 2"
  )

  # stock list
  sd <- jsonlite::fromJSON(sd_url)$value
  sd <- unique(data.frame(
    stock = sd$StockKeyLabel,
    year = sd$ActiveYear,
    eg = sd$ExpertGroup,
    category = category(sd$DataCategory),
    stringsAsFactors = FALSE
  ))
  sd <- sd[!is.na(sd$category), ]

  # the state of each stock in each year: the highest level of its
  # repositories (0 for a repository with no level, NA for no repository) and
  # whether it is published. Read by status/stock-assessments.qmd
  taf_repos$level <- ifelse(is.na(taf_repos$trl), 0, taf_repos$trl)
  state <- aggregate(cbind(level, published) ~ stock + year, taf_repos, max)
  stocks <- merge(sd, state, all.x = TRUE)
  stocks$published <- stocks$published %in% 1
  write.csv(
    stocks[order(stocks$year, stocks$eg, stocks$stock), ],
    file.path(out_dir, "stocks.csv"),
    row.names = FALSE
  )

  # ---- the repositories page -----------------------------------------------

  # status/index.qmd is of the repositories of TAF work: those named for a
  # year and those of the workshops. The others in ices-taf, such as the
  # software and the templates, are left out.
  repos <- repos[grepl(listed_reg, repos$name, ignore.case = TRUE), ]

  n_private <- sum(repos$visibility != "public")
  n_active <- sum(as.Date(substr(repos$pushed, 1, 10)) > Sys.Date() - 365, na.rm = TRUE)
  tile <- function(value, label) {
    sprintf(
      "<div class=\"taf-tile\"><div class=\"taf-tile-value\">%s</div><div class=\"taf-tile-label\">%s</div></div>",
      format(value, big.mark = ","), label
    )
  }
  write_html("status-summary", c(
    if (n_private == 0) {
      paste0(
        "<div class=\"callout callout-style-simple callout-warning\"><div class=\"callout-body\">",
        "<p>Only public repositories could be read when this page was built, ",
        "so the figures leave out the private repositories in ices-taf.</p></div></div>"
      )
    },
    "<div class=\"taf-tiles\">",
    tile(nrow(repos), "repositories"),
    tile(nrow(repos) - n_private, "public"),
    tile(n_private, "private"),
    tile(n_active, "updated in the last 12 months"),
    "</div>",
    sprintf(
      "<p class=\"taf-updated\">Repository status updated %s.</p>",
      format(Sys.time(), "%e %B %Y, %H:%M UTC", tz = "UTC")
    )
  ))

  # read by status/templates.qmd
  write.csv(template_repos(), file.path(out_dir, "templates.csv"), row.names = FALSE, na = "")

  # ---- all repositories ----------------------------------------------------

  # the repositories named for a year, and what they take data from
  data_sources(repos[grepl("^[0-9]{4}_", repos$name), ])

  # read by the R code in status/index.qmd
  write.csv(
    repos[order(repos$pushed, decreasing = TRUE), c("name", "url", "visibility", "year", "type", "pushed")],
    file.path(out_dir, "repos.csv"),
    row.names = FALSE
  )
}

result <- tryCatch(build(), error = function(e) e)

if (inherits(result, "error")) {
  # never publish a site without status data
  if (nzchar(Sys.getenv("CI"))) stop(result)
  warning("Could not build the repository status: ", conditionMessage(result))
  msg <- "<p><em>Repository status is not available: the data could not be read when this page was built.</em></p>"
  for (name in fragments) write_html(name, msg)
}
