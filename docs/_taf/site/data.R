# The tables read by the pages, from what the boot scripts read.
#
# data/issues.csv: the issues, for report.R.
# data/repos.csv: the repositories named for a year and those of the
#   workshops, for status/index.qmd and report.R.
# data/taf-repos.csv, other-repos.csv: the TAF stock assessment repositories
#   and those of some other types of work, with their TAF readiness level,
#   for status/readiness.qmd.
# data/stocks.csv: the state of each stock in each year, for
#   status/stock-assessments.qmd.
# data/advice-sheets.csv: the current advice sheet of each stock and year,
#   with what can be said of its links, for status/advice-sheets.qmd.
# data/rdbes-repos.csv: the repositories of the RDBES workflow, for
#   status/rdbes.qmd.
# data/templates.csv: the TAF templates, for status/templates.qmd.
# data/data-sources.csv, and the files edited by hand that the pages read:
#   copied as they are.
#
# The stock assessment statistics follow model.R in
# https://github.com/ices-taf/shiny_taf-dashboard

library(TAF)
source("utilities.R")

mkdir("data")

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

# the advice sheets and the GitHub links in them, read by _taf/advice
sheets_first_year <- 2024
# what can be said of the link in an advice sheet, the most serious first
link_statuses <- c(
  "Links to ices-taf", "Links elsewhere", "Wrong repository",
  "Repository not public", "Malformed link", "OK"
)

# a repository without a SOFTWARE.bib file fails these two checks together,
# which still counts as passing the QC test. Failing only the second means
# the file is there but not valid, which does not.
qc_no_software_bib <- c("qc.software.bib.exists", "qc.software.bib.valid")

# ---- copied as they are ----------------------------------------------------

# the issues, which github_issues.R could not read if they are not there
if (file.exists(taf.data.path("github_issues", "issues.csv"))) {
  cp(taf.data.path("github_issues", "issues.csv"), "data")
}

# edited by hand, read by status/rdbes.qmd, status/stock-repos.qmd and
# status/templates.qmd
for (file in c("rdbes.json", "rdbes-stock-countries.csv", "repo-types.json", "template-markers.csv")) {
  cp(taf.data.path(file), "data")
}

# ---- helpers ---------------------------------------------------------------

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

# the model scripts of the repositories, as boot/github_repos.R found them:
# the size of model.R and of all the model scripts of each
model_scripts <- function(repos) {
  models <- read.csv(taf.data.path("github_repos", "model-scripts.csv"), stringsAsFactors = FALSE)
  models[match(repos$name, models$name), c("model_r", "model_size")]
}

# the QC test result for each repository: whether it was tested, and whether
# it passed, either outright or failing only for having no SOFTWARE.bib file
qc_results <- function(names) {
  qc <- jsonlite::fromJSON(taf.data.path("qc", "master.json"), simplifyVector = FALSE)
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
  sheets <- read.csv(taf.data.path("previous", "advice-sheets.csv"), colClasses = "character", na.strings = "NA")
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

# the repositories of the RDBES workflow: the national estimation, stock
# coordination and stock assessment repositories, with what is found of
# their connections to the RDBES in their code (by boot/github_repos.R), and
# the TAF readiness level of the first two. The code of the national
# estimation and stock coordination repositories is in folders, by expert
# group or by stock, so all the R scripts in them are looked at for the
# level, not only those at the top level with model or method in their name.
rdbes_repos <- function(repos) {
  rdbes <- jsonlite::fromJSON(taf.data.path("rdbes.json"))
  # the countries named, and those with data for a stock
  rdbes$countries <- sort(union(rdbes$countries, read_stock_countries(taf.data.path("rdbes-stock-countries.csv"))$country))
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
  code <- read.csv(
    taf.data.path("github_repos", "rdbes-code.csv"),
    colClasses = c(name = "character", download = "character", upload = "character")
  )
  j <- match(out$name, code$name)
  out$code_size <- code$code_size[j]
  out$upload <- ifelse(is.na(j), "", code$upload[j])
  out$download <- ifelse(is.na(j), "", code$download[j])
  out$sandbox <- !is.na(j) & code$sandbox[j] %in% TRUE
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

category <- function(data_category) {
  x <- floor(suppressWarnings(as.numeric(data_category)))
  ifelse(x < 2, "Category 1", "Categories 2-6")
}

# ---- build -----------------------------------------------------------------

build <- function() {
  # valid stock codes
  stock_codes <- read.csv(taf.data.path("ices", "stock-codes.csv"), stringsAsFactors = FALSE)$stock

  # all repositories
  repos <- add_name_parts(read.csv(taf.data.path("github_repos", "ices-taf.csv"), stringsAsFactors = FALSE))
  mse <- grepl(mse_reg, repos$name, ignore.case = TRUE)
  repos$type[mse] <- "mse"
  repos$year[mse] <- as.numeric(substr(repos$pushed[mse], 1, 4))
  advice <- add_name_parts(read.csv(taf.data.path("github_repos", "ices-advice.csv"), stringsAsFactors = FALSE))

  # TAF readiness level, see system/readiness-level.qmd
  with_level <- function(x) {
    # level 1: the code is on GitHub, with more in the model scripts than the
    # TAF skeleton
    x <- cbind(x, model_scripts(x))
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
    "data/other-repos.csv",
    row.names = FALSE
  )
  # the RDBES workflow. Read by status/rdbes.qmd
  write.csv(rdbes_repos(repos), "data/rdbes-repos.csv", row.names = FALSE)

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
    "data/advice-sheets.csv",
    row.names = FALSE
  )
  i <- match(paste(taf_repos$stock, taf_repos$year), paste(sheets$stock, sheets$year))
  taf_repos$advice_url <- sheets$url[i]
  taf_repos$advice_status <- sheets$status[i]
  message(nrow(sheets), " advice sheets, ", sum(sheets$status == "OK"), " with a link that is OK")

  # read by status/readiness.qmd
  write.csv(
    taf_repos[
      order(taf_repos$year, taf_repos$stock),
      c("name", "year", "stock", "part", "visibility", "size", "pushed", "model_r", "model_size", "has_model", "qc_tested", "qc_passed", "qc_failed", "qc_date", "trl", "published", "advice_url", "advice_status")
    ],
    "data/taf-repos.csv",
    row.names = FALSE
  )
  message(
    sum(taf_repos$has_model), " at readiness level 1 or above, ",
    sum(taf_repos$trl %in% 2), " at level 2"
  )

  # stock list
  sd <- read.csv(taf.data.path("ices", "stock-list.csv"), stringsAsFactors = FALSE)
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
    "data/stocks.csv",
    row.names = FALSE
  )

  # status/index.qmd is of the repositories of TAF work: those named for a
  # year and those of the workshops. The others in ices-taf, such as the
  # software and the templates, are left out. Read by the R code in
  # status/index.qmd, and by report.R for the summary
  repos <- repos[grepl(listed_reg, repos$name, ignore.case = TRUE), ]
  write.csv(
    repos[order(repos$pushed, decreasing = TRUE), c("name", "url", "visibility", "year", "type", "pushed")],
    "data/repos.csv",
    row.names = FALSE
  )

  # read by status/templates.qmd and status/stock-repos.qmd, as
  # boot/github_repos.R found them
  cp(taf.data.path("github_repos", "templates.csv"), "data")
  cp(taf.data.path("github_repos", "data-sources.csv"), "data")
}

# github_repos.R and ices.R write nothing if they could not read their data, which only happens when not run in CI: report.R then writes
# a note in place of the repository status
result <- tryCatch(build(), error = function(e) e)

if (inherits(result, "error")) {
  # never publish a site without status data
  if (nzchar(Sys.getenv("CI"))) stop(result)
  warning("Could not build the repository status: ", conditionMessage(result))
  unlink("data/repos.csv")
}
