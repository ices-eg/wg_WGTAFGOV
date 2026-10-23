# The issues of the working group, for the progress fragments included by
# index.qmd, progress.qmd and planning/workstreams.qmd.
#
# Reads issues with the GitHub CLI, so needs `gh` to be installed and
# authenticated (locally: `gh auth login`, in GitHub Actions: GH_TOKEN, or
# ISSUES_TOKEN for this script alone).
#
# issues.csv: one row for each issue, without pull requests, with its labels
# separated by semicolons.

repo <- Sys.getenv("GITHUB_REPOSITORY", "ices-eg/wg_WGTAFGOV")

gh_api <- function(args) {
  json <- system2("gh", c("api", args), stdout = TRUE)
  if (!is.null(attr(json, "status"))) {
    stop("`gh api ", paste(args, collapse = " "), "` failed")
  }
  jsonlite::fromJSON(paste(json, collapse = "\n"), simplifyVector = FALSE)
}

# the number of issues, without pull requests, to know when all are read
count_issues <- function() {
  owner_name <- strsplit(repo, "/", fixed = TRUE)[[1]]
  query <- sprintf(
    "{repository(owner:\"%s\",name:\"%s\"){issues{totalCount}}}",
    owner_name[1], owner_name[2]
  )
  gh_api(c("graphql", "-f", shQuote(paste0("query=", query))))$data$repository$issues$totalCount
}

fetch_issues <- function() {
  n <- count_issues()
  all <- list()
  page <- 1
  tries <- 0
  # GitHub at times answers a page with an empty list and no link to the
  # next, so an empty page is only the end once all the issues are read
  while (length(all) < n) {
    endpoint <- sprintf(
      "repos/%s/issues?state=all&per_page=100&page=%d", repo, page
    )
    res <- gh_api(shQuote(endpoint))
    if (length(res) == 0) {
      tries <- tries + 1
      if (tries > 5) stop("read ", length(all), " of ", n, " issues from GitHub")
      Sys.sleep(tries)
      next
    }
    # the issues endpoint also returns pull requests
    all <- c(all, Filter(function(i) is.null(i$pull_request), res))
    page <- page + 1
    tries <- 0
  }

  data.frame(
    number = vapply(all, function(i) i$number, 1L),
    title = vapply(all, function(i) i$title, ""),
    state = vapply(all, function(i) i$state, ""),
    url = vapply(all, function(i) i$html_url, ""),
    created = vapply(all, function(i) i$created_at, ""),
    updated = vapply(all, function(i) i$updated_at, ""),
    closed = vapply(all, function(i) if (is.null(i$closed_at)) NA_character_ else i$closed_at, ""),
    labels = vapply(all, function(i) paste(vapply(i$labels, function(l) l$name, ""), collapse = ";"), ""),
    stringsAsFactors = FALSE
  )
}

# the token for the issues, when it is not the one for the repositories
gh_token <- Sys.getenv("GH_TOKEN", NA)
if (nzchar(Sys.getenv("ISSUES_TOKEN"))) Sys.setenv(GH_TOKEN = Sys.getenv("ISSUES_TOKEN"))
issues <- tryCatch(fetch_issues(), error = function(e) e)
if (is.na(gh_token)) Sys.unsetenv("GH_TOKEN") else Sys.setenv(GH_TOKEN = gh_token)

if (inherits(issues, "error")) {
  # never publish a site without progress data
  if (nzchar(Sys.getenv("CI"))) stop(issues)
  # report.R writes a note in place of the progress fragments
  warning("Could not read issues from GitHub: ", conditionMessage(issues))
} else {
  write.csv(issues, "issues.csv", row.names = FALSE)
}
