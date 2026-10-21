# Build the progress fragments included by index.qmd, progress.qmd and
# planning/workstreams.qmd.
#
# Run from the docs/ folder before `quarto render`; Quarto resolves includes
# before any pre-render step, so this cannot be a pre-render script.
# Reads issues with the GitHub CLI, so needs `gh` to be installed and
# authenticated (locally: `gh auth login`, in GitHub Actions: GH_TOKEN).

repo <- Sys.getenv("GITHUB_REPOSITORY", "ices-eg/wg_WGTAFGOV")
out_dir <- "_generated"
n_recent <- 15
n_sprint <- 8

sprints <- c("development", "governance", "strategy", "templates")

dir.create(out_dir, showWarnings = FALSE)

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

issues_url <- function(query) {
  paste0(
    "https://github.com/", repo, "/issues?q=",
    utils::URLencode(paste("is:issue", query), reserved = TRUE)
  )
}

pct <- function(closed, total) {
  if (total == 0) 0 else round(100 * closed / total)
}

bar <- function(closed, total, label) {
  p <- pct(closed, total)
  sprintf(
    paste0(
      "<div class=\"progress\" role=\"progressbar\" aria-label=\"%s\" ",
      "aria-valuenow=\"%d\" aria-valuemin=\"0\" aria-valuemax=\"100\">",
      "<div class=\"progress-bar\" style=\"width: %d%%\"></div></div>"
    ),
    esc(label), p, p
  )
}

issue_list <- function(x, date_field = NULL) {
  if (nrow(x) == 0) {
    return("<p><em>None.</em></p>")
  }
  date <- if (is.null(date_field)) "" else paste0(" <span class=\"taf-updated\">", substr(x[[date_field]], 1, 10), "</span>")
  c(
    "<ul class=\"taf-issues\">",
    sprintf(
      "<li><a href=\"%s\">#%d</a> %s%s</li>",
      x$url, x$number, esc(x$title), date
    ),
    "</ul>"
  )
}

# ---- fetch -----------------------------------------------------------------

fetch_issues <- function() {
  all <- list()
  page <- 1
  repeat {
    endpoint <- sprintf(
      "repos/%s/issues?state=all&per_page=100&page=%d", repo, page
    )
    json <- system2("gh", c("api", shQuote(endpoint)), stdout = TRUE)
    if (!is.null(attr(json, "status"))) {
      stop("`gh api ", endpoint, "` failed")
    }
    res <- jsonlite::fromJSON(paste(json, collapse = "\n"), simplifyVector = FALSE)
    if (length(res) == 0) break
    all <- c(all, res)
    page <- page + 1
  }
  # the issues endpoint also returns pull requests
  all <- Filter(function(i) is.null(i$pull_request), all)

  data <- data.frame(
    number = vapply(all, function(i) i$number, 1L),
    title = vapply(all, function(i) i$title, ""),
    state = vapply(all, function(i) i$state, ""),
    url = vapply(all, function(i) i$html_url, ""),
    created = vapply(all, function(i) i$created_at, ""),
    updated = vapply(all, function(i) i$updated_at, ""),
    closed = vapply(all, function(i) if (is.null(i$closed_at)) NA_character_ else i$closed_at, ""),
    stringsAsFactors = FALSE
  )
  data$labels <- lapply(all, function(i) vapply(i$labels, function(l) l$name, ""))
  data
}

issues <- tryCatch(fetch_issues(), error = function(e) e)

if (inherits(issues, "error")) {
  # never publish a site without progress data
  if (nzchar(Sys.getenv("CI"))) stop(issues)
  warning("Could not read issues from GitHub: ", conditionMessage(issues))
  msg <- "<p><em>Progress data is not available: could not read issues from GitHub when this page was built.</em></p>"
  for (name in c("summary", "sprints", "areas", "priority", "closed", "opened", paste0("sprint-", sprints))) {
    write_html(name, msg)
  }
  quit(save = "no")
}

has_label <- function(label) vapply(issues$labels, function(l) label %in% l, TRUE)
is_open <- issues$state == "open"

# ---- summary ---------------------------------------------------------------

n_open <- sum(is_open)
n_closed <- sum(!is_open)
write_html("summary", c(
  "<div class=\"taf-bar\">",
  bar(n_closed, nrow(issues), "All issues closed"),
  sprintf(
    "<span class=\"taf-bar-label\"><a href=\"%s\">%d closed</a> · <a href=\"%s\">%d open</a></span>",
    issues_url("state:closed"), n_closed, issues_url("state:open"), n_open
  ),
  "</div>",
  sprintf(
    "<p class=\"taf-updated\">Issue status updated %s.</p>",
    format(Sys.time(), "%e %B %Y, %H:%M UTC", tz = "UTC")
  )
))

# ---- tables by label -------------------------------------------------------

label_table <- function(labels, heading, display = labels) {
  rows <- mapply(function(label, name) {
    sel <- has_label(label)
    open <- sum(sel & is_open)
    closed <- sum(sel & !is_open)
    q <- sprintf("label:\"%s\"", label)
    sprintf(
      paste0(
        "<tr><td>%s</td><td>%s</td><td class=\"num\">%d%%</td>",
        "<td class=\"num\"><a href=\"%s\">%d</a></td>",
        "<td class=\"num\"><a href=\"%s\">%d</a></td></tr>"
      ),
      esc(name), bar(closed, open + closed, paste(name, "closed")),
      pct(closed, open + closed),
      issues_url(paste("state:open", q)), open,
      issues_url(paste("state:closed", q)), closed
    )
  }, labels, display)
  c(
    "<table class=\"table taf-progress\">",
    sprintf(
      "<thead><tr><th>%s</th><th>Closed</th><th></th><th class=\"num\">Open</th><th class=\"num\">Closed</th></tr></thead>",
      heading
    ),
    "<tbody>", rows, "</tbody></table>"
  )
}

all_labels <- sort(unique(unlist(issues$labels)))

sprint_labels <- paste("Sprint:", sprints)
write_html("sprints", label_table(sprint_labels, "Workstream", tools::toTitleCase(sprints)))

area_labels <- grep("^Area", all_labels, value = TRUE)
write_html("areas", label_table(area_labels, "Area", sub("^Area:? *", "", area_labels)))

# ---- issue lists -----------------------------------------------------------

newest <- function(x, field, n) {
  x <- x[order(x[[field]], decreasing = TRUE), ]
  utils::head(x, n)
}

write_html("priority", issue_list(
  newest(issues[is_open & has_label("Priority: High"), ], "updated", Inf)
))
write_html("closed", issue_list(newest(issues[!is_open, ], "closed", n_recent), "closed"))
write_html("opened", issue_list(newest(issues[is_open, ], "created", n_recent), "created"))

# ---- workstream fragments, one per workstream ---------------------------------

for (sprint in sprints) {
  label <- paste("Sprint:", sprint)
  sel <- has_label(label)
  open <- issues[sel & is_open, ]
  closed <- sum(sel & !is_open)
  q <- sprintf("label:\"%s\"", label)
  more <- nrow(open) - n_sprint
  write_html(paste0("sprint-", sprint), c(
    "<div class=\"taf-bar\">",
    bar(closed, sum(sel), paste(label, "closed")),
    sprintf(
      "<span class=\"taf-bar-label\"><a href=\"%s\">%d closed</a> · <a href=\"%s\">%d open</a></span>",
      issues_url(paste("state:closed", q)), closed,
      issues_url(paste("state:open", q)), nrow(open)
    ),
    "</div>",
    issue_list(newest(open, "updated", n_sprint)),
    if (more > 0) {
      sprintf(
        "<p><a href=\"%s\">and %d more open issues</a></p>",
        issues_url(paste("state:open", q)), more
      )
    }
  ))
}
