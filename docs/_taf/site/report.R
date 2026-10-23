# The fragments included by the pages, from the tables of data.R:
#
# report/summary.md, sprints.md, areas.md, priority.md, closed.md,
#   opened.md and sprint-<workstream>.md: the progress of the issues, for
#   index.qmd, progress.qmd and planning/workstreams.qmd.
# report/status-summary.md: the counts of the repositories, for
#   status/index.qmd.
#
# Quarto resolves includes before any pre-render step, so these are made
# before `quarto render`, not by a pre-render script.

library(TAF)
source("utilities.R")

repo <- Sys.getenv("GITHUB_REPOSITORY", "ices-eg/wg_WGTAFGOV")
n_recent <- 15
n_sprint <- 8

sprints <- c("development", "governance", "strategy", "templates")

mkdir("report")

# ---- helpers ---------------------------------------------------------------

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

# ---- progress --------------------------------------------------------------

if (!file.exists("data/issues.csv")) {
  # boot/github_issues.R stops in CI, so this is only when run locally
  msg <- "<p><em>Progress data is not available: could not read issues from GitHub when this page was built.</em></p>"
  for (name in c("summary", "sprints", "areas", "priority", "closed", "opened", paste0("sprint-", sprints))) {
    write_html(name, msg)
  }
} else {
  issues <- read.csv("data/issues.csv", stringsAsFactors = FALSE, na.strings = "NA")
  issues$labels <- strsplit(issues$labels, ";", fixed = TRUE)

  has_label <- function(label) vapply(issues$labels, function(l) label %in% l, TRUE)
  is_open <- issues$state == "open"

  # ---- summary -------------------------------------------------------------

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
      format(file.mtime(taf.data.path("github_issues", "issues.csv")), "%e %B %Y, %H:%M UTC", tz = "UTC")
    )
  ))

  # ---- tables by label -----------------------------------------------------

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

  # ---- issue lists ---------------------------------------------------------

  newest <- function(x, field, n) {
    x <- x[order(x[[field]], decreasing = TRUE), ]
    utils::head(x, n)
  }

  write_html("priority", issue_list(
    newest(issues[is_open & has_label("Priority: High"), ], "updated", Inf)
  ))
  write_html("closed", issue_list(newest(issues[!is_open, ], "closed", n_recent), "closed"))
  write_html("opened", issue_list(newest(issues[is_open, ], "created", n_recent), "created"))

  # ---- workstream fragments, one per workstream -----------------------------

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
}

# ---- the repositories page -------------------------------------------------

if (!file.exists("data/repos.csv")) {
  # data.R stops in CI, so this is only when run locally
  write_html("status-summary", "<p><em>Repository status is not available: the data could not be read when this page was built.</em></p>")
} else {
  # the repositories of TAF work, those named for a year and those of the
  # workshops
  repos <- read.csv("data/repos.csv", stringsAsFactors = FALSE)
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
      format(file.mtime(taf.data.path("github_repos", "ices-taf.csv")), "%e %B %Y, %H:%M UTC", tz = "UTC")
    )
  ))
}
