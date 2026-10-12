# Build the fragments included by the pages in status/.
#
# Run from the docs/ folder before `quarto render`, like progress.R.
#
# Reads the repository lists of the ices-taf and ices-advice organisations
# with the GitHub CLI, the stock codes from the ICES vocabulary (icesVocab)
# and the stock list from the ICES Stock Database. Most
# ices-taf repositories are private, so `gh` must be authenticated as someone
# who can read them (in GitHub Actions: the TAF_STATUS_TOKEN secret), otherwise
# only the public repositories are counted.
#
# Private repositories are only ever reported as counts, never by name.
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
# model.R itself (NA if there is none) and all of model.R, model_*.R together
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
      if (any(tolower(file) == "model.r")) {
        out$model_r[i] <- sum(size[tolower(file) == "model.r"])
        out$model_size[i] <- sum(size[grepl("^model.*[.]r$", tolower(file))])
      }
    }
  }
  out
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

# ---- build -----------------------------------------------------------------

build <- function() {
  # valid stock codes
  stock_codes <- icesVocab::getCodeList("ICES_StockCode")$Key

  # all repositories
  repos <- add_name_parts(org_repos("ices-taf"))
  advice <- add_name_parts(org_repos("ices-advice"))

  # the TAF stock assessment repositories
  taf_repos <- taf_assessments(repos, stock_codes)
  # TAF readiness level, see system/readiness-level.qmd
  # level 1: the code is on GitHub, a model.R script with more than the TAF
  # skeleton in it. No repository has been tested for levels 2 and 3 yet.
  taf_repos <- cbind(taf_repos, model_scripts(taf_repos$name))
  taf_repos$has_model <- taf_repos$model_size > min_model_size
  taf_repos$trl <- ifelse(taf_repos$has_model, 1, NA)
  # published: the stock has a repository for the year in ices-advice. This is
  # recorded separately, as level 4 can only follow levels 2 and 3
  taf_repos$published <-
    sub("_assessment$", "", taf_repos$name) %in% advice$name |
    paste(taf_repos$year, taf_repos$stock, sep = "_") %in% advice$name
  # read by status/readiness.qmd, and for checking locally: _generated/ is
  # not committed or published
  write.csv(
    taf_repos[
      order(taf_repos$year, taf_repos$stock),
      c("name", "year", "stock", "part", "visibility", "size", "pushed", "model_r", "model_size", "has_model", "trl", "published")
    ],
    file.path(out_dir, "taf-repos.csv"),
    row.names = FALSE
  )
  message(
    sum(taf_repos$has_model), " at readiness level 1, ",
    sum(taf_repos$has_model & taf_repos$published), " of them published"
  )
  taf_repos <- taf_repos[taf_repos$has_model, ]

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

  # assessments linked to advice: those with a <year>_<stock code>
  # repository in ices-advice
  is_stock_repo <- function(x) {
    x$size > 2 & x$year >= first_year & x$stock %in% stock_codes
  }
  advice <- advice[grepl(tafreg, advice$name) & is_stock_repo(advice), ]
  linked <- taf_repos[taf_repos$name %in% paste0(advice$name, "_assessment"), ]
  linked <- linked[is_stock_repo(linked), ]

  taf <- merge(linked, sd, by = c("stock", "year"))

  by_year <- merge(
    tally(sd, c("year", "category"), "stocks"),
    tally(taf, c("year", "category"), "taf"),
    all.x = TRUE
  )
  by_year$taf[is.na(by_year$taf)] <- 0
  by_year <- by_year[order(by_year$year, by_year$category), ]

  by_eg <- merge(
    tally(taf, c("eg", "year"), "n"),
    tally(sd, c("eg", "year"), "total")
  )
  by_eg$percent <- round(100 * by_eg$n / by_eg$total, 1)

  # ---- summary tiles -------------------------------------------------------

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

  # ---- public repositories -------------------------------------------------

  public <- repos[repos$visibility == "public", ]
  public <- public[order(public$pushed, decreasing = TRUE), ]
  cell <- function(x) ifelse(is.na(x), "", esc(as.character(x)))
  write_html("status-public-repos", c(
    "<table class=\"table table-sm taf-progress\">",
    "<thead><tr><th>Repository</th><th>Year</th><th>Type</th><th>Last updated</th></tr></thead>",
    "<tbody>",
    sprintf(
      "<tr><td><a href=\"%s\">%s</a></td><td>%s</td><td>%s</td><td>%s</td></tr>",
      public$url, esc(public$name), cell(public$year), cell(public$type),
      substr(public$pushed, 1, 10)
    ),
    "</tbody></table>"
  ))

  # ---- stock assessment table ----------------------------------------------

  write_html("status-stocks-table", c(
    "<table class=\"table table-sm taf-progress\">",
    "<thead><tr><th>Year</th><th>Category</th><th class=\"num\">Stocks</th><th class=\"num\">Implemented in TAF</th><th class=\"num\">Percentage</th></tr></thead>",
    "<tbody>",
    sprintf(
      "<tr><td>%d</td><td>%s</td><td class=\"num\">%d</td><td class=\"num\">%d</td><td class=\"num\">%.1f%%</td></tr>",
      by_year$year, by_year$category, by_year$stocks, by_year$taf,
      100 * by_year$taf / by_year$stocks
    ),
    "</tbody></table>"
  ))

  # ---- chart data ----------------------------------------------------------

  dated <- repos[!is.na(repos$year), ]
  types <- tally(dated, "type", "n")
  types <- types[order(types$n, decreasing = TRUE), ]
  n_types <- 8
  if (nrow(types) > n_types) {
    types <- rbind(
      types[seq_len(n_types), ],
      data.frame(type = "other", n = sum(types$n[-seq_len(n_types)]))
    )
  }

  data <- list(
    repos_by_year = tally(dated, "year", "n"),
    repos_by_type = types,
    stocks_by_year = by_year,
    stocks_by_eg = by_eg
  )
  json <- jsonlite::toJSON(data, dataframe = "rows", auto_unbox = TRUE)
  write_html("status-data", c(
    "<script type=\"application/json\" id=\"taf-status-data\">",
    gsub("</", "<\\/", json, fixed = TRUE),
    "</script>"
  ))
}

result <- tryCatch(build(), error = function(e) e)

if (inherits(result, "error")) {
  # never publish a site without status data
  if (nzchar(Sys.getenv("CI"))) stop(result)
  warning("Could not build the repository status: ", conditionMessage(result))
  msg <- "<p><em>Repository status is not available: the data could not be read when this page was built.</em></p>"
  for (name in fragments) write_html(name, msg)
}
