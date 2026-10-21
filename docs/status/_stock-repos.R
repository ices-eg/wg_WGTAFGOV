# The stock repositories page, status/stock-repos.qmd: the repositories of
# ices-taf, with what is known of each, written into the page for the diagram
# to be drawn from in the browser.

repos <- read.csv("../_generated/repos.csv", stringsAsFactors = FALSE)
repos <- repos[grepl("^[0-9]{4}_", repos$name), ]

# the TAF readiness level, where it is found: for the assessments, the other
# types of work on the readiness page and the repositories of the RDBES
# workflow. NA for a repository with no level, and absent for one whose level
# is not looked for.
levels <- rbind(
  read.csv("../_generated/taf-repos.csv")[c("name", "trl")],
  read.csv("../_generated/other-repos.csv")[c("name", "trl")],
  read.csv("../_generated/rdbes-repos.csv")[c("name", "trl")]
)
levels <- levels[!duplicated(levels$name), ]

# the connections to the RDBES found in the code, for the repositories of the
# RDBES workflow
rdbes_repos <- read.csv("../_generated/rdbes-repos.csv", stringsAsFactors = FALSE, na.strings = "")

# the repositories that each takes data from
sources <- read.csv("../_data/data-sources.csv", stringsAsFactors = FALSE, na.strings = "")

i <- match(repos$name, levels$name)
j <- match(repos$name, rdbes_repos$name)
k <- match(repos$name, sources$name)
stock_repos <- data.frame(
  name = repos$name,
  year = repos$year,
  private = repos$visibility != "public",
  # "none" for no level, "" where the level is not looked for
  level = ifelse(is.na(i), "", ifelse(is.na(levels$trl[i]), "none", levels$trl[i])),
  download = !is.na(j) & !is.na(rdbes_repos$download[j]),
  upload = !is.na(j) & !is.na(rdbes_repos$upload[j]),
  sources = ifelse(is.na(k) | is.na(sources$sources[k]), "", sources$sources[k])
)

payload <- list(
  repos = stock_repos,
  types = jsonlite::fromJSON("../_data/repo-types.json", simplifyVector = FALSE),
  rdbes = jsonlite::fromJSON("../_data/rdbes.json", simplifyVector = FALSE),
  updated = format(file.mtime("../_generated/repos.csv"), "%e %B %Y")
)

stock_repos_data <- function() {
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, dataframe = "rows", null = "null")
  # nothing in the data can close the script element
  json <- gsub("</", "<\\/", json, fixed = TRUE)
  htmltools::HTML(paste0("<script type=\"application/json\" id=\"stock-repos-data\">", json, "</script>"))
}
