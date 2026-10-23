# The products of the last runs, from the releases of ices-eg/wg_WGTAFGOV:
#
# - model-scripts.csv and data-sources.csv, what the previous build of the
#   site read, so that github_repos.R only reads what is new. The PublishDocs
#   workflow uploads them to the release taf-site at the end of each build.
# - advice-sheets.csv, the advice sheets, the product of the advice analysis
#   (_taf/advice), which the UpdateAdvice workflow uploads to the release
#   taf-advice once a week.
#
# Before the first releases, they come from the files that were committed to
# the repository until then. A file that cannot be had from either is left
# out: github_repos.R then starts empty, and data.R stops.

releases <- "https://github.com/ices-eg/wg_WGTAFGOV/releases/download"
committed <- "https://raw.githubusercontent.com/ices-eg/wg_WGTAFGOV/a98c1e0/docs/_data"

products <- c(
  "model-scripts.csv" = "taf-site",
  "data-sources.csv" = "taf-site",
  "advice-sheets.csv" = "taf-advice"
)

for (product in names(products)) {
  urls <- c(paste(releases, products[[product]], product, sep = "/"), paste(committed, product, sep = "/"))
  for (url in urls) {
    ok <- tryCatch(
      download.file(url, product, quiet = TRUE, mode = "wb") == 0,
      error = function(e) FALSE, warning = function(w) FALSE
    )
    if (ok) break
    unlink(product)
  }
  message(if (ok) paste(" ", product, "from", url) else paste(" ", product, "not found"))
}
