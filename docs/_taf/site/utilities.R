# Helpers shared by data.R and report.R.

# an html fragment for a page to include, as markdown that holds raw html
write_html <- function(name, html, dir = "report") {
  writeLines(
    c("```{=html}", html, "```"),
    file.path(dir, paste0(name, ".md"))
  )
}

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

# the countries with data for each stock of the RDBES workflow: one row for
# each stock, with the countries separated by commas
read_stock_countries <- function(file) {
  x <- read.csv(file, stringsAsFactors = FALSE)
  countries <- strsplit(x$CountriesWithData, "\\s*,\\s*")
  data.frame(stock = rep(x$StockCode, lengths(countries)), country = trimws(unlist(countries)))
}
