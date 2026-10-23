# The ICES stock codes and the stock list.
#
# stock-codes.csv: the valid stock codes, the ICES_StockCode vocabulary read
# with icesVocab.
# stock-list.csv: the stocks of each year from first_year on in the ICES
# Stock Database, with their expert group and data category.

first_year <- 2018
sd_url <- paste0(
  "https://sd.ices.dk/services/odata4/StockListDWs4",
  "?$filter=ActiveYear%20ge%20", first_year,
  "&$select=StockKeyLabel,ActiveYear,ExpertGroup,DataCategory"
)

result <- tryCatch({
  codes <- icesVocab::getCodeList("ICES_StockCode")
  write.csv(data.frame(stock = codes$Key), "stock-codes.csv", row.names = FALSE)

  sd <- jsonlite::fromJSON(sd_url)$value
  write.csv(sd[c("StockKeyLabel", "ActiveYear", "ExpertGroup", "DataCategory")], "stock-list.csv", row.names = FALSE)
}, error = function(e) e)

if (inherits(result, "error")) {
  # never publish a site without status data
  if (nzchar(Sys.getenv("CI"))) stop(result)
  warning("Could not read the ICES stock codes and stock list: ", conditionMessage(result))
  unlink(dir())
}
