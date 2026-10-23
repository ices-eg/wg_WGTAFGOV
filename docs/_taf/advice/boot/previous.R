# The advice sheets read by the previous run, so that this one only reads
# what is new: the product of this analysis, advice-sheets.csv, which the
# UpdateAdvice workflow uploads to the release taf-advice at the end of each
# run.
#
# Before the first release, it comes from the file that was committed to the
# repository until then. If it cannot be had from either, it is left out, and
# advice_sheets.R starts empty.

urls <- c(
  "https://github.com/ices-eg/wg_WGTAFGOV/releases/download/taf-advice/advice-sheets.csv",
  "https://raw.githubusercontent.com/ices-eg/wg_WGTAFGOV/a98c1e0/docs/_data/advice-sheets.csv"
)

for (url in urls) {
  ok <- tryCatch(
    download.file(url, "advice-sheets.csv", quiet = TRUE, mode = "wb") == 0,
    error = function(e) FALSE, warning = function(w) FALSE
  )
  if (ok) break
  unlink("advice-sheets.csv")
}
message(if (ok) paste("  advice-sheets.csv from", url) else "  advice-sheets.csv not found, starting empty")
