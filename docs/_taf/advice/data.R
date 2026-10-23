# The product of this analysis, data/advice-sheets.csv: all the advice sheets
# read, with the GitHub links in them. The UpdateAdvice workflow uploads it
# to the release taf-advice, where the site (_taf/site) and the next run of
# this analysis take it from.

library(TAF)

mkdir("data")
cp(taf.data.path("advice_sheets", "advice-sheets.csv"), "data")
