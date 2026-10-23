# Read the ICES advice sheets, running only what is out of date. Run from the
# docs/_taf/advice folder:
#
#   Rscript make.R          what has changed
#   Rscript make.R boot     also read the ICES library again
#
# Each step runs when one of its prerequisites is newer than what it made
# (see TAF::make):
#
# - boot (taf.boot), all of boot/data read again: when DATA.bib or a boot
#   script has changed, or when asked to, as by the weekly UpdateAdvice
#   workflow.
# - data.R, into data: when it or boot/data has changed.

library(TAF)

# source.taf() reports a failed script and carries on: stop instead, so that
# no product is made from half the data
run <- function(script) {
  if (!source.taf(script)) stop(script, " failed")
}

make(
  "boot/DATA.bib", dir("boot", pattern = "[.]R$", full.names = TRUE), "boot/data",
  engine = function(recipe) taf.boot(force = TRUE),
  force = "boot" %in% commandArgs(trailingOnly = TRUE), quiet = FALSE
)
# the boot scripts run in the global environment, where they could replace a
# helper of this script, so the files are listed here
make("data.R", dir("boot/data", recursive = TRUE, full.names = TRUE), "data", engine = run, quiet = FALSE)
