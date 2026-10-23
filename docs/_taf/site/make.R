# Build the data of the site, running only what is out of date. Run from the
# docs/_taf/site folder before `quarto render`:
#
#   Rscript make.R          what has changed
#   Rscript make.R boot     also read everything again from GitHub and ICES
#
# Each step runs when one of its prerequisites is newer than what it made
# (see TAF::make):
#
# - boot (taf.boot), all of boot/data read again: when DATA.bib, a boot
#   script or a file in boot/initial has changed, or when asked to, as by the
#   nightly build. Otherwise boot/data is what the previous build read,
#   which in GitHub Actions comes from a cache.
# - data.R, into data: when it, utilities.R or boot/data has changed.
# - report.R, into report: when it, utilities.R or data has changed.
#
# The boot scripts need the GitHub CLI, logged in (gh auth login), see
# boot/github_repos.R.

library(TAF)

# source.taf() reports a failed script and carries on: stop instead, so that
# the site is never built from half the data
run <- function(script) {
  if (!source.taf(script)) stop(script, " failed")
}

# the boot scripts run in the global environment, where they could replace a
# helper of this script, so the files are listed in each call
make(
  "boot/DATA.bib",
  c(dir("boot", pattern = "[.]R$", full.names = TRUE), dir("boot/initial", recursive = TRUE, full.names = TRUE)),
  "boot/data",
  engine = function(recipe) taf.boot(force = TRUE),
  force = "boot" %in% commandArgs(trailingOnly = TRUE), quiet = FALSE
)
make("data.R", c("utilities.R", dir("boot/data", recursive = TRUE, full.names = TRUE)), "data", engine = run, quiet = FALSE)
make("report.R", c("utilities.R", dir("data", recursive = TRUE, full.names = TRUE)), "report", engine = run, quiet = FALSE)
