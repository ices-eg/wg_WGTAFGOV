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

# run a step with make() in an R session of its own, as the scripts leave
# their objects in the global environment, with the libraries of this one (as
# with renv). source.taf() only reports a failed script: stop instead, so that
# the site is never built from half the data, and remove what the step wrote,
# so that it is not taken as up to date next time.
step <- function(recipe, prereq, target, run, force = FALSE) {
  make(recipe, prereq, target, force = force, quiet = FALSE, engine = function(recipe) {
    status <- system2(
      "Rscript", c("-e", shQuote(sprintf("library(TAF); quit(status = if (isTRUE(%s)) 0 else 1)", run))),
      env = paste0("R_LIBS=", paste(.libPaths(), collapse = .Platform$path.sep))
    )
    if (status != 0) {
      unlink(target, recursive = TRUE)
      stop(recipe, " failed")
    }
  })
}

step(
  "boot/DATA.bib",
  c(dir("boot", pattern = "[.]R$", full.names = TRUE), dir("boot/initial", recursive = TRUE, full.names = TRUE)),
  "boot/data", "{taf.boot(force = TRUE); TRUE}",
  force = "boot" %in% commandArgs(trailingOnly = TRUE)
)
step("data.R", c("utilities.R", dir("boot/data", recursive = TRUE, full.names = TRUE)), "data", "source.taf('data.R')")
step("report.R", c("utilities.R", dir("data", recursive = TRUE, full.names = TRUE)), "report", "source.taf('report.R')")
