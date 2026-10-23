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

# run a step with make() in an R session of its own, as the scripts leave
# their objects in the global environment, with the libraries of this one (as
# with renv). source.taf() only reports a failed script: stop instead, so that
# no product is made from half the data, and remove what the step wrote,
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
  "boot/DATA.bib", dir("boot", pattern = "[.]R$", full.names = TRUE), "boot/data",
  "{taf.boot(force = TRUE); TRUE}",
  force = "boot" %in% commandArgs(trailingOnly = TRUE)
)
step("data.R", dir("boot/data", recursive = TRUE, full.names = TRUE), "data", "source.taf('data.R')")
