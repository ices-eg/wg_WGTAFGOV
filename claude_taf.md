# TAF analyses: what to know before changing one

Learned building `docs/_taf/advice` and `docs/_taf/site` (TAF 4.3.1). It applies to any TAF analysis, including the templates.

## Layout

- `boot/initial/data/`: only what a person writes by hand. One `DATA.bib` entry for each, with `source = {file}`.
- `boot/<key>.R`: fetches one dataset, with `source = {script}` in `DATA.bib`. A plain URL is enough for a file that is always there.
- `boot/data/`: what `taf.boot()` makes. It is never edited or committed.
- `data.R` writes `data/`, `model.R` writes `model/`, `output.R` writes `output/`, `report.R` writes `report/`, and helpers go in `utilities.R`. Leave out a stage that does nothing; there is no `model.R` without a model.
- Use the TAF helpers: `mkdir`, `cp`, `write.taf`, `taf.data.path`.
- `DATA.bib` entries need `title` and `description`, saying what the data are and where they come from.

## How boot really behaves

- A boot script runs with `boot/data/<key>/` as its working directory, and that folder is emptied first.
- Entries are processed in `DATA.bib` order. A script can read `../<earlier key>/`; note that dependency in its `description`.
- `taf.boot()` skips a script whose folder already has files, so use `taf.boot(force = TRUE)` to really fetch again.
- A URL entry fails the whole boot if the URL returns 404. For anything that may not exist yet, write a script with a fallback.
- Boot scripts run in the global environment and can overwrite objects of whatever called them.
- Boot output holds findings (sizes, paths, markers found), never the code or data of private repositories.

## Rules

1. **Data flows forwards.** Never write back into `boot/initial/data`, and never let a workflow commit data.
2. **Caches come from the previous run.** A cache is the previous run's published product, fetched by a boot entry (e.g. `previous.R`). Products go to a fixed-tag GitHub release (`gh release upload <tag> --clobber`). If the release is missing, fall back to a pinned copy, then to empty.
3. **One analysis for each schedule.** Each runs on its own and publishes its products; another analysis takes them as a boot entry.
4. **Re-run only what changed,** with `TAF::make()` in a `make.R` driver:
   - `make.taf()` and `make.all()` leave gaps: `data.R` is not made from `boot/data`, and `report.R` needs an `output/` folder. Give the prerequisites yourself.
   - Run each step in a fresh `Rscript`, passing `R_LIBS = .libPaths()`. renv only activates at the repository root.
   - `source.taf()` returns `FALSE` when a script fails instead of stopping. Stop, and delete that step's output so it is not taken as up to date.
   - Force boot (`Rscript make.R boot`) when the outside data must be read again, as in a nightly run.
5. **Keep what is on the pages the same** unless the change is meant. Compare the outputs with the old ones row for row.

## CI (GitHub Actions)

- Restore the working folders with `actions/cache`: one key per run, found by its prefix.
- `actions/checkout` stamps every file with the current time, so `make()` would re-run everything. Store the commit that was built (`.built-commit`) in the cache. Give files unchanged since then an old time; changed files keep the checkout time.
- Put time limits on the data and render steps, not on the whole job. A cold package cache, as after adding a package, can take more than 15 minutes on its own.

## Quarto

- A folder whose name starts with `_` (`_taf`) is not rendered.
- Keep `quarto render` as a workflow step. Inside `report.R` it needs every page as a prerequisite, plus a cached `_site`.
- Quarto renders nothing under a hidden folder such as `.claude/worktrees/`. Test a render in a copy at an ordinary path.

## Checks before finishing

- A first full run gives the same outputs as before.
- A second run does nothing.
- After touching `data.R`, it and the later steps run but boot does not.
- With an error added to a script, the run stops and that step's output is removed.
