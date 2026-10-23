# TAF system documentation

Source for the site published at <https://ices-eg.github.io/wg_WGTAFGOV/>.

- `system/`: system design pages
- `planning/`: goals and workstreams
- `status/`: status of the TAF repositories, filled in from GitHub, the ICES Stock Database and the ICES library. `status/_plot.R`, `_rdbes.R` and `_stock-repos.R` are helpers of the pages.
- `progress.qmd`: issue status, filled in from GitHub
- `_taf/`: the data of the pages, gathered as two TAF analyses (see below)

## The data: `_taf/`

The data of the status and progress pages are gathered and prepared in `_taf/`, as two TAF analyses made with the [TAF](https://cran.r-project.org/package=TAF) package. Quarto leaves the folder out of the site, as its name starts with `_`. Each analysis has a `make.R` that runs its boot step and its scripts, each only when what it reads has changed (`TAF::make()`), and `boot/DATA.bib` says what each set of data is and where it comes from.

Data only flow forwards: nothing is written back into the repository. What an analysis needs from its previous run, or from the other analysis, is uploaded by its workflow to a release of this repository, as an asset that is replaced each time, and is downloaded from there by its `boot/previous.R`. Before the first releases, these files come from the copies that were committed to the repository until then (commit `a98c1e0`).

```
_taf/advice  --advice-sheets.csv (release taf-advice)-->  _taf/advice (next week)
                                                       \->  _taf/site
_taf/site    --model-scripts.csv, data-sources.csv (release taf-site)-->  _taf/site (next build)
```

### `_taf/advice/`: the advice sheets, once a week

Run by the `UpdateAdvice` workflow every Monday, which always reads the ICES library again and uploads `data/advice-sheets.csv` to the release `taf-advice`.

- `boot/previous.R`: the advice sheets of the previous run, from the release.
- `boot/advice_sheets.R`: the ICES advice sheets from 2024 on and the GitHub links in them, from the ICES library, reading only the sheets that are new or changed since the previous run.
- `data.R`: the product, `data/advice-sheets.csv`.

### `_taf/site/`: the pages, at each build

Run by the `PublishDocs` workflow before the site is rendered.

- `boot/initial/data/`: the files edited by hand, copied into `boot/data/` by the boot step:
  - `templates.csv`: the TAF templates shown on `status/templates.qmd`, with the repository of each in `ices-tools-dev` or `ices-tools-prod` (empty for a planned template), its issue on the project board and, for a template that offers several methods, their names separated by semicolons (`chr;rb;rfb`), which are found in the names of their scripts (`model_chr.R`). `boot/github_repos.R` looks for the markers of progress in the R scripts of each repository, and at its QC workflow.
  - `template-markers.csv`: markers of progress set by hand for a template on `status/templates.qmd`, by `template`, `method` (empty for the template itself) and `marker` (the ids are in the page), with a `status` of `done`, `partly`, `todo` or `na` (does not apply to the method) and an optional `note`, shown when the pointer is held over the marker. These take the place of what is found in the code.
  - `rdbes.json`: the year and stocks of the RDBES workflow shown on `status/rdbes.qmd`, and the countries to show even with no data for a stock. Whether each connection works is found in the code by `boot/github_repos.R`. A `status` of `broken` and a `note` can be recorded for a connection in `links` (by `country` and `stock`), or in `stock_links` for the connection from a stock coordination repository to its assessment. `repositories` sets the status of a repository by name, `runs` or `failing`, with an optional `note`, until there is an automatic run test.
  - `rdbes-stock-countries.csv`: the countries with data for each stock of the RDBES workflow, one row for each stock with the countries separated by commas, in the form the RDBES gives them. These are the connections drawn from the national estimation repositories to the stock coordination repositories. Edited by hand for now.
  - `repo-types.json`: the types of TAF repository shown on `status/stock-repos.qmd`, by the pattern of their names (`{year}` and `{stock}` stand for the year and stock code chosen, and the `short` group is the name shown on the box), the column or the side lane (`lane`) each is drawn in, and the connections (`flows`) expected between them. A type with `"years": "latest"` is shown for the latest year up to the year chosen. Mixed fisheries repositories, which are not named for a stock, can be listed with their stocks under `members`.
- the QC results, downloaded by their entry in `boot/DATA.bib` into `boot/data/qc/`.
- `boot/previous.R`: `model-scripts.csv` and `data-sources.csv` from the release `taf-site`, and `advice-sheets.csv` from the release `taf-advice`.
- `boot/ices.R`: the stock codes of the ICES vocabulary, and the stock list of the ICES Stock Database.
- `boot/github_issues.R`: the issues of this repository.
- `boot/github_repos.R`: the repositories of `ices-taf` and `ices-advice`, and what is found in their files: the size of the model scripts of each (`model-scripts.csv`), the other `ices-taf` repositories named in its `DATA.bib` or in the scripts of its entries with `source = {script}` (`data-sources.csv`), the connections to the RDBES of the repositories of the RDBES workflow (`rdbes-code.csv`) and the markers of progress of the templates (`templates.csv`). The model scripts and data sources are only read again for the repositories pushed to since the previous build. The workflow uploads `model-scripts.csv` and `data-sources.csv` to the release `taf-site`.
- `utilities.R`: helpers shared by `data.R` and `report.R`.
- `data.R`: the tables the pages read, in `data/`.
- `report.R`: the fragments the pages include, in `report/`: the progress of the issues and the summary of the repositories.

`boot/data/`, `data/` and `report/` are made by `make.R` and are not committed.

## Building

The site is rebuilt by the `PublishDocs` workflow on every push to `master` that changes `docs/`, and once a night, when the boot step of `_taf/site` reads everything again to refresh the status and progress pages. The workflow keeps `boot/data/`, `data/` and `report/` of `_taf/site` from one build to the next in a cache, so a push that only changes a page does not read GitHub again. New advice sheets are on the site from the first build after the weekly `UpdateAdvice` run that read them.

To build locally, from this folder:

```sh
cd _taf/site
Rscript make.R        # or Rscript make.R boot, to read everything again
cd ../..
quarto preview
```

`_taf/advice` is run in the same way, from its folder; the site takes the advice sheets from the release, not from a local run.

The boot scripts need the [GitHub CLI](https://cli.github.com/) (`gh auth login`) and the `TAF`, `jsonlite`, `curl` and `icesVocab` R packages. Rendering the pages with R code needs `knitr`, `rmarkdown`, `plotly` and `DT`.

`boot/github_repos.R` counts the repositories in `ices-taf`, most of which are private. Locally it sees what your `gh` login can see. The workflow uses the `TAF_STATUS_TOKEN` repository secret: a token with read access to the metadata and contents of the `ices-taf` repositories. Without it only public repositories are counted and the page says so. The names of private repositories are listed on the site, marked as private; their contents are not, and nothing of their code is kept in `boot/data/`, only what is found in it.

To work on the template repositories alongside the docs, `../scripts/clone-templates.sh` clones those of `_taf/site/boot/initial/data/templates.csv` into `templates/` at the top of the repository (ignored by git), or pulls them if they are there.
