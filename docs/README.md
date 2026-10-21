# TAF system documentation

Source for the site published at <https://ices-eg.github.io/wg_WGTAFGOV/>.

- `system/`: system design pages
- `planning/`: goals and workstreams
- `status/`: status of the TAF repositories, filled in from GitHub and the ICES Stock Database by `_scripts/status.R`
- `_data/advice-sheets.csv`: the ICES advice sheets from 2024 on and the GitHub links in them, updated once a week by the `UpdateAdvice` workflow, which runs `_scripts/advice.R` and commits the result. Building the site reads this file and makes no requests to the ICES library.
- `_data/model-scripts.csv`: the size of the model scripts in each assessment repository, kept so that `_scripts/status.R` only looks again at repositories that have been pushed to. It is committed, and refreshed by the same weekly workflow.
- `_data/rdbes.json`: the countries and stocks of the RDBES workflow shown on `status/rdbes.qmd`, edited by hand. `links` lists which countries provide estimates for which stocks. Whether each connection works is found in the code by `_scripts/status.R`, which writes `_generated/rdbes-repos.csv`. A `status` of `broken` can be recorded on an entry in `links`, or in `stock_links` for the connection from a stock coordination repository to its assessment, with an optional `note`. `repositories` sets the status of a repository by name, `runs` or `failing`, with an optional `note`, until there is an automatic run test.
- `_data/repo-types.json`: the types of TAF repository shown on `status/stock-repos.qmd`, by the pattern of their names (`{year}` and `{stock}` stand for the year and stock code chosen, and the `short` group is the name shown on the box), the column or the side lane (`lane`) each is drawn in, and the connections (`flows`) expected between them. A type with `"years": "latest"` is shown for the latest year up to the year chosen. Mixed fisheries repositories, which are not named for a stock, can be listed with their stocks under `members`. Edited by hand.
- `_data/data-sources.csv`: the other ices-taf repositories named in the `DATA.bib` of each repository, or in the scripts of its entries with `source = {script}`. Kept by `_scripts/status.R` like `model-scripts.csv`, read again only for repositories pushed to since, and committed by the same weekly workflow.
- `progress.qmd`: issue status, filled in from GitHub by `_scripts/progress.R`

The site is rebuilt by the `PublishDocs` workflow on every push to `master` that changes `docs/`, and once a night to refresh the status and progress pages.

To build locally, from this folder:

```sh
Rscript _scripts/progress.R
Rscript _scripts/status.R
quarto preview
```

The scripts need the [GitHub CLI](https://cli.github.com/) (`gh auth login`) and the `jsonlite` and `icesVocab` R packages. Rendering the pages with R code needs `knitr`, `rmarkdown`, `plotly` and `DT`.

`status.R` counts the repositories in `ices-taf`, most of which are private. Locally it sees what your `gh` login can see. The workflow uses the `TAF_STATUS_TOKEN` repository secret: a token with read access to the metadata and contents of the `ices-taf` repositories. Without it only public repositories are counted and the page says so. The names of private repositories are listed on the site, marked as private; their contents are not.
