# TAF system documentation

Source for the site published at <https://ices-eg.github.io/wg_WGTAFGOV/>.

- `system/`: system design pages
- `strategy/`: goals and roadmap
- `status/`: status of the TAF repositories, filled in from GitHub and the ICES Stock Database by `_scripts/status.R`
- `_data/advice-sheets.csv`: the ICES advice sheets from 2024 on and the GitHub links in them, updated once a week by the `UpdateAdvice` workflow, which runs `_scripts/advice.R` and commits the result. Building the site reads this file and makes no requests to the ICES library.
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
