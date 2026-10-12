# TAF system documentation

Source for the site published at <https://ices-eg.github.io/wg_WGTAFGOV/>.

- `system/`: system design pages
- `strategy/`: goals and roadmap
- `status/`: status of the TAF repositories, filled in from GitHub and the ICES Stock Database by `_scripts/status.R`
- `progress.qmd`: issue status, filled in from GitHub by `_scripts/progress.R`

The site is rebuilt by the `PublishDocs` workflow on every push to `master` that changes `docs/`, and once a night to refresh the status and progress pages.

To build locally, from this folder:

```sh
Rscript _scripts/progress.R
Rscript _scripts/status.R
quarto preview
```

The scripts need the [GitHub CLI](https://cli.github.com/) (`gh auth login`) and the `jsonlite` and `icesVocab` R packages.

`status.R` counts the repositories in `ices-taf`, most of which are private. Locally it sees what your `gh` login can see. The workflow uses the `TAF_STATUS_TOKEN` repository secret: a token with read access to the metadata and contents of the `ices-taf` repositories. Without it only public repositories are counted and the page says so. Private repositories are reported as counts only, never by name.
