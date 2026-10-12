# TAF system documentation

Source for the site published at <https://ices-eg.github.io/wg_WGTAFGOV/>.

- `system/`: system design pages
- `strategy/`: goals and roadmap
- `progress.qmd`: issue status, filled in from GitHub by `_scripts/progress.R`

The site is rebuilt by the `PublishDocs` workflow on every push to `master` that changes `docs/`, and once a night to refresh the progress pages.

To build locally, from this folder:

```sh
Rscript _scripts/progress.R
quarto preview
```

`progress.R` needs the [GitHub CLI](https://cli.github.com/) (`gh auth login`) and the `jsonlite` R package.
