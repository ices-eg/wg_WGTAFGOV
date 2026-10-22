# WGTAFGOV

The working group on the governance of the Transparent Assessment Framework (TAF). The documentation site is in `docs/`, a Quarto website published to <https://ices-eg.github.io/wg_WGTAFGOV/>; see `docs/README.md` for how it is built and what each data file is.

## Issues and the project board

- Work is planned in the issues of `ices-eg/wg_WGTAFGOV`, on [project 10](https://github.com/orgs/ices-eg/projects/10), in three levels of issue type: EPIC, then Feature, then Task, linked as sub-issues.
- Issues carry a `Sprint: <name>` label: development, governance, strategy or templates.
- The board has the fields Status, Size, Start date and Target date, set on each new issue.
- Use `gh` for all of it. It is logged in with the `project` scope.

## TAF templates

A TAF template is a complete, well documented TAF analysis for an assessment method: standard plots, tables and diagnostics, a general report, and the SAG and ASD XML files and an FLStock object.

- The templates are listed in `docs/_data/templates.csv`, with their repository in ices-tools-dev or ices-tools-prod, their Feature issue under the EPIC #189, and their methods, if they offer several (Category 3: chr, rb and rfb).
- Their progress is shown on `docs/status/templates.qmd`. `docs/_scripts/status.R` (`template_markers`) finds the markers in the code of each repository on GitHub. A marker that does not apply to a method is set in `docs/_data/template-markers.csv`.
- Each Task issue of a template matches a marker on that page, given in its body. When a task is done, its marker should turn on at the next build of the site.
- Clones of the template repositories are in `templates/`, one folder for each, made and updated by `docs/_scripts/clone-templates.sh`. The folder is ignored by git: each clone is its own repository, committed and pushed on its own. Search a clone by giving its path, since searches of this repository leave out ignored files.
