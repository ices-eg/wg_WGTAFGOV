# Shared by the R code in the status pages: colours and chart styling.

library(plotly)

blue <- "#2a78d6"
orange <- "#eb6834"

# the styling common to all the charts
taf_layout <- function(p, ...) {
  p |>
    layout(
      ...,
      legend = list(orientation = "h", y = 1.12),
      font = list(family = "inherit"),
      hoverlabel = list(namelength = -1),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    ) |>
    config(displayModeBar = FALSE)
}
