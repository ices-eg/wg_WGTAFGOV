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

# a button that clears the selection made by clicking a chart, so that a
# linked table lists everything again
show_all_button <- function(shared, label = "Show all") {
  htmltools::tags$button(
    label,
    type = "button", class = "btn btn-sm btn-outline-secondary mb-2",
    onclick = sprintf("new crosstalk.SelectionHandle('%s').set(null)", shared$groupName())
  )
}
