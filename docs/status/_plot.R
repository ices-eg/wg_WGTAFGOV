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

# ---- TAF readiness ---------------------------------------------------------

readiness_levels <- c("No level", paste("Level", 1:4))
readiness_colours <- setNames(c("#d5d4ce", "#9ec5f4", "#3987e5", "#1c5cab", "#0d366b"), readiness_levels)

# the assessments as they are listed, from the repositories written by
# _scripts/status.R
readiness_assessments <- function(repos) {
  assessments <- data.frame(
    Year = as.character(repos$year),
    # goes to the assessment repository
    Stock = sprintf(
      "<a href=\"https://github.com/ices-taf/%s\">%s</a>",
      repos$name,
      ifelse(is.na(repos$part), repos$stock, paste0(repos$stock, " (", repos$part, ")"))
    ),
    Level = factor(ifelse(is.na(repos$trl), 0, repos$trl), 0:4, readiness_levels),
    Published = ifelse(repos$published, "Published", "Not published"),
    QC = ifelse(!repos$qc_tested, "Not tested", ifelse(repos$qc_passed, "Passed", "Failed"))
  )
  names(assessments)[5] <- "QC test"
  # what was found of the link in the advice sheet of the stock and year, see
  # status/advice-sheets.qmd, going to the advice sheet. There are advice
  # sheets from 2024 on.
  # Nothing is said of an assessment that is not published and whose advice
  # sheet has no link: there is nothing for the sheet to link to.
  nothing_to_say <- is.na(repos$advice_status) | (repos$advice_status == "No link" & !repos$published)
  assessments[["Advice link"]] <- ifelse(
    nothing_to_say, "",
    sprintf("<a href=\"%s\">%s</a>", repos$advice_url, repos$advice_status)
  )
  assessments
}

# the chart of the assessments by year and level, the published beside the
# not published. Given the assessments as data shared with a table, clicking
# a bar filters the table.
readiness_chart <- function(assessments, shared = NULL) {
  # the bars: the number of assessments by year and level, stacked by hand so
  # that a clicked bar is highlighted in place. Each bar carries the keys of
  # its assessments, which is what links it to the rows of the table.
  keys <- if (is.null(shared)) as.character(seq_len(nrow(assessments))) else shared$key()
  bars <- aggregate(
    list(key = keys),
    assessments[c("Level", "Year", "Published")],
    function(x) I(list(x))
  )
  bars$n <- lengths(bars$key)
  bars <- bars[order(bars$Published, bars$Year, bars$Level), ]
  bars$base <- ave(bars$n, bars$Published, bars$Year, FUN = function(n) cumsum(n) - n)

  # one panel of the chart, with a set of bars for each level
  panel <- function(published, legend) {
    p <- plot_ly()
    for (level in levels(droplevels(assessments$Level))) {
      d <- bars[bars$Published == published & bars$Level == level, ]
      if (!is.null(shared)) d <- crosstalk::SharedData$new(d, key = ~key, group = shared$groupName())
      p <- p |>
        add_bars(
          data = d,
          x = ~Year, y = ~n, base = ~base,
          name = level, marker = list(color = readiness_colours[[level]], line = list(color = "white", width = 2)),
          legendgroup = level, showlegend = legend,
          customdata = ~n, hovertemplate = "%{x}: %{customdata} repositories"
        )
    }
    # on top of the not published, a block the size of the published,
    # so that the bars of this panel reach the total for the year
    if (published == "Not published") {
      d <- aggregate(list(key = I(bars$key)), bars[c("Year", "Published")], function(x) I(list(unlist(x))))
      d$n <- lengths(d$key)
      rest <- d[d$Published == "Not published", ]
      d <- d[d$Published == "Published", ]
      d$base <- ifelse(d$Year %in% rest$Year, rest$n[match(d$Year, rest$Year)], 0)
      if (!is.null(shared)) d <- crosstalk::SharedData$new(d, key = ~key, group = shared$groupName())
      p <- p |>
        add_bars(
          data = d,
          x = ~Year, y = ~n, base = ~base,
          name = "Published",
          marker = list(color = "#d3e7c0", line = list(color = "white", width = 2)),
          customdata = ~n, hovertemplate = "%{x}: %{customdata} repositories"
        )
    }
    p |>
      layout(
        barmode = "overlay",
        xaxis = list(title = "", categoryorder = "category ascending"),
        yaxis = list(title = "Assessment repositories")
      )
  }

  title <- function(text, axis) {
    list(text = paste0("<b>", text, "</b>"), x = 0, xref = paste(axis, "domain"), y = 1.04, yref = "paper", xanchor = "left", yanchor = "bottom", showarrow = FALSE)
  }
  # the published assessments only go back a few years
  p <- subplot(panel("Published", TRUE), panel("Not published", FALSE), shareY = TRUE, widths = c(0.28, 0.72), margin = 0.03) |>
    taf_layout(
      annotations = list(title("Published", "x"), title("Not published", "x2")),
      margin = list(t = 40)
    ) |>
    layout(legend = list(y = -0.12, traceorder = "normal"))
  if (!is.null(shared)) {
    p <- p |>
      highlight(
        on = "plotly_click", off = "plotly_doubleclick", opacityDim = 0.35,
        selected = attrs_selected(showlegend = FALSE)
      ) |>
      config(doubleClickDelay = 1000)
  }
  p
}
