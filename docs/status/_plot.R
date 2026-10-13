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

# the bars of a chart, each with the keys of the rows of a table in its "key"
# column, linked to the table so that clicking a bar filters it. A set of bars
# with only one bar in it is not linked properly by plotly, so is given a
# second one with no height, which is not drawn.
linked_bars <- function(bars, shared, n = "n") {
  if (nrow(bars) == 1) {
    empty <- bars
    empty[[n]] <- NA
    empty$key <- I(list(character()))
    bars <- rbind(bars, empty)
  }
  crosstalk::SharedData$new(bars, key = ~key, group = shared$groupName())
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
  keys <- if (is.null(shared)) as.character(seq_len(nrow(assessments))) else shared$key()
  published <- assessments$Published == "Published"
  no_level <- assessments$Level == "No level"
  levels_found <- setdiff(levels(droplevels(assessments$Level)), "No level")

  # the bars: a block of assessments in each year, with the keys of its
  # assessments, which is what links it to the rows of the table
  block <- function(rows, panel, name, colour, group = name) {
    if (!any(rows)) {
      return(NULL)
    }
    d <- aggregate(list(key = keys[rows]), list(Year = assessments$Year[rows]), function(x) I(list(x)))
    data.frame(d, n = lengths(d$key), panel = panel, name = name, colour = colour, group = group)
  }
  # the blocks from the bottom of a bar up. Those with no level are at the
  # top, so that the rest still stand on the axis when "No level" is clicked
  # in the legend to hide them. In the panel of the not published, the
  # published are drawn again on top, so that its bars reach the total for
  # the year.
  blocks <- c(
    lapply(levels_found, function(level) {
      block(published & assessments$Level == level, "Published", level, readiness_colours[[level]])
    }),
    list(block(published & no_level, "Published", "No level", readiness_colours[["No level"]])),
    lapply(levels_found, function(level) {
      block(!published & assessments$Level == level, "Not published", level, readiness_colours[[level]])
    }),
    list(
      block(published & !no_level, "Not published", "Published", "#d3e7c0"),
      block(!published & no_level, "Not published", "No level", readiness_colours[["No level"]]),
      block(published & no_level, "Not published", "Published, no level", "#e9f2df", group = "No level")
    )
  )
  blocks <- Filter(Negate(is.null), blocks)
  # stacked by hand, so that a clicked bar is highlighted in place
  height <- numeric()
  for (i in seq_along(blocks)) {
    at <- paste(blocks[[i]]$panel, blocks[[i]]$Year)
    below <- unname(ifelse(at %in% names(height), height[at], 0))
    blocks[[i]]$base <- below
    height[at] <- below + blocks[[i]]$n
  }
  # one entry in the legend for each name
  names_seen <- vapply(blocks, function(d) d$name[1], "")
  in_legend <- !duplicated(names_seen) & names_seen != "Published, no level"

  # one panel of the chart
  panel <- function(which) {
    p <- plot_ly()
    for (i in seq_along(blocks)) {
      d <- blocks[[i]]
      if (d$panel[1] != which) next
      colour <- d$colour[1]
      data <- if (is.null(shared)) d else linked_bars(d, shared)
      p <- p |>
        add_bars(
          data = data,
          x = ~Year, y = ~n, base = ~base,
          name = d$name[1], marker = list(color = colour, line = list(color = "white", width = 2)),
          legendgroup = d$group[1], showlegend = in_legend[i],
          # those with no level are left out until clicked in the legend
          visible = if (d$group[1] == "No level") "legendonly" else TRUE,
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
  p <- subplot(panel("Published"), panel("Not published"), shareY = TRUE, widths = c(0.28, 0.72), margin = 0.03) |>
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
      config(doubleClickDelay = 500)
  }
  p
}
