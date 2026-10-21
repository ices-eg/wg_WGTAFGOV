# The RDBES workflow page, status/rdbes.qmd: the diagram, the tables and the
# summary, from _data/rdbes.json and what _scripts/status.R found in GitHub.

rdbes <- jsonlite::fromJSON("../_data/rdbes.json")
rdbes_repos <- read.csv("../_generated/rdbes-repos.csv")
taf_repos <- read.csv("../_generated/taf-repos.csv")
sheets <- read.csv("../_generated/advice-sheets.csv")

# the countries with data for each stock of the RDBES workflow: one row for
# each stock, with the countries separated by commas
read_stock_countries <- function(file) {
  x <- read.csv(file, stringsAsFactors = FALSE)
  countries <- strsplit(x$CountriesWithData, "\\s*,\\s*")
  data.frame(stock = rep(x$StockCode, lengths(countries)), country = trimws(unlist(countries)))
}

pairs <- read_stock_countries("../_data/rdbes-stock-countries.csv")

year <- rdbes$year
# the countries named, and those with data for a stock
countries <- sort(union(rdbes$countries, pairs$country))
stocks <- rdbes$stocks

esc <- function(x) htmltools::htmlEscape(x, attribute = TRUE)
fmt <- function(...) sprintf(...)

# ---- the parts and their status --------------------------------------------

# what can be said of a repository, the label shown on it and an explanation
node_statuses <- data.frame(
  key = c("none", "l0", "l1", "l2", "runs", "failing"),
  label = c("No repository", "No level", "Level 1", "Level 2", "Runs", "Failing"),
  title = c(
    "There is no repository yet",
    "The repository has no code in it yet",
    "TAF readiness level 1: the code is on GitHub",
    "TAF readiness level 2: the code passes the QC test",
    "The code runs successfully",
    "The code used to run but now fails"
  )
)

# what can be said of a connection between two parts. It is found in the
# code: connected when the first uploads to the RDBES and the second downloads
# from it, see _scripts/status.R. Broken can only be recorded by hand.
link_statuses <- data.frame(
  key = c("connected", "partial", "not-connected", "broken"),
  label = c("Connected", "Partly connected", "Not connected", "Broken"),
  # as written in _data/rdbes.json
  value = c("connected", "partly connected", "not connected", "broken"),
  symbol = c("\u25CF", "\u25D0", "\u25CB", "\u2715"),
  title = c(
    "The code of the first uploads to the RDBES, and the code of the second downloads from it",
    "Only one of the two has the code to upload or to download",
    "Neither has the code to upload or to download",
    "Recorded by hand: the two used to be connected but are not now"
  )
)

link_key <- function(value) {
  link_statuses$key[match(tolower(value), link_statuses$value)]
}

# a status set by hand in _data/rdbes.json, which is taken over what is found
# in GitHub: there is no automatic run test yet
override <- function(name, field) {
  x <- rdbes$repositories[[name]][[field]]
  if (is.null(x)) NA_character_ else x
}

# a repository, with its status and what was found of its connections to
# the RDBES: the files that download from it and upload to it
repo_node <- function(name, trl) {
  i <- match(name, rdbes_repos$name)
  exists <- rdbes_repos$exists[i] %in% TRUE
  key <- if (!exists) "none" else if (is.na(trl)) "l0" else paste0("l", trl)
  set <- override(name, "status")
  if (!is.na(set)) key <- set
  files <- function(x) if (is.na(x)) "" else x
  data.frame(
    name = name, key = key,
    url = fmt("https://github.com/ices-taf/%s", name),
    note = override(name, "note"),
    download = files(rdbes_repos$download[i]),
    upload = files(rdbes_repos$upload[i]),
    sandbox = rdbes_repos$sandbox[i] %in% TRUE
  )
}

stage <- function(x) rdbes_repos[rdbes_repos$stage == x, ]

national <- cbind(country = countries, do.call(rbind, Map(repo_node, stage("national")$name, stage("national")$trl)))
coordination <- cbind(stock = stocks, do.call(rbind, Map(repo_node, stage("coordination")$name, stage("coordination")$trl)))
# the level of an assessment is that on the readiness page
assessment <- cbind(stock = stocks, do.call(rbind, lapply(stage("assessment")$name, function(name) {
  repo_node(name, taf_repos$trl[match(name, taf_repos$name)])
})))

# the advice sheet of each stock, and what was found of the link in it to the
# published code, see status/advice-sheets.qmd
advice <- do.call(rbind, lapply(stocks, function(stock) {
  i <- match(paste(year, stock), paste(sheets$year, sheets$stock))
  sheet <- !is.na(i)
  link <- if (sheet) sheets$status[i] else "No advice yet"
  data.frame(
    stock = stock,
    name = "Advice",
    key = if (sheet) "published" else "none",
    url = if (sheet) sheets$url[i] else NA,
    note = NA,
    link = link,
    link_key = if (link == "OK") {
      "connected"
    } else if (link %in% c("No advice yet", "No link", "Repository not linked", "Not checked")) {
      "not-connected"
    } else {
      "partial"
    }
  )
}))

# a connection through the RDBES, from what was found in the code of the
# repository that uploads and of the one that downloads, unless a status is
# recorded by hand
connection <- function(from, to, status = NA) {
  up <- nzchar(from$upload)
  down <- nzchar(to$download)
  key <- if (!is.na(status) && !is.na(link_key(status))) {
    link_key(status)
  } else if (up && down) {
    "connected"
  } else if (up || down) {
    "partial"
  } else {
    "not-connected"
  }
  said <- function(node, what, files) {
    fmt("%s %s: %s", node$name, what, if (nzchar(files)) paste("yes, in", files) else "not found in the code")
  }
  sandbox <- if (from$sandbox || to$sandbox) "The RDBES sandbox is used." else NULL
  list(key = key, found = paste(c(said(from, "uploads", from$upload), said(to, "downloads", to$download), sandbox), collapse = ". "))
}

# the connections from the countries to the stocks: those with data, from
# _data/rdbes-stock-countries.csv, with any status or note recorded by hand
# for them in _data/rdbes.json
links <- pairs[pairs$stock %in% stocks, ]
set <- if (length(rdbes$links)) as.data.frame(rdbes$links) else data.frame(country = character(), stock = character())
i <- match(paste(links$country, links$stock), paste(set$country, set$stock))
links$status <- if (is.null(set$status)) NA else set$status[i]
links$note <- if (is.null(set$note)) NA else set$note[i]
found <- lapply(seq_len(nrow(links)), function(i) {
  connection(
    national[match(links$country[i], national$country), ],
    coordination[match(links$stock[i], coordination$stock), ],
    links$status[i]
  )
})
links$key <- vapply(found, `[[`, "", "key")
links$found <- vapply(found, `[[`, "", "found")

# the connections from the stock coordination to the assessments
set <- if (length(rdbes$stock_links)) as.data.frame(rdbes$stock_links) else data.frame(stock = character())
found <- lapply(seq_along(stocks), function(i) {
  j <- match(stocks[i], set$stock)
  connection(coordination[i, ], assessment[i, ], if (is.na(j) || is.null(set$status)) NA else set$status[j])
})
stock_links <- data.frame(
  stock = stocks,
  key = vapply(found, `[[`, "", "key"),
  found = vapply(found, `[[`, "", "found"),
  note = if (is.null(set$note)) NA else set$note[match(stocks, set$stock)]
)

# a stock is connected end to end when every part of it runs and every
# connection is made
end_to_end <- vapply(stocks, function(stock) {
  mine <- links[links$stock == stock, ]
  nrow(mine) > 0 &&
    all(mine$key == "connected") &&
    all(national$key[match(mine$country, national$country)] == "runs") &&
    coordination$key[coordination$stock == stock] == "runs" &&
    stock_links$key[stock_links$stock == stock] == "connected" &&
    assessment$key[assessment$stock == stock] == "runs" &&
    advice$link_key[advice$stock == stock] == "connected"
}, TRUE)

# ---- summary ---------------------------------------------------------------

rdbes_summary <- function() {
  tile <- function(value, of, label) {
    fmt(
      "<div class=\"taf-tile\"><div class=\"taf-tile-value\">%d <small class=\"text-body-secondary\">/ %d</small></div><div class=\"taf-tile-label\">%s</div></div>",
      value, of, label
    )
  }
  has_code <- function(x) sum(x$key %in% c("l1", "l2", "runs"))
  htmltools::HTML(paste0(
    "<div class=\"taf-tiles\">",
    tile(has_code(national), nrow(national), "national estimation repositories with code"),
    tile(has_code(coordination), nrow(coordination), "stock coordination repositories with code"),
    tile(has_code(assessment), nrow(assessment), "assessments at level 1 or above"),
    tile(sum(links$key == "connected"), nrow(links), "countries connected to a stock"),
    tile(sum(end_to_end), length(stocks), "stocks connected end to end"),
    "</div>"
  ))
}

# ---- the diagram -----------------------------------------------------------

# the columns, left to right: x and width
col <- list(
  national = c(0, 130),
  db1 = c(218, 26),
  coordination = c(332, 190),
  db2 = c(570, 26),
  assessment = c(644, 190),
  advice = c(882, 134)
)
width <- 1016
top <- 44
row_h <- 58
node_h <- 44
# as many rows as the longer of the two lists, the other spread over them
rows <- max(length(stocks), length(countries))
height <- top + rows * row_h

stock_y <- setNames(top + (seq_along(stocks) - 0.5) * (rows * row_h / length(stocks)), stocks)
country_y <- setNames(top + (seq_along(countries) - 0.5) * (rows * row_h / length(countries)), countries)

node_svg <- function(column, y, label, node, data) {
  x <- col[[column]][1]
  w <- col[[column]][2]
  status <- if (node$key == "published") {
    "Published"
  } else if (column == "advice") {
    "Not published"
  } else {
    node_statuses$label[match(node$key, node_statuses$key)]
  }
  title <- paste0(
    if (column == "advice") paste(label, "advice") else node$name, ": ",
    if (column == "advice") {
      if (node$key == "published") "the advice is published" else "the advice is not published yet"
    } else {
      node_statuses$title[match(node$key, node_statuses$key)]
    },
    if (column != "advice" && node$key != "none") {
      paste0(
        ". Downloads from the RDBES: ", if (nzchar(node$download)) paste("yes, in", node$download) else "not found in the code",
        if (column != "assessment") {
          paste0(". Uploads to the RDBES: ", if (nzchar(node$upload)) paste("yes, in", node$upload) else "not found in the code")
        },
        if (node$sandbox) ". Uses the RDBES sandbox"
      )
    },
    if (!is.na(node$note)) paste0(". ", node$note) else ""
  )
  # the ports: in on the left, downloading from the RDBES, and out on the
  # right, uploading to it. An assessment uploads nothing.
  port <- function(px, on, what) {
    fmt(
      "<rect class=\"port %s\" x=\"%g\" y=\"%g\" width=\"10\" height=\"10\" rx=\"2\"><title>%s</title></rect>",
      if (on) "port-on" else "port-off", px - 5, y - 5, what
    )
  }
  ports <- if (column == "advice" || node$key == "none") {
    ""
  } else {
    paste0(
      port(x, nzchar(node$download), if (nzchar(node$download)) "Downloads from the RDBES" else "No download from the RDBES found"),
      if (column != "assessment") port(x + w, nzchar(node$upload), if (nzchar(node$upload)) "Uploads to the RDBES" else "No upload to the RDBES found")
    )
  }
  body <- fmt(
    paste0(
      "<g class=\"node s-%s\" %s><title>%s</title>",
      "<rect class=\"box\" x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" rx=\"6\"/>",
      "<circle class=\"lamp\" cx=\"%g\" cy=\"%g\" r=\"7\"/>",
      "<text class=\"node-name\" x=\"%g\" y=\"%g\">%s</text>",
      "<text class=\"node-status\" x=\"%g\" y=\"%g\">%s</text>%s</g>"
    ),
    node$key, data, esc(title),
    x, y - node_h / 2, w, node_h,
    x + 19, y,
    x + 34, y - 4, esc(label),
    x + 34, y + 12, esc(status),
    ports
  )
  if (is.na(node$url)) body else fmt("<a href=\"%s\">%s</a>", esc(node$url), body)
}

pipe_svg <- function(d, key, title, data, mid) {
  broken <- if (key == "broken") {
    fmt("<text class=\"pipe-break\" x=\"%g\" y=\"%g\">✕</text>", mid[1], mid[2] + 5)
  } else {
    ""
  }
  fmt(
    "<g class=\"pipe-g\" %s><title>%s</title><path class=\"pipe-bg\" d=\"%s\"/><path class=\"pipe s-%s\" d=\"%s\"/>%s</g>",
    data, esc(title), d, key, d, broken
  )
}

link_title <- function(from, to, key, found, note) {
  paste0(
    from, " to ", to, ": ",
    link_statuses$label[match(key, link_statuses$key)], ". ", found,
    ifelse(is.na(note), "", paste0(". ", note))
  )
}

rdbes_diagram <- function() {
  right <- function(column) sum(col[[column]])
  left <- function(column) col[[column]][1]

  pipes <- c(
    # the countries to the stock coordination, through the RDBES database
    vapply(seq_len(nrow(links)), function(i) {
      x1 <- right("national")
      x2 <- left("coordination")
      y1 <- country_y[[links$country[i]]]
      y2 <- stock_y[[links$stock[i]]]
      xm <- (x1 + x2) / 2
      pipe_svg(
        fmt("M%g,%g C%g,%g %g,%g %g,%g", x1, y1, xm, y1, xm, y2, x2, y2),
        links$key[i],
        link_title(links$country[i], links$stock[i], links$key[i], links$found[i], links$note[i]),
        fmt("data-country=\"%s\" data-stock=\"%s\"", esc(links$country[i]), esc(links$stock[i])),
        c(xm, (y1 + y2) / 2)
      )
    }, ""),
    # the stock coordination to the assessment, through the RDBES database
    vapply(stocks, function(stock) {
      y <- stock_y[[stock]]
      i <- match(stock, stock_links$stock)
      x1 <- right("coordination")
      x2 <- left("assessment")
      pipe_svg(
        fmt("M%g,%g L%g,%g", x1, y, x2, y),
        stock_links$key[i],
        link_title(paste(stock, "coordination"), "assessment", stock_links$key[i], stock_links$found[i], stock_links$note[i]),
        fmt("data-stock=\"%s\"", esc(stock)),
        c((x1 + x2) / 2, y)
      )
    }, ""),
    # the assessment to the advice
    vapply(stocks, function(stock) {
      y <- stock_y[[stock]]
      i <- match(stock, advice$stock)
      x1 <- right("assessment")
      x2 <- left("advice")
      pipe_svg(
        fmt("M%g,%g L%g,%g", x1, y, x2, y),
        advice$link_key[i],
        paste0(stock, " assessment to advice: ", advice$link[i]),
        fmt("data-stock=\"%s\"", esc(stock)),
        c((x1 + x2) / 2, y)
      )
    }, "")
  )

  # the database, drawn over the pipes so that they go into it and out again
  db <- vapply(c("db1", "db2"), function(column) {
    x <- col[[column]][1]
    w <- col[[column]][2]
    fmt(
      paste0(
        "<g class=\"db\"><title>The RDBES database</title>",
        "<rect x=\"%g\" y=\"%g\" width=\"%g\" height=\"%g\" rx=\"8\"/>",
        "<text x=\"%g\" y=\"%g\" transform=\"rotate(-90 %g %g)\">RDBES</text></g>"
      ),
      x, top - 6, w, height - top + 6,
      x + w / 2 + 5, top + (height - top) / 2, x + w / 2 + 5, top + (height - top) / 2
    )
  }, "")

  nodes <- c(
    vapply(seq_along(countries), function(i) {
      node_svg("national", country_y[[i]], countries[i], national[i, ], fmt("data-country=\"%s\"", esc(countries[i])))
    }, ""),
    unlist(lapply(seq_along(stocks), function(i) {
      data <- fmt("data-stock=\"%s\"", esc(stocks[i]))
      c(
        node_svg("coordination", stock_y[[i]], stocks[i], coordination[i, ], data),
        node_svg("assessment", stock_y[[i]], stocks[i], assessment[i, ], data),
        node_svg("advice", stock_y[[i]], stocks[i], advice[i, ], data)
      )
    }))
  )

  headings <- vapply(
    list(
      c("national", "National estimation"), c("db1", "RDBES"),
      c("coordination", "Stock coordination"), c("db2", "RDBES"),
      c("assessment", "Stock assessment"), c("advice", "Advice")
    ),
    function(h) {
      fmt("<text class=\"heading\" x=\"%g\" y=\"16\">%s</text>", col[[h[1]]][1] + col[[h[1]]][2] / 2, h[2])
    },
    ""
  )

  htmltools::HTML(paste0(
    "<div class=\"rdbes-diagram\" id=\"rdbes-diagram\">",
    fmt(
      "<svg viewBox=\"-8 0 %d %d\" role=\"img\" aria-labelledby=\"rdbes-title\">",
      # room for the ports at the edges
      width + 16, height
    ),
    fmt(
      "<title id=\"rdbes-title\">The RDBES workflow in %d: the national estimation repositories of %d countries, connected through the RDBES database to the stock coordination and stock assessment repositories of %d stocks, and to their advice</title>",
      year, length(countries), length(stocks)
    ),
    paste(headings, collapse = ""),
    paste(pipes, collapse = ""),
    paste(db, collapse = ""),
    paste(nodes, collapse = ""),
    "</svg></div>"
  ))
}

# the key to the diagram
rdbes_legend <- function() {
  lamp <- function(key, label, title) {
    fmt(
      "<span class=\"item\" title=\"%s\"><svg width=\"16\" height=\"16\"><circle class=\"lamp s-%s\" cx=\"8\" cy=\"8\" r=\"6\"/></svg>%s</span>",
      esc(title), key, esc(label)
    )
  }
  pipe <- function(key, label, title) {
    fmt(
      "<span class=\"item\" title=\"%s\"><svg width=\"40\" height=\"16\"><path class=\"pipe-bg\" d=\"M4,8 L36,8\"/><path class=\"pipe s-%s\" d=\"M4,8 L36,8\"/></svg>%s</span>",
      esc(title), key, esc(label)
    )
  }
  htmltools::HTML(paste0(
    "<div class=\"rdbes-legend\"><div><strong>Repositories</strong>",
    paste(unlist(Map(lamp, node_statuses$key, node_statuses$label, node_statuses$title)), collapse = ""),
    "</div><div><strong>Ports</strong>",
    "<span class=\"item\"><svg width=\"14\" height=\"14\"><rect class=\"port port-on\" x=\"2\" y=\"2\" width=\"10\" height=\"10\" rx=\"2\"/></svg>Downloads or uploads</span>",
    "<span class=\"item\"><svg width=\"14\" height=\"14\"><rect class=\"port port-off\" x=\"2\" y=\"2\" width=\"10\" height=\"10\" rx=\"2\"/></svg>Not found in the code</span>",
    "</div><div><strong>Connections</strong>",
    paste(unlist(Map(pipe, link_statuses$key, link_statuses$label, link_statuses$title)), collapse = ""),
    "</div></div>"
  ))
}

# ---- tables ----------------------------------------------------------------

link_cell <- function(key, title) {
  i <- match(key, link_statuses$key)
  fmt(
    "<span class=\"link-mark s-%s\" title=\"%s\">%s</span><span class=\"visually-hidden\">%s</span>",
    key, esc(title), link_statuses$symbol[i], link_statuses$label[i]
  )
}

# the countries by the stocks, with the state of each connection recorded
rdbes_matrix <- function() {
  rows <- vapply(countries, function(country) {
    cells <- vapply(stocks, function(stock) {
      i <- which(links$country == country & links$stock == stock)
      if (!length(i)) {
        return("<td></td>")
      }
      fmt("<td>%s</td>", link_cell(links$key[i[1]], link_title(country, stock, links$key[i[1]], links$found[i[1]], links$note[i[1]])))
    }, "")
    fmt("<tr><th scope=\"row\">%s</th>%s</tr>", esc(country), paste(cells, collapse = ""))
  }, "")
  htmltools::HTML(paste0(
    "<div class=\"table-responsive\"><table class=\"table table-sm rdbes-matrix\"><thead><tr><th></th>",
    paste(fmt("<th scope=\"col\"><span>%s</span></th>", esc(stocks)), collapse = ""),
    "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>"
  ))
}

# one row for each stock, along its chain
rdbes_stocks <- function() {
  repo_cell <- function(node) {
    status <- node_statuses$label[match(node$key, node_statuses$key)]
    if (node$key == "none") {
      return(status)
    }
    fmt("<a href=\"%s\">%s</a>", esc(node$url), status)
  }
  rows <- vapply(seq_along(stocks), function(i) {
    stock <- stocks[i]
    mine <- links[links$stock == stock, ]
    notes <- c(coordination$note[i], stock_links$note[i], assessment$note[i])
    fmt(
      "<tr><th scope=\"row\">%s</th><td>%s</td><td class=\"num\">%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
      esc(stock),
      repo_cell(coordination[i, ]),
      if (nrow(mine)) fmt("%d / %d", sum(mine$key == "connected"), nrow(mine)) else "None recorded",
      repo_cell(assessment[i, ]),
      if (is.na(advice$url[i])) esc(advice$link[i]) else fmt("<a href=\"%s\">%s</a>", esc(advice$url[i]), esc(advice$link[i])),
      if (end_to_end[i]) "Yes" else "No",
      esc(paste(notes[!is.na(notes)], collapse = " "))
    )
  }, "")
  htmltools::HTML(paste0(
    "<div class=\"table-responsive\"><table class=\"table table-sm\"><thead><tr>",
    "<th scope=\"col\">Stock</th><th scope=\"col\">Coordination</th><th scope=\"col\">Countries connected</th>",
    "<th scope=\"col\">Assessment</th><th scope=\"col\">Advice link</th><th scope=\"col\">End to end</th><th scope=\"col\">Notes</th>",
    "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>"
  ))
}

# what was found in the code of each repository: the files that download from
# the RDBES and that upload to it
rdbes_found <- function() {
  nodes <- rbind(
    cbind(part = "National estimation", national[c("name", "key", "url", "download", "upload", "sandbox")]),
    cbind(part = "Stock coordination", coordination[c("name", "key", "url", "download", "upload", "sandbox")]),
    cbind(part = "Stock assessment", assessment[c("name", "key", "url", "download", "upload", "sandbox")])
  )
  files <- function(x, none = "Not found") {
    ifelse(nzchar(x), vapply(strsplit(x, " "), function(f) paste0("<code>", esc(f), "</code>", collapse = "<br>"), ""), none)
  }
  rows <- fmt(
    "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
    nodes$part,
    ifelse(nodes$key == "none", paste(esc(nodes$name), "(no repository)"), fmt("<a href=\"%s\">%s</a>", esc(nodes$url), esc(nodes$name))),
    ifelse(nodes$key == "none", "", files(nodes$download)),
    ifelse(nodes$key == "none" | nodes$part == "Stock assessment", "", files(nodes$upload)),
    ifelse(nodes$sandbox, "Yes", "")
  )
  htmltools::HTML(paste0(
    "<div class=\"table-responsive\"><table class=\"table table-sm\"><thead><tr>",
    "<th scope=\"col\">Part</th><th scope=\"col\">Repository</th><th scope=\"col\">Downloads from the RDBES</th>",
    "<th scope=\"col\">Uploads to the RDBES</th><th scope=\"col\">Sandbox</th>",
    "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>"
  ))
}
