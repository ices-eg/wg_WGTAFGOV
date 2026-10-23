# The list of ICES advice sheets, and the GitHub links in them.
#
# advice-sheets.csv: the advice sheets, starting from those the previous run
# read (../previous/advice-sheets.csv), with what is new or changed since.
# The site chooses the current sheet of each stock and year, see
# _taf/site/data.R.
#
# The advice sheets are the "ICES Advice: Recurrent advice" group in the ICES
# library, read with the figshare API. Only advice from first_year on is of
# interest, and requests are kept to a minimum:
#   - one request lists the sheets added to the library since first_year
#   - the details of a sheet, which hold its stock code and year, are read
#     once, and again only if the sheet is modified
#   - the pdf of a sheet is downloaded once, and again only if it is replaced
#
# The pdf is searched for links to GitHub repositories of the ICES
# organisations. These are hyperlinks, not text, so are looked for in the
# contents of the file rather than in the text of the pages.

data_file <- "advice-sheets.csv"
previous_file <- "../previous/advice-sheets.csv"
first_year <- 2024
api <- "https://api.figshare.com/v2"
# ICES Advice: Recurrent advice
advice_group <- 37143

columns <- c(
  "id", "modified", "year", "stock", "published", "doi", "url", "title",
  "pdf_id", "pdf_url", "links"
)

# ---- the figshare API ------------------------------------------------------

# one request. The API allows about a request a second, answers 429 when asked
# for more and then refuses everything for a while, so keep to that pace and
# wait before trying again.
fetch <- function(url) {
  for (attempt in 1:5) {
    started <- Sys.time()
    res <- tryCatch(curl::curl_fetch_memory(url), error = function(e) NULL)
    if (!is.null(res) && res$status_code == 200) {
      Sys.sleep(max(0, 1 - as.numeric(difftime(Sys.time(), started, units = "secs"))))
      return(res$content)
    }
    Sys.sleep(60 * attempt)
  }
  NULL
}

api_get <- function(path) {
  content <- fetch(paste0(api, path))
  if (is.null(content)) NULL else jsonlite::fromJSON(rawToChar(content), simplifyVector = FALSE)
}

# the sheets added to the library since first_year: advice cannot have been
# added before the year it was given
list_sheets <- function() {
  all <- list()
  page <- 1
  repeat {
    res <- api_get(sprintf(
      "/articles?group=%d&published_since=%d-01-01&page_size=1000&page=%d",
      advice_group, first_year, page
    ))
    if (is.null(res)) stop("could not list the advice sheets")
    all <- c(all, res)
    if (length(res) < 1000) break
    page <- page + 1
  }
  data.frame(
    id = vapply(all, function(x) as.character(x$id), ""),
    modified = vapply(all, function(x) x$modified_date, ""),
    stringsAsFactors = FALSE
  )
}

# the details of one sheet
read_details <- function(id) api_get(sprintf("/articles/%s", id))

# what is needed from the details of a sheet
summarise_sheet <- function(x, stock_codes) {
  field <- function(name) {
    value <- Filter(function(f) f$name == name, x$custom_fields)
    if (length(value) && is.character(value[[1]]$value)) value[[1]]$value else NA_character_
  }
  citation <- field("Recommended citation")
  pdf <- Filter(function(f) grepl("[.]pdf$", f$name, ignore.case = TRUE), x$files)

  # the stock code: a tag, the name of the pdf, or in the citation
  candidates <- c(
    unlist(x$tags),
    sub("[.]pdf$", "", vapply(pdf, function(f) f$name, ""), ignore.case = TRUE),
    regmatches(citation, gregexpr("[a-z]{3}[.][0-9a-zA-Z.-]+[0-9a-zA-Z]", citation))[[1]]
  )
  stock <- candidates[candidates %in% stock_codes]

  # the year the advice was given, "ICES Advice 2026" in the citation, which
  # is not the year a sheet was added to the library
  year <- regmatches(citation, regexpr("ICES Advice,? (20[0-9]{2})", citation))
  year <- if (length(year)) sub(".* ", "", year) else substr(x$timeline$publisherPublication, 1, 4)

  data.frame(
    id = as.character(x$id),
    modified = x$modified_date,
    year = if (length(year)) as.numeric(year) else NA_real_,
    stock = if (length(stock)) stock[1] else NA_character_,
    published = substr(x$published_date, 1, 10),
    doi = x$doi,
    url = x$url_public_html,
    title = gsub("<[^>]+>", "", x$title),
    pdf_id = if (length(pdf)) as.character(pdf[[1]]$id) else NA_character_,
    pdf_url = if (length(pdf)) pdf[[1]]$download_url else NA_character_,
    links = NA_character_,
    stringsAsFactors = FALSE
  )
}

# ---- links in a pdf --------------------------------------------------------

# the links to GitHub repositories of the ICES organisations in a pdf, as
# they are written in the file. A link is held in the file as /URI (...),
# often inside a compressed part of it, so look in the file as it is and in
# each part that can be decompressed.
pdf_links <- function(file) {
  raw <- readBin(file, "raw", file.size(file))
  as_text <- function(x) rawToChar(x[x != as.raw(0)])
  find_links <- function(text) {
    text <- iconv(text, "latin1", "UTF-8", sub = "")
    uri <- regmatches(text, gregexpr("/URI\\s*\\(([^)]*)\\)", text))[[1]]
    uri <- sub("^/URI\\s*\\(", "", sub("\\)$", "", uri))
    uri[grepl("github[.]com/ices-", uri, ignore.case = TRUE)]
  }

  links <- find_links(as_text(raw))
  starts <- grepRaw("stream\r?\n", raw, all = TRUE)
  ends <- grepRaw("endstream", raw, all = TRUE)
  for (start in starts) {
    # the end of "endstream" also looks like a start
    if (start >= 4 && rawToChar(raw[(start - 3):(start - 1)]) == "end") next
    from <- start + if (raw[start + 6] == as.raw(13)) 8 else 7
    to <- ends[ends > from][1] - 1
    if (is.na(to) || to <= from) next
    part <- tryCatch(memDecompress(raw[from:to], type = "gzip"), error = function(e) NULL)
    if (!is.null(part)) links <- c(links, find_links(as_text(part)))
  }
  unique(utils::URLdecode(links))
}

# ---- update ----------------------------------------------------------------

stock_codes <- icesVocab::getCodeList("ICES_StockCode")$Key

known <- if (file.exists(previous_file)) {
  read.csv(previous_file, colClasses = "character", na.strings = "NA")
} else {
  setNames(data.frame(matrix(character(), 0, length(columns)), stringsAsFactors = FALSE), columns)
}
known$year <- as.numeric(known$year)

# read the details of the sheets that are new or modified. If the library
# cannot be read, what the previous run read still holds.
listed <- tryCatch(list_sheets(), error = function(e) {
  warning("Could not list the advice sheets, keeping those read before: ", conditionMessage(e))
  known[c("id", "modified")]
})
message(nrow(listed), " advice sheets added to the library since ", first_year)
new <- which(!paste(listed$id, listed$modified) %in% paste(known$id, known$modified))
if (length(new)) message("reading the details of ", length(new), " advice sheets")
for (i in new) {
  details <- read_details(listed$id[i])
  if (is.null(details)) {
    warning("stopped at an advice sheet that could not be read; run again to carry on")
    break
  }
  sheet <- summarise_sheet(details, stock_codes)
  # the links of a pdf that has not been replaced still hold
  old <- known[known$id == sheet$id, ]
  if (nrow(old) && identical(old$pdf_id, sheet$pdf_id)) sheet$links <- old$links
  known <- rbind(known[known$id != sheet$id, ], sheet)
}
# sheets taken out of the library
known <- known[known$id %in% listed$id, ]

# look in the pdf of the advice from first_year on that has not been looked at
todo <- which(known$year >= first_year & !is.na(known$pdf_url) & is.na(known$links))
if (length(todo)) message("looking for links in ", length(todo), " advice sheets")
for (i in todo) {
  content <- fetch(known$pdf_url[i])
  if (is.null(content)) {
    warning("stopped at a pdf that could not be downloaded; run again to carry on")
    break
  }
  file <- tempfile(fileext = ".pdf")
  writeBin(content, file)
  known$links[i] <- paste(pdf_links(file), collapse = " | ")
  unlink(file)
}

known <- known[order(known$year, known$stock, known$id), columns]
write.csv(known, data_file, row.names = FALSE)

recent <- known[known$year %in% first_year:9999, ]
message(
  nrow(recent), " advice sheets from ", first_year, " on, ",
  sum(nzchar(recent$links), na.rm = TRUE), " with a link to GitHub"
)
