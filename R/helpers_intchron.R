#' Read data from IntChron
#'
#' Reads data from IntChron as a data frame.
#'
#' @param url  Address of the IntChron record. The file extension (if any) is
#'  ignored in favour of the one specified by `format`.
#' @param format Format to read. Currently only "csv" is implemented.
#'
#' @details
#' To avoid unnecessary file system operations, this function reads directly
#' from a URL connection, not a file.
#'
#' @return
#' A data frame.
#'
#' @noRd
read_intchron <- function(url, format = c("csv")) {
  # Format URL
  url_ext <- tools::file_ext(url)
  if (url_ext == "") {
    url <- paste0(url, ".", format)
  }
  else if (url_ext != format) {
    url <- sub(paste0(".", url_ext), paste0(".", format), url)
  }
  url <- URLencode(url)

  message("DEBUG: Reading ", url, " ...")

  # Retrieve data from IntChron
  check_connection_to_url(url)
  lines <- readLines(url, warn = FALSE)

  if (format == "csv") {
    return(read_intchron_csv(lines))
  }
  else {
    stop(format, " format not supported.")
  }
}

#' Read a CSV file from IntChron
#'
#' Reads data is retrieved in the 'csv' format provided by IntChron. This is a
#' regular CSV file with a few quirks (see details).
#'
#' @param lines Vector of lines retrieved from IntChron, i.e. from [readLines()].
#'
#' @details
#' Quirks identified so far:
#'
#' * A variable number of comment lines (denoted with '#') before and after the data.
#' * The comment line immediately above the data contains the column headings
#' * A variable number of empty columns at the beginning of a row.
#' * A trailing comma on every except the header
#' * Missing values coded as: "", "-"
#' * Doesn't have a final EOL (so readLines() gives a warning with default options)
#' * Some tables are fundamentally malformed (e.g. unmatched quotes)
#'
#' Currently all comments apart from the column names are discarded. In future,
#' we might want to extract some of this information (e.g. the footer often
#' contains bibliographic references.)
#'
#' @return
#' A data frame.
#'
#' @noRd
read_intchron_csv <- function(lines) {
  # Check whether there's actually any non-comment lines
  if (all(grepl("^#", lines) | grepl("^$", lines))) {
    return(data.frame(NA))
  }

  # Reformat the header row
  nheader <- grep("^,", lines)[1] - 1
  lines[nheader] <- sub("#", "", lines[nheader])
  lines[nheader] <- paste0(lines[nheader], ",")

  # Read data table
  # Catch errors here so that malformed records don't break an entire crawl
  data <- NULL
  # TODO: Replace with tryCatch and a more informative error message
  try({
    data <- read.csv(text = lines, stringsAsFactors = FALSE,
                     comment.char = "#", na.strings = c("", "-"))
  })
  if (is.null(data)) {
    return(data.frame(NA))
  }

  # Drop unnamed columns (assumed to be empty)
  data <- data[!grepl("^X(\\.[0-9]+)?$", names(data))]

  return(data)
}

#' Recursively retrieve IntChron records
#'
#' Retrieves the entire IntChron database from a given entry point page. This
#' function works recursively; it retrieves the data for the entry page, and if
#' this contains a 'file' column with links to another set of pages, calls
#' itself on each of these.
#'
#' @param url  Page to start crawling
#' @param ignore  Branches to ignore
#'
#' @return
#' A data frame combining all retrieved data tables. All columns are coerced to
#' character to ensure they can be combined with [dplyr::bind_rows()].
#'
#' @noRd
crawl_intchron <- function(url, ignore = NA) {
  data <- read_intchron(url)
  if ("file" %in% names(data)) {
    data <- data[!basename(tools::file_path_sans_ext(data$file)) %in% ignore,]
    return(
      purrr::pmap_dfr(data,
                  function(file, ..., ignore = NA) {
                    out <- crawl_intchron(file, ignore)
                    # Bypass vctrs' strict type checking in dplyr::bind_rows()
                    out <- purrr::map_dfc(out, as.character)
                    out <- dplyr::bind_cols(..., out,
                                            .name_repair = c("minimal"))
                    return(out)
                  }, ignore = ignore)
    )
  }
  else {
    return(data)
  }
}

#' Reconcile duplicate columns in IntChron data
#'
#' Identifies duplicate columns in data from IntChron (those with names ending
#' in "...X") and reconciles them into a single column using some sensible
#' heuristics.
#'
#' @param data  Data frame returned by [crawl_intchron()]
#'
#' @return
#' `data` with duplicate columns replaced with a single reconciled column.
#'
#' @noRd
intchron_reconcile <- function(data) {
  # Identify groups of duplicate columns
  dup_mark <- "\\.\\.\\.\\d+$"
  dup_cols <- data.frame(name = names(data)[grepl(dup_mark, names(data))])
  dup_cols %>%
    dplyr::mutate(basename = sub(dup_mark, "", .data$name)) %>%
    dplyr::group_by(.data$basename) %>%
    dplyr::summarise(names = list(.data$name), .groups = "drop_last") ->
    dup_cols

  # Reconcile duplicate columns
  new_cols <- purrr::map2_dfc(dup_cols$basename, dup_cols$names,
                             ~intchron_reconcile_columns(data[.y], .x))

  # Drop duplicate columns and add reconciled columns
  data <- data[!names(data) %in% unlist(dup_cols$names)]
  data <- cbind(data, new_cols)

  return(data)
}

#' @rdname intchron_reconcile
#' @noRd
intchron_reconcile_columns <- function(data, basename) {
  #TODO: special handling for lat/long

  # Otherwise, paste together unique values, excluding NAs:
  data[basename] <- apply(data, 1, function(x) paste(unique(x[!is.na(x)]), collapse = "/"))

  return(data[basename])
}
