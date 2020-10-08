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
  # Reformat the header row
  nheader <- grep("^,", lines)[1] - 1
  lines[nheader] <- sub("#", "", lines[nheader])
  lines[nheader] <- paste0(lines[nheader], ",")

  # Read data table
  data <- read.csv(text = lines, stringsAsFactors = FALSE,
                   comment.char = "#", na.strings = c("", "-"))

  # Drop unnamed columns (assumed to be empty)
  data <- data[!grepl("^X(\\.[0-9]+)?$", names(data))]

  return(data)
}
