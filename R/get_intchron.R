#' @rdname db_getter_backend
#' @export
get_intchron <- function(db_url = get_db_url("intchron")) {
  check_connection_to_url(db_url)
  hosts <- read_intchron(db_url)
}
