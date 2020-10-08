#' @rdname db_getter_backend
#' @export
get_intchron <- function(db_url = get_db_url("intchron")) {
  intchron <- crawl_intchron(db_url, ignore = c("ref", "series", "intimate"))
}
