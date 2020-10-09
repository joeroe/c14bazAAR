#' @rdname db_getter_backend
#' @export
get_intchron <- function(db_url = get_db_url("intchron")) {
  intchron <- crawl_intchron(db_url, ignore = c("ref", "series", "intimate"))

  # The following assumes the returned data matches the schema at:
  # https://intchron.org/schema
  # As of 2020-10-09

  # Reconcile duplicate columns
  intchron <- intchron_reconcile(intchron)

  # Drop metadata columns we aren't interested in
  intchron <- intchron[!names(intchron) %in% c("Source", "record", "url")]

  return(intchron)
}
