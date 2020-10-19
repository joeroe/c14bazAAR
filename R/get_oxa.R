#' @rdname db_getter_backend
#' @export
get_oxa <- function(db_url = get_db_url("oxa")) {
  check_connection_to_url(db_url)
  oxa <- rintchron::intchron("oxa")

  oxa %>%
    dplyr::transmute(
      sourcedb = "oxa",
      sourcedb_version = get_db_version("oxa"),
      labnr = labcode,
      c14age = r_date,
      c14std = r_date_sigma,
      c13val = d13C,
      site = record_site,
      feature = sample,
      material = material,
      species = species,
      country = record_country,
      lat = record_latitude,
      lon = record_longitude
    ) %>%
    as.c14_date_list()
}
