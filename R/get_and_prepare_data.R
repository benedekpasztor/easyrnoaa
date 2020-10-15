#' Queries and prepares monthly GHCND data for a selected location list, and between a start and end date
#' @param places_to_check string vector, date_start date in YYYY-MM-DD, date_end date in YYYY-MM-DD
#' @return data frame
#' @export
#' @examples
#' places_to_check <- c("Hungary", "Spain", "United States")
#' date_start <- '2000-01-01'
#' date_end <- '2010-01-01'
#' get_and_prepare_data(places_to_check, date_start, date_end)


get_and_prepare_data <- function(places_to_check, date_start, date_end){
  data <- get_geocode_for_places(places_to_check)
  closest_stations_list <- get_closest_stations_list(data, date_start, date_end)
  d <- ghcnd_data_query_for_stationidlist_between_dates(closest_stations_list, date_start, date_end)
  return(d)
}
