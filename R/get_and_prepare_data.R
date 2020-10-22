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
  geocode <- get_geocode_for_places(places_to_check)

  ghcnd_data <- get_closest_stations_list(geocode, date_start, date_end)

  d <- ghcnd_data_query_for_stationidlist_between_dates(ghcnd_data$id, date_start, date_end)

  geocode['station_id'] <- ghcnd_data[apply(geocode, 1, get_place_for_stations), 'id']


  d <- d %>%
    mutate(station_id = substr(station, 7, length(station))) %>%
    dplyr::left_join(geocode %>% select(c('id', 'station_id'))) %>%
    plyr::rename(c("id" = "country"))

  return(d)
}
