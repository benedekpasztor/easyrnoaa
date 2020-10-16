#' Gets 10 closes station ids and takes the one with most of GHCND data.
#' @param geotags data frame, date_start date in YYYY-MM-DD, date_end date in YYYY-MM-DD
#' @return list
#' @export
#' @examples
#' station_id_list: geotags with columns 'lat' and 'lon referring to latitude and longitude
#' date_start <- '2000-01-01'
#' date_end <- '2010-01-01'
#' get_closest_stations_list(station_id_list, date_start, date_end)


get_closest_stations_list <- function(geotags, date_start, date_end)
{
  closest_stations_list <- rnoaa::meteo_nearby_stations(geotags,
                                                        station_data = rnoaa::ghcnd_stations(),
                                                        lat_colname = 'lat',
                                                        lon_colname = 'lon',
                                                        year_min = lubridate::year(date_start),
                                                        year_max = lubridate::year(date_end),
                                                        limit = 10)

  closest_stations_df <<-  do.call(rbind.data.frame, closest_stations_list)


  ghcnd_stations_i <- rnoaa::ghcnd_stations(first_year = lubridate::year(date_start), last_year = lubridate::year(date_end))

  ghcnd_data <<- ghcnd_stations_i %>%
    filter(id %in% closest_stations_df$id) %>%
    mutate(year_diff = last_year - first_year,
           country = substr(id, 1, 2)) %>%
    mutate(country = as.factor(country)) %>%
    group_by(country) %>%
    top_n(year_diff, n = 1) %>%
    filter(row_number() == 1)


  return(ghcnd_data)
}
