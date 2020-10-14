get_closest_stations_list <- function(geotags, date_start, date_end)
{
  closest_stations_list <- rnoaa::meteo_nearby_stations(geotags,  station_data = rnoaa::ghcnd_stations(), lat_colname = 'lat', lon_colname = 'lon', year_min = lubridate::year(date_start), year_max = lubridate::year(date_end), limit = 10)
  closest_stations_df <<-  do.call(rbind.data.frame, closest_stations_list)


  ghcnd_stations <<- rnoaa::ghcnd_stations(first_year = lubridate::year(date_start), last_year = lubridate::year(date_end))

  ghcnd_data <<- ghcnd_stations %>%
    filter(id %in% closest_stations_df$id) %>%
    mutate(year_diff = last_year - first_year,
           country = substr(id, 1, 2)) %>%
    mutate(country = as.factor(country)) %>%
    group_by(country) %>%
    top_n(year_diff, n = 1) %>%
    filter(row_number() == 1)


  station_id_list <- ghcnd_data$id

  return(station_id_list)
}
