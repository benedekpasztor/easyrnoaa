#' Gets closest station_id for latlon columns
#' @param places_to_check string vector, date_start date in YYYY-MM-DD, date_end date in YYYY-MM-DD
#' @return vector
#' @export
#' @examples

get_place_for_stations <- function(x, place_latlons){
  distance <- apply(ghcnd_data,
                    1,
                    function(y, ghcnd_data){geosphere::distm(c(as.numeric(y['longitude']), as.numeric(y['latitude'])),
                                                             c(as.numeric(x['lon']), as.numeric(x['lat'])),
                                                             fun = geosphere::distHaversine)})

  return(which(distance == min(distance)))
}


