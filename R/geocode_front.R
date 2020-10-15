#' Geocoding places in a dataframe
#' @export

get_geocode_for_places <- function(places_to_check){
  places <- data.frame(places_to_check)
  colnames(places) <- c('id')

  ## Manual calls to arbitrary avoid errors
  geotags <- data.frame()
  for (i in 1:nrow(places)){
    print('next ...')
    print(paste0('Location:', i, ', geocoding ', places[i, 'id']))
    Sys.sleep(runif(1, 1, 2))
    geotags <- plyr::rbind.fill(geotags, get_geocode(places[i, 'id']))
  }
  geotags$id <- geotags$address
  return(geotags)
}
