get_geocode_for_countries <- function(countries_to_check){
  countries <- data.frame(countries_to_check)
  colnames(countries) <- c('country')

  ## Manual calls to arbitrary avoid errors
  geotags <- data.frame()
  for (i in 1:nrow(countries)){
    print(paste0('Country:', i, ', geocoding ', countries[i, 'country']))
    Sys.sleep(runif(1, 1, 2))
    geotags <- plyr::rbind.fill(geotags, get_geocode(countries[i, 'country']))
    print('next ...')
  }
  geotags$id <- geotags$address
  return(geotags)
}
