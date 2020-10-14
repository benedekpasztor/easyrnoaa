#####
nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  d <- data.frame()
  tryCatch(
    d <- jsonlite::fromJSON(
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame())
  )
  if (nrow(d) == 0) {return(data.frame())} else {return(data.frame(lon = as.numeric(d$lon),
                                                                   lat = as.numeric(d$lat), as.character(d$display_name)))}
}


get_geocode <-  function(address) {
  #set the elapsed time counter to 0
  t <- Sys.time()
  #calling the nominatim OSM API
  api_output <- data.frame()
  try(api_output <- nominatim_osm(address))
  #get the elapsed time
  t <- difftime(Sys.time(), t, 'secs')
  #return data.frame with the input address, output of the nominatim_osm function and elapsed time
  if (nrow(api_output) == 0){return(data.frame(address = address, elapsed_time = t))}
  else{return(data.frame(address = address, api_output, elapsed_time = t))}
}



