#' Queries GHCND data for a selected station id_list, and between a start and end date
#' @param station_id_list list, date_start date in YYYY-MM-DD, date_end date in YYYY-MM-DD
#' @return data frame
#' @export
#' @examples
#' station_id_list <- c("HU000012942", "LOE00105562", "NOE00112089", "SP000003195")
#' date_start <- '2000-01-01'
#' date_end <- '2010-01-01'
#' ghcnd_data_query_for_stationidlist_between_dates(station_id_list, date_start, date_end)


ghcnd_data_query_for_stationidlist_between_dates <- function(station_id_list, date_start, date_end)
  {
  out <<- data.frame()
  date_start_i <- date_start
  date_end_i <- date_end


  while (date_start_i + months(9) <= date_end){
    if (lubridate::year(date_end) - lubridate::year(date_start_i) >= 10){
      date_end_i <- date_start_i + lubridate::years(9)
    } else{date_end_i <- date_end}

    for (i in 1:length(station_id_list)){
      print(paste("Querying data for ", station_id_list[i], ", for period between ", date_start_i, " and ", date_end_i))
      out_i <<- ghcnd_data_query_for_stationid_between_dates(station_id_i = station_id_list[i],
                             d_start = date_start_i,
                             d_end = date_end_i)

      print(paste("Preparation of data data for ", station_id_list[i], ", for period between ", date_start_i, " and ", date_end_i))
      out <<- add_data_to_df(out, out_i, i, date_start_i, date_end_i)

    }

    date_start_i <- date_end_i + months(1)
  }

  return(out)
}

ghcnd_data_query_for_stationid_between_dates <- function(station_id_i, d_start, d_end){
  tryCatch(
    {
      out_i <- rnoaa::ncdc(datasetid='GSOM', stationid=paste0('GHCND:', station_id_i), startdate = d_start, enddate = d_end, limit=900)
      return(out_i)
    },
    error = function(cond){
      next_token(token_i)
      tryCatch({
        out_i <- rnoaa::ncdc(datasetid='GSOM', stationid=paste0('GHCND:', station_id_i), startdate = d_start, enddate = d_end, limit=900)
        return(out_i)
      },
      error = function(cond){
        message(paste0("Probably token error when querying for ", station_id_i, "in period ", d_start, " to ", d_end))
      }
      )
    }
  )
}
