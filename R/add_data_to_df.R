add_data_to_df <- function(out = NULL, out_i = NULL, i, date_start_i, date_end_i){
  tryCatch({
    out_i <- out_i$data
    out_i <- out_i %>%
      select(date, datatype, station, value) %>%
      spread(key = datatype, value = value)

    out <- plyr::rbind.fill(out, out_i)
    return(out)
  },
  error = function(cond){
    message(paste0("No values found for ", station_id_list[i], "in period ", date_start_i, " to ", date_end_i))
    return(out)
  }
  )
}
