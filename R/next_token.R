#' Next token
#' @export

next_token <- function(token_i){
  noaakey <<- noaakeys[token_i][[1]]

  if (is.null(noaakey) == TRUE)
  {
    print("Run out of TOKENS for NOAA")
  }
  else
  {
    print(paste0("Next token is ",  noaakey, 'belonging to: ', names(noaakeys[token_i])))
  }

  options(noaakey = noaakey)
  token_i <<- token_i + 1
  return(token_i)
}
