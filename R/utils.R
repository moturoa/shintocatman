
#' Json Utilities
#' @description Wrapper around `jsonlite::fromJSON`, returns input argument if it
#' cannot be converted from JSON. 
#' @export
#' @rdname json
from_json <- function(x, ...){
  
  if(!is.character(x))return(x)
  
  tryCatch(
    jsonlite::fromJSON(x, ...),
    error = function(e)x
  )
  
}

#' @rdname json
#' @export
to_json <- function(x, ...){
  jsonlite::toJSON(x, auto_unbox = TRUE, ...)
}
