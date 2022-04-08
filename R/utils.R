
#' Json Utilities
#' @export
#' @rdname json
from_json <- function(x, ...){
  jsonlite::fromJSON(x, ...)
}

#' @rdname json
#' @export
to_json <- function(x, ...){
  jsonlite::toJSON(x, auto_unbox = TRUE, ...)
}
