
from_json <- function(x, ...){
  jsonlite::fromJSON(x, ...)
}

to_json <- function(x, ...){
  jsonlite::toJSON(x, auto_unbox = TRUE, ...)
}
