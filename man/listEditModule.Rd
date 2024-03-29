% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listEditModule.R
\name{listEditModuleUI}
\alias{listEditModuleUI}
\alias{listEditModule}
\title{Module to edit a vector}
\usage{
listEditModuleUI(id)

listEditModule(
  input,
  output,
  session,
  data = reactive(list()),
  edit_name = TRUE,
  show_name = TRUE,
  widths = c(6, 6),
  options = reactive(c("add", "delete")),
  options_labels = c(delete = "Laatste verwijderen", add = "Toevoegen", save = "Opslaan")
)
}
\description{
Like \code{\link[=jsonEditModule]{jsonEditModule()}} but without bugs. Also does not use JSON,
but vector in / vector out.
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
