#' Export a Medication Regimen
#'
#' Generates a type-aware representation of a medication regimen
#' for use in variable diagnostics.
#'
#' @param x A `med_regimen` object.
#'
#' @return A list with fields: `type`, `class`, `print`, `summary`,
#'   and `schedule`.
#'
#' @rawNamespace S3method(openqaly::export, med_regimen)
export.med_regimen <- function(x) {
  res <- list()
  res$type <- "med_regimen"
  res$class <- class(x)
  res$print <- tryCatch(
    paste(capture.output(x, split = FALSE), collapse = "\n"),
    error = function(e) NULL
  )
  res$summary <- tryCatch(
    paste(capture.output(summarise_regimen(x)), collapse = "\n"),
    error = function(e) NULL
  )
  res$schedule <- tryCatch({
    sched <- detail_schedule(x)
    if (is.data.frame(sched)) {
      lapply(seq_len(nrow(sched)), function(i) as.list(sched[i, , drop = FALSE]))
    } else {
      NULL
    }
  }, error = function(e) NULL)
  res$dosing <- tryCatch(
    paste(capture.output(diagnose_dosing(x)), collapse = "\n"),
    error = function(e) NULL
  )
  res
}

#' Export a Combination Medication Regimen
#'
#' Generates a type-aware representation of a combination regimen
#' for use in variable diagnostics.
#'
#' @param x A `med_regimen_combo` object.
#'
#' @return A list with fields: `type`, `class`, `print`, and `regimens`
#'   (a list of exported individual regimens).
#'
#' @rawNamespace S3method(openqaly::export, med_regimen_combo)
export.med_regimen_combo <- function(x) {
  res <- list()
  res$type <- "med_regimen_combo"
  res$class <- class(x)
  res$print <- tryCatch(
    paste(capture.output(x, split = FALSE), collapse = "\n"),
    error = function(e) NULL
  )
  res$regimens <- tryCatch(
    lapply(x$regimens, export),
    error = function(e) NULL
  )
  res
}
