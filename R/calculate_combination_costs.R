#' Calculate combined cost for a list of regimens
#'
#' @description
#' \strong{Deprecated.} Use [calculate_med_costs()] instead, passing
#' regimens as separate arguments rather than a list.
#'
#' @param regimens         List of \code{med_regimen} objects.
#' @param ...              Arguments passed to \code{calculate_med_costs}.
#' @return A data.frame with per-cycle totals and per-drug breakdowns.
#'
#' @export
calculate_combination_costs <- function(regimens, ...) {
    .Deprecated("calculate_med_costs",
                msg = paste("calculate_combination_costs() is deprecated.",
                            "Pass regimens directly to calculate_med_costs()."))
    do.call(calculate_med_costs, c(regimens, list(...)))
}
