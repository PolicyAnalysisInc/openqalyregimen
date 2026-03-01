#' Calculate combined cost for a list of regimens
#'
#' @param regimens         List of \code{med_regimen} objects.
#' @param ...              Arguments passed to \code{calculate_med_costs}.
#' @return A data.frame with per-cycle totals and per-drug breakdowns.
#'
#' @export
calculate_combination_costs <- function(regimens, ...) {
    stopifnot(is.list(regimens))
    stopifnot(all(vapply(regimens, inherits, logical(1), "med_regimen")))

    results <- lapply(regimens, function(reg) {
        res <- calculate_med_costs(regimen = reg, ...)
        res$drug_name <- reg$name
        res
    })

    base <- results[[1]][, c("cycle", "cycle_start_day", "cycle_end_day")]

    for (res in results) {
        nm <- gsub("[^a-zA-Z0-9]", "_", res$drug_name[1])
        base[[paste0(nm, "_drug_cost")]]  <- res$drug_cost
        base[[paste0(nm, "_admin_cost")]] <- res$admin_cost
        base[[paste0(nm, "_total_cost")]] <- res$total_cost
    }

    cost_cols <- grep("_total_cost$", names(base), value = TRUE)
    base$total_cost <- rowSums(base[, cost_cols, drop = FALSE])

    base
}
