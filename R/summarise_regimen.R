#' Print a summary of a regimen's dosing, units and costs
#'
#' Shows the dose, the units dispensed per administration, how the drug is
#' costed (timing, rounding, wastage) and the fees.
#'
#' @param reg A \code{med_regimen} object.
#' @return Invisibly returns reg. Called for its side effect (printing).
#'
#' @export
summarise_regimen <- function(reg) {
    stopifnot(inherits(reg, "med_regimen"))
    n_admin_per_cycle <- length(reg$admin_days)
    dist <- regimen_distribution(reg)
    has_dist <- !is.null(dist)

    divider <- paste0(rep("-", 65), collapse = "")
    cat("\n", divider, "\n", sep = "")
    cat(" Regimen Summary: ", reg$name, "\n", sep = "")
    cat(divider, "\n\n", sep = "")

    # ── Dosing ───────────────────────────────────────────────────────────
    cat("DOSING\n")
    cat("  Route:              ", reg$route, "\n")
    cat("  Prescribed dose:    ", reg$dose_per_admin, " mg",
        if (reg$dose_basis == "weight") "/kg"
        else if (reg$dose_basis == "bsa") "/m2"
        else "", "\n", sep = "")
    if (reg$dose_basis == "weight") {
        cat("  Patient weight:     ", reg$patient_weight, " kg\n", sep = "")
        if (has_dist) {
            cat("  Weight SD:          ", reg$patient_weight_sd, " kg\n", sep = "")
        }
    } else if (reg$dose_basis == "bsa") {
        cat("  Patient BSA:        ", reg$patient_bsa, " m2\n", sep = "")
        if (has_dist) {
            cat("  BSA SD:             ", reg$patient_bsa_sd, " m2\n", sep = "")
        }
    }
    mean_tag <- if (has_dist) " (at the mean)" else ""
    cat("  Dose:               ", reg$actual_dose_mg, " mg", mean_tag, "\n",
        sep = "")

    # ── Costing ──────────────────────────────────────────────────────────
    cat("\n")
    detail <- cost_single_dose(reg$actual_dose_mg, reg)
    u <- regimen_units(reg)
    ord <- order(u$sizes, decreasing = TRUE)
    cat("AVAILABLE UNITS\n")
    for (k in ord) {
        cat("  ", format_mg(u$sizes[k]), "mg @ $", u$costs[k], " ($",
            signif(u$costs[k] / u$sizes[k], 4), "/mg)\n", sep = "")
    }
    cat("\n")
    at_mean <- if (has_dist) ", at the mean" else ""
    if (reg$no_wastage) {
        per_mg <- u$costs / u$sizes
        best <- which.min(per_mg)
        cat("NO-WASTAGE PRICING (per administration", at_mean, ")\n",
            sep = "")
        cat("  Lowest cost per mg: $", signif(per_mg[best], 6), "/mg (",
            format_mg(u$sizes[best]), "mg unit)\n", sep = "")
        cat("  Costed dose:        ", detail$prescribed_mg, " mg\n", sep = "")
    } else {
        cat("UNITS (per administration", at_mean, ")\n", sep = "")
        for (i in seq_along(detail$units)) {
            if (detail$units[i] > 0) {
                cat("  ", detail$units[i], " x ", names(detail$units)[i],
                    "\n", sep = "")
            }
        }
        cat("  Drug in units:      ", detail$dispensed_mg, " mg\n", sep = "")
        cat("  Dose given:         ", detail$given_mg, " mg\n", sep = "")
        if (detail$shortfall_mg > 0) {
            cat("  Rounded down by:    ", detail$shortfall_mg, " mg (",
                signif(100 * detail$shortfall_mg / detail$prescribed_mg, 3),
                "% of the prescribed dose)\n", sep = "")
        }
        if (detail$leftover_mg > 0) {
            cat("  Left over:          ", detail$leftover_mg, " mg\n",
                sep = "")
        }
    }
    cat("\nCOSTING\n")
    cat("  Exact dosing:       ", if (reg$exact) "yes" else "no", "\n",
        sep = "")
    cat("  Max dose rounding:  ", reg$max_dose_rounding * 100,
        "% of the prescribed dose\n", sep = "")
    cat("  No-wastage pricing: ", if (reg$no_wastage) "yes" else "no",
        "\n", sep = "")
    cat("  Drug cost per admin: $", round(detail$cost, 4),
        if (has_dist) " (at the mean)" else "", "\n", sep = "")
    per_admin <- cost_single_admin_drug(reg)
    if (has_dist) {
        cat("  Expected cost per admin: $", round(per_admin, 4), "\n",
            sep = "")
    }
    cat("  Charge timing:      ", reg$charge_timing, "\n", sep = "")
    cat("  Drug cost per med cycle: $",
        round(per_admin * n_admin_per_cycle, 4), "\n", sep = "")
    cat("  Admin cost:         $", reg$admin_cost, " per administration\n",
        sep = "")
    cat("  Dispensing cost:    $", reg$dispensing_cost,
        " per medication cycle\n", sep = "")

    # ── Schedule ─────────────────────────────────────────────────────────
    cat("\n")
    cat("MEDICATION CYCLE SCHEDULE\n")
    cat("  Medication cycle length: ", reg$med_cycle_length, " days\n",
        sep = "")
    cat("  Administrations per cycle: ", n_admin_per_cycle, "\n", sep = "")
    if (length(reg$admin_days) <= 10) {
        cat("  Administration days: ",
            paste(reg$admin_days, collapse = ", "), "\n", sep = "")
    } else {
        runs <- rle(diff(reg$admin_days))
        if (length(runs$lengths) == 1 && runs$values == 1) {
            cat("  Administration days: ",
                min(reg$admin_days), "-", max(reg$admin_days),
                " (continuous)\n", sep = "")
        } else {
            cat("  Administration days: ",
                paste(reg$admin_days, collapse = ", "), "\n", sep = "")
        }
    }
    off_days <- setdiff(seq_len(reg$med_cycle_length), reg$admin_days)
    if (length(off_days) > 0) {
        if (length(off_days) <= 10) {
            cat("  Off days: ", paste(off_days, collapse = ", "), "\n", sep = "")
        } else {
            cat("  Off days: ", min(off_days), "-", max(off_days), "\n", sep = "")
        }
    }
    if (is.finite(reg$max_med_cycles)) {
        cat("  Max medication cycles: ", reg$max_med_cycles, "\n", sep = "")
        cat("  Max treatment duration: ",
            reg$max_med_cycles * reg$med_cycle_length, " days\n", sep = "")
    } else {
        cat("  Max medication cycles: unlimited\n")
    }
    start_day <- reg$start_day %||% 0
    if (start_day > 0) {
        cat("  Start day: ", start_day, "\n", sep = "")
    }

    cat("\n", divider, "\n\n", sep = "")
    invisible(reg)
}
