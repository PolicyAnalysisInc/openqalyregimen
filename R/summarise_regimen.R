#' Print a summary of a regimen's dispensing and administration structure
#'
#' Shows how dosing translates to tablets/vials, what constitutes a dispensing
#' event vs an administration event, and the cost breakdown at each level.
#'
#' @param reg A \code{med_regimen} object.
#' @return Invisibly returns reg. Called for its side effect (printing).
#'
#' @export
summarise_regimen <- function(reg) {
    stopifnot(inherits(reg, "med_regimen"))
    n_admin_per_cycle <- length(reg$admin_days)

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
        if (!is.null(reg$patient_weight_sd)) {
            cat("  Weight SD:          ", reg$patient_weight_sd, " kg\n", sep = "")
            bins <- generate_cost_bins(
                reg, reg$patient_weight, reg$patient_weight_sd
            )
            cat("  Cost-based bins:    ", nrow(bins), "\n", sep = "")
        }
    } else if (reg$dose_basis == "bsa") {
        cat("  Patient BSA:        ", reg$patient_bsa, " m2\n", sep = "")
        if (!is.null(reg$patient_bsa_sd)) {
            cat("  BSA SD:             ", reg$patient_bsa_sd, " m2\n", sep = "")
            bins <- generate_cost_bins(
                reg, reg$patient_bsa, reg$patient_bsa_sd
            )
            cat("  Cost-based bins:    ", nrow(bins), "\n", sep = "")
        }
    }
    has_dist <- (!is.null(reg$patient_weight_sd) && reg$dose_basis == "weight") ||
                (!is.null(reg$patient_bsa_sd) && reg$dose_basis == "bsa")
    mean_tag <- if (has_dist) " (mean)" else ""
    cat("  Actual dose:        ", reg$actual_dose_mg, " mg", mean_tag, "\n",
        sep = "")

    # ── Tablet / vial breakdown ──────────────────────────────────────────
    if (reg$route == "oral") {
        if (!has_dist) {
            if (!is.null(names(reg$unit_cost))) {
                res <- cost_oral_single(
                    reg$actual_dose_mg, reg$available_strengths,
                    cost_per_strength = reg$unit_cost
                )
            } else {
                res <- cost_oral_single(
                    reg$actual_dose_mg, reg$available_strengths,
                    cost_per_tablet = reg$unit_cost
                )
            }
            cat("\n")
            cat("TABLET BREAKDOWN (per administration)\n")
            for (i in seq_along(res$tablets)) {
                if (res$tablets[i] > 0) {
                    cat("  ", res$tablets[i], " x ",
                        names(res$tablets)[i], " tablet(s)\n", sep = "")
                }
            }
            dispensed_mg <- sum(
                res$tablets * reg$available_strengths[
                    order(reg$available_strengths, decreasing = TRUE)
                ]
            )
            cat("  Tablet mg per admin: ", dispensed_mg, " mg\n", sep = "")
            if (res$waste_mg > 1e-9) {
                cat("  Tablet rounding waste: ", round(res$waste_mg, 2),
                    " mg per admin\n", sep = "")
            } else {
                cat("  Tablet rounding waste: none\n")
            }
            cat("  Drug cost per admin: $", res$drug_cost, " (point estimate)\n",
                sep = "")
            dist_cost <- cost_single_admin_drug(reg)
            if (dist_cost != res$drug_cost) {
                cat("  Dist-avg cost/admin: $", round(dist_cost, 4), "\n",
                    sep = "")
            }
        } else {
            cat("\nCOST (per administration, distribution-averaged)\n")
            dist_cost <- cost_single_admin_drug(reg)
            cat("  Drug cost per admin: $", round(dist_cost, 4), "\n", sep = "")
        }

    } else {
        if (!has_dist) {
            # IV/SC/IM point estimate
            res <- cost_iv_single(
                reg$actual_dose_mg, reg$vial_sizes, reg$vial_cost,
                reg$wastage_threshold
            )
            cat("\n")
            cat("VIAL BREAKDOWN (per administration)\n")
            for (i in seq_along(res$n_vials)) {
                if (res$n_vials[i] > 0) {
                    cat("  ", res$n_vials[i], " x ",
                        names(res$n_vials)[i], " vial(s)\n", sep = "")
                }
            }
            cat("  Drug waste: ", round(res$waste_mg, 2), " mg\n", sep = "")
            cat("  Wastage threshold: ", reg$wastage_threshold * 100, "%\n",
                sep = "")
            cat("  Drug cost per admin: $", res$drug_cost, " (point estimate)\n",
                sep = "")
            dist_cost <- cost_single_admin_drug(reg)
            if (dist_cost != res$drug_cost) {
                cat("  Dist-avg cost/admin: $", round(dist_cost, 4), "\n",
                    sep = "")
            }
        } else {
            cat("\nCOST (per administration, distribution-averaged)\n")
            dist_cost <- cost_single_admin_drug(reg)
            cat("  Wastage threshold: ", reg$wastage_threshold * 100, "%\n",
                sep = "")
            cat("  Drug cost per admin: $", round(dist_cost, 4), "\n", sep = "")
        }
    }

    # ── Schedule ─────────────────────────────────────────────────────────
    cat("\n")
    cat("MEDICATION CYCLE SCHEDULE\n")
    cat("  Medication cycle length: ", reg$med_cycle_length, " days\n",
        sep = "")
    cat("  Administrations per cycle: ", n_admin_per_cycle, "\n",
        sep = "")

    # Compact display of admin days
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
            cat("  Off days: ",
                paste(off_days, collapse = ", "), "\n", sep = "")
        } else {
            cat("  Off days: ", min(off_days), "-", max(off_days), "\n",
                sep = "")
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
        if (is.finite(reg$max_med_cycles)) {
            treat_end <- start_day + reg$max_med_cycles * reg$med_cycle_length
            cat("  Treatment window: day ", start_day, " - day ", treat_end,
                "\n", sep = "")
        } else {
            cat("  Treatment window: day ", start_day, " onward\n", sep = "")
        }
    }

    # ── Dispensing vs administration ─────────────────────────────────────
    cat("\n")
    cat("DISPENSING vs ADMINISTRATION\n")

    if (reg$route == "oral" && reg$oral_wastage) {
        disp_cost <- cost_oral_cycle_dispensing(reg)

        cat("  Mode: ORAL WASTAGE (dispense full cycle upfront)\n")
        cat("  Dispensing event (day 1 of each med cycle):\n")
        if (!has_dist) {
            tabs_per_admin <- sum(res$tablets)
            tabs_per_disp  <- tabs_per_admin * n_admin_per_cycle
            cat("    Tablets dispensed: ", tabs_per_disp, "\n", sep = "")
        }
        cat("    Drug cost:        $", disp_cost, "\n", sep = "")
        if (reg$admin_cost > 0) {
            cat("    Dispensing cost:  $", reg$admin_cost, "\n", sep = "")
            cat("    Total:           $", disp_cost + reg$admin_cost,
                "\n", sep = "")
        }
        cat("\n")
        cat("  Administration events (days ",
            min(reg$admin_days), "-", max(reg$admin_days), "):\n", sep = "")
        cat("    ", n_admin_per_cycle,
            " administrations per cycle\n", sep = "")
        cat("    Cost already captured at dispensing\n")
        cat("    (patients who discontinue mid-cycle still\n")
        cat("     incur the full dispensing cost)\n")

    } else if (reg$route == "oral" && !reg$oral_wastage) {
        per_admin <- cost_single_admin_drug(reg)
        cat("  Mode: PER-ADMINISTRATION (no oral wastage)\n")
        cat("  Each administration costed individually:\n")
        cat("    Drug cost per admin: $", per_admin, "\n", sep = "")
        if (reg$admin_cost > 0)
            cat("    Admin cost per admin: $", reg$admin_cost, "\n", sep = "")
        cat("    Total per admin: $", per_admin + reg$admin_cost, "\n",
            sep = "")
        cat("    Total per med cycle: $",
            (per_admin + reg$admin_cost) * n_admin_per_cycle, "\n",
            sep = "")

    } else {
        # IV
        per_admin <- cost_single_admin_drug(reg)
        cat("  Mode: PER-ADMINISTRATION (", toupper(reg$route), ")\n", sep = "")
        cat("  Each visit costed individually:\n")
        cat("    Drug cost per admin: $", per_admin, "\n", sep = "")
        cat("    Admin cost per admin: $", reg$admin_cost, "\n", sep = "")
        cat("    Total per admin: $", per_admin + reg$admin_cost, "\n",
            sep = "")
        cat("    Total per med cycle (",
            n_admin_per_cycle, " admins): $",
            (per_admin + reg$admin_cost) * n_admin_per_cycle,
            "\n", sep = "")
    }

    cat("\n", divider, "\n\n", sep = "")
    invisible(reg)
}
