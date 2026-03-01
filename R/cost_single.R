#' Calculate IV drug cost for a single administration
#'
#' Handles multiple vial sizes, greedy packing, and a wastage threshold that
#' controls whether the final (partially-used) vial is opened.
#'
#' @param dose_mg         Required dose in mg.
#' @param vial_sizes      Numeric vector of available vial sizes (mg).
#' @param vial_costs      Numeric vector of corresponding vial costs.
#' @param wastage_threshold Fraction 0-1.  If residual / vial size <= this,
#'                          round down (skip that vial).
#' @return List with drug_cost, n_vials (named vector), waste_mg.
#' @keywords internal
cost_iv_single <- function(dose_mg, vial_sizes, vial_costs,
                           wastage_threshold = 0) {
    # Sort vial sizes descending for greedy fill
    ord <- order(vial_sizes, decreasing = TRUE)
    sizes  <- vial_sizes[ord]
    costs  <- vial_costs[ord]

    remaining <- dose_mg
    n_vials   <- integer(length(sizes))
    total_cost <- 0

    for (i in seq_along(sizes)) {
        if (remaining <= 0) break
        n <- floor(remaining / sizes[i])
        n_vials[i] <- n
        remaining  <- remaining - n * sizes[i]
        total_cost <- total_cost + n * costs[i]
    }

    # Handle residual
    if (remaining > 0) {
        # Find smallest vial that covers the residual
        cover_idx <- which(sizes >= remaining)
        if (length(cover_idx) > 0) {
            best <- cover_idx[length(cover_idx)]
            frac_used <- remaining / sizes[best]
            if (frac_used > wastage_threshold) {
                n_vials[best] <- n_vials[best] + 1L
                total_cost    <- total_cost + costs[best]
                remaining     <- sizes[best] - remaining
            }
        } else {
            best <- length(sizes)
            n_vials[best] <- n_vials[best] + 1L
            total_cost    <- total_cost + costs[best]
            remaining     <- sizes[best] - remaining
        }
    }

    waste_mg <- remaining
    names(n_vials) <- paste0(sizes[seq_along(n_vials)], "mg")

    list(drug_cost = total_cost, n_vials = n_vials, waste_mg = max(waste_mg, 0))
}


#' Calculate oral drug cost for a single administration
#'
#' Handles tablet rounding: dose is rounded up to the nearest achievable
#' combination of available tablet strengths (whole tablets only). This is
#' per-administration rounding (you can't split a pill), distinct from oral
#' wastage (dispensing a full cycle upfront).
#'
#' @param dose_mg             Required dose in mg.
#' @param available_strengths Numeric vector of tablet strengths (mg).
#' @param cost_per_tablet     Numeric. Cost per tablet (assumed same across
#'                            strengths unless cost_per_strength is provided).
#' @param cost_per_strength   Named numeric vector. Cost per tablet for each
#'                            strength. Names should match available_strengths.
#' @return List with drug_cost, tablets (named vector), waste_mg.
#' @keywords internal
cost_oral_single <- function(dose_mg, available_strengths,
                             cost_per_tablet    = NULL,
                             cost_per_strength  = NULL) {
    # Sort descending
    ord      <- order(available_strengths, decreasing = TRUE)
    strengths <- available_strengths[ord]

    if (!is.null(cost_per_strength)) {
        tab_costs <- cost_per_strength[ord]
    } else {
        tab_costs <- rep(cost_per_tablet, length(strengths))
    }

    remaining <- dose_mg
    n_tabs    <- integer(length(strengths))
    total_cost <- 0

    for (i in seq_along(strengths)) {
        if (remaining <= 0) break
        n <- floor(remaining / strengths[i])
        n_tabs[i]  <- n
        remaining  <- remaining - n * strengths[i]
        total_cost <- total_cost + n * tab_costs[i]
    }

    # Tablet rounding: if any residual, round up with smallest tablet
    if (remaining > 1e-9) {
        smallest_idx <- length(strengths)
        n_tabs[smallest_idx] <- n_tabs[smallest_idx] + 1L
        total_cost <- total_cost + tab_costs[smallest_idx]
        remaining  <- strengths[smallest_idx] - remaining
    }

    waste_mg <- max(remaining, 0)
    names(n_tabs) <- paste0(strengths, "mg")

    list(drug_cost = total_cost, tablets = n_tabs, waste_mg = waste_mg)
}


#' Cost of a single administration (dispatch by route)
#'
#' @param reg A med_regimen object.
#' @return Numeric scalar: drug cost only (no admin cost) of one admin.
#' @keywords internal
cost_single_admin_drug <- function(reg) {
    if (reg$route == "iv") {
        res <- cost_iv_single(
            dose_mg           = reg$actual_dose_mg,
            vial_sizes        = reg$vial_sizes,
            vial_costs        = reg$vial_cost,
            wastage_threshold = reg$wastage_threshold
        )
        return(res$drug_cost)
    } else {
        if (!is.null(names(reg$unit_cost))) {
            res <- cost_oral_single(
                dose_mg             = reg$actual_dose_mg,
                available_strengths = reg$available_strengths,
                cost_per_strength   = reg$unit_cost
            )
        } else {
            res <- cost_oral_single(
                dose_mg             = reg$actual_dose_mg,
                available_strengths = reg$available_strengths,
                cost_per_tablet     = reg$unit_cost
            )
        }
        return(res$drug_cost)
    }
}


#' Calculate the total drug cost for one full medication cycle (oral wastage)
#'
#' This is the oral wastage dispensing cost: the full set of tablets dispensed
#' at the start of a medication cycle.
#' = cost_per_admin * n_administrations_per_cycle.
#'
#' @param reg  A med_regimen object.
#' @return Numeric scalar: total drug cost for one medication cycle dispensing.
#' @keywords internal
cost_oral_cycle_dispensing <- function(reg) {
    per_admin <- cost_single_admin_drug(reg)
    per_admin * reg$n_admin_per_cycle
}
