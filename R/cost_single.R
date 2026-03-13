#' Calculate IV drug cost for a single administration
#'
#' Finds the minimum-cost combination of vials that covers the required dose,
#' using dynamic programming (optimal covering formulation).
#'
#' @param dose_mg         Required dose in mg.
#' @param vial_sizes      Numeric vector of available vial sizes (mg).
#' @param vial_costs      Numeric vector of corresponding vial costs.
#' @param wastage_threshold Fraction 0-1.  If the fraction of the marginal
#'                          vial used is <= this, skip that vial.
#' @return List with drug_cost, n_vials (named vector), waste_mg.
#' @keywords internal
cost_iv_single <- function(dose_mg, vial_sizes, vial_costs,
                           wastage_threshold = 0) {
    result <- optimal_vial_cost_single(dose_mg, vial_sizes, vial_costs,
                                       wastage_threshold)
    ord <- order(vial_sizes, decreasing = TRUE)
    n_vials <- result$n_vials[ord]
    names(n_vials) <- paste0(vial_sizes[ord], "mg")
    list(
        drug_cost = result$drug_cost,
        n_vials   = n_vials,
        waste_mg  = result$waste_mg
    )
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


#' Find all achievable exact-fill amounts (coin-change reachability)
#'
#' Given a set of vial sizes (or tablet strengths), returns all non-negative
#' integer amounts that can be expressed as a sum of non-negative integer
#' multiples of those sizes, up to \code{max_amount}.
#'
#' @param sizes Numeric vector of vial sizes or tablet strengths.
#' @param max_amount Numeric scalar. Upper bound on amounts to consider.
#' @return Integer vector of all achievable amounts (including 0), sorted.
#' @keywords internal
vial_breakpoints <- function(sizes, max_amount) {
    max_int <- ceiling(max_amount)
    reachable <- logical(max_int + 1L)
    reachable[1L] <- TRUE # 0 is reachable
    for (s in sizes) {
        s_int <- max(1L, as.integer(round(s)))
        if (s_int > max_int) next
        for (j in seq.int(s_int, max_int)) {
            if (reachable[j - s_int + 1L]) {
                reachable[j + 1L] <- TRUE
            }
        }
    }
    which(reachable) - 1L
}


#' Generate cost-based bins from a lognormal distribution of patient values
#'
#' Identifies natural breakpoints where the cost function changes (vial/tablet
#' boundaries), then computes lognormal probabilities for each cost interval.
#' Users supply natural-scale mean+SD; conversion to lognormal parameters is
#' handled internally.
#'
#' @param reg      A med_regimen object.
#' @param mean_val Numeric. Mean of the patient parameter (weight or BSA).
#' @param sd_val   Numeric. Standard deviation.
#' @param include_packing Logical. If TRUE, compute per-bin packing detail
#'   via \code{cost_iv_single()} instead of \code{cost_single_dose_vec()}.
#'   Only supported for vial-based routes.
#' @return Data frame with columns: bin, dose_lo, dose_hi, cost, probability.
#'   When \code{include_packing = TRUE}, additional columns are returned
#'   (param_lo, param_hi, midpoint, n_vials, waste_mg, mg_administered,
#'   weighted_cost) and attributes \code{"packing_results"},
#'   \code{"expected_cost"}, \code{"dose_per_admin"}.
#' @importFrom stats plnorm
#' @keywords internal
generate_cost_bins <- function(reg, mean_val, sd_val,
                               include_packing = FALSE) {
    dose_per_admin <- reg$dose_per_admin

    if (include_packing && !(reg$route %in% VIAL_ROUTES)) {
        stop("include_packing only supported for vial-based routes.")
    }

    # Convert natural-scale mean+SD to lognormal parameters
    sigma2 <- log(1 + (sd_val / mean_val)^2)
    mu <- log(mean_val) - sigma2 / 2
    sigma <- sqrt(sigma2)

    # Dose range: ±5SD bounds
    dose_min <- dose_per_admin * max(0, mean_val - 5 * sd_val)
    dose_max <- dose_per_admin * (mean_val + 5 * sd_val)

    # 1. Candidate breakpoints: all achievable exact-fill amounts
    if (reg$route %in% VIAL_ROUTES) {
        achievable <- vial_breakpoints(reg$vial_sizes, dose_max)
        if (reg$wastage_threshold > 0) {
            offsets <- unlist(lapply(reg$vial_sizes, function(s) {
                achievable + s * reg$wastage_threshold
            }))
            candidates <- sort(unique(c(achievable, offsets)))
        } else {
            candidates <- achievable
        }
    } else {
        candidates <- vial_breakpoints(reg$available_strengths, dose_max)
    }
    all_candidates <- candidates
    candidates <- all_candidates[all_candidates >= dose_min &
                                     all_candidates <= dose_max]
    # Ensure at least one candidate (narrow distributions may exclude all)
    if (length(candidates) == 0) {
        below <- all_candidates[all_candidates <= dose_max]
        candidates <- if (length(below) > 0) max(below) else 0
    }

    # 2. Interval bounds and costs
    n <- length(candidates)
    dose_lo <- candidates
    dose_hi <- c(candidates[-1], Inf)
    smallest <- if (reg$route %in% VIAL_ROUTES)
        min(reg$vial_sizes) else min(reg$available_strengths)
    midpoints <- c(
        (candidates[-n] + candidates[-1]) / 2,
        candidates[n] + smallest / 2
    )

    if (include_packing) {
        # Per-bin packing via cost_iv_single (pre-merge)
        pre_packing <- lapply(midpoints, function(dose) {
            cost_iv_single(
                dose_mg           = dose,
                vial_sizes        = reg$vial_sizes,
                vial_costs        = reg$vial_cost,
                wastage_threshold = reg$wastage_threshold
            )
        })
        costs <- vapply(pre_packing, function(p) p$drug_cost, numeric(1))
    } else {
        costs <- cost_single_dose_vec(midpoints, reg)
    }

    # 3. Merge consecutive same-cost intervals (rle)
    runs <- rle(costs)
    end_idx <- cumsum(runs$lengths)
    start_idx <- c(1L, end_idx[-length(end_idx)] + 1L)
    merged_lo <- dose_lo[start_idx]
    merged_cost <- runs$values
    merged_hi <- c(merged_lo[-1], Inf)

    # 4. Probabilities (lognormal CDF on patient-value scale)
    patient_lo <- merged_lo / dose_per_admin
    patient_hi <- merged_hi / dose_per_admin
    # First bin: plnorm(hi, mu, sigma) captures all mass from 0
    # (lognormal is naturally bounded at 0, no negative values)
    patient_lo[1] <- 0
    probs <- plnorm(patient_hi, mu, sigma) -
        plnorm(patient_lo, mu, sigma)

    if (!include_packing) {
        return(data.frame(
            bin = seq_along(merged_lo), dose_lo = merged_lo,
            dose_hi = merged_hi, cost = merged_cost,
            probability = probs
        ))
    }

    # include_packing = TRUE: build enriched result
    # Use start_idx to pick the representative packing per merged bin
    merged_packing <- pre_packing[start_idx]
    merged_midpoints <- pmin(midpoints[start_idx], dose_max)

    n_vials_vec <- as.integer(vapply(merged_packing,
        function(p) sum(p$n_vials), numeric(1)))
    total_content_vec <- vapply(merged_packing, function(p) {
        sizes <- as.numeric(sub("mg$", "", names(p$n_vials)))
        sum(sizes * p$n_vials)
    }, numeric(1))
    weighted_cost <- merged_cost * probs

    # Display bounds: actual candidates, capped at ±5SD
    out_param_lo <- merged_lo / dose_per_admin
    out_param_hi <- merged_hi / dose_per_admin
    out_param_hi[length(out_param_hi)] <- mean_val + 5 * sd_val
    out_dose_hi <- merged_hi
    out_dose_hi[length(out_dose_hi)] <- dose_max

    # Range columns: waste and mg_administered vary with dose within each bin
    waste_mg_lo <- pmax(total_content_vec - out_dose_hi, 0)
    waste_mg_hi <- total_content_vec - merged_lo
    mg_administered_lo <- merged_lo
    mg_administered_hi <- out_dose_hi

    result <- data.frame(
        bin                = seq_along(merged_lo),
        param_lo           = out_param_lo,
        param_hi           = out_param_hi,
        dose_lo            = merged_lo,
        dose_hi            = out_dose_hi,
        n_vials            = n_vials_vec,
        total_vial_mg      = total_content_vec,
        mg_administered_lo = mg_administered_lo,
        mg_administered_hi = mg_administered_hi,
        waste_mg_lo        = waste_mg_lo,
        waste_mg_hi        = waste_mg_hi,
        cost               = merged_cost,
        probability        = probs,
        weighted_cost      = weighted_cost,
        stringsAsFactors   = FALSE
    )

    attr(result, "packing_results") <- merged_packing
    attr(result, "expected_cost")   <- sum(weighted_cost)
    attr(result, "dose_per_admin")  <- dose_per_admin
    result
}


#' Cost of a single dose (dispatch by route)
#'
#' @param dose_mg Numeric. Dose in mg.
#' @param reg     A med_regimen object (for route, vial/tablet info).
#' @return Numeric scalar: drug cost for this dose.
#' @keywords internal
cost_single_dose <- function(dose_mg, reg) {
    if (reg$route %in% VIAL_ROUTES) {
        res <- cost_iv_single(
            dose_mg           = dose_mg,
            vial_sizes        = reg$vial_sizes,
            vial_costs        = reg$vial_cost,
            wastage_threshold = reg$wastage_threshold
        )
        return(res$drug_cost)
    } else {
        if (!is.null(names(reg$unit_cost))) {
            res <- cost_oral_single(
                dose_mg             = dose_mg,
                available_strengths = reg$available_strengths,
                cost_per_strength   = reg$unit_cost
            )
        } else {
            res <- cost_oral_single(
                dose_mg             = dose_mg,
                available_strengths = reg$available_strengths,
                cost_per_tablet     = reg$unit_cost
            )
        }
        return(res$drug_cost)
    }
}


#' Vectorized IV drug cost
#'
#' Accepts a vector of doses and returns a vector of costs.
#' @param dose_mg         Numeric vector of doses in mg.
#' @param vial_sizes      Numeric vector of available vial sizes (mg).
#' @param vial_costs      Numeric vector of corresponding vial costs.
#' @param wastage_threshold Fraction 0-1.
#' @return Numeric vector of drug costs.
#' @keywords internal
cost_iv_vec <- function(dose_mg, vial_sizes, vial_costs,
                        wastage_threshold = 0) {
    optimal_vial_cost_vec(dose_mg, vial_sizes, vial_costs,
                          wastage_threshold)
}


#' Vectorized oral drug cost
#'
#' Accepts a vector of doses and returns a vector of costs.
#' @param dose_mg             Numeric vector of doses in mg.
#' @param available_strengths Numeric vector of tablet strengths (mg).
#' @param cost_per_tablet     Numeric scalar or NULL.
#' @param cost_per_strength   Numeric vector or NULL.
#' @return Numeric vector of drug costs.
#' @keywords internal
cost_oral_vec <- function(dose_mg, available_strengths,
                          cost_per_tablet = NULL,
                          cost_per_strength = NULL) {
    ord <- order(available_strengths, decreasing = TRUE)
    strengths <- available_strengths[ord]
    tab_costs <- if (!is.null(cost_per_strength))
        cost_per_strength[ord]
    else rep(cost_per_tablet, length(strengths))
    remaining <- dose_mg
    total_cost <- numeric(length(dose_mg))
    for (i in seq_along(strengths)) {
        n <- pmax(floor(remaining / strengths[i]), 0)
        total_cost <- total_cost + n * tab_costs[i]
        remaining <- remaining - n * strengths[i]
    }
    needs_rounding <- remaining > 1e-9
    total_cost[needs_rounding] <-
        total_cost[needs_rounding] + tab_costs[length(strengths)]
    total_cost
}


#' Vectorized cost dispatcher
#'
#' @param dose_mg Numeric vector of doses in mg.
#' @param reg     A med_regimen object.
#' @return Numeric vector of drug costs.
#' @keywords internal
cost_single_dose_vec <- function(dose_mg, reg) {
    if (reg$route %in% VIAL_ROUTES) {
        cost_iv_vec(dose_mg, reg$vial_sizes, reg$vial_cost,
                    reg$wastage_threshold)
    } else if (!is.null(names(reg$unit_cost))) {
        cost_oral_vec(dose_mg, reg$available_strengths,
                      cost_per_strength = reg$unit_cost)
    } else {
        cost_oral_vec(dose_mg, reg$available_strengths,
                      cost_per_tablet = reg$unit_cost)
    }
}


#' Cost of a single administration (dispatch by route)
#'
#' When distribution parameters (patient_weight_sd or patient_bsa_sd) are
#' present, computes the weighted average cost across quantile bins of the
#' patient parameter distribution.
#'
#' @param reg A med_regimen object.
#' @return Numeric scalar: drug cost only (no admin cost) of one admin.
#' @keywords internal
cost_single_admin_drug <- function(reg) {
    if (reg$dose_basis == "weight" &&
        !is.null(reg$patient_weight_sd)) {
        bins <- generate_cost_bins(
            reg, reg$patient_weight, reg$patient_weight_sd
        )
        return(sum(bins$cost * bins$probability))
    }
    if (reg$dose_basis == "bsa" &&
        !is.null(reg$patient_bsa_sd)) {
        bins <- generate_cost_bins(
            reg, reg$patient_bsa, reg$patient_bsa_sd
        )
        return(sum(bins$cost * bins$probability))
    }
    cost_single_dose(reg$actual_dose_mg, reg)
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
    per_admin * length(reg$admin_days)
}
