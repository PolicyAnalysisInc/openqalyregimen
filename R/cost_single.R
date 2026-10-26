#' Integer grid scale for a set of unit sizes
#'
#' The smallest power of 10 (1 to 1000) that makes every unit size a whole
#' number. The scale depends on the unit sizes only, never on the dose.
#'
#' @param sizes Numeric vector of unit sizes (mg).
#' @return Numeric scalar.
#' @keywords internal
unit_grid_scale <- function(sizes) {
    for (k in 0:3) {
        sc <- 10^k
        v <- sizes * sc
        if (all(abs(v - round(v)) <= 1e-9 * pmax(1, abs(v)))) return(sc)
    }
    stop("unit_size values must have at most 3 decimal places.", call. = FALSE)
}


#' Build the exact-fill table for a regimen's units
#'
#' @param sizes Numeric vector of unit sizes (mg).
#' @param costs Numeric vector of unit costs (same length as sizes).
#' @param max_dose Largest dose (mg) the table must support.
#' @return A list: sizes, costs, scale, int_sizes, cost, units, parent, max_int.
#' @keywords internal
unit_table <- function(sizes, costs, max_dose) {
    sc <- unit_grid_scale(sizes)
    int_sizes <- as.integer(round(sizes * sc))
    max_cells <- ceiling(max(max_dose, 0) * sc - 1e-9) + max(int_sizes) + 1
    if (!is.finite(max_cells) || max_cells > 5e7) {
        stop("Dose is too large relative to the smallest unit_size step ",
             "(table would need more than 50 million cells).", call. = FALSE)
    }
    max_int <- as.integer(max_cells)
    tab <- exact_fill_table(int_sizes, as.numeric(costs), max_int)
    c(list(sizes = sizes, costs = as.numeric(costs), scale = sc,
           int_sizes = int_sizes, max_int = max_int), tab)
}


# Count units of each size in the combination for grid amount x.
unit_counts <- function(tab, x) {
    n <- integer(length(tab$sizes))
    while (x > 0) {
        k <- tab$parent[x + 1L] + 1L
        n[k] <- n[k] + 1L
        x <- x - tab$int_sizes[k]
    }
    ord <- order(tab$sizes, decreasing = TRUE)
    n <- n[ord]
    names(n) <- paste0(format_mg(tab$sizes[ord]), "mg")
    n
}

format_mg <- function(x) format(x, trim = TRUE, drop0trailing = TRUE,
                                 scientific = FALSE)

# Is grid amount x reachable?
reachable <- function(tab, x) {
    x >= 0 && x <= tab$max_int && is.finite(tab$cost[x + 1L])
}

# Grid position of a dose: the amount it sits on (within a relative 1e-9),
# else the smallest grid amount above it. on = whether it sits on one.
dose_grid <- function(dose, sc) {
    v <- dose * sc
    x0 <- round(v)
    on <- abs(v - x0) <= 1e-9 * pmax(1, v)
    list(lo = as.integer(ifelse(on, x0, ceiling(v))), on = on)
}

# Nearest reachable grid amount strictly below grid position lo (> 0), or NA.
nearest_reachable_below <- function(tab, lo) {
    x <- lo - 1L
    while (x >= 1L) {
        if (is.finite(tab$cost[x + 1L])) return(x)
        x <- x - 1L
    }
    NA_integer_
}

# Nearest reachable grid amount at or above grid position lo, or NA.
nearest_reachable_above <- function(tab, lo) {
    x <- lo
    while (x <= tab$max_int) {
        if (is.finite(tab$cost[x + 1L])) return(x)
        x <- x + 1L
    }
    NA_integer_
}


#' Cost and units for a single dose
#'
#' Applies the costing rules to one dose:
#' \itemize{
#'   \item \code{exact = FALSE}: the cheapest units that cover the dose
#'     (ties: least drug, then fewest units); the remainder is discarded and
#'     paid for. If \code{max_dose_rounding > 0} and the units hold more than
#'     the dose, the dose is rounded down to the nearest amount the units fill
#'     exactly, when the shortfall is at most \code{max_dose_rounding} times the
#'     prescribed dose and that amount is strictly cheaper.
#'   \item \code{exact = TRUE}: the units must make up the prescribed dose
#'     exactly. If they can't, an error is raised. Rounding never applies.
#'   \item \code{no_wastage = TRUE}: the cost is the prescribed dose times the
#'     lowest cost per mg among the unit sizes. Rounding does not apply.
#' }
#'
#' @param dose Prescribed dose (mg).
#' @param tab Table from \code{unit_table()}.
#' @param exact Logical.
#' @param max_dose_rounding Numeric in [0, 1).
#' @param no_wastage Logical.
#' @return A list: cost, units (named counts), prescribed_mg, given_mg,
#'   shortfall_mg, dispensed_mg, leftover_mg.
#' @keywords internal
cost_dose_detail <- function(dose, tab, exact, max_dose_rounding = 0,
                             no_wastage = FALSE) {
    if (!is.finite(dose) || dose < 0) {
        stop("Dose must be a finite, non-negative number of mg.", call. = FALSE)
    }
    sc <- tab$scale
    out <- list(prescribed_mg = dose)
    if (dose == 0) {
        return(c(out, list(cost = 0, units = unit_counts(tab, 0L), given_mg = 0,
                           shortfall_mg = 0, dispensed_mg = 0, leftover_mg = 0)))
    }
    t <- if (no_wastage) 0 else max_dose_rounding
    g <- dose_grid(dose, sc)
    lo <- g$lo                 # grid amount the dose sits on, or just above it
    on_grid <- g$on
    # A dose within tolerance of an amount is treated as that amount
    dose_eff <- if (on_grid) lo / sc else dose

    within <- function(x) (dose - x / sc) <= t * dose + 1e-9 * max(1, dose)

    if (exact) {
        if (on_grid && reachable(tab, lo)) {
            x <- lo
        } else {
            xb <- nearest_reachable_below(tab, lo)
            xa <- nearest_reachable_above(tab, lo + as.integer(on_grid))
            near <- c(if (!is.na(xb)) xb / sc, if (!is.na(xa)) xa / sc)
            stop("The dose of ", format_mg(dose), " mg cannot be made ",
                 "exactly from unit sizes ",
                 paste0(format_mg(tab$sizes), collapse = ", "), " mg",
                 ". Nearest reachable amounts: ",
                 paste0(format_mg(near), " mg", collapse = " and "), ".",
                 call. = FALSE)
        }
        given <- x / sc
        dispensed <- x / sc
    } else {
        smax <- max(tab$int_sizes)
        cand <- lo:(lo + smax - 1L)
        cand <- cand[cand <= tab$max_int]
        cand <- cand[is.finite(tab$cost[cand + 1L])]
        if (length(cand) == 0L) stop("No combination of units covers the dose.",
                                     call. = FALSE)
        cc <- tab$cost[cand + 1L]
        o <- order(signif(cc, 12), cand, tab$units[cand + 1L])
        x <- cand[o[1]]
        given <- dose_eff
        dispensed <- x / sc
        if (t > 0 && dispensed > dose_eff + 1e-9 * max(1, dose)) {
            xb <- nearest_reachable_below(tab, lo)
            if (!is.na(xb) && within(xb) &&
                tab$cost[xb + 1L] < tab$cost[x + 1L] -
                    1e-9 * max(1, tab$cost[x + 1L])) {
                x <- xb
                given <- xb / sc
                dispensed <- xb / sc
            }
        }
    }

    cost <- tab$cost[x + 1L]
    units <- unit_counts(tab, x)
    if (no_wastage) {
        cost <- dose * min(tab$costs / tab$sizes)
        given <- dose
        dose_eff <- dose
    }
    c(out, list(
        cost = cost,
        units = units,
        given_mg = given,
        shortfall_mg = max(dose_eff - given, 0),
        dispensed_mg = dispensed,
        leftover_mg = max(dispensed - given, 0)
    ))
}


#' Unit sizes and costs of a regimen, with costs recycled to sizes
#' @param reg A med_regimen object.
#' @return list(sizes, costs)
#' @keywords internal
regimen_units <- function(reg) {
    costs <- reg$unit_cost
    if (length(costs) == 1L) costs <- rep(costs, length(reg$unit_size))
    list(sizes = reg$unit_size, costs = as.numeric(costs))
}


#' Cost detail of one dose for a regimen
#'
#' @param dose_mg Dose in mg.
#' @param reg A med_regimen object.
#' @param tab Optional pre-built table.
#' @return See \code{cost_dose_detail()}.
#' @keywords internal
cost_single_dose <- function(dose_mg, reg, tab = NULL) {
    if (is.null(tab)) {
        u <- regimen_units(reg)
        tab <- unit_table(u$sizes, u$costs, dose_mg)
    }
    cost_dose_detail(dose_mg, tab, reg$exact, reg$max_dose_rounding,
                     reg$no_wastage)
}


#' Patient distribution parameters for a regimen, or NULL
#' @param reg A med_regimen object.
#' @return list(mean, sd) or NULL.
#' @keywords internal
regimen_distribution <- function(reg) {
    if (reg$dose_basis == "weight" && !is.null(reg$patient_weight_sd)) {
        return(list(mean = reg$patient_weight, sd = reg$patient_weight_sd))
    }
    if (reg$dose_basis == "bsa" && !is.null(reg$patient_bsa_sd)) {
        return(list(mean = reg$patient_bsa, sd = reg$patient_bsa_sd))
    }
    NULL
}


#' Generate cost bins over a lognormal distribution of patient values
#'
#' The cost of a dose only changes where the dose crosses an amount the units
#' fill exactly, or (with rounding) where the shortfall to the nearest such
#' amount reaches \code{max_dose_rounding} times the dose (dose = amount /
#' (1 - max_dose_rounding)). Between these breakpoints the cost is constant,
#' so each bin is costed at its midpoint and weighted by its lognormal
#' probability.
#'
#' @param reg A med_regimen object (not exact, not no_wastage).
#' @param mean_val Mean of the patient parameter (natural scale).
#' @param sd_val Standard deviation of the patient parameter.
#' @param details Logical. If TRUE, attach the per-bin unit detail (from
#'   \code{cost_dose_detail()}) as attribute \code{"details"}.
#' @return A data.frame with one row per bin: bin, param_lo, param_hi,
#'   dose_lo, dose_hi, cost, probability; with \code{details = TRUE}, the
#'   per-bin unit detail in attribute \code{"details"}.
#' @importFrom stats plnorm qlnorm
#' @keywords internal
generate_cost_bins <- function(reg, mean_val, sd_val, details = FALSE) {
    dpa <- reg$dose_per_admin
    sigma2 <- log(1 + (sd_val / mean_val)^2)
    mu <- log(mean_val) - sigma2 / 2
    sigma <- sqrt(sigma2)

    dose_min <- dpa * stats::qlnorm(1e-9, mu, sqrt(sigma2))
    dose_max <- dpa * stats::qlnorm(1 - 1e-9, mu, sqrt(sigma2))

    u <- regimen_units(reg)
    tab <- unit_table(u$sizes, u$costs, dose_max)
    reach <- (which(is.finite(tab$cost)) - 1L) / tab$scale
    reach <- reach[reach > 0]
    t <- reg$max_dose_rounding
    cand <- reach
    if (t > 0) cand <- c(cand, reach / (1 - t))
    cand <- sort(unique(round(cand, 9)))
    cand <- cand[cand > dose_min & cand < dose_max]
    edges <- c(dose_min, cand, dose_max)
    mids <- (edges[-1] + edges[-length(edges)]) / 2

    # Cost every midpoint at once (exact = FALSE; exact can't be combined
    # with a distribution). Same rules as cost_dose_detail().
    sc <- tab$scale
    g <- dose_grid(mids, sc)
    best <- tab$best
    if (is.null(best)) best <- cover_best(tab$cost)
    x <- best[g$lo + 1L]
    if (anyNA(x) || any(x < 0L)) {
        stop("No combination of units covers the dose.", call. = FALSE)
    }
    dose_eff <- ifelse(g$on, g$lo / sc, mids)
    rounded <- logical(length(mids))
    if (t > 0) {
        reach_idx <- which(is.finite(tab$cost)) - 1L
        j <- findInterval(g$lo - 1L, reach_idx)
        xb <- ifelse(j >= 1L, reach_idx[pmax(j, 1L)], 0L)
        cx <- tab$cost[x + 1L]
        cb <- tab$cost[xb + 1L]
        rounded <- x / sc > dose_eff + 1e-9 * pmax(1, mids) &
            xb > 0L &
            (mids - xb / sc) <= t * mids + 1e-9 * pmax(1, mids) &
            cb < cx - 1e-9 * pmax(1, cx)
        x[rounded] <- xb[rounded]
    }
    costs <- tab$cost[x + 1L]

    # Merge consecutive bins with the same cost, units and rounding (the
    # units are determined by the grid amount x)
    keys <- paste(signif(costs, 12), x, rounded)
    runs <- rle(keys)
    end_idx <- cumsum(runs$lengths)
    start_idx <- c(1L, end_idx[-length(end_idx)] + 1L)
    lo <- edges[start_idx]
    hi <- edges[end_idx + 1L]
    p_lo <- lo / dpa
    p_hi <- hi / dpa
    p_lo[1] <- 0
    p_hi[length(p_hi)] <- Inf
    probs <- stats::plnorm(p_hi, mu, sigma) - stats::plnorm(p_lo, mu, sigma)

    out <- data.frame(
        bin = seq_along(lo),
        param_lo = lo / dpa,
        param_hi = hi / dpa,
        dose_lo = lo,
        dose_hi = hi,
        cost = costs[start_idx],
        probability = probs
    )
    if (details) {
        attr(out, "details") <- lapply(mids[start_idx], function(d) {
            cost_dose_detail(d, tab, reg$exact, t, reg$no_wastage)
        })
    }
    out
}


#' Expected drug cost of one administration
#'
#' For a fixed dose, the cost of that dose. With a patient distribution
#' (\code{patient_weight_sd} or \code{patient_bsa_sd} matching the dose
#' basis), the expected cost over the lognormal distribution. Under
#' \code{no_wastage = TRUE}, the lowest cost per mg times the expected dose.
#'
#' @param reg A med_regimen object.
#' @return Numeric scalar.
#' @keywords internal
cost_single_admin_drug <- function(reg) {
    dist <- regimen_distribution(reg)
    if (is.null(dist)) {
        return(cost_single_dose(reg$actual_dose_mg, reg)$cost)
    }
    if (reg$no_wastage) {
        u <- regimen_units(reg)
        return(reg$actual_dose_mg * min(u$costs / u$sizes))
    }
    bins <- generate_cost_bins(reg, dist$mean, dist$sd)
    sum(bins$cost * bins$probability)
}
