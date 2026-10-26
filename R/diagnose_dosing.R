#' Diagnose dosing across a patient-parameter distribution
#'
#' Shows how patient weight/BSA variation creates dose bins with different
#' unit combinations, costs and probabilities, explaining how the
#' distribution-averaged cost is computed.
#'
#' @param reg A \code{med_regimen} object (from \code{\link{define_regimen}})
#'   with a distribution parameter (\code{patient_weight_sd} for weight-based
#'   or \code{patient_bsa_sd} for BSA-based dosing).
#' @return A data.frame with S3 class \code{dosing_detail}, one row per
#'   dose bin. Columns: \code{bin}, \code{probability}, \code{param_lo},
#'   \code{param_hi}, \code{dose_per_admin}, \code{dose_lo}, \code{dose_hi},
#'   \code{packing}, \code{n_units}, \code{total_unit_mg},
#'   \code{leftover_mg_lo}, \code{leftover_mg_hi} (drug in the units not
#'   given, across the bin), \code{shortfall_mg_lo}, \code{shortfall_mg_hi}
#'   (prescribed dose rounded away, across the bin; non-zero only where the
#'   dose is rounded down), \code{cost}, \code{weighted_cost}.
#'
#'   Attributes: \code{drug_name}, \code{dose_basis}, \code{mean_param},
#'   \code{sd_param}, \code{expected_cost} (= \code{sum(weighted_cost)}),
#'   \code{unit_size}, \code{unit_cost}, \code{max_dose_rounding}.
#'
#' @export
diagnose_dosing <- function(reg) {
    if (!inherits(reg, "med_regimen")) {
        stop("reg must be a med_regimen object.")
    }
    dist <- regimen_distribution(reg)
    if (is.null(dist)) {
        stop("diagnose_dosing() requires a distribution parameter ",
             "(patient_weight_sd for weight-based, patient_bsa_sd for ",
             "bsa-based dosing).")
    }
    if (reg$no_wastage) {
        stop("diagnose_dosing() shows unit combinations; with no_wastage = TRUE ",
             "the cost is the lowest cost per mg times the dose.")
    }

    bins <- generate_cost_bins(reg, dist$mean, dist$sd, details = TRUE)
    details <- attr(bins, "details")

    packing <- vapply(details, function(d) format_packing(d$units), character(1))
    n_units <- vapply(details, function(d) as.integer(sum(d$units)), integer(1))
    total_mg <- vapply(details, `[[`, numeric(1), "dispensed_mg")
    rounded <- vapply(details, function(d) d$shortfall_mg > 0, logical(1))
    # Leftover across the bin: drug in units minus the dose given
    leftover_lo <- ifelse(rounded, 0, pmax(total_mg - bins$dose_hi, 0))
    leftover_hi <- ifelse(rounded, 0, pmax(total_mg - bins$dose_lo, 0))
    # Shortfall across the bin where the dose is rounded down to the units
    shortfall_lo <- ifelse(rounded, pmax(bins$dose_lo - total_mg, 0), 0)
    shortfall_hi <- ifelse(rounded, pmax(bins$dose_hi - total_mg, 0), 0)

    result <- data.frame(
        bin             = bins$bin,
        probability     = bins$probability,
        param_lo        = bins$param_lo,
        param_hi        = bins$param_hi,
        dose_per_admin  = reg$dose_per_admin,
        dose_lo         = bins$dose_lo,
        dose_hi         = bins$dose_hi,
        packing         = packing,
        n_units         = n_units,
        total_unit_mg   = total_mg,
        leftover_mg_lo  = leftover_lo,
        leftover_mg_hi  = leftover_hi,
        shortfall_mg_lo = shortfall_lo,
        shortfall_mg_hi = shortfall_hi,
        cost            = bins$cost,
        weighted_cost   = bins$cost * bins$probability,
        stringsAsFactors = FALSE
    )

    attr(result, "drug_name")     <- reg$name
    attr(result, "dose_basis")    <- reg$dose_basis
    attr(result, "mean_param")    <- dist$mean
    attr(result, "sd_param")      <- dist$sd
    attr(result, "expected_cost") <- sum(result$weighted_cost)
    u <- regimen_units(reg)
    attr(result, "unit_size") <- u$sizes
    attr(result, "unit_cost") <- u$costs
    attr(result, "max_dose_rounding") <- reg$max_dose_rounding
    class(result) <- c("dosing_detail", "data.frame")
    result
}


#' Format a unit combination as a string
#'
#' @param units Named integer vector of unit counts (names like "100mg").
#' @return Character string, e.g. \code{"2x100mg + 1x40mg"}.
#' @keywords internal
format_packing <- function(units) {
    units <- units[units > 0]
    if (length(units) == 0L) return("none")
    paste0(units, "x", names(units), collapse = " + ")
}
