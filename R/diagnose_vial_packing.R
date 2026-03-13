#' Diagnose dosing across a patient-parameter distribution
#'
#' Shows how patient weight/BSA variation creates different dose bins with
#' different vial packings and probabilities, explaining how the
#' distribution-averaged cost is computed.
#'
#' @param reg A \code{med_regimen} object (from \code{\link{define_regimen}}).
#'   Must have a vial-based route (iv, sc, im) and a distribution parameter
#'   (\code{patient_weight_sd} or \code{patient_bsa_sd}).
#' @return A data.frame with S3 class \code{dosing_detail}, one row per
#'   dose bin.  Columns: \code{bin}, \code{probability}, \code{param_lo},
#'   \code{param_hi}, \code{dose_per_admin}, \code{dose_lo}, \code{dose_hi},
#'   \code{packing}, \code{n_vials}, \code{total_vial_mg},
#'   \code{waste_mg_lo}, \code{waste_mg_hi}, \code{cost},
#'   \code{weighted_cost}.
#'
#'   Attributes: \code{drug_name}, \code{dose_basis}, \code{mean_param},
#'   \code{sd_param}, \code{expected_cost} (= \code{sum(weighted_cost)}).
#'
#' @export
diagnose_dosing <- function(reg) {
    if (!inherits(reg, "med_regimen")) {
        stop("reg must be a med_regimen object.")
    }
    if (!(reg$route %in% VIAL_ROUTES)) {
        stop("diagnose_dosing() only works for vial-based routes ",
             "(iv, sc, im).  Got route = '", reg$route, "'.")
    }

    # Resolve distribution parameters
    if (reg$dose_basis == "weight" && !is.null(reg$patient_weight_sd)) {
        mean_val <- reg$patient_weight
        sd_val   <- reg$patient_weight_sd
    } else if (reg$dose_basis == "bsa" && !is.null(reg$patient_bsa_sd)) {
        mean_val <- reg$patient_bsa
        sd_val   <- reg$patient_bsa_sd
    } else {
        stop("diagnose_dosing() requires a distribution parameter ",
             "(patient_weight_sd for weight-based, patient_bsa_sd for ",
             "bsa-based dosing).")
    }

    bins <- generate_cost_bins(reg, mean_val, sd_val, include_packing = TRUE)

    # Format packing strings (pure string assembly, no domain logic)
    packing_results <- attr(bins, "packing_results")
    packing <- vapply(packing_results, function(p) {
        format_packing(p, reg$wastage_threshold)
    }, character(1))

    result <- data.frame(
        bin             = bins$bin,
        probability     = bins$probability,
        param_lo        = bins$param_lo,
        param_hi        = bins$param_hi,
        dose_per_admin  = reg$dose_per_admin,
        dose_lo         = bins$dose_lo,
        dose_hi         = bins$dose_hi,
        packing         = packing,
        n_vials         = bins$n_vials,
        total_vial_mg   = bins$total_vial_mg,
        waste_mg_lo     = bins$waste_mg_lo,
        waste_mg_hi     = bins$waste_mg_hi,
        cost            = bins$cost,
        weighted_cost   = bins$weighted_cost,
        stringsAsFactors = FALSE
    )

    attr(result, "drug_name")     <- reg$name
    attr(result, "dose_basis")    <- reg$dose_basis
    attr(result, "mean_param")    <- mean_val
    attr(result, "sd_param")      <- sd_val
    attr(result, "expected_cost") <- attr(bins, "expected_cost")
    class(result) <- c("dosing_detail", "data.frame")
    result
}

#' @rdname diagnose_dosing
#' @export
diagnose_vial_packing <- function(reg) {
    .Deprecated("diagnose_dosing")
    diagnose_dosing(reg)
}


#' Format vial packing as a human-readable string
#'
#' Reads \code{n_vials} and \code{waste_mg} from a \code{cost_iv_single()}
#' result. If there is waste, identifies the smallest vial in the solution
#' larger than the waste as the "open" (partially used) vial.
#'
#' @param packing_result Output from \code{cost_iv_single()}.
#' @param wastage_threshold Numeric. Unused; kept for API compatibility.
#' @return Character string, e.g. \code{"2x100mg"} or
#'   \code{"1x100mg + open 1x50mg"}.
#' @keywords internal
format_packing <- function(packing_result, wastage_threshold = 0) {
    n_vials  <- packing_result$n_vials
    waste_mg <- packing_result$waste_mg
    sizes    <- as.numeric(sub("mg$", "", names(n_vials)))

    if (sum(n_vials) == 0) return("none")

    if (waste_mg <= 0) {
        # All vials fully used
        parts <- character(0)
        for (i in seq_along(sizes)) {
            if (n_vials[i] > 0)
                parts <- c(parts, paste0(n_vials[i], "x", sizes[i], "mg"))
        }
        return(paste(parts, collapse = " + "))
    }

    # Has waste: find the open vial (smallest vial in solution > waste)
    open_idx <- NA
    open_size <- Inf
    for (i in seq_along(sizes)) {
        if (n_vials[i] > 0 && sizes[i] > waste_mg - 1e-9 &&
            sizes[i] < open_size) {
            open_size <- sizes[i]
            open_idx <- i
        }
    }

    parts <- character(0)
    for (i in seq_along(sizes)) {
        fully_used <- n_vials[i]
        if (!is.na(open_idx) && i == open_idx) {
            fully_used <- fully_used - 1L
        }
        if (fully_used > 0)
            parts <- c(parts, paste0(fully_used, "x", sizes[i], "mg"))
    }
    if (!is.na(open_idx)) {
        parts <- c(parts, paste0("open 1x", sizes[open_idx], "mg"))
    }
    paste(parts, collapse = " + ")
}
