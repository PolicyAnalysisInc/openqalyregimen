#' Create a medication regimen specification
#'
#' A regimen describes a dose, a schedule, the units the drug is dispensed in,
#' and how it is costed. Costing is the same for every product: the route is a
#' label only.
#'
#' @param name            Character. Drug name (for labelling).
#' @param route           "iv", "oral", "sc" (subcutaneous), or "im"
#'                        (intramuscular). A label only; it does not affect
#'                        costing.
#' @param dose_per_admin  Numeric. Dose per administration in mg (flat dose)
#'                        or mg/kg or mg/m² depending on \code{dose_basis}.
#' @param dose_basis      "flat", "weight", or "bsa".
#' @param patient_weight  Numeric. Patient weight in kg (used if dose_basis
#'                        = "weight").
#' @param patient_bsa     Numeric. Patient BSA in m² (used if dose_basis
#'                        = "bsa").
#' @param patient_weight_sd Numeric or NULL. Standard deviation of patient
#'                        weight (kg). With dose_basis = "weight", costs are
#'                        averaged over a lognormal weight distribution.
#' @param patient_bsa_sd  Numeric or NULL. Standard deviation of patient BSA
#'                        (m²). With dose_basis = "bsa", costs are averaged
#'                        over a lognormal BSA distribution.
#' @param med_cycle_length Numeric. Length of one *medication* cycle in days.
#'                         This may differ from the model cycle length.
#' @param admin_days       Integer vector. Days within the medication cycle on
#'                         which administration occurs (1-indexed, no
#'                         duplicates). Day d is given d - 1 days after the
#'                         cycle starts, which must be within the cycle.
#' @param max_med_cycles   Numeric or Inf. Maximum number of medication cycles.
#' @param start_day        Numeric. Day on which this regimen's treatment
#'                         begins (default 0).
#' @param unit_size        Numeric vector. Sizes of the units the drug is
#'                         dispensed in (mg per vial, tablet, pen, ...).
#' @param unit_cost        Numeric. Cost per unit: one value for every size,
#'                         or one value per \code{unit_size}, in the same
#'                         order.
#' @param exact            Logical, required. \code{TRUE}: the units must make
#'                         up the dose exactly (whole units, nothing left
#'                         over); a dose that can't be reached is an error at
#'                         runtime. \code{FALSE}: the cheapest units covering
#'                         the dose are dispensed and the remainder is paid
#'                         for.
#' @param max_dose_rounding Numeric in [0, 1). The largest share of the
#'                         prescribed dose that may be rounded away. When the
#'                         cheapest units covering the dose leave drug over,
#'                         the dose is rounded down to the nearest amount the
#'                         units make up exactly, if that is cheaper. Only
#'                         with \code{exact = FALSE}; with \code{exact = TRUE}
#'                         it must be 0.
#' @param no_wastage       Logical. \code{TRUE} costs the prescribed dose at
#'                         the lowest cost per mg among the unit sizes (the
#'                         no-wastage scenario). Rounding does not apply.
#' @param charge_timing    Required. \code{"per_admin"}: each dose is costed on
#'                         its administration day. \code{"per_dispensing"}: a
#'                         whole medication cycle's doses are costed on day 1
#'                         of that cycle.
#' @param admin_cost       Numeric. Fee charged on each administration (for
#'                         example an infusion or injection).
#' @param dispensing_cost  Numeric. Fee charged once per medication cycle (the
#'                         pharmacy dispensing fee).
#' @return A list of class "med_regimen".
#'
#' @export
define_regimen <- function(
    name              = "Drug A",
    route             = c("iv", "oral", "sc", "im"),
    dose_per_admin    = 100,
    dose_basis        = c("flat", "weight", "bsa"),
    patient_weight    = 70,
    patient_bsa       = 1.8,
    patient_weight_sd = NULL,
    patient_bsa_sd    = NULL,
    med_cycle_length  = 21,
    admin_days,
    max_med_cycles    = Inf,
    start_day         = 0,
    unit_size         = NULL,
    unit_cost         = NULL,
    exact,
    max_dose_rounding = 0,
    no_wastage        = FALSE,
    charge_timing,
    admin_cost        = 0,
    dispensing_cost   = 0
) {
    route      <- match.arg(route)
    dose_basis <- match.arg(dose_basis)

    if (missing(admin_days)) {
        stop("admin_days must be provided.", call. = FALSE)
    }
    if (!is.numeric(admin_days) || length(admin_days) == 0L ||
        any(is.na(admin_days)) || any(admin_days < 1L) ||
        any(admin_days != as.integer(admin_days))) {
        stop("admin_days must be a non-empty integer vector of positive day values.",
             call. = FALSE)
    }
    admin_days <- as.integer(admin_days)

    check_scalar_number(med_cycle_length, "med_cycle_length", positive = TRUE)
    if (anyDuplicated(admin_days)) {
        stop("admin_days must not contain duplicate days.", call. = FALSE)
    }
    if (any(admin_days - 1 >= med_cycle_length - 1e-9)) {
        stop("admin_days must be days within the medication cycle: day d is ",
             "d - 1 days after the cycle starts, which must be less than ",
             "med_cycle_length (", med_cycle_length, ").", call. = FALSE)
    }
    if (!is.numeric(max_med_cycles) || length(max_med_cycles) != 1L ||
        is.na(max_med_cycles) || max_med_cycles <= 0) {
        stop("max_med_cycles must be a positive number or Inf.", call. = FALSE)
    }
    if (!is.numeric(start_day) || length(start_day) != 1 ||
        !is.finite(start_day) || start_day < 0) {
        stop("start_day must be a finite, non-negative numeric scalar.",
             call. = FALSE)
    }
    check_scalar_number(admin_cost, "admin_cost")
    check_scalar_number(dispensing_cost, "dispensing_cost")
    check_scalar_number(dose_per_admin, "dose_per_admin")
    check_scalar_number(patient_weight, "patient_weight", positive = TRUE)
    check_scalar_number(patient_bsa, "patient_bsa", positive = TRUE)
    if (!is.null(patient_weight_sd)) {
        check_scalar_number(patient_weight_sd, "patient_weight_sd",
                            positive = TRUE)
    }
    if (!is.null(patient_bsa_sd)) {
        check_scalar_number(patient_bsa_sd, "patient_bsa_sd", positive = TRUE)
    }

    actual_dose_mg <- switch(dose_basis,
        flat   = dose_per_admin,
        weight = dose_per_admin * patient_weight,
        bsa    = dose_per_admin * patient_bsa
    )

    has_dist <- (dose_basis == "weight" && !is.null(patient_weight_sd)) ||
                (dose_basis == "bsa" && !is.null(patient_bsa_sd))

    if (is.null(unit_size)) {
        stop("unit_size must be provided.", call. = FALSE)
    }
    if (!is.numeric(unit_size) || length(unit_size) == 0L ||
        any(!is.finite(unit_size)) || any(unit_size <= 0)) {
        stop("unit_size must be a vector of positive, finite numbers.",
             call. = FALSE)
    }
    if (anyDuplicated(unit_size)) {
        stop("unit_size must not contain duplicate sizes.", call. = FALSE)
    }
    if (is.null(unit_cost)) {
        stop("unit_cost must be provided.", call. = FALSE)
    }
    if (!is.numeric(unit_cost) || any(!is.finite(unit_cost)) ||
        any(unit_cost < 0) ||
        !(length(unit_cost) %in% c(1L, length(unit_size)))) {
        stop("unit_cost must be non-negative and have length 1 or the ",
             "same length as unit_size.", call. = FALSE)
    }
    unit_cost <- unname(as.numeric(unit_cost))
    unit_size <- unname(as.numeric(unit_size))
    unit_grid_scale(unit_size)

    if (missing(exact)) {
        stop("exact must be provided (TRUE or FALSE).", call. = FALSE)
    }
    if (!is.logical(exact) || length(exact) != 1L || is.na(exact)) {
        stop("exact must be TRUE or FALSE.", call. = FALSE)
    }
    if (exact && has_dist) {
        stop("exact = TRUE can't be used with a patient distribution ",
             "(patient_weight_sd / patient_bsa_sd): exact dosing needs a ",
             "fixed dose.", call. = FALSE)
    }
    if (missing(charge_timing)) {
        stop("charge_timing must be provided (\"per_admin\" or ",
             "\"per_dispensing\").", call. = FALSE)
    }
    if (!is.character(charge_timing) || length(charge_timing) != 1L ||
        !(charge_timing %in% c("per_admin", "per_dispensing"))) {
        stop("charge_timing must be \"per_admin\" or \"per_dispensing\".",
             call. = FALSE)
    }
    if (!is.numeric(max_dose_rounding) || length(max_dose_rounding) != 1L ||
        is.na(max_dose_rounding) || max_dose_rounding < 0 ||
        max_dose_rounding >= 1) {
        stop("max_dose_rounding must be a number in [0, 1).", call. = FALSE)
    }
    if (exact && max_dose_rounding > 0) {
        stop("max_dose_rounding can't be used with exact = TRUE: exact ",
             "dosing gives the prescribed dose or errors.", call. = FALSE)
    }
    if (!is.logical(no_wastage) || length(no_wastage) != 1L ||
        is.na(no_wastage)) {
        stop("no_wastage must be TRUE or FALSE.", call. = FALSE)
    }

    structure(
        list(
            name               = name,
            route              = route,
            dose_per_admin     = dose_per_admin,
            dose_basis         = dose_basis,
            patient_weight     = patient_weight,
            patient_bsa        = patient_bsa,
            patient_weight_sd  = patient_weight_sd,
            patient_bsa_sd     = patient_bsa_sd,
            actual_dose_mg     = actual_dose_mg,
            med_cycle_length   = med_cycle_length,
            admin_days         = admin_days,
            max_med_cycles     = max_med_cycles,
            start_day          = start_day,
            unit_size          = unit_size,
            unit_cost          = unit_cost,
            exact              = exact,
            max_dose_rounding  = max_dose_rounding,
            no_wastage         = no_wastage,
            charge_timing      = charge_timing,
            admin_cost         = admin_cost,
            dispensing_cost    = dispensing_cost
        ),
        class = "med_regimen"
    )
}


# Validate a numeric scalar argument.
check_scalar_number <- function(x, arg, positive = FALSE) {
    ok <- is.numeric(x) && length(x) == 1L && is.finite(x) &&
        (if (positive) x > 0 else x >= 0)
    if (!ok) {
        stop(arg, " must be a ", if (positive) "positive" else "non-negative",
             " finite number.", call. = FALSE)
    }
    invisible(TRUE)
}
