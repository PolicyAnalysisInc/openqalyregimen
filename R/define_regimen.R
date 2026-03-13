#' Routes that use vial-based costing (greedy packing, wastage threshold)
#' @keywords internal
VIAL_ROUTES <- c("iv", "sc", "im")

#' Create a medication regimen specification
#'
#' @param name            Character. Drug name (for labelling).
#' @param route           "iv", "oral", "sc" (subcutaneous), or "im"
#'                        (intramuscular).
#' @param dose_per_admin  Numeric. Dose per administration in mg (flat dose)
#'                        or mg/kg or mg/m² depending on \code{dose_basis}.
#' @param dose_basis      "flat", "weight", or "bsa".
#' @param patient_weight  Numeric. Patient weight in kg (used if dose_basis
#'                        = "weight").
#' @param patient_bsa     Numeric. Patient BSA in m² (used if dose_basis
#'                        = "bsa").
#' @param patient_weight_sd Numeric or NULL. Standard deviation of patient
#'                        weight (kg). If provided with dose_basis = "weight",
#'                        enables distribution-averaged cost calculation.
#' @param patient_bsa_sd  Numeric or NULL. Standard deviation of patient BSA
#'                        (m²). If provided with dose_basis = "bsa", enables
#'                        distribution-averaged cost calculation.
#' @param med_cycle_length Numeric. Length of one *medication* cycle in days.
#'                         This may differ from the model cycle length.
#' @param admin_days       Integer vector. Explicit days within the medication
#'                         cycle on which administration occurs (1-indexed).
#'                         This argument is required.
#' @param max_med_cycles   Numeric or Inf. Maximum number of medication cycles
#'                         (i.e. treatment duration cap). Inf = no cap.
#' @param start_day        Numeric. Day on which this regimen's treatment
#'                         begins (default 0). Shifts all treatment events by
#'                         this offset on the shared model timeline. Useful for
#'                         chaining regimen objects (e.g. loading + maintenance).
#' @param unit_cost        Numeric. Cost per tablet (oral) or per vial (IV).
#' @param units_per_pack   Numeric. Tablets per pack or vials per pack (for
#'                         costing; default 1, meaning unit_cost is per single
#'                         unit).
#' @param tablet_strength  Numeric. mg per tablet (oral only).
#' @param available_strengths Numeric vector. Available tablet strengths in mg
#'                         (oral). If supplied, the function picks the optimal
#'                         combination to minimise tablet rounding waste.
#'                         If NULL, uses \code{tablet_strength}.
#' @param oral_wastage     Logical. If TRUE (default for oral), the full
#'                         medication cycle's tablets are dispensed at the
#'                         start of each medication cycle. Cost is charged
#'                         to the model cycle containing that dispensing day,
#'                         regardless of whether all tablets are consumed.
#'                         This is the standard approach in CEA models.
#'                         If FALSE, cost is attributed per-administration
#'                         (same as IV logic).
#' @param vial_size        Numeric. mg per vial (IV only).
#' @param vial_sizes       Numeric vector. Available vial sizes in mg (IV).
#'                         If supplied, optimal vial combination is selected.
#'                         If NULL, uses \code{vial_size}.
#' @param vial_cost        Numeric or named numeric vector. Cost per vial. If
#'                         \code{vial_sizes} is a vector, this should be a
#'                         vector of corresponding costs.
#' @param wastage_threshold Numeric 0-1 (IV only). If the residual dose in the
#'                          last vial is <= this fraction of the vial size, do
#'                          NOT open that vial (dose-band down). E.g. 0.1 means
#'                          skip the last vial if you would use ≤10% of it.
#'                          Set to 0 to always round up (open every vial needed).
#' @param admin_cost       Numeric. Administration / dispensing cost per
#'                         administration event (IV) or per dispensing event
#'                         (oral with wastage).
#' @param cost_per_day     Numeric or NULL. If supplied, enables the simple
#'                         "cost per unit time" mode — all other dosing detail
#'                         is ignored and the cost is simply
#'                         cost_per_day × days in cycle.
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
    unit_cost         = NULL,
    units_per_pack    = 1,
    # oral-specific
    tablet_strength     = NULL,
    available_strengths = NULL,
    oral_wastage        = NULL,
    # iv-specific
    vial_size         = NULL,
    vial_sizes        = NULL,
    vial_cost         = NULL,
    wastage_threshold = 0.0,
    # common
    admin_cost        = 0,
    # simple mode
    cost_per_day      = NULL
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

    # Validate distribution parameters
    if (!is.null(patient_weight_sd)) {
        if (!is.numeric(patient_weight_sd) || length(patient_weight_sd) != 1 ||
            patient_weight_sd <= 0) {
            stop("patient_weight_sd must be a positive numeric scalar.")
        }
        if (dose_basis != "weight") {
            warning("patient_weight_sd is ignored when dose_basis != 'weight'.")
        }
    }
    if (!is.null(patient_bsa_sd)) {
        if (!is.numeric(patient_bsa_sd) || length(patient_bsa_sd) != 1 ||
            patient_bsa_sd <= 0) {
            stop("patient_bsa_sd must be a positive numeric scalar.")
        }
        if (dose_basis != "bsa") {
            warning("patient_bsa_sd is ignored when dose_basis != 'bsa'.")
        }
    }
    # Validate start_day
    if (!is.numeric(start_day) || length(start_day) != 1 ||
        !is.finite(start_day) || start_day < 0) {
        stop("start_day must be a finite, non-negative numeric scalar.")
    }

    # Default oral_wastage: TRUE for oral, FALSE for IV/SC/IM
    if (is.null(oral_wastage)) {
        oral_wastage <- (route == "oral")
    }
    if (oral_wastage && route != "oral") {
        stop("oral_wastage = TRUE is only valid for route = 'oral'.")
    }

    # Resolve actual dose in mg
    actual_dose_mg <- switch(dose_basis,
        flat   = dose_per_admin,
        weight = dose_per_admin * patient_weight,
        bsa    = dose_per_admin * patient_bsa
    )

    # Resolve vial / tablet info
    if (route %in% VIAL_ROUTES) {
        if (is.null(vial_sizes) && !is.null(vial_size)) {
            vial_sizes <- vial_size
        }
        if (is.null(vial_cost) && !is.null(unit_cost)) {
            vial_cost <- unit_cost
        }
        if (is.null(vial_sizes) && is.null(cost_per_day)) {
            stop(paste0(toupper(route),
                " regimen requires vial_size / vial_sizes or cost_per_day."))
        }
    }
    if (route == "oral") {
        if (is.null(available_strengths) && !is.null(tablet_strength)) {
            available_strengths <- tablet_strength
        }
        if (is.null(unit_cost) && is.null(cost_per_day)) {
            stop("Oral regimen requires unit_cost or cost_per_day.")
        }
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
            unit_cost          = unit_cost,
            units_per_pack     = units_per_pack,
            tablet_strength    = tablet_strength,
            available_strengths = available_strengths,
            oral_wastage       = oral_wastage,
            vial_size          = vial_size,
            vial_sizes         = vial_sizes,
            vial_cost          = vial_cost,
            wastage_threshold  = wastage_threshold,
            admin_cost         = admin_cost,
            cost_per_day       = cost_per_day
        ),
        class = "med_regimen"
    )
}
