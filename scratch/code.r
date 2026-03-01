###############################################################################
# Medication Costing Engine for Markov Cohort Models
# --------------------------------------------------
# Fully vectorized cycle-by-cycle medication cost calculation.
#
# Features:
#   - Flexible regimen specification (cycle length, admin days, frequencies)
#   - IV and oral therapies with distinct wastage logic
#   - Weight-based (mg/kg) and BSA-based (mg/m²) or flat dosing
#   - Tablet rounding (can't split a tablet — rounds up to nearest whole tablet)
#   - Oral wastage: full medication cycle dispensed upfront at cycle start;
#     cost charged to the model cycle containing the dispensing event even if
#     the patient discontinues mid-medication-cycle
#   - IV wastage with dose-banding / vial rounding and a threshold to avoid
#     opening a vial for a small residual amount
#   - Optional sub-cycle precision (e.g. daily granularity aggregated to
#     model cycles)
#   - Optional simple "cost-per-unit-time" mode
#   - Fully vectorized over an arbitrary vector of cycle times
###############################################################################

# ─────────────────────────────────────────────────────────────────────────────
# 1.  HELPER: define a medication regimen
# ─────────────────────────────────────────────────────────────────────────────

#' Create a medication regimen specification
#'
#' @param name            Character. Drug name (for labelling).
#' @param route           "iv" or "oral".
#' @param dose_per_admin  Numeric. Dose per administration in mg (flat dose)
#'                        or mg/kg or mg/m² depending on \code{dose_basis}.
#' @param dose_basis      "flat", "weight", or "bsa".
#' @param patient_weight  Numeric. Patient weight in kg (used if dose_basis
#'                        = "weight").
#' @param patient_bsa     Numeric. Patient BSA in m² (used if dose_basis
#'                        = "bsa").
#' @param med_cycle_length Numeric. Length of one *medication* cycle in days.
#'                         This may differ from the model cycle length.
#' @param n_admin_per_cycle Integer. Number of administrations per medication
#'                          cycle. Ignored if \code{admin_days} is specified.
#' @param admin_days       Integer vector. Explicit days within the medication
#'                         cycle on which administration occurs (1-indexed).
#'                         Overrides \code{n_admin_per_cycle} if supplied.
#' @param max_med_cycles   Numeric or Inf. Maximum number of medication cycles
#'                         (i.e. treatment duration cap). Inf = no cap.
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
define_regimen <- function(
    name              = "Drug A",
    route             = c("iv", "oral"),
    dose_per_admin    = 100,
    dose_basis        = c("flat", "weight", "bsa"),
    patient_weight    = 70,
    patient_bsa       = 1.8,
    med_cycle_length  = 21,
    n_admin_per_cycle = 1,
    admin_days        = NULL,
    max_med_cycles    = Inf,
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

    # Default oral_wastage: TRUE for oral, FALSE for IV
    if (is.null(oral_wastage)) {
        oral_wastage <- (route == "oral")
    }

    # Resolve admin_days
    if (is.null(admin_days)) {
        if (n_admin_per_cycle == 1) {
            admin_days <- 1L
        } else {
            # Evenly space across the cycle
            admin_days <- as.integer(
                round(seq(1, med_cycle_length,
                          length.out = n_admin_per_cycle + 1)[
                    1:n_admin_per_cycle
                ])
            )
        }
    }
    n_admin_per_cycle <- length(admin_days)

    # Resolve actual dose in mg
    actual_dose_mg <- switch(dose_basis,
        flat   = dose_per_admin,
        weight = dose_per_admin * patient_weight,
        bsa    = dose_per_admin * patient_bsa
    )

    # Resolve vial / tablet info
    if (route == "iv") {
        if (is.null(vial_sizes) && !is.null(vial_size)) {
            vial_sizes <- vial_size
        }
        if (is.null(vial_cost) && !is.null(unit_cost)) {
            vial_cost <- unit_cost
        }
        if (is.null(vial_sizes) && is.null(cost_per_day)) {
            stop("IV regimen requires vial_size / vial_sizes or cost_per_day.")
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
            actual_dose_mg     = actual_dose_mg,
            med_cycle_length   = med_cycle_length,
            n_admin_per_cycle  = n_admin_per_cycle,
            admin_days         = admin_days,
            max_med_cycles     = max_med_cycles,
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


# ─────────────────────────────────────────────────────────────────────────────
# 2.  CORE COSTING HELPERS
# ─────────────────────────────────────────────────────────────────────────────

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
#' @return Numeric scalar: drug cost only (no admin cost) of one admin.
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
cost_oral_cycle_dispensing <- function(reg) {
    per_admin <- cost_single_admin_drug(reg)
    per_admin * reg$n_admin_per_cycle
}


# ─────────────────────────────────────────────────────────────────────────────
# 3.  MAIN ENGINE: calculate_med_costs()
# ─────────────────────────────────────────────────────────────────────────────

#' Calculate medication costs cycle-by-cycle for a Markov model
#'
#' @param regimen         A \code{med_regimen} object (from
#'                        \code{define_regimen}).
#' @param model_cycle_length Numeric. Length of one model cycle in days.
#' @param n_cycles        Integer. Number of model cycles.
#' @param cycle_times     Numeric vector. Alternatively, supply start times
#'                        (in days) of each model cycle directly. If supplied,
#'                        \code{n_cycles} and \code{model_cycle_length} are
#'                        inferred.
#' @param subcycle_precision Logical. If TRUE, builds a daily-level vector of
#'                        events and aggregates to model cycles. Provides exact
#'                        counts when med cycles and model cycles are
#'                        misaligned. For oral with wastage, sub-cycle
#'                        precision tracks dispensing events at med cycle
#'                        starts.
#' @param daily_detail    Logical. If TRUE (requires subcycle_precision = TRUE),
#'                        returns a daily-resolution data frame with one row
#'                        per day showing dispensing events, administration
#'                        events, and costs on each day. The daily frame
#'                        includes a \code{model_cycle} column so you can
#'                        aggregate yourself or inspect the raw schedule.
#' @param simple_daily_cost Logical. If TRUE (and regimen has cost_per_day),
#'                          simply returns cost_per_day × days per model cycle.
#' @return A data.frame. If \code{daily_detail = FALSE} (default): one row
#'         per model cycle with aggregated costs. If \code{daily_detail = TRUE}:
#'         one row per day with event flags and costs.
calculate_med_costs <- function(
    regimen,
    model_cycle_length = 7,
    n_cycles           = 52,
    cycle_times        = NULL,
    subcycle_precision = FALSE,
    daily_detail       = FALSE,
    simple_daily_cost  = FALSE
) {

    stopifnot(inherits(regimen, "med_regimen"))

    # daily_detail implies subcycle_precision
    if (daily_detail) subcycle_precision <- TRUE

    # ── Resolve cycle_times ──────────────────────────────────────────────
    if (!is.null(cycle_times)) {
        n_cycles           <- length(cycle_times)
        cycle_starts       <- cycle_times
        if (n_cycles > 1) {
            model_cycle_length <- cycle_starts[2] - cycle_starts[1]
        }
    } else {
        cycle_starts <- (seq_len(n_cycles) - 1) * model_cycle_length
    }

    cycle_ends <- cycle_starts + model_cycle_length

    # ── MODE 1: Simple cost-per-day ──────────────────────────────────────
    if (simple_daily_cost && !is.null(regimen$cost_per_day)) {
        daily_cost <- regimen$cost_per_day
        total_cost <- daily_cost * model_cycle_length
        out <- data.frame(
            cycle             = seq_len(n_cycles),
            cycle_start_day   = cycle_starts,
            cycle_end_day     = cycle_ends,
            n_administrations = NA_real_,
            drug_cost         = total_cost,
            admin_cost        = 0,
            total_cost        = total_cost
        )
        if (is.finite(regimen$max_med_cycles)) {
            max_day <- regimen$max_med_cycles * regimen$med_cycle_length
            beyond  <- cycle_starts >= max_day
            out$drug_cost[beyond]  <- 0
            out$admin_cost[beyond] <- 0
            out$total_cost[beyond] <- 0
        }
        return(out)
    }

    # ── Pre-compute costs ────────────────────────────────────────────────
    single_drug_cost <- cost_single_admin_drug(regimen)
    single_adm_cost  <- regimen$admin_cost

    # Treatment cap in days
    if (is.finite(regimen$max_med_cycles)) {
        treat_end <- regimen$max_med_cycles * regimen$med_cycle_length
    } else {
        treat_end <- Inf
    }

    # ──────────────────────────────────────────────────────────────────────
    # ORAL WASTAGE PATH
    # ──────────────────────────────────────────────────────────────────────
    # When oral_wastage = TRUE, the costing unit is the *medication cycle
    # dispensing event*, not the individual administration. At the start of
    # each medication cycle (day 1), the full cycle's worth of tablets is
    # dispensed and paid for. The cost is assigned to whichever model cycle
    # contains that dispensing day.
    #
    # In a Markov cohort model, this means patients who transition off
    # treatment mid-medication-cycle have already incurred the full cycle's
    # drug cost — the unused tablets are wasted.
    # ──────────────────────────────────────────────────────────────────────

    if (regimen$oral_wastage) {

        dispensing_cost <- cost_oral_cycle_dispensing(regimen)

        # Dispensing events occur at day 0 of each medication cycle:
        # absolute days 0, mcl, 2*mcl, 3*mcl, ...
        mcl <- regimen$med_cycle_length

        if (subcycle_precision) {
            # ── Sub-cycle: enumerate all dispensing days, bin into model
            #    cycles
            horizon <- max(cycle_ends)
            eff_treat_end <- if (is.finite(treat_end)) treat_end else horizon

            n_med_cycles <- ceiling(eff_treat_end / mcl)
            dispensing_days <- (seq_len(n_med_cycles) - 1L) * mcl

            # Keep only those within treatment window AND model horizon
            dispensing_days <- dispensing_days[
                dispensing_days < eff_treat_end &
                dispensing_days < horizon
            ]

            # Bin into model cycles
            cycle_idx <- findInterval(dispensing_days, cycle_starts,
                                      rightmost.closed = FALSE)
            cycle_idx <- pmin(cycle_idx, n_cycles)

            disp_counts <- tabulate(cycle_idx, nbins = n_cycles)

            # ── Daily detail: build a day-level frame ────────────────────
            if (daily_detail) {
                # Derive all administration days
                mc_starts <- (seq_len(n_med_cycles) - 1L) * mcl
                mc_starts <- mc_starts[mc_starts < eff_treat_end &
                                       mc_starts < horizon]
                all_admin <- as.vector(
                    outer(regimen$admin_days - 1L, mc_starts, "+")
                )
                all_admin <- sort(all_admin[all_admin < eff_treat_end &
                                            all_admin < horizon])

                # Pre-compute per-admin detail
                if (!is.null(names(regimen$unit_cost))) {
                    tab_res <- cost_oral_single(
                        regimen$actual_dose_mg,
                        regimen$available_strengths,
                        cost_per_strength = regimen$unit_cost)
                } else {
                    tab_res <- cost_oral_single(
                        regimen$actual_dose_mg,
                        regimen$available_strengths,
                        cost_per_tablet = regimen$unit_cost)
                }
                tabs_per_admin   <- sum(tab_res$tablets)
                mg_dispensed_per_admin <- sum(
                    tab_res$tablets * regimen$available_strengths[
                        order(regimen$available_strengths, decreasing = TRUE)])
                mg_wasted_per_admin <- tab_res$waste_mg
                cost_per_admin   <- tab_res$drug_cost

                n_admin_per_cyc  <- regimen$n_admin_per_cycle
                tabs_per_disp    <- tabs_per_admin * n_admin_per_cyc
                mg_disp_per_disp <- mg_dispensed_per_admin * n_admin_per_cyc

                all_days <- seq(0L, horizon - 1L)
                day_model_cycle <- pmin(
                    findInterval(all_days, cycle_starts), n_cycles)
                day_med_cycle     <- floor(all_days / mcl) + 1L
                day_med_cycle_day <- (all_days %% mcl) + 1L

                is_disp  <- all_days %in% dispensing_days
                is_admin <- all_days %in% all_admin
                on_tx    <- all_days < eff_treat_end

                # Doses taken that day (0 or 1 per admin day)
                doses_taken <- as.integer(is_admin & on_tx)
                # mg actually consumed (prescribed dose, not rounded)
                mg_taken    <- doses_taken * regimen$actual_dose_mg
                # mg dispensed (rounded to whole tablets)
                mg_dispensed_today <- doses_taken * mg_dispensed_per_admin
                # Tablets patient takes that day
                tabs_taken  <- doses_taken * tabs_per_admin

                # Dispensing: tablets & mg costed at dispensing event
                doses_costed <- ifelse(is_disp & on_tx, n_admin_per_cyc, 0L)
                tabs_costed  <- ifelse(is_disp & on_tx, tabs_per_disp, 0L)
                mg_costed    <- ifelse(is_disp & on_tx, mg_disp_per_disp, 0)
                drug_cost    <- ifelse(is_disp & on_tx, dispensing_cost, 0)
                admin_cost   <- ifelse(is_disp & on_tx, single_adm_cost, 0)

                daily <- data.frame(
                    day             = all_days,
                    model_cycle     = day_model_cycle,
                    med_cycle       = ifelse(on_tx, day_med_cycle, NA_integer_),
                    med_cycle_day   = ifelse(on_tx, day_med_cycle_day, NA_integer_),
                    is_dispensing   = is_disp & on_tx,
                    is_admin        = is_admin & on_tx,
                    doses_taken     = doses_taken,
                    mg_taken        = mg_taken,
                    mg_dispensed    = mg_dispensed_today,
                    tabs_taken      = tabs_taken,
                    doses_costed    = doses_costed,
                    tabs_costed     = tabs_costed,
                    mg_costed       = mg_costed,
                    drug_cost       = drug_cost,
                    admin_cost      = admin_cost,
                    total_cost      = drug_cost + admin_cost,
                    stringsAsFactors = FALSE
                )
                return(daily)
            }

        } else {
            # ── Analytic: count dispensing events per model cycle
            # Dispensing on day d = k * mcl, k = 0, 1, 2, ...
            # Need k * mcl in [cycle_start, cycle_end) AND < treat_end

            effective_end <- pmin(cycle_ends, treat_end)

            k_lo <- ceiling(cycle_starts / mcl)
            k_hi <- ceiling(effective_end / mcl) - 1L
            k_lo <- pmax(k_lo, 0L)
            disp_counts <- pmax(k_hi - k_lo + 1L, 0L)
        }

        # Calculate tablets per dispensing for reporting
        admin_res <- cost_oral_single(
            dose_mg             = regimen$actual_dose_mg,
            available_strengths = regimen$available_strengths,
            cost_per_tablet     = if (is.null(names(regimen$unit_cost)))
                                      regimen$unit_cost else NULL,
            cost_per_strength   = if (!is.null(names(regimen$unit_cost)))
                                      regimen$unit_cost else NULL
        )
        tabs_per_admin <- sum(admin_res$tablets)
        tabs_per_dispensing <- tabs_per_admin * regimen$n_admin_per_cycle

        out <- data.frame(
            cycle                = seq_len(n_cycles),
            cycle_start_day      = cycle_starts,
            cycle_end_day        = cycle_ends,
            n_dispensing_events  = disp_counts,
            n_tablets_dispensed  = disp_counts * tabs_per_dispensing,
            drug_cost            = disp_counts * dispensing_cost,
            admin_cost           = disp_counts * single_adm_cost,
            total_cost           = disp_counts * (dispensing_cost +
                                                  single_adm_cost)
        )
        return(out)
    }

    # ──────────────────────────────────────────────────────────────────────
    # PER-ADMINISTRATION PATH (IV, or oral without wastage)
    # ──────────────────────────────────────────────────────────────────────

    if (subcycle_precision) {
        # ── Sub-cycle precision: enumerate all admin days
        horizon <- max(cycle_ends)
        eff_treat_end <- if (is.finite(treat_end)) treat_end else horizon

        n_full_med_cycles <- ceiling(min(eff_treat_end, horizon) /
                                     regimen$med_cycle_length)
        med_cycle_offsets <- (seq_len(n_full_med_cycles) - 1) *
                             regimen$med_cycle_length
        all_admin_days <- as.vector(
            outer(regimen$admin_days - 1, med_cycle_offsets, "+")
        )
        all_admin_days <- sort(
            all_admin_days[all_admin_days < eff_treat_end &
                           all_admin_days < horizon]
        )

        cycle_idx <- findInterval(all_admin_days, cycle_starts,
                                  rightmost.closed = FALSE)
        cycle_idx <- pmin(cycle_idx, n_cycles)

        admin_counts <- tabulate(cycle_idx, nbins = n_cycles)

        # ── Daily detail: build a day-level frame ────────────────────
        if (daily_detail) {
            all_days <- seq(0L, horizon - 1L)
            day_model_cycle <- pmin(
                findInterval(all_days, cycle_starts), n_cycles)
            day_med_cycle     <- floor(all_days / regimen$med_cycle_length) + 1L
            day_med_cycle_day <- (all_days %% regimen$med_cycle_length) + 1L

            is_admin     <- all_days %in% all_admin_days
            on_tx        <- all_days < eff_treat_end

            doses_taken <- as.integer(is_admin & on_tx)

            if (regimen$route == "iv") {
                # IV detail
                iv_res <- cost_iv_single(
                    regimen$actual_dose_mg, regimen$vial_sizes,
                    regimen$vial_cost, regimen$wastage_threshold)
                vials_per_admin  <- sum(iv_res$n_vials)
                mg_given         <- regimen$actual_dose_mg
                mg_from_vials    <- mg_given + iv_res$waste_mg

                daily <- data.frame(
                    day             = all_days,
                    model_cycle     = day_model_cycle,
                    med_cycle       = ifelse(on_tx, day_med_cycle, NA_integer_),
                    med_cycle_day   = ifelse(on_tx, day_med_cycle_day, NA_integer_),
                    is_admin        = is_admin & on_tx,
                    doses_taken     = doses_taken,
                    mg_taken        = doses_taken * mg_given,
                    mg_from_vials   = doses_taken * mg_from_vials,
                    mg_wasted       = doses_taken * iv_res$waste_mg,
                    vials_used      = doses_taken * vials_per_admin,
                    drug_cost       = doses_taken * single_drug_cost,
                    admin_cost      = doses_taken * single_adm_cost,
                    total_cost      = doses_taken * (single_drug_cost +
                                                     single_adm_cost),
                    stringsAsFactors = FALSE
                )
            } else {
                # Oral no-wastage detail
                if (!is.null(names(regimen$unit_cost))) {
                    tab_res <- cost_oral_single(
                        regimen$actual_dose_mg,
                        regimen$available_strengths,
                        cost_per_strength = regimen$unit_cost)
                } else {
                    tab_res <- cost_oral_single(
                        regimen$actual_dose_mg,
                        regimen$available_strengths,
                        cost_per_tablet = regimen$unit_cost)
                }
                tabs_per_admin <- sum(tab_res$tablets)
                mg_dispensed   <- sum(
                    tab_res$tablets * regimen$available_strengths[
                        order(regimen$available_strengths,
                              decreasing = TRUE)])

                daily <- data.frame(
                    day             = all_days,
                    model_cycle     = day_model_cycle,
                    med_cycle       = ifelse(on_tx, day_med_cycle, NA_integer_),
                    med_cycle_day   = ifelse(on_tx, day_med_cycle_day, NA_integer_),
                    is_admin        = is_admin & on_tx,
                    doses_taken     = doses_taken,
                    mg_taken        = doses_taken * regimen$actual_dose_mg,
                    mg_dispensed    = doses_taken * mg_dispensed,
                    mg_wasted       = doses_taken * tab_res$waste_mg,
                    tabs_taken      = doses_taken * tabs_per_admin,
                    drug_cost       = doses_taken * single_drug_cost,
                    admin_cost      = doses_taken * single_adm_cost,
                    total_cost      = doses_taken * (single_drug_cost +
                                                     single_adm_cost),
                    stringsAsFactors = FALSE
                )
            }
            return(daily)
        }
        # ── Analytic: vectorized count over admin days
        mcl   <- regimen$med_cycle_length
        adays <- regimen$admin_days - 1L

        admin_counts <- rep(0L, n_cycles)

        for (a in adays) {
            effective_end <- pmin(cycle_ends, treat_end)
            k_lo <- ceiling((cycle_starts - a) / mcl)
            k_hi <- ceiling((effective_end - a) / mcl) - 1L
            k_lo <- pmax(k_lo, 0L)
            counts_a <- pmax(k_hi - k_lo + 1L, 0L)
            admin_counts <- admin_counts + counts_a
        }
    }

    out <- data.frame(
        cycle             = seq_len(n_cycles),
        cycle_start_day   = cycle_starts,
        cycle_end_day     = cycle_ends,
        n_administrations = admin_counts,
        drug_cost         = admin_counts * single_drug_cost,
        admin_cost        = admin_counts * single_adm_cost,
        total_cost        = admin_counts * (single_drug_cost + single_adm_cost)
    )
    return(out)
}


# ─────────────────────────────────────────────────────────────────────────────
# 4.  CONVENIENCE: cost multiple regimens (combination therapy)
# ─────────────────────────────────────────────────────────────────────────────

#' Calculate combined cost for a list of regimens
#'
#' @param regimens         List of \code{med_regimen} objects.
#' @param ...              Arguments passed to \code{calculate_med_costs}.
#' @return A data.frame with per-cycle totals and per-drug breakdowns.
calculate_combination_costs <- function(regimens, ...) {
    stopifnot(is.list(regimens))
    stopifnot(all(sapply(regimens, inherits, "med_regimen")))

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

    return(base)
}


# ─────────────────────────────────────────────────────────────────────────────
# 5.  REPORTING: summarise_regimen() and detail_schedule()
# ─────────────────────────────────────────────────────────────────────────────

#' Print a summary of a regimen's dispensing and administration structure
#'
#' Shows how dosing translates to tablets/vials, what constitutes a dispensing
#' event vs an administration event, and the cost breakdown at each level.
#'
#' @param reg A \code{med_regimen} object.
#' @return Invisibly returns reg. Called for its side effect (printing).
summarise_regimen <- function(reg) {
    stopifnot(inherits(reg, "med_regimen"))

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
    } else if (reg$dose_basis == "bsa") {
        cat("  Patient BSA:        ", reg$patient_bsa, " m2\n", sep = "")
    }
    cat("  Actual dose:        ", reg$actual_dose_mg, " mg\n", sep = "")

    # ── Tablet / vial breakdown ──────────────────────────────────────────
    if (reg$route == "oral") {
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
        cat("  Dispensed per admin: ", dispensed_mg, " mg\n", sep = "")
        if (res$waste_mg > 1e-9) {
            cat("  Tablet rounding waste: ", round(res$waste_mg, 2),
                " mg per admin\n", sep = "")
        } else {
            cat("  Tablet rounding waste: none\n")
        }
        cat("  Drug cost per admin: $", res$drug_cost, "\n", sep = "")

    } else {
        # IV
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
        cat("  IV waste: ", round(res$waste_mg, 2), " mg\n", sep = "")
        cat("  Wastage threshold: ", reg$wastage_threshold * 100, "%\n",
            sep = "")
        cat("  Drug cost per admin: $", res$drug_cost, "\n", sep = "")
    }

    # ── Schedule ─────────────────────────────────────────────────────────
    cat("\n")
    cat("MEDICATION CYCLE SCHEDULE\n")
    cat("  Medication cycle length: ", reg$med_cycle_length, " days\n",
        sep = "")
    cat("  Administrations per cycle: ", reg$n_admin_per_cycle, "\n",
        sep = "")

    # Compact display of admin days
    if (length(reg$admin_days) <= 10) {
        cat("  Administration days: ",
            paste(reg$admin_days, collapse = ", "), "\n", sep = "")
    } else {
        # Show as range(s)
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

    # ── Dispensing vs administration ─────────────────────────────────────
    cat("\n")
    cat("DISPENSING vs ADMINISTRATION\n")

    if (reg$route == "oral" && reg$oral_wastage) {
        disp_cost <- cost_oral_cycle_dispensing(reg)
        per_admin <- cost_single_admin_drug(reg)
        tabs_per_admin <- sum(res$tablets)
        tabs_per_disp  <- tabs_per_admin * reg$n_admin_per_cycle

        cat("  Mode: ORAL WASTAGE (dispense full cycle upfront)\n")
        cat("  ┌─ Dispensing event (day 1 of each med cycle):\n")
        cat("  │    Tablets dispensed: ", tabs_per_disp, "\n", sep = "")
        cat("  │    Drug cost:        $", disp_cost, "\n", sep = "")
        if (reg$admin_cost > 0) {
            cat("  │    Dispensing cost:  $", reg$admin_cost, "\n", sep = "")
            cat("  │    Total:           $", disp_cost + reg$admin_cost,
                "\n", sep = "")
        }
        cat("  │\n")
        cat("  └─ Administration events (days ",
            min(reg$admin_days), "-", max(reg$admin_days), "):\n", sep = "")
        cat("       ", reg$n_admin_per_cycle,
            " administrations per cycle\n", sep = "")
        cat("       Cost already captured at dispensing\n")
        cat("       (patients who discontinue mid-cycle still\n")
        cat("        incur the full dispensing cost)\n")

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
            (per_admin + reg$admin_cost) * reg$n_admin_per_cycle, "\n",
            sep = "")

    } else {
        # IV
        per_admin <- cost_single_admin_drug(reg)
        cat("  Mode: PER-ADMINISTRATION (IV)\n")
        cat("  Each visit costed individually:\n")
        cat("    Drug cost per admin: $", per_admin, "\n", sep = "")
        cat("    Admin cost per admin: $", reg$admin_cost, "\n", sep = "")
        cat("    Total per admin: $", per_admin + reg$admin_cost, "\n",
            sep = "")
        cat("    Total per med cycle (",
            reg$n_admin_per_cycle, " admins): $",
            (per_admin + reg$admin_cost) * reg$n_admin_per_cycle,
            "\n", sep = "")
    }

    cat("\n", divider, "\n\n", sep = "")
    invisible(reg)
}


#' Generate a detailed day-level event schedule
#'
#' Shows every dispensing and administration event, which medication cycle
#' and model cycle it falls in, and the associated cost. This is the
#' "unrolled" view before aggregation to model cycles.
#'
#' @param regimen            A \code{med_regimen} object.
#' @param model_cycle_length Numeric. Model cycle length in days.
#' @param n_cycles           Integer. Number of model cycles.
#' @param cycle_times        Numeric vector. Custom cycle start times.
#' @return A data.frame with one row per event (dispensing or administration).
detail_schedule <- function(
    regimen,
    model_cycle_length = 7,
    n_cycles           = 52,
    cycle_times        = NULL
) {
    stopifnot(inherits(regimen, "med_regimen"))

    # Resolve model cycles
    if (!is.null(cycle_times)) {
        n_cycles     <- length(cycle_times)
        cycle_starts <- cycle_times
        if (n_cycles > 1)
            model_cycle_length <- cycle_starts[2] - cycle_starts[1]
    } else {
        cycle_starts <- (seq_len(n_cycles) - 1) * model_cycle_length
    }
    cycle_ends <- cycle_starts + model_cycle_length
    horizon    <- max(cycle_ends)

    mcl <- regimen$med_cycle_length

    # Treatment cap
    if (is.finite(regimen$max_med_cycles)) {
        treat_end <- regimen$max_med_cycles * mcl
    } else {
        treat_end <- horizon
    }
    eff_end <- min(treat_end, horizon)

    # Enumerate all med cycles
    n_med_cycles    <- ceiling(eff_end / mcl)
    med_cycle_starts <- (seq_len(n_med_cycles) - 1L) * mcl
    med_cycle_starts <- med_cycle_starts[med_cycle_starts < eff_end]

    # Pre-compute costs
    single_drug  <- cost_single_admin_drug(regimen)
    single_admin <- regimen$admin_cost

    if (regimen$oral_wastage) {
        disp_cost <- cost_oral_cycle_dispensing(regimen)
    }

    # Build event rows
    events <- list()
    row_i  <- 0L

    for (mc in seq_along(med_cycle_starts)) {
        mc_start <- med_cycle_starts[mc]

        # ── Dispensing event (oral wastage only) ─────────────────────────
        if (regimen$oral_wastage) {
            if (mc_start < eff_end) {
                row_i <- row_i + 1L
                model_cyc <- findInterval(mc_start, cycle_starts)
                model_cyc <- min(model_cyc, n_cycles)
                events[[row_i]] <- data.frame(
                    day            = mc_start,
                    med_cycle      = mc,
                    med_cycle_day  = 1L,
                    model_cycle    = model_cyc,
                    event_type     = "dispensing",
                    drug_cost      = disp_cost,
                    admin_cost     = single_admin,
                    total_cost     = disp_cost + single_admin,
                    stringsAsFactors = FALSE
                )
            }
        }

        # ── Administration events ────────────────────────────────────────
        for (ad in regimen$admin_days) {
            abs_day <- mc_start + (ad - 1L)
            if (abs_day >= eff_end) next

            row_i <- row_i + 1L
            model_cyc <- findInterval(abs_day, cycle_starts)
            model_cyc <- min(model_cyc, n_cycles)

            if (regimen$oral_wastage) {
                # Cost already captured at dispensing
                events[[row_i]] <- data.frame(
                    day            = abs_day,
                    med_cycle      = mc,
                    med_cycle_day  = ad,
                    model_cycle    = model_cyc,
                    event_type     = "administration",
                    drug_cost      = 0,
                    admin_cost     = 0,
                    total_cost     = 0,
                    stringsAsFactors = FALSE
                )
            } else {
                events[[row_i]] <- data.frame(
                    day            = abs_day,
                    med_cycle      = mc,
                    med_cycle_day  = ad,
                    model_cycle    = model_cyc,
                    event_type     = "administration",
                    drug_cost      = single_drug,
                    admin_cost     = single_admin,
                    total_cost     = single_drug + single_admin,
                    stringsAsFactors = FALSE
                )
            }
        }
    }

    out <- do.call(rbind, events)
    rownames(out) <- NULL
    return(out)
}


###############################################################################
# 6.  EXAMPLES / DEMONSTRATION
###############################################################################

cat("=== Medication Costing Engine for Markov Models ===\n\n")

# ─── Example 1: Oral WITH wastage (user's case) ─────────────────────────────
oral_wastage_drug <- define_regimen(
    name              = "Oral Drug",
    route             = "oral",
    dose_per_admin    = 200,
    dose_basis        = "flat",
    med_cycle_length  = 21,
    admin_days        = 1:14,
    available_strengths = 100,
    unit_cost         = 50,
    oral_wastage      = TRUE,
    admin_cost        = 0
)

# ── Regimen summary ──
summarise_regimen(oral_wastage_drug)

# ── Aggregated model-cycle costs ──
cat("--- Aggregated to 30-day model cycles ---\n")
result_wastage <- calculate_med_costs(
    regimen            = oral_wastage_drug,
    model_cycle_length = 30,
    n_cycles           = 6,
    subcycle_precision = TRUE
)
print(result_wastage)
cat(sprintf("\nTotal: $%.2f\n\n", sum(result_wastage$total_cost)))

# ── Daily detail view ──
cat("--- Daily detail (first 2 model cycles = 60 days) ---\n")
daily <- calculate_med_costs(
    regimen            = oral_wastage_drug,
    model_cycle_length = 30,
    n_cycles           = 2,
    daily_detail       = TRUE
)
print(daily)

# ── Event schedule ──
cat("\n--- Event schedule (dispensing + admin events only) ---\n")
sched <- detail_schedule(
    regimen            = oral_wastage_drug,
    model_cycle_length = 30,
    n_cycles           = 4
)
print(head(sched, 40))


# ─── Example 2: IV therapy ──────────────────────────────────────────────────
cat("\n\n")
iv_drug <- define_regimen(
    name              = "IV Drug A",
    route             = "iv",
    dose_per_admin    = 75,
    dose_basis        = "bsa",
    patient_bsa       = 1.8,
    med_cycle_length  = 21,
    admin_days        = c(1, 8),
    max_med_cycles    = 4,
    vial_sizes        = c(100, 40),
    vial_cost         = c(1500, 650),
    wastage_threshold = 0.10,
    admin_cost        = 350
)

summarise_regimen(iv_drug)

cat("--- IV daily detail (first 3 weeks) ---\n")
iv_daily <- calculate_med_costs(
    regimen            = iv_drug,
    model_cycle_length = 7,
    n_cycles           = 3,
    daily_detail       = TRUE
)
print(iv_daily)

cat("\n--- IV aggregated (weekly model cycles) ---\n")
iv_agg <- calculate_med_costs(
    regimen            = iv_drug,
    model_cycle_length = 7,
    n_cycles           = 14,
    subcycle_precision = TRUE
)
print(iv_agg)