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
#'
#' @export
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
    if (regimen$oral_wastage) {
        return(calculate_oral_wastage(
            regimen, n_cycles, cycle_starts, cycle_ends,
            model_cycle_length, single_drug_cost, single_adm_cost,
            treat_end, subcycle_precision, daily_detail
        ))
    }

    # ──────────────────────────────────────────────────────────────────────
    # PER-ADMINISTRATION PATH (IV, or oral without wastage)
    # ──────────────────────────────────────────────────────────────────────
    calculate_per_admin(
        regimen, n_cycles, cycle_starts, cycle_ends,
        model_cycle_length, single_drug_cost, single_adm_cost,
        treat_end, subcycle_precision, daily_detail
    )
}


#' Oral wastage costing path
#'
#' When oral_wastage = TRUE, the costing unit is the *medication cycle
#' dispensing event*, not the individual administration. At the start of
#' each medication cycle (day 1), the full cycle's worth of tablets is
#' dispensed and paid for. The cost is assigned to whichever model cycle
#' contains that dispensing day.
#'
#' @param regimen A med_regimen object.
#' @param n_cycles Integer number of model cycles.
#' @param cycle_starts Numeric vector of model cycle start days.
#' @param cycle_ends Numeric vector of model cycle end days.
#' @param model_cycle_length Numeric model cycle length in days.
#' @param single_drug_cost Numeric drug cost per administration.
#' @param single_adm_cost Numeric admin cost per event.
#' @param treat_end Numeric treatment end day.
#' @param subcycle_precision Logical for sub-cycle precision.
#' @param daily_detail Logical for daily detail output.
#' @return A data.frame.
#' @keywords internal
calculate_oral_wastage <- function(
    regimen, n_cycles, cycle_starts, cycle_ends,
    model_cycle_length, single_drug_cost, single_adm_cost,
    treat_end, subcycle_precision, daily_detail
) {
    dispensing_cost <- cost_oral_cycle_dispensing(regimen)
    mcl <- regimen$med_cycle_length

    if (subcycle_precision) {
        horizon <- max(cycle_ends)
        eff_treat_end <- if (is.finite(treat_end)) treat_end else horizon

        n_med_cycles <- ceiling(eff_treat_end / mcl)
        dispensing_days <- (seq_len(n_med_cycles) - 1L) * mcl

        dispensing_days <- dispensing_days[
            dispensing_days < eff_treat_end &
            dispensing_days < horizon
        ]

        cycle_idx <- findInterval(dispensing_days, cycle_starts,
                                  rightmost.closed = FALSE)
        cycle_idx <- pmin(cycle_idx, n_cycles)

        disp_counts <- tabulate(cycle_idx, nbins = n_cycles)

        # ── Daily detail ─────────────────────────────────────────────
        if (daily_detail) {
            return(build_oral_wastage_daily(
                regimen, n_cycles, cycle_starts, mcl,
                eff_treat_end, horizon, dispensing_days, n_med_cycles,
                dispensing_cost, single_adm_cost
            ))
        }

    } else {
        # ── Analytic: count dispensing events per model cycle
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

    data.frame(
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
}


#' Build daily detail data frame for oral wastage path
#'
#' @param regimen A med_regimen object.
#' @param n_cycles Integer number of model cycles.
#' @param cycle_starts Numeric vector of model cycle start days.
#' @param mcl Medication cycle length.
#' @param eff_treat_end Effective treatment end day.
#' @param horizon Model horizon in days.
#' @param dispensing_days Numeric vector of dispensing day indices.
#' @param n_med_cycles Number of medication cycles.
#' @param dispensing_cost Drug cost per dispensing event.
#' @param single_adm_cost Admin cost per event.
#' @return A data.frame with one row per day.
#' @keywords internal
build_oral_wastage_daily <- function(
    regimen, n_cycles, cycle_starts, mcl,
    eff_treat_end, horizon, dispensing_days, n_med_cycles,
    dispensing_cost, single_adm_cost
) {
    mc_starts <- (seq_len(n_med_cycles) - 1L) * mcl
    mc_starts <- mc_starts[mc_starts < eff_treat_end &
                           mc_starts < horizon]
    all_admin <- as.vector(
        outer(regimen$admin_days - 1L, mc_starts, "+")
    )
    all_admin <- sort(all_admin[all_admin < eff_treat_end &
                                all_admin < horizon])

    # Per-admin detail
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

    doses_taken <- as.integer(is_admin & on_tx)
    mg_taken    <- doses_taken * regimen$actual_dose_mg
    mg_dispensed_today <- doses_taken * mg_dispensed_per_admin
    tabs_taken  <- doses_taken * tabs_per_admin

    doses_costed <- ifelse(is_disp & on_tx, n_admin_per_cyc, 0L)
    tabs_costed  <- ifelse(is_disp & on_tx, tabs_per_disp, 0L)
    mg_costed    <- ifelse(is_disp & on_tx, mg_disp_per_disp, 0)
    drug_cost    <- ifelse(is_disp & on_tx, dispensing_cost, 0)
    admin_cost   <- ifelse(is_disp & on_tx, single_adm_cost, 0)

    data.frame(
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
}


#' Per-administration costing path (IV, or oral without wastage)
#'
#' @param regimen A med_regimen object.
#' @param n_cycles Integer number of model cycles.
#' @param cycle_starts Numeric vector of model cycle start days.
#' @param cycle_ends Numeric vector of model cycle end days.
#' @param model_cycle_length Numeric model cycle length in days.
#' @param single_drug_cost Numeric drug cost per administration.
#' @param single_adm_cost Numeric admin cost per event.
#' @param treat_end Numeric treatment end day.
#' @param subcycle_precision Logical for sub-cycle precision.
#' @param daily_detail Logical for daily detail output.
#' @return A data.frame.
#' @keywords internal
calculate_per_admin <- function(
    regimen, n_cycles, cycle_starts, cycle_ends,
    model_cycle_length, single_drug_cost, single_adm_cost,
    treat_end, subcycle_precision, daily_detail
) {
    if (subcycle_precision) {
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

        # ── Daily detail ─────────────────────────────────────────────
        if (daily_detail) {
            return(build_per_admin_daily(
                regimen, n_cycles, cycle_starts, horizon,
                eff_treat_end, all_admin_days,
                single_drug_cost, single_adm_cost
            ))
        }
    } else {
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

    data.frame(
        cycle             = seq_len(n_cycles),
        cycle_start_day   = cycle_starts,
        cycle_end_day     = cycle_ends,
        n_administrations = admin_counts,
        drug_cost         = admin_counts * single_drug_cost,
        admin_cost        = admin_counts * single_adm_cost,
        total_cost        = admin_counts * (single_drug_cost + single_adm_cost)
    )
}


#' Build daily detail data frame for per-administration path
#'
#' @param regimen A med_regimen object.
#' @param n_cycles Integer number of model cycles.
#' @param cycle_starts Numeric vector of model cycle start days.
#' @param horizon Model horizon in days.
#' @param eff_treat_end Effective treatment end day.
#' @param all_admin_days Numeric vector of all administration days.
#' @param single_drug_cost Numeric drug cost per administration.
#' @param single_adm_cost Numeric admin cost per event.
#' @return A data.frame with one row per day.
#' @keywords internal
build_per_admin_daily <- function(
    regimen, n_cycles, cycle_starts, horizon,
    eff_treat_end, all_admin_days,
    single_drug_cost, single_adm_cost
) {
    all_days <- seq(0L, horizon - 1L)
    day_model_cycle <- pmin(
        findInterval(all_days, cycle_starts), n_cycles)
    day_med_cycle     <- floor(all_days / regimen$med_cycle_length) + 1L
    day_med_cycle_day <- (all_days %% regimen$med_cycle_length) + 1L

    is_admin     <- all_days %in% all_admin_days
    on_tx        <- all_days < eff_treat_end

    doses_taken <- as.integer(is_admin & on_tx)

    if (regimen$route == "iv") {
        iv_res <- cost_iv_single(
            regimen$actual_dose_mg, regimen$vial_sizes,
            regimen$vial_cost, regimen$wastage_threshold)
        vials_per_admin  <- sum(iv_res$n_vials)
        mg_given         <- regimen$actual_dose_mg
        mg_from_vials    <- mg_given + iv_res$waste_mg

        data.frame(
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

        data.frame(
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
}
