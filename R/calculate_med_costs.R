#' Resolve model cycle boundaries
#'
#' Centralises computation of cycle_starts and cycle_ends so that
#' non-uniform \code{cycle_times} vectors produce correct boundaries.
#'
#' @param n_cycles Integer.
#' @param model_cycle_length Numeric.
#' @param cycle_times Optional numeric vector.
#' @return Named list: cycle_starts, cycle_ends, n_cycles
#' @keywords internal
resolve_cycles <- function(n_cycles, model_cycle_length, cycle_times = NULL) {
    if (!is.null(cycle_times)) {
        n_cycles     <- length(cycle_times)
        cycle_starts <- cycle_times
        if (n_cycles > 1L) {
            gaps       <- diff(cycle_starts)
            cycle_ends <- c(cycle_starts[-1L],
                            cycle_starts[n_cycles] + gaps[n_cycles - 1L])
        } else {
            cycle_ends <- cycle_starts + model_cycle_length
        }
    } else {
        cycle_starts <- (seq_len(n_cycles) - 1L) * model_cycle_length
        cycle_ends   <- cycle_starts + model_cycle_length
    }
    list(cycle_starts = cycle_starts, cycle_ends = cycle_ends, n_cycles = n_cycles)
}


#' Calculate medication costs cycle-by-cycle for a Markov model
#'
#' Accepts one or more \code{med_regimen} objects. When a single regimen is
#' supplied the return value is identical to the previous API (a data.frame).
#' When multiple regimens are supplied and \code{daily_detail = FALSE}, the
#' result is a wide-format data.frame with per-drug columns
#' (\code{{Name}_drug_cost}, \code{{Name}_admin_cost},
#' \code{{Name}_total_cost}) plus a combined \code{total_cost}. When multiple
#' regimens are supplied with \code{daily_detail = TRUE}, the result is a
#' named list of per-drug daily data.frames.
#'
#' @param ...              One or more \code{med_regimen} objects (from
#'                         \code{define_regimen}).
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
#'                          simply returns cost_per_day x days per model cycle.
#' @param shared_admin Controls whether drugs in a combination share
#'   administration costs on overlapping days. When multiple drugs are
#'   administered on the same day, only the highest administration cost in
#'   a sharing group is charged; the others are zeroed out.
#'   \itemize{
#'     \item \code{NULL} (default): no sharing, backward compatible.
#'     \item \code{TRUE}: all drugs in one sharing group.
#'     \item A list of character vectors: explicit groups, e.g.
#'       \code{list(c("Drug A", "Drug B"))}. Drug names must match the
#'       \code{name} parameter from \code{define_regimen()}.
#'   }
#'   When active, adds \code{{Drug}_pre_sharing_admin_cost} diagnostic columns
#'   showing original admin costs before sharing adjustment.
#' @return See description.
#'
#' @export
calculate_med_costs <- function(
    ...,
    model_cycle_length = 7,
    n_cycles           = 52,
    cycle_times        = NULL,
    subcycle_precision = FALSE,
    daily_detail       = FALSE,
    simple_daily_cost  = FALSE,
    shared_admin       = NULL
) {
    regimens <- list(...)

    if (length(regimens) == 0L) {
        stop("At least one med_regimen object must be supplied.")
    }
    not_regimen <- !vapply(regimens, inherits, logical(1), "med_regimen")
    if (any(not_regimen)) {
        stop("All unnamed arguments must be med_regimen objects.")
    }

    shared_args <- list(
        model_cycle_length = model_cycle_length,
        n_cycles           = n_cycles,
        cycle_times        = cycle_times,
        subcycle_precision = subcycle_precision,
        daily_detail       = daily_detail,
        simple_daily_cost  = simple_daily_cost
    )

    # ── Single regimen: backward-compatible return ─────────────────────
    if (length(regimens) == 1L) {
        if (!is.null(shared_admin) && !identical(shared_admin, FALSE)) {
            warning("`shared_admin` is ignored for single-regimen calls.",
                    call. = FALSE)
        }
        result <- do.call(calculate_single_regimen,
                          c(list(regimen = regimens[[1]]), shared_args))
        cls <- if (daily_detail) "regimen_daily_detail" else "regimen_costs"
        class(result) <- c(cls, class(result))
        return(result)
    }

    # ── Multiple regimens ──────────────────────────────────────────────
    results <- lapply(regimens, function(reg) {
        do.call(calculate_single_regimen,
                c(list(regimen = reg), shared_args))
    })
    drug_names <- vapply(regimens, function(r) r$name, character(1))
    names(results) <- drug_names

    # ── Validate shared_admin ────────────────────────────────────────
    shared_admin <- validate_shared_admin(shared_admin, drug_names)

    # ── Shared admin branch ──────────────────────────────────────────
    if (!is.null(shared_admin)) {
        # Force daily computation internally
        daily_args <- list(
            model_cycle_length = model_cycle_length,
            n_cycles           = n_cycles,
            cycle_times        = cycle_times,
            subcycle_precision = TRUE,
            daily_detail       = TRUE,
            simple_daily_cost  = simple_daily_cost
        )
        daily_results <- lapply(regimens, function(reg) {
            do.call(calculate_single_regimen,
                    c(list(regimen = reg), daily_args))
        })
        names(daily_results) <- drug_names

        # Build wide-format daily data
        base <- daily_results[[1]][, c("day", "model_cycle")]
        drug_columns <- list()
        for (i in seq_along(daily_results)) {
            res <- daily_results[[i]]
            nm <- gsub("[^a-zA-Z0-9]", "_", drug_names[i])
            per_drug_cols <- setdiff(names(res), c("day", "model_cycle"))
            prefixed <- paste0(nm, "_", per_drug_cols)
            drug_columns[[drug_names[i]]] <- prefixed
            for (j in seq_along(per_drug_cols)) {
                base[[prefixed[j]]] <- res[[per_drug_cols[j]]]
            }
        }
        tc_cols <- grep("_total_cost$", names(base), value = TRUE)
        base$total_cost <- rowSums(base[, tc_cols, drop = FALSE])

        # Apply admin sharing
        base <- apply_admin_sharing(base, shared_admin, drug_names)

        # Update drug_columns to include pre_sharing_admin_cost columns
        grouped_drugs <- unlist(shared_admin)
        for (drug in grouped_drugs) {
            nm <- gsub("[^a-zA-Z0-9]", "_", drug)
            pre_col <- paste0(nm, "_pre_sharing_admin_cost")
            # Insert pre_sharing column before admin_cost in the drug's columns
            cols <- drug_columns[[drug]]
            admin_idx <- which(cols == paste0(nm, "_admin_cost"))
            if (length(admin_idx) > 0L) {
                drug_columns[[drug]] <- append(cols, pre_col,
                                               after = admin_idx - 1L)
            } else {
                drug_columns[[drug]] <- c(cols, pre_col)
            }
        }

        # Reorder columns so each drug's columns are grouped together
        col_order <- c("day", "model_cycle",
                       unlist(drug_columns, use.names = FALSE),
                       "total_cost")
        base <- base[, col_order]

        if (daily_detail) {
            attr(base, "drug_names")    <- drug_names
            attr(base, "drug_columns")  <- drug_columns
            attr(base, "shared_admin")  <- shared_admin
            class(base) <- c("combo_regimen_daily_detail", "data.frame")
            return(base)
        }

        # Aggregate to cycle level
        cyc_agg <- resolve_cycles(n_cycles, model_cycle_length, cycle_times)
        agg_cycle_starts <- cyc_agg$cycle_starts
        agg_cycle_ends   <- cyc_agg$cycle_ends
        n_agg            <- cyc_agg$n_cycles

        cycle_result <- aggregate_shared_daily_to_cycle(
            base, drug_names, shared_admin,
            agg_cycle_starts, agg_cycle_ends
        )
        attr(cycle_result, "drug_names")   <- drug_names
        attr(cycle_result, "shared_admin") <- shared_admin
        class(cycle_result) <- c("combo_regimen_costs", class(cycle_result))
        return(cycle_result)
    }

    # daily_detail = TRUE: return wide-format data.frame
    if (daily_detail) {
        base <- results[[1]][, c("day", "model_cycle")]
        drug_columns <- list()

        for (i in seq_along(results)) {
            res <- results[[i]]
            nm <- gsub("[^a-zA-Z0-9]", "_", drug_names[i])
            per_drug_cols <- setdiff(names(res), c("day", "model_cycle"))
            prefixed <- paste0(nm, "_", per_drug_cols)
            drug_columns[[drug_names[i]]] <- prefixed
            for (j in seq_along(per_drug_cols)) {
                base[[prefixed[j]]] <- res[[per_drug_cols[j]]]
            }
        }

        tc_cols <- grep("_total_cost$", names(base), value = TRUE)
        base$total_cost <- rowSums(base[, tc_cols, drop = FALSE])

        attr(base, "drug_names") <- drug_names
        attr(base, "drug_columns") <- drug_columns
        class(base) <- c("combo_regimen_daily_detail", "data.frame")
        return(base)
    }

    # Aggregate into wide-format data.frame
    base <- results[[1]][, c("cycle", "cycle_start_day", "cycle_end_day")]

    for (i in seq_along(results)) {
        res <- results[[i]]
        nm  <- gsub("[^a-zA-Z0-9]", "_", drug_names[i])
        base[[paste0(nm, "_drug_cost")]]  <- res$drug_cost
        base[[paste0(nm, "_admin_cost")]] <- res$admin_cost
        base[[paste0(nm, "_total_cost")]] <- res$total_cost
    }

    cost_cols <- grep("_total_cost$", names(base), value = TRUE)
    base$total_cost <- rowSums(base[, cost_cols, drop = FALSE])

    attr(base, "drug_names") <- drug_names
    class(base) <- c("combo_regimen_costs", class(base))
    base
}


#' Internal helper: calculate costs for a single regimen
#'
#' @param regimen A \code{med_regimen} object.
#' @param model_cycle_length Numeric model cycle length in days.
#' @param n_cycles Integer number of model cycles.
#' @param cycle_times Numeric vector of cycle start times.
#' @param subcycle_precision Logical.
#' @param daily_detail Logical.
#' @param simple_daily_cost Logical.
#' @return A data.frame.
#' @keywords internal
calculate_single_regimen <- function(
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
    cyc <- resolve_cycles(n_cycles, model_cycle_length, cycle_times)
    n_cycles     <- cyc$n_cycles
    cycle_starts <- cyc$cycle_starts
    cycle_ends   <- cyc$cycle_ends

    start_day <- regimen$start_day %||% 0

    # ── MODE 1: Simple cost-per-day ──────────────────────────────────────
    if (simple_daily_cost && !is.null(regimen$cost_per_day)) {
        daily_cost <- regimen$cost_per_day
        cycle_lengths <- cycle_ends - cycle_starts
        drug_cost_per_cycle <- daily_cost * cycle_lengths
        daily_admin_cost <- regimen$admin_cost / regimen$med_cycle_length
        admin_cost_per_cycle <- daily_admin_cost * cycle_lengths
        out <- data.frame(
            cycle             = seq_len(n_cycles),
            cycle_start_day   = cycle_starts,
            cycle_end_day     = cycle_ends,
            drug_cost         = drug_cost_per_cycle,
            admin_cost        = admin_cost_per_cycle,
            total_cost        = drug_cost_per_cycle + admin_cost_per_cycle
        )
        # Zero out cycles entirely before start_day
        before <- cycle_ends <= start_day
        out$drug_cost[before]  <- 0
        out$admin_cost[before] <- 0
        out$total_cost[before] <- 0
        if (is.finite(regimen$max_med_cycles)) {
            max_day <- start_day + regimen$max_med_cycles * regimen$med_cycle_length
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
        treat_end <- start_day + regimen$max_med_cycles * regimen$med_cycle_length
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
    start_day <- regimen$start_day %||% 0

    if (subcycle_precision) {
        horizon <- max(cycle_ends)
        eff_treat_end <- if (is.finite(treat_end)) treat_end else horizon

        treat_duration <- max(0, eff_treat_end - start_day)
        n_med_cycles <- ceiling(treat_duration / mcl)
        dispensing_days <- start_day + (seq_len(n_med_cycles) - 1L) * mcl

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

        k_lo <- ceiling((cycle_starts - start_day) / mcl)
        k_hi <- ceiling((effective_end - start_day) / mcl) - 1L
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
    tabs_per_dispensing <- tabs_per_admin * length(regimen$admin_days)

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
    start_day <- regimen$start_day %||% 0
    mc_starts <- start_day + (seq_len(n_med_cycles) - 1L) * mcl
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

    n_admin_per_cyc  <- length(regimen$admin_days)
    tabs_per_disp    <- tabs_per_admin * n_admin_per_cyc
    mg_disp_per_disp <- mg_dispensed_per_admin * n_admin_per_cyc

    all_days <- seq(0L, horizon - 1L)
    day_model_cycle <- pmin(
        findInterval(all_days, cycle_starts), n_cycles)
    days_since_start  <- all_days - start_day
    day_med_cycle     <- floor(days_since_start / mcl) + 1L
    day_med_cycle_day <- (days_since_start %% mcl) + 1L

    is_disp  <- all_days %in% dispensing_days
    is_admin <- all_days %in% all_admin
    on_tx    <- all_days >= start_day & all_days < eff_treat_end

    doses_taken <- as.integer(is_admin & on_tx)
    mg_taken    <- doses_taken * regimen$actual_dose_mg
    mg_dispensed_today <- ifelse(is_disp & on_tx, mg_disp_per_disp, 0)
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
        doses_taken     = doses_taken,
        tabs_taken      = tabs_taken,
        mg_taken        = mg_taken,
        doses_dispensed = doses_costed,
        tabs_dispensed  = tabs_costed,
        mg_dispensed    = mg_costed,
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
    start_day <- regimen$start_day %||% 0

    if (subcycle_precision) {
        horizon <- max(cycle_ends)
        eff_treat_end <- if (is.finite(treat_end)) treat_end else horizon

        treat_duration <- max(0, min(eff_treat_end, horizon) - start_day)
        n_full_med_cycles <- ceiling(treat_duration /
                                     regimen$med_cycle_length)
        med_cycle_offsets <- start_day + (seq_len(n_full_med_cycles) - 1) *
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

        # For oral drugs, compute dispensing counts for admin_cost
        if (regimen$route == "oral") {
            dispensing_days <- med_cycle_offsets[
                med_cycle_offsets < eff_treat_end &
                med_cycle_offsets < horizon
            ]
            disp_cycle_idx <- findInterval(dispensing_days, cycle_starts,
                                           rightmost.closed = FALSE)
            disp_cycle_idx <- pmin(disp_cycle_idx, n_cycles)
            disp_counts <- tabulate(disp_cycle_idx, nbins = n_cycles)
        }

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

        effective_end <- pmin(cycle_ends, treat_end)
        # Matrix: rows=cycles, cols=admin_days
        a_matrix <- matrix(adays, nrow = n_cycles, ncol = length(adays), byrow = TRUE)
        k_lo <- ceiling((cycle_starts - start_day - a_matrix) / mcl)
        k_hi <- ceiling((effective_end - start_day - a_matrix) / mcl) - 1L
        k_lo <- pmax(k_lo, 0L)
        counts <- pmax(k_hi - k_lo + 1L, 0L)
        admin_counts <- rowSums(counts)

        # For oral drugs, compute dispensing counts for admin_cost
        if (regimen$route == "oral") {
            mcl_d <- regimen$med_cycle_length
            k_lo_d <- ceiling((cycle_starts - start_day) / mcl_d)
            k_hi_d <- ceiling((effective_end - start_day) / mcl_d) - 1L
            k_lo_d <- pmax(k_lo_d, 0L)
            disp_counts <- pmax(k_hi_d - k_lo_d + 1L, 0L)
        }
    }

    # Compute admin_cost based on route
    if (regimen$route == "oral") {
        admin_cost_vec <- disp_counts * single_adm_cost
    } else {
        admin_cost_vec <- admin_counts * single_adm_cost
    }

    data.frame(
        cycle             = seq_len(n_cycles),
        cycle_start_day   = cycle_starts,
        cycle_end_day     = cycle_ends,
        n_administrations = admin_counts,
        drug_cost         = admin_counts * single_drug_cost,
        admin_cost        = admin_cost_vec,
        total_cost        = admin_counts * single_drug_cost + admin_cost_vec
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
    start_day <- regimen$start_day %||% 0
    all_days <- seq(0L, horizon - 1L)
    day_model_cycle <- pmin(
        findInterval(all_days, cycle_starts), n_cycles)
    days_since_start  <- all_days - start_day
    day_med_cycle     <- floor(days_since_start / regimen$med_cycle_length) + 1L
    day_med_cycle_day <- (days_since_start %% regimen$med_cycle_length) + 1L

    is_admin     <- all_days %in% all_admin_days
    on_tx        <- all_days >= start_day & all_days < eff_treat_end

    doses_taken <- as.integer(is_admin & on_tx)

    if (regimen$route %in% VIAL_ROUTES) {
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
        is_disp <- day_med_cycle_day == 1L & on_tx
        admin_cost_daily <- ifelse(is_disp, single_adm_cost, 0)

        data.frame(
            day             = all_days,
            model_cycle     = day_model_cycle,
            med_cycle       = ifelse(on_tx, day_med_cycle, NA_integer_),
            med_cycle_day   = ifelse(on_tx, day_med_cycle_day, NA_integer_),
            doses_taken     = doses_taken,
            mg_taken        = doses_taken * regimen$actual_dose_mg,
            drug_cost       = doses_taken * single_drug_cost,
            admin_cost      = admin_cost_daily,
            total_cost      = doses_taken * single_drug_cost +
                              admin_cost_daily,
            stringsAsFactors = FALSE
        )
    }
}


#' Validate and normalise the shared_admin parameter
#'
#' @param shared_admin NULL, TRUE, FALSE, or a list of character vectors.
#' @param drug_names Character vector of drug names from the regimens.
#' @return NULL (no sharing) or a list of character vectors (sharing groups).
#' @keywords internal
validate_shared_admin <- function(shared_admin, drug_names) {
    if (is.null(shared_admin) || identical(shared_admin, FALSE)) {
        return(NULL)
    }
    if (identical(shared_admin, TRUE)) {
        if (length(drug_names) < 2L) {
            return(NULL)
        }
        return(list(drug_names))
    }
    if (!is.list(shared_admin)) {
        stop("`shared_admin` must be NULL, TRUE, FALSE, or a list of ",
             "character vectors.", call. = FALSE)
    }
    all_grouped <- unlist(shared_admin)
    if (any(duplicated(all_grouped))) {
        dups <- unique(all_grouped[duplicated(all_grouped)])
        stop("Drug(s) appear in multiple shared_admin groups: ",
             paste(dups, collapse = ", "), call. = FALSE)
    }
    unknown <- setdiff(all_grouped, drug_names)
    if (length(unknown) > 0L) {
        stop("Unknown drug name(s) in shared_admin: ",
             paste(unknown, collapse = ", "), call. = FALSE)
    }
    for (grp in shared_admin) {
        if (length(grp) < 2L) {
            stop("Each shared_admin group must contain at least 2 drugs.",
                 call. = FALSE)
        }
    }
    shared_admin
}


#' Apply administration cost sharing within groups
#'
#' Operates on a wide-format daily data frame. For each sharing group, on days
#' where more than one drug has admin_cost > 0, keeps only the highest cost
#' (first by input order on ties) and zeros out the rest.
#'
#' @param daily Wide-format daily data frame.
#' @param sharing_groups List of character vectors (drug names).
#' @param drug_names Character vector of all drug names (in input order).
#' @return The modified daily data frame with pre_sharing_admin_cost and
#'   adjusted admin_cost columns.
#' @keywords internal
apply_admin_sharing <- function(daily, sharing_groups, drug_names) {
    # For every drug in a sharing group, copy admin_cost → pre_sharing_admin_cost
    grouped_drugs <- unlist(sharing_groups)
    for (drug in grouped_drugs) {
        nm <- gsub("[^a-zA-Z0-9]", "_", drug)
        admin_col <- paste0(nm, "_admin_cost")
        pre_col   <- paste0(nm, "_pre_sharing_admin_cost")
        daily[[pre_col]] <- daily[[admin_col]]
    }

    # For each sharing group, zero out non-winners
    for (grp in sharing_groups) {
        sanitized <- gsub("[^a-zA-Z0-9]", "_", grp)
        admin_cols <- paste0(sanitized, "_admin_cost")

        # Build matrix of admin costs for this group
        admin_mat <- as.matrix(daily[, admin_cols, drop = FALSE])
        has_admin <- admin_mat > 0
        n_with_admin <- rowSums(has_admin)

        # Rows where sharing applies (>1 drug with admin cost)
        sharing_rows <- which(n_with_admin > 1L)

        if (length(sharing_rows) > 0L) {
            sub_mat <- admin_mat[sharing_rows, , drop = FALSE]
            # Winner = column with max cost; ties broken by first (input order)
            winner_col <- max.col(sub_mat, ties.method = "first")

            # Zero out all, then restore winner
            for (j in seq_along(admin_cols)) {
                daily[sharing_rows, admin_cols[j]] <- 0
            }
            for (idx in seq_along(sharing_rows)) {
                row_i <- sharing_rows[idx]
                col_j <- winner_col[idx]
                daily[row_i, admin_cols[col_j]] <- sub_mat[idx, col_j]
            }
        }
    }

    # Recompute per-drug total_cost and combined total_cost
    for (drug in drug_names) {
        nm <- gsub("[^a-zA-Z0-9]", "_", drug)
        drug_cost_col  <- paste0(nm, "_drug_cost")
        admin_cost_col <- paste0(nm, "_admin_cost")
        total_cost_col <- paste0(nm, "_total_cost")
        daily[[total_cost_col]] <- daily[[drug_cost_col]] + daily[[admin_cost_col]]
    }
    tc_cols <- paste0(gsub("[^a-zA-Z0-9]", "_", drug_names), "_total_cost")
    daily$total_cost <- rowSums(daily[, tc_cols, drop = FALSE])

    daily
}


#' Aggregate shared daily data to cycle level
#'
#' Groups daily post-sharing data by model_cycle and sums cost columns.
#'
#' @param daily Wide-format daily data frame (post-sharing).
#' @param drug_names Character vector of all drug names.
#' @param sharing_groups List of character vectors (drug names in sharing groups).
#' @param cycle_starts Numeric vector of cycle start days.
#' @param cycle_ends Numeric vector of cycle end days.
#' @return A cycle-level data frame.
#' @keywords internal
aggregate_shared_daily_to_cycle <- function(daily, drug_names, sharing_groups,
                                            cycle_starts, cycle_ends) {
    n_cycles <- length(cycle_starts)
    grouped_drugs <- unlist(sharing_groups)

    base <- data.frame(
        cycle           = seq_len(n_cycles),
        cycle_start_day = cycle_starts,
        cycle_end_day   = cycle_ends
    )

    for (drug in drug_names) {
        nm <- gsub("[^a-zA-Z0-9]", "_", drug)
        drug_cost_col  <- paste0(nm, "_drug_cost")
        admin_cost_col <- paste0(nm, "_admin_cost")
        total_cost_col <- paste0(nm, "_total_cost")

        base[[drug_cost_col]]  <- as.numeric(tapply(daily[[drug_cost_col]],
                                         daily$model_cycle, sum))
        base[[admin_cost_col]] <- as.numeric(tapply(daily[[admin_cost_col]],
                                         daily$model_cycle, sum))

        if (drug %in% grouped_drugs) {
            pre_col <- paste0(nm, "_pre_sharing_admin_cost")
            base[[pre_col]] <- as.numeric(tapply(daily[[pre_col]],
                                      daily$model_cycle, sum))
        }

        base[[total_cost_col]] <- base[[drug_cost_col]] + base[[admin_cost_col]]
    }

    tc_cols <- paste0(gsub("[^a-zA-Z0-9]", "_", drug_names), "_total_cost")
    base$total_cost <- rowSums(base[, tc_cols, drop = FALSE])

    base
}
