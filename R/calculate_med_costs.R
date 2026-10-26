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
#' Accepts one or more \code{med_regimen} objects. With a single regimen the
#' result is a data.frame with one row per model cycle and columns
#' \code{cycle}, \code{cycle_start_day}, \code{cycle_end_day},
#' \code{n_administrations}, \code{n_dispensings}, \code{drug_cost},
#' \code{admin_cost}, \code{dispensing_cost} and \code{total_cost}. With
#' several regimens the result is wide, with per-drug columns
#' (\code{{Name}_drug_cost}, \code{{Name}_admin_cost},
#' \code{{Name}_dispensing_cost}, \code{{Name}_total_cost}) and a combined
#' \code{total_cost}.
#'
#' Drug cost is recorded per administration (\code{charge_timing =
#' "per_admin"}) or as a whole medication cycle's doses on day 1 of that cycle
#' (\code{"per_dispensing"}). \code{admin_cost} is charged on every
#' administration and \code{dispensing_cost} once per medication cycle.
#'
#' @param ...              One or more \code{med_regimen} objects (from
#'                         \code{define_regimen}).
#' @param model_cycle_length Numeric. Length of one model cycle in days.
#' @param n_cycles        Integer. Number of model cycles.
#' @param cycle_times     Numeric vector. Alternatively, supply start times
#'                        (in days) of each model cycle directly. If supplied,
#'                        \code{n_cycles} and \code{model_cycle_length} are
#'                        inferred.
#' @param daily_detail    Logical. If TRUE, returns one row per day showing
#'                        administrations, dispensings and costs.
#' @param shared_admin Controls whether drugs in a combination share fees on
#'   the same day. When several drugs in a sharing group have a fee on the
#'   same day, only the highest is charged; this applies separately to
#'   \code{admin_cost} and \code{dispensing_cost}.
#'   \itemize{
#'     \item \code{NULL} (default): no sharing.
#'     \item \code{TRUE}: all drugs in one sharing group.
#'     \item A list of character vectors: explicit groups, e.g.
#'       \code{list(c("Drug A", "Drug B"))}. Drug names must match the
#'       \code{name} parameter from \code{define_regimen()}.
#'   }
#'   When active, adds \code{{Drug}_pre_sharing_admin_cost} and
#'   \code{{Drug}_pre_sharing_dispensing_cost} diagnostic columns.
#' @return See description.
#'
#' @export
calculate_med_costs <- function(
    ...,
    model_cycle_length = 7,
    n_cycles           = 52,
    cycle_times        = NULL,
    daily_detail       = FALSE,
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

    cyc <- resolve_cycles(n_cycles, model_cycle_length, cycle_times)

    # ── Single regimen ─────────────────────────────────────────────────
    if (length(regimens) == 1L) {
        if (!is.null(shared_admin) && !identical(shared_admin, FALSE)) {
            warning("`shared_admin` is ignored for single-regimen calls.",
                    call. = FALSE)
        }
        daily <- regimen_daily(regimens[[1]], cyc)
        if (daily_detail) {
            attr(daily, "n_dispensings") <- NULL
            class(daily) <- c("regimen_daily_detail", class(daily))
            return(daily)
        }
        result <- aggregate_daily(daily, cyc)
        class(result) <- c("regimen_costs", class(result))
        return(result)
    }

    # ── Multiple regimens ──────────────────────────────────────────────
    drug_names <- vapply(regimens, function(r) r$name, character(1))
    if (anyDuplicated(drug_names)) {
        stop("Regimens in one call must have distinct names.", call. = FALSE)
    }
    shared_admin <- validate_shared_admin(shared_admin, drug_names)
    # Column prefixes: cleaned names, made unique in input order
    prefix <- stats::setNames(make.unique(safe_name(drug_names), sep = "_"),
                              drug_names)

    dailies <- lapply(regimens, regimen_daily, cyc = cyc)
    names(dailies) <- drug_names

    base <- dailies[[1]][, c("day", "model_cycle")]
    drug_columns <- list()
    for (i in seq_along(dailies)) {
        res <- dailies[[i]]
        nm <- prefix[[drug_names[i]]]
        per_drug_cols <- setdiff(names(res), c("day", "model_cycle"))
        prefixed <- paste0(nm, "_", per_drug_cols)
        drug_columns[[drug_names[i]]] <- prefixed
        for (j in seq_along(per_drug_cols)) {
            base[[prefixed[j]]] <- res[[per_drug_cols[j]]]
        }
    }

    if (!is.null(shared_admin)) {
        base <- apply_admin_sharing(base, shared_admin, drug_names, prefix)
        for (drug in unlist(shared_admin)) {
            nm <- prefix[[drug]]
            for (fee in c("admin_cost", "dispensing_cost")) {
                pre_col <- paste0(nm, "_pre_sharing_", fee)
                cols <- drug_columns[[drug]]
                idx <- which(cols == paste0(nm, "_", fee))
                drug_columns[[drug]] <- append(cols, pre_col, after = idx - 1L)
            }
        }
    }
    tc_cols <- paste0(prefix, "_total_cost")
    base$total_cost <- rowSums(base[, tc_cols, drop = FALSE])
    base <- base[, c("day", "model_cycle",
                     unlist(drug_columns, use.names = FALSE), "total_cost")]

    if (daily_detail) {
        attr(base, "drug_names")   <- drug_names
        attr(base, "drug_columns") <- drug_columns
        attr(base, "shared_admin") <- shared_admin
        class(base) <- c("combo_regimen_daily_detail", "data.frame")
        return(base)
    }

    out <- data.frame(
        cycle           = seq_len(cyc$n_cycles),
        cycle_start_day = cyc$cycle_starts,
        cycle_end_day   = cyc$cycle_ends
    )
    cycle_columns <- list()
    for (drug in drug_names) {
        nm <- prefix[[drug]]
        cost_cols <- c("drug_cost", "admin_cost", "dispensing_cost")
        if (drug %in% unlist(shared_admin)) {
            cost_cols <- c("drug_cost", "pre_sharing_admin_cost", "admin_cost",
                           "pre_sharing_dispensing_cost", "dispensing_cost")
        }
        cols <- paste0(nm, "_", c(cost_cols, "total_cost"))
        cycle_columns[[drug]] <- cols
        for (col in cols) {
            out[[col]] <- sum_by_cycle(base[[col]], base$model_cycle,
                                       cyc$n_cycles)
        }
    }
    out$total_cost <- rowSums(out[, tc_cols, drop = FALSE])

    attr(out, "drug_names")   <- drug_names
    attr(out, "drug_columns") <- cycle_columns
    attr(out, "shared_admin") <- shared_admin
    class(out) <- c("combo_regimen_costs", class(out))
    out
}


# Column-safe version of a drug name.
safe_name <- function(x) gsub("[^a-zA-Z0-9]", "_", x)

# Sum a row vector into model cycles (rows outside the model are dropped).
sum_by_cycle <- function(values, model_cycle, n_cycles) {
    keep <- !is.na(model_cycle)
    out <- numeric(n_cycles)
    s <- tapply(values[keep], factor(model_cycle[keep], levels = seq_len(n_cycles)),
                sum)
    s[is.na(s)] <- 0
    out[] <- as.numeric(s)
    out
}

# Which interval [starts[i], starts[i + 1]) contains each point (NA before
# the first start). A small tolerance keeps a point computed as a boundary
# (e.g. 2 x 30.4375) from falling just before it.
interval_of <- function(t, starts) {
    i <- findInterval(t + 1e-9 * pmax(1, abs(t)), starts)
    i[i < 1L] <- NA_integer_
    i
}


#' Treatment events of a regimen
#'
#' Every dispensing (day 1 of a medication cycle) and every administration
#' is a point in continuous time. Medication cycle k starts at
#' \code{start_day + (k - 1) * med_cycle_length}; administration day d of that
#' cycle is at the cycle start + (d - 1). Events at or after the end of
#' treatment or the model horizon are excluded.
#'
#' @param reg A med_regimen object.
#' @param horizon Model horizon (day).
#' @return list(dispensing = data.frame(time, med_cycle),
#'   admin = data.frame(time, med_cycle, med_cycle_day), treat_end)
#' @keywords internal
regimen_events <- function(reg, horizon) {
    mcl <- reg$med_cycle_length
    start_day <- reg$start_day %||% 0
    treat_end <- if (is.finite(reg$max_med_cycles)) {
        start_day + reg$max_med_cycles * mcl
    } else Inf
    eff_end <- min(treat_end, horizon)
    n_med <- max(0, ceiling((eff_end - start_day) / mcl))
    k <- seq_len(n_med)
    starts <- start_day + (k - 1) * mcl
    keep <- starts < eff_end
    disp <- data.frame(time = starts[keep], med_cycle = k[keep])
    ad <- reg$admin_days
    admin <- data.frame(
        time = as.vector(outer(ad - 1, disp$time, "+")),
        med_cycle = rep(disp$med_cycle, each = length(ad)),
        med_cycle_day = rep(ad, times = nrow(disp))
    )
    admin <- admin[admin$time < eff_end, , drop = FALSE]
    admin <- admin[order(admin$time), , drop = FALSE]
    rownames(admin) <- NULL
    list(dispensing = disp, admin = admin, treat_end = treat_end)
}


#' Day intervals of the model timeline
#'
#' One row per day interval [d, d + 1) from day 0 to the model horizon, cut
#' at model-cycle boundaries, so that every row lies in exactly one model
#' cycle.
#'
#' @param cyc Output of \code{resolve_cycles()}.
#' @return data.frame(day, from, to, model_cycle)
#' @keywords internal
day_intervals <- function(cyc) {
    horizon <- max(cyc$cycle_ends)
    n_days <- max(ceiling(horizon - 1e-9 * max(1, horizon)), 1)
    bounds <- c(cyc$cycle_starts, cyc$cycle_ends)
    bounds <- bounds[bounds > 0 & bounds < horizon]
    # Drop boundaries that coincide with a whole day
    bounds <- bounds[abs(bounds - round(bounds)) > 1e-9 * pmax(1, bounds)]
    cuts <- sort(unique(c(seq.int(0, n_days - 1), bounds)))
    to <- c(cuts[-1], horizon)
    keep <- to > cuts
    cuts <- cuts[keep]
    to <- to[keep]
    data.frame(
        day = as.integer(floor(cuts + 1e-9 * pmax(1, cuts))),
        from = cuts,
        to = to,
        model_cycle = interval_of(cuts, cyc$cycle_starts)
    )
}


#' Day-level cost detail for one regimen
#'
#' One row per day interval (cut at model-cycle boundaries; see
#' \code{day_intervals()}). Each administration and dispensing is a point in
#' time and is attributed to the row whose interval contains it. Drug cost is
#' placed on administrations (\code{"per_admin"}) or on medication-cycle
#' starts (\code{"per_dispensing"}); \code{admin_cost} on administrations;
#' \code{dispensing_cost} on medication-cycle starts. Dose and unit columns
#' (\code{mg_taken}, \code{units_dispensed}, \code{mg_dispensed}) are
#' included only for a fixed dose; with a patient distribution the drug cost
#' is an expected value (see \code{diagnose_dosing()}). Under
#' \code{no_wastage}, \code{units_dispensed} is omitted and
#' \code{mg_dispensed} is the prescribed mg that is costed.
#'
#' @param reg A med_regimen object.
#' @param cyc Output of \code{resolve_cycles()}.
#' @return A data.frame, with attribute \code{n_dispensings} (dispensings
#'   per row).
#' @keywords internal
regimen_daily <- function(reg, cyc) {
    stopifnot(inherits(reg, "med_regimen"))
    rows <- day_intervals(cyc)
    horizon <- max(cyc$cycle_ends)
    ev <- regimen_events(reg, horizon)
    n_rows <- nrow(rows)

    row_of <- function(t) findInterval(t + 1e-9 * pmax(1, abs(t)), rows$from)
    admin_row <- row_of(ev$admin$time)
    disp_row <- row_of(ev$dispensing$time)
    doses <- tabulate(admin_row, nbins = n_rows)
    disp <- tabulate(disp_row, nbins = n_rows)

    # Medication cycle position of each row: that of its administration if
    # it has one, else of the treatment time at the start of the row.
    start_day <- reg$start_day %||% 0
    mcl <- reg$med_cycle_length
    t_end <- min(ev$treat_end, horizon)
    on_tx <- rows$to > start_day & rows$from < t_end
    x <- pmax(rows$from, start_day)
    since <- x - start_day
    med_cycle <- floor(since / mcl + 1e-9) + 1
    med_cycle_day <- floor(since - (med_cycle - 1) * mcl + 1e-9) + 1
    med_cycle[admin_row] <- ev$admin$med_cycle
    med_cycle_day[admin_row] <- ev$admin$med_cycle_day
    med_cycle[!on_tx] <- NA
    med_cycle_day[!on_tx] <- NA

    n_admin_cycle <- length(reg$admin_days)
    admin_fee <- doses * reg$admin_cost
    disp_fee  <- disp * reg$dispensing_cost

    per_dose <- cost_single_admin_drug(reg)
    n_dose_costed <- if (reg$charge_timing == "per_admin") {
        doses
    } else {
        disp * n_admin_cycle
    }
    drug <- n_dose_costed * per_dose
    out <- data.frame(
        day = rows$day, model_cycle = rows$model_cycle,
        med_cycle = med_cycle, med_cycle_day = med_cycle_day,
        doses_taken = doses
    )

    # Dose and unit columns only where they describe what is costed: not
    # with a patient distribution (the cost is an average over patients;
    # see diagnose_dosing()), and no units under no_wastage (the cost is a
    # price per mg).
    if (is.null(regimen_distribution(reg))) {
        detail <- cost_single_dose(reg$actual_dose_mg, reg)
        out$mg_taken <- doses * detail$given_mg
        if (reg$no_wastage) {
            out$mg_dispensed <- n_dose_costed * detail$prescribed_mg
        } else {
            out$units_dispensed <- n_dose_costed * sum(detail$units)
            out$mg_dispensed <- n_dose_costed * detail$dispensed_mg
        }
    }

    out$drug_cost <- drug
    out$admin_cost <- admin_fee
    out$dispensing_cost <- disp_fee
    out$total_cost <- drug + admin_fee + disp_fee
    attr(out, "n_dispensings") <- disp
    out
}


#' Aggregate a single regimen's daily detail into model cycles
#'
#' @param daily Output of \code{regimen_daily()}.
#' @param cyc Output of \code{resolve_cycles()}.
#' @return A data.frame.
#' @keywords internal
aggregate_daily <- function(daily, cyc) {
    n <- cyc$n_cycles
    mc <- daily$model_cycle
    data.frame(
        cycle             = seq_len(n),
        cycle_start_day   = cyc$cycle_starts,
        cycle_end_day     = cyc$cycle_ends,
        n_administrations = as.integer(sum_by_cycle(daily$doses_taken, mc, n)),
        n_dispensings     = as.integer(sum_by_cycle(
            attr(daily, "n_dispensings"), mc, n)),
        drug_cost         = sum_by_cycle(daily$drug_cost, mc, n),
        admin_cost        = sum_by_cycle(daily$admin_cost, mc, n),
        dispensing_cost   = sum_by_cycle(daily$dispensing_cost, mc, n),
        total_cost        = sum_by_cycle(daily$total_cost, mc, n)
    )
}


#' Validate and normalise the shared_admin parameter
#'
#' @param shared_admin NULL, TRUE, FALSE, or a list of character vectors.
#' @param drug_names Character vector of drug names from the regimens.
#' @return NULL (no sharing) or a list of character vectors (sharing groups),
#'   each in input order.
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
    # Ties go to the first drug in input order, so order each group by input
    lapply(shared_admin, function(grp) drug_names[drug_names %in% grp])
}


#' Apply fee sharing within groups
#'
#' Operates on a wide-format daily data frame. For each sharing group and each
#' fee (\code{admin_cost}, \code{dispensing_cost}), on rows where more than
#' one drug has that fee, keeps only the highest (first by input order on
#' ties) and zeros out the rest.
#'
#' @param daily Wide-format daily data frame.
#' @param sharing_groups List of character vectors (drug names), each in
#'   input order.
#' @param drug_names Character vector of all drug names (in input order).
#' @param prefix Named character vector: column prefix for each drug name.
#' @return The modified daily data frame with pre-sharing fee columns and
#'   adjusted fee and total columns.
#' @keywords internal
apply_admin_sharing <- function(daily, sharing_groups, drug_names, prefix) {
    fees <- c("admin_cost", "dispensing_cost")
    for (drug in unlist(sharing_groups)) {
        nm <- prefix[[drug]]
        for (fee in fees) {
            daily[[paste0(nm, "_pre_sharing_", fee)]] <- daily[[paste0(nm, "_", fee)]]
        }
    }
    for (grp in sharing_groups) {
        grp_prefix <- prefix[grp]
        for (fee in fees) {
            cols <- paste0(grp_prefix, "_", fee)
            mat <- as.matrix(daily[, cols, drop = FALSE])
            rows <- which(rowSums(mat > 0) > 1L)
            if (length(rows) == 0L) next
            sub <- mat[rows, , drop = FALSE]
            winner <- max.col(sub, ties.method = "first")
            for (j in seq_along(cols)) daily[rows, cols[j]] <- 0
            for (idx in seq_along(rows)) {
                daily[rows[idx], cols[winner[idx]]] <- sub[idx, winner[idx]]
            }
        }
    }
    for (drug in drug_names) {
        nm <- prefix[[drug]]
        daily[[paste0(nm, "_total_cost")]] <- daily[[paste0(nm, "_drug_cost")]] +
            daily[[paste0(nm, "_admin_cost")]] +
            daily[[paste0(nm, "_dispensing_cost")]]
    }
    daily
}
