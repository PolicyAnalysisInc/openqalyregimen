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
#' @param simple_daily_cost  Logical. If TRUE (and regimen has cost_per_day),
#'                           produces one row per day with flat daily costs.
#' @return A data.frame with one row per event (dispensing or administration).
#'
#' @export
detail_schedule <- function(
    regimen,
    model_cycle_length = 7,
    n_cycles           = 52,
    cycle_times        = NULL,
    simple_daily_cost  = FALSE
) {
    stopifnot(inherits(regimen, "med_regimen"))

    # Resolve model cycles
    cyc <- resolve_cycles(n_cycles, model_cycle_length, cycle_times)
    n_cycles     <- cyc$n_cycles
    cycle_starts <- cyc$cycle_starts
    cycle_ends   <- cyc$cycle_ends
    horizon    <- max(cycle_ends)

    mcl       <- regimen$med_cycle_length
    start_day <- regimen$start_day %||% 0

    # Treatment cap
    if (is.finite(regimen$max_med_cycles)) {
        treat_end <- start_day + regimen$max_med_cycles * mcl
    } else {
        treat_end <- horizon
    }
    eff_end <- min(treat_end, horizon)

    # ── Simple daily cost mode ──────────────────────────────────────────
    if (simple_daily_cost && !is.null(regimen$cost_per_day)) {
        daily_drug  <- regimen$cost_per_day
        daily_admin <- regimen$admin_cost / mcl
        all_days    <- seq(from = start_day, to = eff_end - 1)
        all_days    <- all_days[all_days < horizon]
        if (length(all_days) == 0L) {
            out <- data.frame(
                day = integer(0), med_cycle = integer(0),
                med_cycle_day = integer(0), model_cycle = integer(0),
                event_type = character(0), drug_cost = numeric(0),
                admin_cost = numeric(0), total_cost = numeric(0),
                stringsAsFactors = FALSE
            )
            class(out) <- c("regimen_schedule", class(out))
            return(out)
        }
        med_cycle_vals <- ((all_days - start_day) %/% mcl) + 1L
        med_cycle_day_vals <- ((all_days - start_day) %% mcl) + 1L
        model_cycle_vals <- pmin(findInterval(all_days, cycle_starts), n_cycles)
        out <- data.frame(
            day           = all_days,
            med_cycle     = med_cycle_vals,
            med_cycle_day = med_cycle_day_vals,
            model_cycle   = model_cycle_vals,
            event_type    = "daily",
            drug_cost     = daily_drug,
            admin_cost    = daily_admin,
            total_cost    = daily_drug + daily_admin,
            stringsAsFactors = FALSE
        )
        rownames(out) <- NULL
        class(out) <- c("regimen_schedule", class(out))
        return(out)
    }

    # Enumerate all med cycles
    treat_duration  <- max(0, eff_end - start_day)
    n_med_cycles    <- ceiling(treat_duration / mcl)
    med_cycle_starts <- start_day + (seq_len(n_med_cycles) - 1L) * mcl
    med_cycle_starts <- med_cycle_starts[med_cycle_starts < eff_end]

    # Pre-compute costs
    single_drug  <- cost_single_admin_drug(regimen)
    single_admin <- regimen$admin_cost

    if (regimen$oral_wastage) {
        disp_cost <- cost_oral_cycle_dispensing(regimen)
    }

    # ── Administration events (vectorized) ────────────────────────────
    admin_day_matrix <- outer(regimen$admin_days - 1L, med_cycle_starts, "+")
    abs_days         <- as.vector(admin_day_matrix)
    med_cycle_day_vals <- rep(regimen$admin_days, times = length(med_cycle_starts))
    med_cycle_vals     <- rep(seq_along(med_cycle_starts),
                              each = length(regimen$admin_days))

    keep <- abs_days < eff_end
    abs_days           <- abs_days[keep]
    med_cycle_day_vals <- med_cycle_day_vals[keep]
    med_cycle_vals     <- med_cycle_vals[keep]

    model_cycle_vals <- pmin(findInterval(abs_days, cycle_starts), n_cycles)

    if (regimen$oral_wastage) {
        admin_df <- data.frame(
            day            = abs_days,
            med_cycle      = med_cycle_vals,
            med_cycle_day  = med_cycle_day_vals,
            model_cycle    = model_cycle_vals,
            event_type     = "administration",
            drug_cost      = 0,
            admin_cost     = 0,
            total_cost     = 0,
            stringsAsFactors = FALSE
        )
    } else {
        admin_df <- data.frame(
            day            = abs_days,
            med_cycle      = med_cycle_vals,
            med_cycle_day  = med_cycle_day_vals,
            model_cycle    = model_cycle_vals,
            event_type     = "administration",
            drug_cost      = single_drug,
            admin_cost     = single_admin,
            total_cost     = single_drug + single_admin,
            stringsAsFactors = FALSE
        )
    }

    # ── Dispensing events (oral wastage only) ──────────────────────────
    if (regimen$oral_wastage) {
        disp_model_cycle <- pmin(
            findInterval(med_cycle_starts, cycle_starts), n_cycles)
        disp_df <- data.frame(
            day            = med_cycle_starts,
            med_cycle      = seq_along(med_cycle_starts),
            med_cycle_day  = 1L,
            model_cycle    = disp_model_cycle,
            event_type     = "dispensing",
            drug_cost      = disp_cost,
            admin_cost     = single_admin,
            total_cost     = disp_cost + single_admin,
            stringsAsFactors = FALSE
        )
        out <- rbind(disp_df, admin_df)
        out <- out[order(out$day, out$event_type == "administration"), ]
    } else {
        out <- admin_df[order(admin_df$day), ]
    }

    rownames(out) <- NULL
    class(out) <- c("regimen_schedule", class(out))
    out
}
