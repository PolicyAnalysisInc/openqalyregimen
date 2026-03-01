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
#'
#' @export
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
    out
}
