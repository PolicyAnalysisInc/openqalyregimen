#' Generate a detailed day-level event schedule
#'
#' Shows every dispensing and administration event, which medication cycle
#' and model cycle it falls in, and the associated costs. This is the
#' "unrolled" view before aggregation to model cycles. Costs follow the same
#' rules as \code{calculate_med_costs()}.
#'
#' @param regimen            A \code{med_regimen} object.
#' @param model_cycle_length Numeric. Model cycle length in days.
#' @param n_cycles           Integer. Number of model cycles.
#' @param cycle_times        Numeric vector. Custom cycle start times.
#' @return A data.frame with one row per event: \code{"dispensing"} (day 1 of
#'   each medication cycle, when drug is costed per dispensing or a
#'   dispensing fee applies) or \code{"administration"}. \code{day} is the
#'   event's exact time (it may be fractional, e.g. with a fractional
#'   \code{start_day}); \code{model_cycle} is the model cycle containing it.
#'   Events before the first model cycle are left out, as in
#'   \code{calculate_med_costs()}.
#'
#' @export
detail_schedule <- function(
    regimen,
    model_cycle_length = 7,
    n_cycles           = 52,
    cycle_times        = NULL
) {
    stopifnot(inherits(regimen, "med_regimen"))

    cyc <- resolve_cycles(n_cycles, model_cycle_length, cycle_times)
    ev <- regimen_events(regimen, max(cyc$cycle_ends))
    per_disp <- regimen$charge_timing == "per_dispensing"
    per_dose <- cost_single_admin_drug(regimen)
    n_admin_cycle <- length(regimen$admin_days)

    disp <- ev$dispensing
    disp_df <- data.frame(
        day = disp$time, med_cycle = disp$med_cycle,
        med_cycle_day = rep(1L, nrow(disp)),
        model_cycle = interval_of(disp$time, cyc$cycle_starts),
        event_type = rep("dispensing", nrow(disp)),
        drug_cost = if (per_disp) rep(per_dose * n_admin_cycle, nrow(disp))
                    else numeric(nrow(disp)),
        admin_cost = numeric(nrow(disp)),
        dispensing_cost = rep(regimen$dispensing_cost, nrow(disp)),
        stringsAsFactors = FALSE
    )
    if (!(per_disp || regimen$dispensing_cost > 0)) disp_df <- disp_df[0, ]

    adm <- ev$admin
    admin_df <- data.frame(
        day = adm$time, med_cycle = adm$med_cycle,
        med_cycle_day = as.integer(adm$med_cycle_day),
        model_cycle = interval_of(adm$time, cyc$cycle_starts),
        event_type = rep("administration", nrow(adm)),
        drug_cost = if (per_disp) numeric(nrow(adm))
                    else rep(per_dose, nrow(adm)),
        admin_cost = rep(regimen$admin_cost, nrow(adm)),
        dispensing_cost = numeric(nrow(adm)),
        stringsAsFactors = FALSE
    )

    out <- rbind(disp_df, admin_df)
    out$total_cost <- out$drug_cost + out$admin_cost + out$dispensing_cost
    # Events before the first model cycle are outside the model
    out <- out[!is.na(out$model_cycle), , drop = FALSE]
    out <- out[order(out$day, out$event_type == "administration"), ]
    rownames(out) <- NULL
    class(out) <- c("regimen_schedule", class(out))
    out
}
