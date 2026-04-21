#' Define Regimen Object(s) from Tabular Input
#'
#' Builds one or more regimen objects from a pre-filtered data frame. One row
#' defines one regimen. Multiple rows define a combination regimen.
#'
#' The table should contain only regimen-owned inputs. External context values
#' such as patient BSA or weight should be supplied through \code{...}.
#'
#' @param x A data frame with one row per regimen.
#' @param shared_admin Optional shared administration rule for combination
#'   regimens. Stored on the combination object and used by default by
#'   \code{calculate_regimen_cost()}.
#' @param ... External regimen arguments applied to every row, such as
#'   \code{patient_bsa} or \code{patient_weight}.
#'
#' @return A \code{med_regimen} for one row, or a \code{med_regimen_combo} for
#'   multiple rows.
#' @export
define_regimen_table <- function(x, shared_admin = NULL, ...) {
    if (!is.data.frame(x)) {
        stop("x must be a data.frame.", call. = FALSE)
    }
    if (nrow(x) == 0L) {
        stop("x must contain at least one regimen row.", call. = FALSE)
    }

    context_args <- list(...)
    regimens <- lapply(seq_len(nrow(x)), function(i) {
        row_args <- normalize_regimen_table_row(x[i, , drop = FALSE], i)
        do.call(define_regimen, c(row_args, context_args))
    })

    if (length(regimens) == 1L) {
        return(regimens[[1]])
    }

    structure(
        list(
            regimens = regimens,
            shared_admin = shared_admin
        ),
        class = "med_regimen_combo"
    )
}

#' Calculate a Regimen Cost Vector
#'
#' Calculates a cycle-aligned cost vector from a regimen object for use in an
#' \code{openqaly} model.
#'
#' @param x A \code{med_regimen} or \code{med_regimen_combo}.
#' @param type One of \code{"medication"}, \code{"administration"}, or
#'   \code{"total"}.
#' @param model_cycle_length Numeric model cycle length in days.
#' @param n_cycles Integer number of model cycles.
#' @param cycle_times Optional numeric vector of cycle start times.
#' @param shared_admin Optional override for combination admin sharing.
#' @param subcycle_precision Logical. Passed to \code{calculate_med_costs()}.
#' @param simple_daily_cost Logical. Passed to \code{calculate_med_costs()}.
#' @param time Optional numeric vector of day values used to expand the
#'   cost vector. When supplied, day values are converted to cycle indices
#'   via \code{round(time / model_cycle_length)} and the returned vector
#'   has \code{length(time)} elements obtained by indexing into the
#'   per-cycle cost vector. Indices outside the valid range (less than 1
#'   or greater than \code{n_cycles}) are mapped to zero. This is useful
#'   when a model namespace has more rows than cycles (e.g. tunnel states)
#'   and the cost vector must match \code{nrow(ns$df)}.
#'
#' @return Numeric vector of cost by cycle when \code{time} is \code{NULL},
#'   or a vector of \code{length(time)} when \code{time} is supplied.
#' @export
calculate_regimen_cost <- function(
    x,
    type = c("medication", "administration", "total"),
    model_cycle_length = cycle_length_days,
    n_cycles = max(cycle),
    cycle_times = NULL,
    shared_admin = NULL,
    subcycle_precision = FALSE,
    simple_daily_cost = FALSE,
    time = NULL
) {
    type <- match.arg(type)

    if (inherits(x, "med_regimen_combo")) {
        regimens <- x$regimens
        effective_shared_admin <- if (!is.null(shared_admin)) {
            shared_admin
        } else {
            x$shared_admin
        }
    } else if (inherits(x, "med_regimen")) {
        regimens <- list(x)
        effective_shared_admin <- NULL
    } else {
        stop("x must be a med_regimen or med_regimen_combo object.", call. = FALSE)
    }

    costs <- do.call(
        calculate_med_costs,
        c(
            regimens,
            list(
                model_cycle_length = model_cycle_length,
                n_cycles = as.integer(n_cycles),
                cycle_times = cycle_times,
                subcycle_precision = subcycle_precision,
                simple_daily_cost = simple_daily_cost,
                shared_admin = effective_shared_admin
            )
        )
    )

    if (type == "total") {
        cost_vector <- costs$total_cost
    } else if (type == "medication") {
        if ("drug_cost" %in% names(costs)) {
            cost_vector <- costs$drug_cost
        } else {
            drug_cols <- grep("_drug_cost$", names(costs), value = TRUE)
            cost_vector <- rowSums(costs[, drug_cols, drop = FALSE])
        }
    } else {
        if ("admin_cost" %in% names(costs)) {
            cost_vector <- costs$admin_cost
        } else {
            admin_cols <- grep("_admin_cost$", names(costs), value = TRUE)
            admin_cols <- admin_cols[!grepl("_pre_sharing_admin_cost$", admin_cols)]
            cost_vector <- rowSums(costs[, admin_cols, drop = FALSE])
        }
    }

    if (!is.null(time)) {
        time_idx <- as.integer(round(as.numeric(time) / model_cycle_length))
        expanded <- numeric(length(time_idx))
        valid <- time_idx >= 1L & time_idx <= length(cost_vector)
        expanded[valid] <- cost_vector[time_idx[valid]]
        return(expanded)
    }
    cost_vector
}

#' @export
print.med_regimen_combo <- function(x, ...) {
    cat("Combination regimen with ", length(x$regimens), " components:\n", sep = "")
    for (reg in x$regimens) {
        cat("  - ", reg$name, "\n", sep = "")
    }
    invisible(x)
}

required_regimen_table_cols <- c(
    "name", "route", "dose", "dose_basis", "med_cycle_length", "admin_days"
)

forbidden_regimen_table_cols <- c(
    "patient_weight", "patient_bsa", "patient_weight_sd", "patient_bsa_sd",
    "n_admin_per_cycle"
)

normalize_regimen_table_row <- function(row, row_index) {
    cols <- names(row)
    missing_cols <- setdiff(required_regimen_table_cols, cols)
    if (length(missing_cols) > 0L) {
        stop("Regimen table row ", row_index,
             " is missing required columns: ",
             paste(missing_cols, collapse = ", "), ".",
             call. = FALSE)
    }

    bad_cols <- intersect(forbidden_regimen_table_cols, cols)
    if (length(bad_cols) > 0L) {
        stop("Regimen table row ", row_index,
             " contains non-regimen context column(s): ",
             paste(bad_cols, collapse = ", "), ".",
             call. = FALSE)
    }

    row_list <- as.list(row[1, , drop = FALSE])

    args <- list(
        name = as.character(row_list$name),
        route = as.character(row_list$route),
        dose_per_admin = as.numeric(row_list$dose),
        dose_basis = as.character(row_list$dose_basis),
        med_cycle_length = as.numeric(row_list$med_cycle_length),
        admin_days = parse_admin_days_spec(row_list$admin_days, row_index)
    )

    optional_map <- list(
        max_cycles = "max_med_cycles",
        start_day = "start_day",
        unit_cost = "unit_cost",
        units_per_pack = "units_per_pack",
        tablet_strength = "tablet_strength",
        available_strengths = "available_strengths",
        oral_wastage = "oral_wastage",
        vial_size = "vial_size",
        vial_sizes = "vial_sizes",
        vial_cost = "vial_cost",
        wastage_threshold = "wastage_threshold",
        admin_cost = "admin_cost",
        cost_per_day = "cost_per_day"
    )

    for (col_name in names(optional_map)) {
        if (!col_name %in% cols) next
        value <- row_list[[col_name]]
        if (is_missing_regimen_value(value)) next

        parsed <- switch(
            col_name,
            available_strengths = parse_numeric_csv(value, col_name, row_index),
            vial_sizes = parse_numeric_csv(value, col_name, row_index),
            vial_cost = parse_numeric_csv(value, col_name, row_index),
            oral_wastage = parse_regimen_logical(value, col_name, row_index),
            as.numeric(value)
        )

        args[[optional_map[[col_name]]]] <- parsed
    }

    args
}

is_missing_regimen_value <- function(x) {
    if (is.null(x) || length(x) == 0L) {
        return(TRUE)
    }
    if (length(x) == 1L && is.character(x) && trimws(x) == "") {
        return(TRUE)
    }
    all(is.na(x))
}

parse_numeric_csv <- function(x, col_name, row_index) {
    if (is.numeric(x)) {
        return(as.numeric(x))
    }
    parts <- trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1]])
    parts <- parts[nzchar(parts)]
    nums <- suppressWarnings(as.numeric(parts))
    if (length(parts) == 0L || any(is.na(nums))) {
        stop("Regimen table row ", row_index, " has invalid numeric list in ",
             col_name, ".", call. = FALSE)
    }
    nums
}

parse_regimen_logical <- function(x, col_name, row_index) {
    if (is.logical(x)) {
        return(as.logical(x)[1])
    }
    value <- tolower(trimws(as.character(x)[1]))
    if (value %in% c("true", "t", "1")) return(TRUE)
    if (value %in% c("false", "f", "0")) return(FALSE)
    stop("Regimen table row ", row_index, " has invalid logical value in ",
         col_name, ".", call. = FALSE)
}

parse_admin_days_spec <- function(x, row_index = NULL) {
    if (is.numeric(x)) {
        days <- as.integer(x)
    } else {
        spec <- trimws(as.character(x)[1])
        if (spec == "" || is.na(spec)) {
            stop_admin_days_parse(spec, row_index)
        }

        tokens <- trimws(strsplit(spec, ",", fixed = TRUE)[[1]])
        days <- integer(0)
        for (token in tokens) {
            if (grepl("^[0-9]+$", token)) {
                days <- c(days, as.integer(token))
            } else if (grepl("^[0-9]+-[0-9]+$", token)) {
                bounds <- as.integer(strsplit(token, "-", fixed = TRUE)[[1]])
                if (bounds[1] > bounds[2]) {
                    stop_admin_days_parse(token, row_index)
                }
                days <- c(days, seq.int(bounds[1], bounds[2]))
            } else {
                stop_admin_days_parse(token, row_index)
            }
        }
    }

    if (length(days) == 0L || any(is.na(days)) || any(days < 1L) ||
        any(duplicated(days))) {
        stop_admin_days_parse(as.character(x)[1], row_index)
    }

    as.integer(days)
}

stop_admin_days_parse <- function(token, row_index) {
    prefix <- if (!is.null(row_index)) {
        paste0("Regimen table row ", row_index, " ")
    } else {
        ""
    }
    stop(prefix, "has invalid admin_days value: ", token, ".", call. = FALSE)
}
