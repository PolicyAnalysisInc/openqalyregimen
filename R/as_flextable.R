# ── Internal helpers ─────────────────────────────────────────────────────────

check_flextable_installed <- function() {
    if (!requireNamespace("flextable", quietly = TRUE)) {
        stop(
            "Package 'flextable' is required for as_flextable() methods.\n",
            "Install it with: install.packages(\"flextable\")",
            call. = FALSE
        )
    }
}

#' Map raw column names to display labels
#' @param col_names Character vector of column names.
#' @return Character vector of display labels.
#' @keywords internal
regimen_col_labels <- function(col_names) {
    label_map <- c(
        cycle           = "Cycle",
        cycle_start_day = "Start Day",
        cycle_end_day   = "End Day",
        n_administrations    = "N Admins",
        n_dispensings        = "N Dispensing",
        drug_cost       = "Drug Cost",
        admin_cost      = "Admin Cost",
        dispensing_cost = "Dispensing Cost",
        total_cost      = "Total Cost",
        day             = "Day",
        model_cycle     = "Model Cycle",
        med_cycle       = "Med Cycle",
        med_cycle_day   = "Cycle Day",
        doses_taken     = "Doses",
        mg_taken        = "mg Given",
        units_dispensed = "Units",
        mg_dispensed    = "mg Dispensed",
        event_type      = "Event Type",
        # dosing diagnostics
        bin                = "Bin",
        probability        = "Prob.",
        param_lo           = "Param Lo",
        param_hi           = "Param Hi",
        param_range        = "Patient Range",
        dose_per_admin     = "Dose Rate",
        dose_lo            = "Dose Lo",
        dose_hi            = "Dose Hi",
        dose_range         = "Dose Range (mg)",
        n_units            = "N Units",
        total_unit_mg      = "Drug in Units (mg)",
        leftover_mg_lo     = "Left Over Lo (mg)",
        leftover_mg_hi     = "Left Over Hi (mg)",
        leftover_mg        = "Left Over (mg)",
        shortfall_mg_lo    = "Rounded Down Lo (mg)",
        shortfall_mg_hi    = "Rounded Down Hi (mg)",
        shortfall_mg       = "Rounded Down (mg)",
        packing            = "Unit Combination",
        cost               = "Drug Cost",
        weighted_cost      = "Weighted Cost",
        pre_sharing_admin_cost = "Admin (Pre-Share)",
        pre_sharing_dispensing_cost = "Dispensing (Pre-Share)"
    )
    labels <- label_map[col_names]
    labels[is.na(labels)] <- col_names[is.na(labels)]
    unname(labels)
}

#' Apply standard formatting theme to a regimen flextable
#' @param ft A flextable object.
#' @param cost_cols Column names to format as currency.
#' @param int_cols Column names to format as integers.
#' @param mg_cols Column names to format with 2 decimal places.
#' @return A formatted flextable.
#' @keywords internal
theme_regimen <- function(ft, cost_cols = NULL, int_cols = NULL,
                          mg_cols = NULL) {
    ft <- flextable::theme_booktabs(ft)
    std_border <- flextable::fp_border_default(width = 0.5)
    ft <- flextable::border_outer(ft, border = std_border, part = "all")
    ft <- flextable::border_inner(ft, border = std_border, part = "all")
    ft <- flextable::set_table_properties(ft, layout = "autofit",
        opts_html = list(
            extra_css = ".tabwid table { border-collapse: collapse; }"
        ))
    ft <- flextable::bg(ft, bg = "#F0F0F0", part = "header")
    ft <- flextable::align(ft, align = "center", part = "header")
    left_cols <- intersect(c("day", "model_cycle"), ft$col_keys)
    if (length(left_cols) > 0) {
        ft <- flextable::align(ft, j = left_cols, align = "left",
            part = "header")
    }
    ft <- flextable::autofit(ft)
    if (length(cost_cols) > 0) {
        for (col in cost_cols) {
            ft <- flextable::colformat_double(
                ft, j = col, prefix = "$", big.mark = ",", digits = 2
            )
        }
    }
    if (length(int_cols) > 0) {
        for (col in int_cols) {
            ft <- flextable::colformat_int(ft, j = col)
        }
    }
    if (length(mg_cols) > 0) {
        for (col in mg_cols) {
            ft <- flextable::colformat_double(ft, j = col, digits = 2)
        }
    }
    ft
}

# Convert logical columns to "Yes"/"No" strings for display
format_logical_cols <- function(df) {
    logical_cols <- vapply(df, is.logical, logical(1))
    df[logical_cols] <- lapply(df[logical_cols], function(x) ifelse(x, "Yes", "No"))
    df
}

# Column type of a per-drug combination column ("Drug_A_admin_cost" ->
# "admin_cost"), read from its known ending (longest first, so e.g.
# "pre_sharing_admin_cost" wins over "admin_cost").
drug_column_suffix <- function(col) {
    known <- c("pre_sharing_dispensing_cost", "pre_sharing_admin_cost",
               "units_dispensed", "dispensing_cost", "med_cycle_day",
               "mg_dispensed", "doses_taken", "admin_cost", "total_cost",
               "drug_cost", "med_cycle", "mg_taken")
    for (k in known) {
        if (endsWith(col, paste0("_", k))) return(k)
    }
    col
}

# Identify cost/int/mg columns from a set of column names
classify_columns <- function(col_names) {
    cost_cols <- col_names[grepl("_cost$", col_names)]
    # Also include bare "cost" column (from vial packing bins)
    if ("cost" %in% col_names) cost_cols <- union(cost_cols, "cost")
    int_cols <- intersect(col_names, c(
        "cycle", "day", "model_cycle", "med_cycle", "med_cycle_day",
        "n_administrations", "n_dispensings", "doses_taken",
        "units_dispensed", "n_units", "bin"
    ))
    mg_cols <- intersect(col_names, c(
        "mg_taken", "mg_dispensed", "leftover_mg_lo", "leftover_mg_hi",
        "shortfall_mg_lo", "shortfall_mg_hi", "total_unit_mg"
    ))
    list(cost = cost_cols, int = int_cols, mg = mg_cols)
}


# ── Method 1: regimen_costs (tables A, B) ───────────────────────────────────

#' Convert regimen_costs to flextable
#'
#' @param x A \code{regimen_costs} object.
#' @param ... Additional arguments (unused).
#' @return A flextable object.
#' @exportS3Method flextable::as_flextable
as_flextable.regimen_costs <- function(x, ...) {
    check_flextable_installed()
    df <- as.data.frame(x)
    col_names <- names(df)
    labels <- regimen_col_labels(col_names)

    ft <- flextable::flextable(df)

    header_df <- data.frame(
        col_keys = col_names,
        labels = labels,
        stringsAsFactors = FALSE
    )
    ft <- flextable::set_header_df(ft, mapping = header_df, key = "col_keys")

    cls <- classify_columns(col_names)
    ft <- theme_regimen(ft, cls$cost, cls$int, cls$mg)
    ft
}


# ── Method 2: combo_regimen_costs (table C) ─────────────────────────────────

#' Convert combo_regimen_costs to flextable
#'
#' @param x A \code{combo_regimen_costs} object.
#' @param ... Additional arguments (unused).
#' @return A flextable object with two-row header.
#' @exportS3Method flextable::as_flextable
as_flextable.combo_regimen_costs <- function(x, ...) {
    check_flextable_installed()
    df <- as.data.frame(x)
    drug_names <- attr(x, "drug_names")
    col_names <- names(df)

    shared_cols <- c("cycle", "cycle_start_day", "cycle_end_day")
    combined_col <- "total_cost"

    top_row <- stats::setNames(
        as.list(regimen_col_labels(col_names)), col_names
    )
    bottom_row <- top_row

    drug_columns <- attr(x, "drug_columns")
    for (drug in drug_names) {
        for (dc in intersect(drug_columns[[drug]], col_names)) {
            top_row[[dc]] <- drug
            bottom_row[[dc]] <- regimen_col_labels(drug_column_suffix(dc))
        }
    }

    for (sc in shared_cols) {
        top_row[[sc]] <- regimen_col_labels(sc)
        bottom_row[[sc]] <- regimen_col_labels(sc)
    }
    top_row[[combined_col]] <- "Combined Total"
    bottom_row[[combined_col]] <- "Combined Total"

    ft <- flextable::flextable(df)

    header_df <- data.frame(
        col_keys = col_names,
        top = unlist(top_row[col_names]),
        bottom = unlist(bottom_row[col_names]),
        stringsAsFactors = FALSE
    )

    ft <- flextable::set_header_df(ft, mapping = header_df, key = "col_keys")

    for (sc in c(shared_cols, combined_col)) {
        ft <- flextable::merge_v(ft, j = sc, part = "header")
    }

    for (drug in drug_names) {
        drug_cols <- intersect(drug_columns[[drug]], col_names)
        if (length(drug_cols) > 1) {
            ft <- flextable::merge_at(
                ft, i = 1, j = which(col_names %in% drug_cols),
                part = "header"
            )
        }
    }

    ft <- flextable::align(ft, align = "center", part = "header")

    cls <- classify_columns(col_names)
    ft <- theme_regimen(ft, cls$cost, cls$int, cls$mg)
    ft
}


# ── Method 3: regimen_daily_detail (tables D, E, F) ─────────────────────────

#' Convert regimen_daily_detail to flextable
#'
#' @param x A \code{regimen_daily_detail} object.
#' @param ... Additional arguments (unused).
#' @return A flextable object with two-row grouped header.
#' @exportS3Method flextable::as_flextable
as_flextable.regimen_daily_detail <- function(x, ...) {
    check_flextable_installed()
    df <- as.data.frame(x)
    df <- format_logical_cols(df)
    col_names <- names(df)

    standalone_cols <- c("day", "model_cycle")
    med_cycle_cols <- c("med_cycle", "med_cycle_day")
    cost_cols_group <- intersect(
        c("drug_cost", "admin_cost", "dispensing_cost", "total_cost"), col_names
    )
    group_spec <- list(
        "Medication Cycle" = med_cycle_cols,
        "Given" = intersect(c("doses_taken", "mg_taken"), col_names),
        "Dispensed" = intersect(c("units_dispensed", "mg_dispensed"), col_names),
        "Costs" = cost_cols_group
    )
    group_spec <- group_spec[lengths(group_spec) > 0]

    top_row <- stats::setNames(vector("list", length(col_names)), col_names)
    bottom_row <- stats::setNames(vector("list", length(col_names)), col_names)

    for (sc in standalone_cols) {
        top_row[[sc]] <- regimen_col_labels(sc)
        bottom_row[[sc]] <- regimen_col_labels(sc)
    }

    for (group_name in names(group_spec)) {
        cols <- group_spec[[group_name]]
        for (col in cols) {
            top_row[[col]] <- group_name
            bottom_row[[col]] <- regimen_col_labels(col)
        }
    }

    header_df <- data.frame(
        col_keys = col_names,
        top = unlist(top_row[col_names]),
        bottom = unlist(bottom_row[col_names]),
        stringsAsFactors = FALSE
    )

    ft <- flextable::flextable(df)
    ft <- flextable::set_header_df(ft, mapping = header_df, key = "col_keys")

    for (sc in standalone_cols) {
        ft <- flextable::merge_v(ft, j = sc, part = "header")
    }

    for (group_name in names(group_spec)) {
        cols <- group_spec[[group_name]]
        if (length(cols) > 1) {
            ft <- flextable::merge_at(
                ft, i = 1, j = which(col_names %in% cols), part = "header"
            )
        }
    }

    ft <- flextable::align(ft, align = "center", part = "header")

    cls <- classify_columns(col_names)
    ft <- theme_regimen(ft, cls$cost, cls$int, cls$mg)
    ft
}


# ── Method 4: combo_regimen_daily_detail (table G) ──────────────────────────

#' Convert combo_regimen_daily_detail to flextable
#'
#' @param x A \code{combo_regimen_daily_detail} object.
#' @param ... Additional arguments (unused).
#' @return A flextable object with two-row header.
#' @exportS3Method flextable::as_flextable
as_flextable.combo_regimen_daily_detail <- function(x, ...) {
    check_flextable_installed()
    df <- as.data.frame(x)
    df <- format_logical_cols(df)
    drug_names <- attr(x, "drug_names")
    drug_columns <- attr(x, "drug_columns")
    col_names <- names(df)

    shared_cols <- c("day", "model_cycle")
    combined_col <- "total_cost"

    top_row <- stats::setNames(vector("list", length(col_names)), col_names)
    bottom_row <- stats::setNames(vector("list", length(col_names)), col_names)

    for (sc in shared_cols) {
        top_row[[sc]] <- regimen_col_labels(sc)
        bottom_row[[sc]] <- regimen_col_labels(sc)
    }

    top_row[[combined_col]] <- "Combined Total Cost"
    bottom_row[[combined_col]] <- "Combined Total Cost"

    for (drug in drug_names) {
        for (col in intersect(drug_columns[[drug]], col_names)) {
            top_row[[col]] <- drug
            bottom_row[[col]] <- regimen_col_labels(drug_column_suffix(col))
        }
    }

    header_df <- data.frame(
        col_keys = col_names,
        top = unlist(top_row[col_names]),
        bottom = unlist(bottom_row[col_names]),
        stringsAsFactors = FALSE
    )

    ft <- flextable::flextable(df)
    ft <- flextable::set_header_df(ft, mapping = header_df, key = "col_keys")

    for (sc in c(shared_cols, combined_col)) {
        ft <- flextable::merge_v(ft, j = sc, part = "header")
    }

    for (drug in drug_names) {
        cols <- drug_columns[[drug]]
        cols <- cols[cols %in% col_names]
        if (length(cols) > 1) {
            ft <- flextable::merge_at(
                ft, i = 1, j = which(col_names %in% cols), part = "header"
            )
        }
    }

    ft <- flextable::align(ft, align = "center", part = "header")

    cls <- classify_columns(col_names)
    ft <- theme_regimen(ft, cls$cost, cls$int, cls$mg)
    ft
}


# ── Method 5: regimen_schedule (table H) ────────────────────────────────────

#' Convert regimen_schedule to flextable
#'
#' @param x A \code{regimen_schedule} object.
#' @param ... Additional arguments (unused).
#' @return A flextable object with two-row grouped header.
#' @exportS3Method flextable::as_flextable
as_flextable.regimen_schedule <- function(x, ...) {
    check_flextable_installed()
    df <- as.data.frame(x)
    col_names <- names(df)

    standalone_cols <- c("day", "model_cycle", "event_type")
    med_cycle_cols <- c("med_cycle", "med_cycle_day")
    cost_cols_group <- intersect(
        c("drug_cost", "admin_cost", "dispensing_cost", "total_cost"), col_names
    )

    top_row <- stats::setNames(vector("list", length(col_names)), col_names)
    bottom_row <- stats::setNames(vector("list", length(col_names)), col_names)

    for (sc in standalone_cols) {
        top_row[[sc]] <- regimen_col_labels(sc)
        bottom_row[[sc]] <- regimen_col_labels(sc)
    }

    for (col in med_cycle_cols) {
        top_row[[col]] <- "Medication Cycle"
        bottom_row[[col]] <- regimen_col_labels(col)
    }

    for (col in cost_cols_group) {
        top_row[[col]] <- "Costs"
        bottom_row[[col]] <- regimen_col_labels(col)
    }

    header_df <- data.frame(
        col_keys = col_names,
        top = unlist(top_row[col_names]),
        bottom = unlist(bottom_row[col_names]),
        stringsAsFactors = FALSE
    )

    ft <- flextable::flextable(df)
    ft <- flextable::set_header_df(ft, mapping = header_df, key = "col_keys")

    for (sc in standalone_cols) {
        ft <- flextable::merge_v(ft, j = sc, part = "header")
    }

    if (length(med_cycle_cols) > 1) {
        ft <- flextable::merge_at(
            ft, i = 1, j = which(col_names %in% med_cycle_cols),
            part = "header"
        )
    }
    if (length(cost_cols_group) > 1) {
        ft <- flextable::merge_at(
            ft, i = 1, j = which(col_names %in% cost_cols_group),
            part = "header"
        )
    }

    ft <- flextable::align(ft, align = "center", part = "header")

    cls <- classify_columns(col_names)
    ft <- theme_regimen(ft, cls$cost, cls$int, cls$mg)
    ft
}


# ── Method 6: dosing_detail (distribution bin table) ─────────────────────────

#' Convert dosing_detail to flextable
#'
#' @param x A \code{dosing_detail} object.
#' @param ... Additional arguments (unused).
#' @return A flextable object with two-row grouped header and summary footer.
#' @exportS3Method flextable::as_flextable
as_flextable.dosing_detail <- function(x, ...) {
    check_flextable_installed()
    dose_basis <- attr(x, "dose_basis")
    param_unit <- if (dose_basis == "weight") "kg" else "m\u00B2"

    # Build display data frame: merge lo/hi pairs into range strings
    param_digits <- if (dose_basis == "weight") 1 else 2
    fmt_param <- function(v) formatC(v, format = "f", digits = param_digits)
    # 4 significant digits, so small amounts (0.026 mg) aren't shown as 0
    fmt_dose  <- function(v) format_mg(signif(v, 4))
    fmt_mg    <- fmt_dose

    param_range <- paste0(fmt_param(x$param_lo), " \u2013 ",
                          fmt_param(x$param_hi))
    dose_range  <- paste0(fmt_dose(x$dose_lo), " \u2013 ",
                          fmt_dose(x$dose_hi))
    leftover_range <- ifelse(
        x$leftover_mg_lo == x$leftover_mg_hi,
        fmt_mg(x$leftover_mg_lo),
        paste0(fmt_mg(x$leftover_mg_lo), " \u2013 ",
               fmt_mg(x$leftover_mg_hi))
    )

    shortfall_range <- ifelse(
        x$shortfall_mg_lo == x$shortfall_mg_hi,
        fmt_mg(x$shortfall_mg_lo),
        paste0(fmt_mg(x$shortfall_mg_lo), " \u2013 ",
               fmt_mg(x$shortfall_mg_hi))
    )

    df <- data.frame(
        bin            = x$bin,
        probability    = x$probability,
        param_range    = param_range,
        dose_per_admin = x$dose_per_admin,
        dose_range     = dose_range,
        packing        = x$packing,
        n_units        = x$n_units,
        total_unit_mg  = fmt_mg(x$total_unit_mg),
        leftover_mg    = leftover_range,
        shortfall_mg   = shortfall_range,
        cost           = x$cost,
        weighted_cost  = x$weighted_cost,
        stringsAsFactors = FALSE
    )
    col_names <- names(df)

    standalone_cols <- c("bin", "probability", "param_range",
                         "dose_per_admin", "dose_range",
                         "total_unit_mg", "leftover_mg", "shortfall_mg")
    packing_cols    <- intersect(c("packing", "n_units"), col_names)
    cost_cols_group <- intersect(c("cost", "weighted_cost"), col_names)

    group_spec <- list()
    group_spec[["Units"]] <- packing_cols
    group_spec[["Cost"]] <- cost_cols_group

    top_row    <- stats::setNames(vector("list", length(col_names)), col_names)
    bottom_row <- stats::setNames(vector("list", length(col_names)), col_names)

    for (sc in standalone_cols) {
        top_row[[sc]]    <- regimen_col_labels(sc)
        bottom_row[[sc]] <- regimen_col_labels(sc)
    }
    # Override param_range label to include unit
    param_label <- paste0("Patient Range (", param_unit, ")")
    top_row[["param_range"]]    <- param_label
    bottom_row[["param_range"]] <- param_label

    # Override dose_per_admin label to include unit
    dose_unit <- if (dose_basis == "weight") "mg/kg" else "mg/m\u00B2"
    dpa_label <- paste0("Dose Rate (", dose_unit, ")")
    top_row[["dose_per_admin"]]    <- dpa_label
    bottom_row[["dose_per_admin"]] <- dpa_label

    for (group_name in names(group_spec)) {
        cols <- group_spec[[group_name]]
        for (col in cols) {
            top_row[[col]]    <- group_name
            bottom_row[[col]] <- regimen_col_labels(col)
        }
    }

    header_df <- data.frame(
        col_keys = col_names,
        top      = unlist(top_row[col_names]),
        bottom   = unlist(bottom_row[col_names]),
        stringsAsFactors = FALSE
    )

    ft <- flextable::flextable(df)
    ft <- flextable::set_header_df(ft, mapping = header_df, key = "col_keys")

    for (sc in standalone_cols) {
        ft <- flextable::merge_v(ft, j = sc, part = "header")
    }

    for (group_name in names(group_spec)) {
        cols <- group_spec[[group_name]]
        if (length(cols) > 1) {
            ft <- flextable::merge_at(
                ft, i = 1, j = which(col_names %in% cols), part = "header"
            )
        }
    }

    # Footer summary
    drug_name   <- attr(x, "drug_name")
    mean_param  <- attr(x, "mean_param")
    sd_param    <- attr(x, "sd_param")
    exp_cost    <- attr(x, "expected_cost")

    footer_text <- paste0(
        "Expected cost: $",
        formatC(exp_cost, format = "f", digits = 2, big.mark = ","),
        " | Drug: ", drug_name,
        " | Dose basis: ", dose_basis,
        " (", round(mean_param, 2), " \u00B1 ", round(sd_param, 2),
        " ", param_unit, ")"
    )
    unit_sizes <- attr(x, "unit_size")
    unit_costs <- attr(x, "unit_cost")
    ord <- order(unit_sizes, decreasing = TRUE)
    units_text <- paste0(
        "Units: ",
        paste0(format_mg(unit_sizes[ord]), "mg @ $",
               formatC(unit_costs[ord], format = "f", digits = 2,
                       big.mark = ","), collapse = ", "),
        " | Max dose rounding: ", attr(x, "max_dose_rounding") * 100,
        "% of the prescribed dose"
    )
    ft <- flextable::add_footer_lines(ft, values = c(footer_text, units_text))

    ft <- flextable::align(ft, align = "center", part = "header")

    cls <- classify_columns(col_names)
    ft <- theme_regimen(ft, cls$cost, cls$int, cls$mg)

    # Format probability column
    ft <- flextable::colformat_double(ft, j = "probability", digits = 4)
    # Format dose_per_admin
    ft <- flextable::colformat_double(ft, j = "dose_per_admin", digits = 2)

    ft
}
