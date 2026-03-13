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
        n_dispensing_events  = "N Dispensing",
        n_tablets_dispensed  = "N Tablets",
        drug_cost       = "Drug Cost",
        admin_cost      = "Admin Cost",
        total_cost      = "Total Cost",
        day             = "Day",
        model_cycle     = "Model Cycle",
        med_cycle       = "Med Cycle",
        med_cycle_day   = "Cycle Day",
        doses_taken     = "Doses",
        tabs_taken      = "Tabs",
        mg_taken        = "mg",
        mg_from_vials   = "mg Vials",
        mg_wasted       = "mg Waste",
        vials_used      = "Vials",
        doses_dispensed = "Doses",
        tabs_dispensed  = "Tabs",
        mg_dispensed    = "mg",
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
        n_vials            = "N Vials",
        total_vial_mg      = "Total Vial (mg)",
        waste_mg_lo        = "Waste Lo (mg)",
        waste_mg_hi        = "Waste Hi (mg)",
        waste_mg           = "Waste (mg)",
        packing            = "Vial Combination",
        cost               = "Drug Cost",
        weighted_cost      = "Weighted Cost",
        pre_sharing_admin_cost = "Admin (Pre-Share)"
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

# Identify cost/int/mg columns from a set of column names
classify_columns <- function(col_names) {
    cost_cols <- col_names[grepl("_cost$", col_names)]
    # Also include bare "cost" column (from vial packing bins)
    if ("cost" %in% col_names) cost_cols <- union(cost_cols, "cost")
    int_cols <- intersect(col_names, c(
        "cycle", "day", "model_cycle", "med_cycle", "med_cycle_day",
        "n_administrations", "n_dispensing_events", "n_tablets_dispensed",
        "doses_taken", "vials_used", "tabs_taken", "doses_dispensed",
        "tabs_dispensed", "n_vials", "bin"
    ))
    mg_cols <- intersect(col_names, c(
        "mg_taken", "mg_from_vials", "mg_wasted", "mg_dispensed",
        "waste_mg", "waste_mg_lo", "waste_mg_hi",
        "mg_administered", "mg_administered_lo", "mg_administered_hi",
        "total_vial_mg"
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

    for (drug in drug_names) {
        nm <- gsub("[^a-zA-Z0-9]", "_", drug)
        drug_prefix <- paste0(nm, "_")
        drug_cols <- col_names[startsWith(col_names, drug_prefix)]
        drug_cols <- setdiff(drug_cols, c(shared_cols, combined_col))
        for (dc in drug_cols) {
            if (dc %in% col_names) {
                top_row[[dc]] <- drug
                suffix <- sub(paste0("^", nm, "_"), "", dc)
                bottom_row[[dc]] <- regimen_col_labels(suffix)
            }
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
        nm <- gsub("[^a-zA-Z0-9]", "_", drug)
        drug_prefix <- paste0(nm, "_")
        drug_cols <- col_names[startsWith(col_names, drug_prefix)]
        drug_cols <- setdiff(drug_cols, c(shared_cols, combined_col))
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

    is_iv <- "vials_used" %in% col_names
    is_oral_wastage <- "doses_dispensed" %in% col_names

    standalone_cols <- c("day", "model_cycle")
    med_cycle_cols <- c("med_cycle", "med_cycle_day")
    cost_cols_group <- intersect(
        c("drug_cost", "admin_cost", "total_cost"), col_names
    )

    if (is_iv) {
        dosing_cols <- intersect(
            c("doses_taken", "mg_taken", "mg_from_vials",
              "mg_wasted", "vials_used"),
            col_names
        )
        group_spec <- list(
            "Medication Cycle" = med_cycle_cols,
            "Dosing Detail" = dosing_cols,
            "Costs" = cost_cols_group
        )
    } else if (is_oral_wastage) {
        taken_cols <- intersect(
            c("doses_taken", "tabs_taken", "mg_taken"),
            col_names
        )
        dispensed_cols <- intersect(
            c("doses_dispensed", "tabs_dispensed", "mg_dispensed"),
            col_names
        )
        group_spec <- list(
            "Medication Cycle" = med_cycle_cols,
            "Taken" = taken_cols,
            "Dispensed" = dispensed_cols,
            "Costs" = cost_cols_group
        )
    } else {
        dosing_cols <- intersect(
            c("doses_taken", "mg_taken"),
            col_names
        )
        group_spec <- list(
            "Medication Cycle" = med_cycle_cols,
            "Dosing Detail" = dosing_cols,
            "Costs" = cost_cols_group
        )
    }

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
        cols <- drug_columns[[drug]]
        nm <- gsub("[^a-zA-Z0-9]", "_", drug)
        for (col in cols) {
            if (col %in% col_names) {
                top_row[[col]] <- drug
                raw_name <- sub(paste0("^", nm, "_"), "", col)
                bottom_row[[col]] <- regimen_col_labels(raw_name)
            }
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
    df$event_type <- NULL
    col_names <- names(df)

    standalone_cols <- c("day", "model_cycle")
    med_cycle_cols <- c("med_cycle", "med_cycle_day")
    cost_cols_group <- intersect(
        c("drug_cost", "admin_cost", "total_cost"), col_names
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
    fmt_dose  <- function(v) formatC(v, format = "f", digits = 1)
    fmt_mg    <- function(v) formatC(v, format = "f", digits = 1)

    param_range <- paste0(fmt_param(x$param_lo), " \u2013 ",
                          fmt_param(x$param_hi))
    dose_range  <- paste0(fmt_dose(x$dose_lo), " \u2013 ",
                          fmt_dose(x$dose_hi))
    waste_range <- ifelse(
        x$waste_mg_lo == x$waste_mg_hi,
        fmt_mg(x$waste_mg_lo),
        paste0(fmt_mg(x$waste_mg_lo), " \u2013 ",
               fmt_mg(x$waste_mg_hi))
    )

    df <- data.frame(
        bin            = x$bin,
        probability    = x$probability,
        param_range    = param_range,
        dose_per_admin = x$dose_per_admin,
        dose_range     = dose_range,
        packing        = x$packing,
        n_vials        = x$n_vials,
        total_vial_mg  = x$total_vial_mg,
        waste_mg       = waste_range,
        cost           = x$cost,
        weighted_cost  = x$weighted_cost,
        stringsAsFactors = FALSE
    )
    col_names <- names(df)

    standalone_cols <- c("bin", "probability", "param_range",
                         "dose_per_admin", "dose_range",
                         "total_vial_mg", "waste_mg")
    packing_cols    <- intersect(c("packing", "n_vials"), col_names)
    cost_cols_group <- intersect(c("cost", "weighted_cost"), col_names)

    group_spec <- list()
    group_spec[["Vial Packing"]] <- packing_cols
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
    ft <- flextable::add_footer_lines(ft, values = footer_text)

    ft <- flextable::align(ft, align = "center", part = "header")

    cls <- classify_columns(col_names)
    ft <- theme_regimen(ft, cls$cost, cls$int, cls$mg)

    # Format probability column
    ft <- flextable::colformat_double(ft, j = "probability", digits = 4)
    # Format dose_per_admin
    ft <- flextable::colformat_double(ft, j = "dose_per_admin", digits = 2)
    # Format total_vial_mg
    ft <- flextable::colformat_double(ft, j = "total_vial_mg", digits = 1)

    ft
}


# ── Method 6b: vial_packing_detail (backward compat) ────────────────────────

#' Convert vial_packing_detail to flextable
#'
#' @param x A \code{vial_packing_detail} object.
#' @param ... Additional arguments (unused).
#' @return A flextable object with two-row grouped header and summary footer.
#' @exportS3Method flextable::as_flextable
as_flextable.vial_packing_detail <- function(x, ...) {
    check_flextable_installed()
    df <- as.data.frame(x)
    col_names <- names(df)

    standalone_cols <- "bin"
    dose_cols       <- c("dose_lo", "dose_hi")
    packing_cols    <- intersect(c("n_vials", "waste_mg", "packing"), col_names)
    cost_cols_group <- intersect(c("cost", "probability", "weighted_cost"),
                                 col_names)

    group_spec <- list(
        "Dose Range"            = dose_cols,
        "Packing"               = packing_cols,
        "Cost"                  = cost_cols_group
    )

    top_row    <- stats::setNames(vector("list", length(col_names)), col_names)
    bottom_row <- stats::setNames(vector("list", length(col_names)), col_names)

    for (sc in standalone_cols) {
        top_row[[sc]]    <- regimen_col_labels(sc)
        bottom_row[[sc]] <- regimen_col_labels(sc)
    }

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
    dose_basis  <- attr(x, "dose_basis")
    mean_param  <- attr(x, "mean_param")
    sd_param    <- attr(x, "sd_param")
    exp_cost    <- attr(x, "expected_cost")

    basis_unit <- if (dose_basis == "weight") "kg" else "m\u00B2"
    footer_text <- paste0(
        "Expected cost: $",
        formatC(exp_cost, format = "f", digits = 2, big.mark = ","),
        " | Drug: ", drug_name,
        " | Dose basis: ", dose_basis,
        " (", round(mean_param, 2), " \u00B1 ", round(sd_param, 2),
        " ", basis_unit, ")"
    )
    ft <- flextable::add_footer_lines(ft, values = footer_text)

    ft <- flextable::align(ft, align = "center", part = "header")

    cls <- classify_columns(col_names)
    ft <- theme_regimen(ft, cls$cost, cls$int, cls$mg)

    # Format probability and dose columns
    for (col in intersect(c("probability"), col_names)) {
        ft <- flextable::colformat_double(ft, j = col, digits = 4)
    }
    for (col in intersect(c("dose_lo", "dose_hi"), col_names)) {
        ft <- flextable::colformat_double(ft, j = col, digits = 1)
    }

    ft
}
