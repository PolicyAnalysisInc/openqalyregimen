# ===========================================================================
# diagnose_dosing (distribution-averaged bin table)
# ===========================================================================

# ── Helper: weight-based regimen with SD ──────────────────────────────────

make_weight_reg <- function(
    dose_per_admin = 10, vial_sizes = c(500, 100),
    vial_cost = c(2000, 400), patient_weight = 70,
    patient_weight_sd = 15, wastage_threshold = 0
) {
    define_regimen(
        name = "TestDrug", route = "iv",
        dose_per_admin = dose_per_admin,
        dose_basis = "weight",
        patient_weight = patient_weight,
        patient_weight_sd = patient_weight_sd,
        vial_sizes = vial_sizes,
        vial_cost = vial_cost,
        wastage_threshold = wastage_threshold,
        admin_days = 1L
    )
}

make_bsa_reg <- function(
    dose_per_admin = 375, vial_sizes = c(500, 100),
    vial_cost = c(2000, 400), patient_bsa = 1.8,
    patient_bsa_sd = 0.3, wastage_threshold = 0
) {
    define_regimen(
        name = "BSADrug", route = "iv",
        dose_per_admin = dose_per_admin,
        dose_basis = "bsa",
        patient_bsa = patient_bsa,
        patient_bsa_sd = patient_bsa_sd,
        vial_sizes = vial_sizes,
        vial_cost = vial_cost,
        wastage_threshold = wastage_threshold,
        admin_days = 1L
    )
}


# ── S3 class and attributes ──────────────────────────────────────────────

test_that("returns dosing_detail class with expected attributes", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    expect_s3_class(res, "dosing_detail")
    expect_true(is.data.frame(res))
    expect_equal(attr(res, "drug_name"), "TestDrug")
    expect_equal(attr(res, "dose_basis"), "weight")
    expect_equal(attr(res, "mean_param"), 70)
    expect_equal(attr(res, "sd_param"), 15)
    expect_true(is.numeric(attr(res, "expected_cost")))
})

test_that("all expected columns are present", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    expected_cols <- c(
        "bin", "probability",
        "param_lo", "param_hi",
        "dose_per_admin",
        "dose_lo", "dose_hi",
        "packing", "n_vials", "total_vial_mg",
        "waste_mg_lo", "waste_mg_hi",
        "cost", "weighted_cost"
    )
    expect_equal(names(res), expected_cols)
})


# ── Numeric column types ─────────────────────────────────────────────────

test_that("all non-string columns are numeric", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    numeric_cols <- c("bin", "probability", "param_lo", "param_hi",
                      "dose_per_admin", "dose_lo", "dose_hi",
                      "n_vials", "total_vial_mg",
                      "waste_mg_lo", "waste_mg_hi",
                      "cost", "weighted_cost")
    for (col in numeric_cols) {
        expect_true(is.numeric(res[[col]]), info = paste(col, "should be numeric"))
    }
    expect_true(is.character(res$packing))
})


# ── dose_per_admin column ────────────────────────────────────────────────

test_that("dose_per_admin matches the regimen's value", {
    reg <- make_weight_reg(dose_per_admin = 10)
    res <- diagnose_dosing(reg)
    expect_true(all(res$dose_per_admin == 10))
})

test_that("dose_per_admin matches for BSA regimen", {
    reg <- make_bsa_reg(dose_per_admin = 375)
    res <- diagnose_dosing(reg)
    expect_true(all(res$dose_per_admin == 375))
})


# ── Dose calculation trace ───────────────────────────────────────────────

test_that("dose_lo == param_lo * dose_per_admin", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    expect_equal(res$dose_lo, res$param_lo * res$dose_per_admin)
})

test_that("dose_hi == param_hi * dose_per_admin", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    expect_equal(res$dose_hi, res$param_hi * res$dose_per_admin)
})


# ── total_vial_mg and waste relationships ────────────────────────────────

test_that("total_vial_mg >= dose_hi for all bins with vials", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    has_vials <- res$n_vials > 0
    expect_true(all(res$total_vial_mg[has_vials] >= res$dose_hi[has_vials] - 1e-9))
})

test_that("waste_mg_lo == total_vial_mg - dose_hi (clamped to 0)", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    expected <- pmax(res$total_vial_mg - res$dose_hi, 0)
    expect_equal(res$waste_mg_lo, expected)
})

test_that("waste_mg_hi == total_vial_mg - dose_lo", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    expect_equal(res$waste_mg_hi, res$total_vial_mg - res$dose_lo)
})

test_that("total_vial_mg == dose + waste consistency", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    # dose_lo + waste_mg_hi == total_vial_mg
    expect_equal(res$dose_lo + res$waste_mg_hi, res$total_vial_mg)
})


# ── Weight-based regimen ─────────────────────────────────────────────────

test_that("weight-based: probabilities sum to ~1", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    expect_true(abs(sum(res$probability) - 1) < 0.01)
})

test_that("weight-based: weighted costs sum to expected cost attribute", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    expect_equal(
        sum(res$weighted_cost),
        attr(res, "expected_cost")
    )
})

test_that("weight-based: expected cost matches cost_single_admin_drug()", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    expected <- openqalyregimen:::cost_single_admin_drug(reg)
    expect_equal(attr(res, "expected_cost"), expected, tolerance = 1e-10)
})


# ── BSA-based regimen ────────────────────────────────────────────────────

test_that("BSA-based: probabilities sum to ~1", {
    reg <- make_bsa_reg()
    res <- diagnose_dosing(reg)
    expect_true(abs(sum(res$probability) - 1) < 0.01)
})

test_that("BSA-based: weighted costs sum to expected cost attribute", {
    reg <- make_bsa_reg()
    res <- diagnose_dosing(reg)
    expect_equal(
        sum(res$weighted_cost),
        attr(res, "expected_cost")
    )
})

test_that("BSA-based: expected cost matches cost_single_admin_drug()", {
    reg <- make_bsa_reg()
    res <- diagnose_dosing(reg)
    expected <- openqalyregimen:::cost_single_admin_drug(reg)
    expect_equal(attr(res, "expected_cost"), expected, tolerance = 1e-10)
})


# ── Packing string format ───────────────────────────────────────────────

test_that("packing string contains vial sizes from regimen", {
    reg <- make_weight_reg(vial_sizes = c(500, 100), vial_cost = c(2000, 400))
    res <- diagnose_dosing(reg)
    # At least some bins should reference 500mg or 100mg
    has_vial_ref <- grepl("500mg|100mg", res$packing)
    expect_true(any(has_vial_ref))
})

test_that("packing string shows 'open' for residual bins", {
    # With weight SD, some bins should have partial vials
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    # Not all bins will be exact-fit, so some should have "open"
    has_open <- grepl("open", res$packing)
    # At least one bin with waste should have "open"
    waste_bins <- res$waste_mg_hi > 0
    if (any(waste_bins)) {
        expect_true(any(has_open[waste_bins]))
    }
})

test_that("bins with zero waste have no 'open' in packing", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    zero_waste <- res$waste_mg_hi == 0
    if (any(zero_waste)) {
        expect_false(any(grepl("open", res$packing[zero_waste])))
    }
    # Regardless, verify consistency: 'open' appears iff waste > 0
    has_open <- grepl("open", res$packing)
    has_waste <- res$waste_mg_hi > 0
    expect_true(all(has_open == has_waste | res$packing == "none"))
})


# ── Single vial size ────────────────────────────────────────────────────

test_that("single vial size: all bins reference that size", {
    reg <- make_weight_reg(vial_sizes = 100, vial_cost = 400)
    res <- diagnose_dosing(reg)
    non_none <- res$packing != "none"
    if (any(non_none)) {
        expect_true(all(grepl("100mg", res$packing[non_none])))
    }
})


# ── Multiple vial sizes ────────────────────────────────────────────────

test_that("multiple vial sizes: bins show correct combinations", {
    reg <- make_weight_reg(
        vial_sizes = c(500, 100, 50),
        vial_cost = c(2000, 400, 200)
    )
    res <- diagnose_dosing(reg)
    # Should have bins with various combinations
    expect_true(nrow(res) > 1)
    # At least some bins should reference the larger vial sizes
    expect_true(any(grepl("500mg|100mg|50mg", res$packing)))
})


# ── Wastage threshold ──────────────────────────────────────────────────

test_that("wastage threshold affects packing decisions", {
    reg_no_thresh <- make_weight_reg(wastage_threshold = 0)
    reg_thresh    <- make_weight_reg(wastage_threshold = 0.2)
    res_no <- diagnose_dosing(reg_no_thresh)
    res_th <- diagnose_dosing(reg_thresh)
    # With a threshold, some doses that would open a vial now skip it,
    # so expected cost should differ
    expect_false(
        isTRUE(all.equal(
            attr(res_no, "expected_cost"),
            attr(res_th, "expected_cost")
        ))
    )
})


# ── Error cases ─────────────────────────────────────────────────────────

test_that("errors on oral route med_regimen", {
    reg <- define_regimen(
        route = "oral", dose_per_admin = 100,
        tablet_strength = 50, unit_cost = 10, admin_days = 1L
    )
    expect_error(diagnose_dosing(reg), "vial-based routes")
})

test_that("errors when no SD parameter (flat dose)", {
    reg <- define_regimen(
        route = "iv", dose_per_admin = 100,
        vial_sizes = 100, vial_cost = 500, admin_days = 1L
    )
    expect_error(diagnose_dosing(reg), "distribution parameter")
})

test_that("errors when no SD parameter (weight basis, no SD)", {
    reg <- define_regimen(
        route = "iv", dose_per_admin = 10,
        dose_basis = "weight", patient_weight = 70,
        vial_sizes = 100, vial_cost = 500, admin_days = 1L
    )
    expect_error(diagnose_dosing(reg), "distribution parameter")
})

test_that("errors when reg is not a med_regimen", {
    expect_error(
        diagnose_dosing(list(route = "iv")),
        "med_regimen"
    )
})


# ── Bin structure ───────────────────────────────────────────────────────

test_that("bins are numbered sequentially from 1", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    expect_equal(res$bin, seq_len(nrow(res)))
})

test_that("n_vials are non-negative integers", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    expect_true(all(res$n_vials >= 0))
    expect_true(all(res$n_vials == as.integer(res$n_vials)))
})

test_that("dose ordering: dose_lo <= dose_hi within each bin", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    expect_true(all(res$dose_lo <= res$dose_hi))
})

test_that("param ordering: param_lo <= param_hi within each bin", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    expect_true(all(res$param_lo <= res$param_hi))
})

test_that("waste ranges: waste_mg_lo <= waste_mg_hi", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    expect_true(all(res$waste_mg_lo <= res$waste_mg_hi))
})

test_that("cost and weighted_cost are non-negative", {
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    expect_true(all(res$cost >= 0))
    expect_true(all(res$weighted_cost >= 0))
})


# ── Consistency across multiple regimens ────────────────────────────────

test_that("expected_cost matches cost_single_admin_drug for various regimens", {
    regs <- list(
        make_weight_reg(dose_per_admin = 5, vial_sizes = c(200, 50),
                        vial_cost = c(800, 200)),
        make_weight_reg(dose_per_admin = 15, vial_sizes = 500,
                        vial_cost = 2000),
        make_bsa_reg(dose_per_admin = 200, vial_sizes = c(100, 50),
                     vial_cost = c(400, 200)),
        make_weight_reg(dose_per_admin = 10, vial_sizes = c(500, 100),
                        vial_cost = c(2000, 400),
                        wastage_threshold = 0.1)
    )
    for (reg in regs) {
        res <- diagnose_dosing(reg)
        expected <- openqalyregimen:::cost_single_admin_drug(reg)
        expect_equal(attr(res, "expected_cost"), expected, tolerance = 1e-10,
                     info = paste("Drug:", reg$name, "basis:", reg$dose_basis))
    }
})


# ── Deprecated alias ────────────────────────────────────────────────────

test_that("diagnose_vial_packing warns deprecated and returns dosing_detail", {
    reg <- make_weight_reg()
    expect_warning(
        res <- diagnose_vial_packing(reg),
        "deprecated"
    )
    expect_s3_class(res, "dosing_detail")
    expect_true(is.data.frame(res))
})


# ── Flextable conversion ───────────────────────────────────────────────

test_that("as_flextable.dosing_detail returns flextable with 2-row header", {
    skip_if_not_installed("flextable")
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    ft <- flextable::as_flextable(res)
    expect_s3_class(ft, "flextable")
    expect_equal(nrow(ft$header$dataset), 2)
})

test_that("as_flextable.dosing_detail has footer", {
    skip_if_not_installed("flextable")
    reg <- make_weight_reg()
    res <- diagnose_dosing(reg)
    ft <- flextable::as_flextable(res)
    expect_true(nrow(ft$footer$dataset) > 0)
})


# ── BSA lower bound (5SD) ────────────────────────────────────────────────

test_that("BSA-based: param_lo[1] > 0 (impossible values excluded)", {
    reg <- make_bsa_reg(patient_bsa = 1.8, patient_bsa_sd = 0.15)
    res <- diagnose_dosing(reg)
    # mean - 5*SD = 1.8 - 0.75 = 1.05 > 0, so first bin should start above 0
    expect_true(res$param_lo[1] > 0)
})

test_that("weight-based: param_lo[1] == 0 when mean - 5*SD < 0", {
    # mean=70, sd=15 => mean - 5*sd = -5 < 0, so lower bound clamps to 0
    reg <- make_weight_reg(patient_weight = 70, patient_weight_sd = 15)
    res <- diagnose_dosing(reg)
    expect_equal(res$param_lo[1], 0)
})
