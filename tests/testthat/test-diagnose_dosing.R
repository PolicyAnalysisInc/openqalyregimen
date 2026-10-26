make_weight_reg <- function(...) {
  args <- list(name = "TestDrug", dose_per_admin = 10, dose_basis = "weight",
               patient_weight = 70, patient_weight_sd = 15,
               unit_size = c(500, 100), unit_cost = c(2000, 400),
               admin_days = 1L, exact = FALSE, charge_timing = "per_admin")
  over <- list(...)
  args[names(over)] <- over
  do.call(define_regimen, args)
}

make_bsa_reg <- function(...) {
  args <- list(name = "BSADrug", dose_per_admin = 375, dose_basis = "bsa",
               patient_bsa = 1.8, patient_bsa_sd = 0.3,
               unit_size = c(500, 100), unit_cost = c(2000, 400),
               admin_days = 1L, exact = FALSE, charge_timing = "per_admin")
  over <- list(...)
  args[names(over)] <- over
  do.call(define_regimen, args)
}

test_that("returns dosing_detail with expected attributes and columns", {
  res <- diagnose_dosing(make_weight_reg())
  expect_s3_class(res, "dosing_detail")
  expect_equal(attr(res, "drug_name"), "TestDrug")
  expect_equal(attr(res, "dose_basis"), "weight")
  expect_equal(attr(res, "mean_param"), 70)
  expect_equal(attr(res, "sd_param"), 15)
  expect_equal(names(res), c("bin", "probability", "param_lo", "param_hi",
                             "dose_per_admin", "dose_lo", "dose_hi", "packing",
                             "n_units", "total_unit_mg", "leftover_mg_lo",
                             "leftover_mg_hi", "shortfall_mg_lo",
                             "shortfall_mg_hi", "cost", "weighted_cost"))
  expect_equal(attr(res, "unit_size"), c(500, 100))
  expect_equal(attr(res, "unit_cost"), c(2000, 400))
  expect_equal(attr(res, "max_dose_rounding"), 0)
})

test_that("dose columns are the parameter columns times the dose", {
  res <- diagnose_dosing(make_bsa_reg())
  expect_true(all(res$dose_per_admin == 375))
  expect_equal(res$dose_lo, res$param_lo * 375)
  expect_equal(res$dose_hi, res$param_hi * 375)
  expect_true(all(res$dose_lo <= res$dose_hi))
  expect_equal(res$bin, seq_len(nrow(res)))
})

test_that("probabilities sum to 1 and expected cost matches the costing", {
  for (reg in list(make_weight_reg(), make_bsa_reg(),
                   make_weight_reg(max_dose_rounding = 0.1),
                   make_weight_reg(unit_size = c(150, 420),
                                   unit_cost = c(407.40, 1184),
                                   dose_per_admin = 8))) {
    res <- diagnose_dosing(reg)
    expect_equal(sum(res$probability), 1, tolerance = 1e-6)
    expect_equal(sum(res$weighted_cost), attr(res, "expected_cost"))
    expect_equal(attr(res, "expected_cost"), cost_single_admin_drug(reg))
  }
})

test_that("units cover the dose in non-rounded bins", {
  res <- diagnose_dosing(make_weight_reg())
  expect_true(all(res$total_unit_mg >= res$dose_hi - 1e-9))
  expect_equal(res$leftover_mg_lo, pmax(res$total_unit_mg - res$dose_hi, 0))
  expect_equal(res$leftover_mg_hi, res$total_unit_mg - res$dose_lo)
  expect_true(all(res$leftover_mg_lo <= res$leftover_mg_hi))
})

test_that("rounded bins have no leftover and units below the dose", {
  res <- diagnose_dosing(make_weight_reg(max_dose_rounding = 0.1))
  rounded <- res$total_unit_mg <= res$dose_lo + 1e-9 &
    res$total_unit_mg < res$dose_hi
  expect_true(any(rounded))
  expect_true(all(res$leftover_mg_lo[rounded] == 0))
  expect_true(all(res$leftover_mg_hi[rounded] == 0))
  # Shortfall = prescribed dose minus the dose given, within the rounding limit
  expect_equal(res$shortfall_mg_lo[rounded],
               res$dose_lo[rounded] - res$total_unit_mg[rounded])
  expect_equal(res$shortfall_mg_hi[rounded],
               res$dose_hi[rounded] - res$total_unit_mg[rounded])
  expect_true(all(res$shortfall_mg_hi[rounded] <=
                    0.1 * res$dose_hi[rounded] + 1e-9))
  expect_true(all(res$shortfall_mg_hi[!rounded] == 0))
})

test_that("packing strings name the unit sizes", {
  res <- diagnose_dosing(make_weight_reg())
  expect_true(all(grepl("^[0-9]+x(500|100)mg", res$packing)))
  expect_true(any(grepl("500mg \\+ ", res$packing)))
  expect_equal(format_packing(c("100mg" = 2L, "40mg" = 1L)), "2x100mg + 1x40mg")
  expect_equal(format_packing(c("100mg" = 0L)), "none")
})

test_that("n_units matches the packing", {
  res <- diagnose_dosing(make_weight_reg())
  counts <- vapply(strsplit(res$packing, " \\+ "), function(p) {
    sum(as.integer(sub("x.*", "", p)))
  }, numeric(1))
  expect_equal(res$n_units, as.integer(counts))
})

test_that("BSA lower bound excludes impossible values", {
  res <- diagnose_dosing(make_bsa_reg())
  expect_gt(res$param_lo[1], 0)
})

test_that("errors without a distribution or with no_wastage", {
  expect_error(diagnose_dosing(make_weight_reg(patient_weight_sd = NULL)),
               "distribution parameter")
  expect_error(diagnose_dosing(make_weight_reg(dose_basis = "flat")),
               "distribution parameter")
  expect_error(diagnose_dosing(make_weight_reg(no_wastage = TRUE)),
               "no_wastage")
  expect_error(diagnose_dosing(list()), "med_regimen")
})

test_that("works for any route", {
  res <- diagnose_dosing(make_bsa_reg(route = "oral", dose_per_admin = 1000,
                                      unit_size = c(150, 500),
                                      unit_cost = 2.21))
  expect_equal(sum(res$probability), 1, tolerance = 1e-6)
})
