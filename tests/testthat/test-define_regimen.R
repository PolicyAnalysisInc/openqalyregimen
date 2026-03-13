test_that("define_regimen returns object of class med_regimen", {
  reg <- define_regimen(vial_size = 100, vial_cost = 50, admin_days = 1L)
  expect_s3_class(reg, "med_regimen")
})

test_that("define_regimen defaults route to iv", {
  reg <- define_regimen(vial_size = 100, vial_cost = 50, admin_days = 1L)
  expect_equal(reg$route, "iv")
})

test_that("define_regimen defaults dose_basis to flat", {
  reg <- define_regimen(vial_size = 100, vial_cost = 50, admin_days = 1L)
  expect_equal(reg$dose_basis, "flat")
})

test_that("flat dose sets actual_dose_mg equal to dose_per_admin", {
  reg <- define_regimen(dose_per_admin = 200,
    vial_size = 100, vial_cost = 50, admin_days = 1L)
  expect_equal(reg$actual_dose_mg, 200)
})

test_that("weight dose sets actual_dose_mg to dose_per_admin * patient_weight", {
  reg <- define_regimen(dose_per_admin = 5, dose_basis = "weight",
    patient_weight = 80, vial_size = 100, vial_cost = 50, admin_days = 1L)
  expect_equal(reg$actual_dose_mg, 5 * 80)
})

test_that("BSA dose sets actual_dose_mg to dose_per_admin * patient_bsa", {
  reg <- define_regimen(dose_per_admin = 75, dose_basis = "bsa",
    patient_bsa = 1.9, vial_size = 100, vial_cost = 50, admin_days = 1L)
  expect_equal(reg$actual_dose_mg, 75 * 1.9)
})

test_that("define_regimen requires admin_days", {
  expect_error(
    define_regimen(vial_size = 100, vial_cost = 50),
    "admin_days must be provided"
  )
})

test_that("oral route defaults oral_wastage to TRUE", {
  reg <- define_regimen(route = "oral",
    unit_cost = 10, tablet_strength = 50, admin_days = 1L)
  expect_true(reg$oral_wastage)
})

test_that("IV route defaults oral_wastage to FALSE", {
  reg <- define_regimen(route = "iv",
    vial_size = 100, vial_cost = 50, admin_days = 1L)
  expect_false(reg$oral_wastage)
})

test_that("explicit oral_wastage = FALSE overrides oral default", {
  reg <- define_regimen(route = "oral", oral_wastage = FALSE,
    unit_cost = 10, tablet_strength = 50, admin_days = 1L)
  expect_false(reg$oral_wastage)
})

test_that("tablet_strength resolves to available_strengths", {
  reg <- define_regimen(route = "oral",
    unit_cost = 10, tablet_strength = 50, admin_days = 1L)
  expect_equal(reg$available_strengths, 50)
})

test_that("vial_size resolves to vial_sizes", {
  reg <- define_regimen(vial_size = 100, vial_cost = 50, admin_days = 1L)
  expect_equal(reg$vial_sizes, 100)
})

test_that("unit_cost resolves to vial_cost for IV", {
  reg <- define_regimen(vial_size = 100, unit_cost = 50, admin_days = 1L)
  expect_equal(reg$vial_cost, 50)
})

test_that("define_regimen errors on invalid route", {
  expect_error(define_regimen(route = "subcutaneous"))
})

test_that("define_regimen errors on invalid dose_basis", {
  expect_error(define_regimen(dose_basis = "invalid",
    vial_size = 100, vial_cost = 50, admin_days = 1L))
})

test_that("define_regimen errors on IV without vial_size and no cost_per_day", {
  expect_error(define_regimen(route = "iv", admin_days = 1L), "vial_size")
})

test_that("define_regimen errors on oral without unit_cost and no cost_per_day", {
  expect_error(define_regimen(route = "oral", admin_days = 1L), "unit_cost")
})

test_that("explicit admin_days are preserved", {
  reg <- define_regimen(
    admin_days = c(1L, 8L, 15L),
    vial_size = 100,
    vial_cost = 50
  )
  expect_equal(reg$admin_days, c(1L, 8L, 15L))
})

test_that("IV with cost_per_day bypasses vial requirement", {
  expect_no_error(
    define_regimen(route = "iv", cost_per_day = 100, admin_days = 1L)
  )
})

test_that("all expected list fields present in output", {
  reg <- define_regimen(vial_size = 100, vial_cost = 50, admin_days = 1L)
  expected_fields <- c(
    "name", "route", "dose_per_admin", "dose_basis",
    "patient_weight", "patient_bsa",
    "patient_weight_sd", "patient_bsa_sd",
    "actual_dose_mg",
    "med_cycle_length", "admin_days",
    "max_med_cycles", "start_day", "unit_cost", "units_per_pack",
    "tablet_strength", "available_strengths", "oral_wastage",
    "vial_size", "vial_sizes", "vial_cost",
    "wastage_threshold", "admin_cost", "cost_per_day"
  )
  expect_equal(length(expected_fields), 24)
  expect_true(all(expected_fields %in% names(reg)))
  expect_equal(length(names(reg)), 24)
})

test_that("explicit vial_sizes not overwritten by vial_size", {
  reg <- define_regimen(
    vial_size = 100,
    vial_sizes = c(50, 200),
    vial_cost = c(25, 100),
    admin_days = 1L
  )
  expect_equal(reg$vial_sizes, c(50, 200))
})

test_that("explicit available_strengths not overwritten by tablet_strength", {
  reg <- define_regimen(
    route = "oral",
    tablet_strength = 50,
    available_strengths = c(25, 100),
    unit_cost = 10,
    admin_days = 1L
  )
  expect_equal(reg$available_strengths, c(25, 100))
})


# ===========================================================================
# Distribution parameters (patient_weight_sd, patient_bsa_sd)
# ===========================================================================

test_that("patient_weight_sd stored correctly", {
  reg <- define_regimen(
    dose_basis = "weight",
    patient_weight = 70,
    patient_weight_sd = 15,
    vial_size = 100, vial_cost = 50, admin_days = 1L
  )
  expect_equal(reg$patient_weight_sd, 15)
})

test_that("patient_bsa_sd stored correctly", {
  reg <- define_regimen(
    dose_basis = "bsa",
    patient_bsa = 1.8,
    patient_bsa_sd = 0.2,
    vial_size = 100, vial_cost = 50, admin_days = 1L
  )
  expect_equal(reg$patient_bsa_sd, 0.2)
})

test_that("patient_weight_sd defaults to NULL", {
  reg <- define_regimen(vial_size = 100, vial_cost = 50, admin_days = 1L)
  expect_null(reg$patient_weight_sd)
})

test_that("patient_bsa_sd defaults to NULL", {
  reg <- define_regimen(vial_size = 100, vial_cost = 50, admin_days = 1L)
  expect_null(reg$patient_bsa_sd)
})

test_that("patient_weight_sd must be positive", {
  expect_error(
    define_regimen(dose_basis = "weight", patient_weight_sd = -1,
      vial_size = 100, vial_cost = 50, admin_days = 1L),
    "positive"
  )
  expect_error(
    define_regimen(dose_basis = "weight", patient_weight_sd = 0,
      vial_size = 100, vial_cost = 50, admin_days = 1L),
    "positive"
  )
})

test_that("patient_bsa_sd must be positive", {
  expect_error(
    define_regimen(dose_basis = "bsa", patient_bsa_sd = -0.1,
      vial_size = 100, vial_cost = 50, admin_days = 1L),
    "positive"
  )
})

test_that("patient_weight_sd warns when dose_basis != weight", {
  expect_warning(
    define_regimen(dose_basis = "flat", patient_weight_sd = 15,
      vial_size = 100, vial_cost = 50, admin_days = 1L),
    "ignored"
  )
})

test_that("patient_bsa_sd warns when dose_basis != bsa", {
  expect_warning(
    define_regimen(dose_basis = "flat", patient_bsa_sd = 0.2,
      vial_size = 100, vial_cost = 50, admin_days = 1L),
    "ignored"
  )
})

# ===========================================================================
# start_day parameter
# ===========================================================================

test_that("start_day defaults to 0", {
  reg <- define_regimen(vial_size = 100, vial_cost = 50, admin_days = 1L)
  expect_equal(reg$start_day, 0)
})

test_that("explicit start_day is stored", {
  reg <- define_regimen(vial_size = 100, vial_cost = 50, start_day = 21, admin_days = 1L)
  expect_equal(reg$start_day, 21)
})

test_that("start_day must be non-negative", {
  expect_error(
    define_regimen(vial_size = 100, vial_cost = 50, start_day = -1, admin_days = 1L),
    "start_day"
  )
})

test_that("start_day must be finite", {
  expect_error(
    define_regimen(vial_size = 100, vial_cost = 50, start_day = Inf, admin_days = 1L),
    "start_day"
  )
})

test_that("start_day must be numeric scalar", {
  expect_error(
    define_regimen(vial_size = 100, vial_cost = 50, start_day = "a", admin_days = 1L),
    "start_day"
  )
  expect_error(
    define_regimen(vial_size = 100, vial_cost = 50, start_day = c(0, 1), admin_days = 1L),
    "start_day"
  )
})


# ===========================================================================
# SC / IM routes
# ===========================================================================

test_that("SC regimen created with vial params", {
  reg <- define_regimen(route = "sc", vial_size = 50, vial_cost = 200, admin_days = 1L)
  expect_s3_class(reg, "med_regimen")
  expect_equal(reg$route, "sc")
  expect_equal(reg$vial_sizes, 50)
  expect_equal(reg$vial_cost, 200)
})

test_that("IM regimen created with vial params", {
  reg <- define_regimen(route = "im", vial_size = 100, vial_cost = 300, admin_days = 1L)
  expect_s3_class(reg, "med_regimen")
  expect_equal(reg$route, "im")
})

test_that("SC defaults oral_wastage to FALSE", {
  reg <- define_regimen(route = "sc", vial_size = 50, vial_cost = 200, admin_days = 1L)
  expect_false(reg$oral_wastage)
})

test_that("IM defaults oral_wastage to FALSE", {
  reg <- define_regimen(route = "im", vial_size = 50, vial_cost = 200, admin_days = 1L)
  expect_false(reg$oral_wastage)
})

test_that("SC requires vial_size or cost_per_day", {
  expect_error(define_regimen(route = "sc", admin_days = 1L), "SC regimen requires")
})

test_that("IM requires vial_size or cost_per_day", {
  expect_error(define_regimen(route = "im", admin_days = 1L), "IM regimen requires")
})

test_that("oral_wastage = TRUE errors for SC", {
  expect_error(
    define_regimen(route = "sc", oral_wastage = TRUE,
      vial_size = 50, vial_cost = 200, admin_days = 1L),
    "oral_wastage = TRUE is only valid for route = 'oral'"
  )
})

test_that("oral_wastage = TRUE errors for IM", {
  expect_error(
    define_regimen(route = "im", oral_wastage = TRUE,
      vial_size = 50, vial_cost = 200, admin_days = 1L),
    "oral_wastage = TRUE is only valid for route = 'oral'"
  )
})

test_that("SC with weight-based dosing works", {
  reg <- define_regimen(
    route = "sc",
    dose_per_admin = 3,
    dose_basis = "weight",
    patient_weight = 80,
    vial_size = 100,
    vial_cost = 500,
    admin_days = 1L
  )
  expect_equal(reg$actual_dose_mg, 3 * 80)
})

test_that("SC with cost_per_day bypasses vial requirement", {
  expect_no_error(
    define_regimen(route = "sc", cost_per_day = 50, admin_days = 1L)
  )
})

test_that("IM with cost_per_day bypasses vial requirement", {
  expect_no_error(
    define_regimen(route = "im", cost_per_day = 75, admin_days = 1L)
  )
})

test_that("SC unit_cost resolves to vial_cost", {
  reg <- define_regimen(route = "sc", vial_size = 50, unit_cost = 200, admin_days = 1L)
  expect_equal(reg$vial_cost, 200)
})
