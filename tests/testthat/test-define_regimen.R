test_that("define_regimen returns object of class med_regimen", {
  reg <- define_regimen(vial_size = 100, vial_cost = 50)
  expect_s3_class(reg, "med_regimen")
})

test_that("define_regimen defaults route to iv", {
  reg <- define_regimen(vial_size = 100, vial_cost = 50)
  expect_equal(reg$route, "iv")
})

test_that("define_regimen defaults dose_basis to flat", {
  reg <- define_regimen(vial_size = 100, vial_cost = 50)
  expect_equal(reg$dose_basis, "flat")
})

test_that("flat dose sets actual_dose_mg equal to dose_per_admin", {
  reg <- define_regimen(dose_per_admin = 200,
    vial_size = 100, vial_cost = 50)
  expect_equal(reg$actual_dose_mg, 200)
})

test_that("weight dose sets actual_dose_mg to dose_per_admin * patient_weight", {
  reg <- define_regimen(dose_per_admin = 5, dose_basis = "weight",
    patient_weight = 80, vial_size = 100, vial_cost = 50)
  expect_equal(reg$actual_dose_mg, 5 * 80)
})

test_that("BSA dose sets actual_dose_mg to dose_per_admin * patient_bsa", {
  reg <- define_regimen(dose_per_admin = 75, dose_basis = "bsa",
    patient_bsa = 1.9, vial_size = 100, vial_cost = 50)
  expect_equal(reg$actual_dose_mg, 75 * 1.9)
})

test_that("single admin defaults admin_days to 1", {
  reg <- define_regimen(vial_size = 100, vial_cost = 50)
  expect_equal(reg$admin_days, 1L)
})

test_that("oral route defaults oral_wastage to TRUE", {
  reg <- define_regimen(route = "oral",
    unit_cost = 10, tablet_strength = 50)
  expect_true(reg$oral_wastage)
})

test_that("IV route defaults oral_wastage to FALSE", {
  reg <- define_regimen(route = "iv",
    vial_size = 100, vial_cost = 50)
  expect_false(reg$oral_wastage)
})

test_that("explicit oral_wastage = FALSE overrides oral default", {
  reg <- define_regimen(route = "oral", oral_wastage = FALSE,
    unit_cost = 10, tablet_strength = 50)
  expect_false(reg$oral_wastage)
})

test_that("tablet_strength resolves to available_strengths", {
  reg <- define_regimen(route = "oral",
    unit_cost = 10, tablet_strength = 50)
  expect_equal(reg$available_strengths, 50)
})

test_that("vial_size resolves to vial_sizes", {
  reg <- define_regimen(vial_size = 100, vial_cost = 50)
  expect_equal(reg$vial_sizes, 100)
})

test_that("unit_cost resolves to vial_cost for IV", {
  reg <- define_regimen(vial_size = 100, unit_cost = 50)
  expect_equal(reg$vial_cost, 50)
})

test_that("define_regimen errors on invalid route", {
  expect_error(define_regimen(route = "subcutaneous"))
})

test_that("define_regimen errors on invalid dose_basis", {
  expect_error(define_regimen(dose_basis = "invalid",
    vial_size = 100, vial_cost = 50))
})

test_that("define_regimen errors on IV without vial_size and no cost_per_day", {
  expect_error(define_regimen(route = "iv"), "vial_size")
})

test_that("define_regimen errors on oral without unit_cost and no cost_per_day", {
  expect_error(define_regimen(route = "oral"), "unit_cost")
})

test_that("admin_days approximately evenly spaced when n_admin_per_cycle > 1", {
  reg <- define_regimen(
    med_cycle_length = 21,
    n_admin_per_cycle = 3,
    vial_size = 100,
    vial_cost = 50
  )
  expect_equal(length(reg$admin_days), 3)
  diffs <- diff(reg$admin_days)
  # Spacing should be approximately even (within 1 day due to rounding)
  expect_true(max(diffs) - min(diffs) <= 1)
})

test_that("explicit admin_days overrides n_admin_per_cycle", {
  reg <- define_regimen(
    n_admin_per_cycle = 5,
    admin_days = c(1L, 8L, 15L),
    vial_size = 100,
    vial_cost = 50
  )
  expect_equal(reg$admin_days, c(1L, 8L, 15L))
  expect_equal(reg$n_admin_per_cycle, 3L)
})

test_that("IV with cost_per_day bypasses vial requirement", {
  expect_no_error(
    define_regimen(route = "iv", cost_per_day = 100)
  )
})

test_that("all 22 expected list fields present in output", {
  reg <- define_regimen(vial_size = 100, vial_cost = 50)
  expected_fields <- c(
    "name", "route", "dose_per_admin", "dose_basis",
    "patient_weight", "patient_bsa", "actual_dose_mg",
    "med_cycle_length", "n_admin_per_cycle", "admin_days",
    "max_med_cycles", "unit_cost", "units_per_pack",
    "tablet_strength", "available_strengths", "oral_wastage",
    "vial_size", "vial_sizes", "vial_cost",
    "wastage_threshold", "admin_cost", "cost_per_day"
  )
  expect_equal(length(expected_fields), 22)
  expect_true(all(expected_fields %in% names(reg)))
  expect_equal(length(names(reg)), 22)
})

test_that("explicit vial_sizes not overwritten by vial_size", {
  reg <- define_regimen(
    vial_size = 100,
    vial_sizes = c(50, 200),
    vial_cost = c(25, 100)
  )
  expect_equal(reg$vial_sizes, c(50, 200))
})

test_that("explicit available_strengths not overwritten by tablet_strength", {
  reg <- define_regimen(
    route = "oral",
    tablet_strength = 50,
    available_strengths = c(25, 100),
    unit_cost = 10
  )
  expect_equal(reg$available_strengths, c(25, 100))
})
