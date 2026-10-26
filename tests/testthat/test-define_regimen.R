# A minimal valid unit-costed regimen; arguments override the defaults.
reg_unit <- function(...) {
  args <- list(unit_size = 100, unit_cost = 50, admin_days = 1L,
               exact = FALSE, charge_timing = "per_admin")
  over <- list(...)
  args[names(over)] <- over
  do.call(define_regimen, args)
}

test_that("define_regimen returns object of class med_regimen", {
  expect_s3_class(reg_unit(), "med_regimen")
})

test_that("define_regimen defaults route to iv and dose_basis to flat", {
  reg <- reg_unit()
  expect_equal(reg$route, "iv")
  expect_equal(reg$dose_basis, "flat")
})

test_that("flat dose sets actual_dose_mg equal to dose_per_admin", {
  expect_equal(reg_unit(dose_per_admin = 200)$actual_dose_mg, 200)
})

test_that("weight dose sets actual_dose_mg to dose_per_admin * patient_weight", {
  reg <- reg_unit(dose_per_admin = 5, dose_basis = "weight", patient_weight = 80)
  expect_equal(reg$actual_dose_mg, 5 * 80)
})

test_that("BSA dose sets actual_dose_mg to dose_per_admin * patient_bsa", {
  reg <- reg_unit(dose_per_admin = 75, dose_basis = "bsa", patient_bsa = 1.9)
  expect_equal(reg$actual_dose_mg, 75 * 1.9)
})

test_that("define_regimen requires admin_days", {
  expect_error(
    define_regimen(unit_size = 100, unit_cost = 50, exact = FALSE,
                   charge_timing = "per_admin"),
    "admin_days must be provided"
  )
})

test_that("explicit admin_days are preserved", {
  expect_equal(reg_unit(admin_days = c(1L, 8L, 15L))$admin_days, c(1L, 8L, 15L))
})

test_that("admin_days must be positive integers", {
  expect_error(reg_unit(admin_days = 0L), "admin_days")
  expect_error(reg_unit(admin_days = 1.5), "admin_days")
})

test_that("define_regimen errors on invalid route and dose_basis", {
  expect_error(reg_unit(route = "subcutaneous"))
  expect_error(reg_unit(dose_basis = "invalid"))
})

test_that("all expected fields present in output", {
  expected <- c(
    "name", "route", "dose_per_admin", "dose_basis",
    "patient_weight", "patient_bsa", "patient_weight_sd", "patient_bsa_sd",
    "actual_dose_mg", "med_cycle_length", "admin_days", "max_med_cycles",
    "start_day", "unit_size", "unit_cost", "exact", "max_dose_rounding",
    "no_wastage", "charge_timing", "admin_cost", "dispensing_cost"
  )
  reg <- reg_unit()
  expect_setequal(names(reg), expected)
})

# ===========================================================================
# Required arguments
# ===========================================================================

test_that("exact has no default and must be given", {
  expect_error(
    define_regimen(unit_size = 100, unit_cost = 50, admin_days = 1L,
                   charge_timing = "per_admin"),
    "exact must be provided"
  )
})

test_that("exact must be TRUE or FALSE", {
  expect_error(reg_unit(exact = NA), "exact must be TRUE or FALSE")
  expect_error(reg_unit(exact = "yes"), "exact must be TRUE or FALSE")
})

test_that("charge_timing has no default and must be given", {
  expect_error(
    define_regimen(unit_size = 100, unit_cost = 50, admin_days = 1L,
                   exact = FALSE),
    "charge_timing must be provided"
  )
})

test_that("charge_timing accepts per_admin and per_dispensing only", {
  expect_equal(reg_unit(charge_timing = "per_dispensing")$charge_timing,
               "per_dispensing")
  expect_error(reg_unit(charge_timing = "per_cycle"), "charge_timing must be")
})

# ===========================================================================
# Units
# ===========================================================================

test_that("unit_size is required in unit mode", {
  expect_error(
    define_regimen(unit_cost = 50, admin_days = 1L, exact = FALSE,
                   charge_timing = "per_admin"),
    "unit_size must be provided"
  )
})

test_that("unit_cost is required in unit mode", {
  expect_error(reg_unit(unit_cost = NULL), "unit_cost must be provided")
})

test_that("unit_size must be positive, finite and unique", {
  expect_error(reg_unit(unit_size = c(100, 0), unit_cost = 1), "unit_size")
  expect_error(reg_unit(unit_size = -5), "unit_size")
  expect_error(reg_unit(unit_size = Inf), "unit_size")
  expect_error(reg_unit(unit_size = c(100, 100), unit_cost = 1), "duplicate")
})

test_that("unit_size with more than 3 decimals errors", {
  expect_error(reg_unit(unit_size = 0.0005), "3 decimal places")
})

test_that("unit_cost must be length 1 or match unit_size", {
  expect_error(reg_unit(unit_size = c(100, 50, 10), unit_cost = c(5, 3)),
               "unit_cost")
  expect_silent(reg_unit(unit_size = c(100, 50), unit_cost = 5))
  expect_silent(reg_unit(unit_size = c(100, 50), unit_cost = c(5, 3)))
})

test_that("unit_cost must be non-negative and finite", {
  expect_error(reg_unit(unit_cost = -1), "unit_cost")
  expect_error(reg_unit(unit_cost = NA_real_), "unit_cost")
})

test_that("negative or NA dose errors", {
  expect_error(reg_unit(dose_per_admin = -10), "dose_per_admin")
  expect_error(reg_unit(dose_per_admin = NA_real_), "dose_per_admin")
})

# ===========================================================================
# Rounding and no-wastage settings
# ===========================================================================

test_that("max_dose_rounding defaults to 0 and is stored", {
  expect_equal(reg_unit()$max_dose_rounding, 0)
  expect_equal(reg_unit(max_dose_rounding = 0.1)$max_dose_rounding, 0.1)
})

test_that("max_dose_rounding must be in [0, 1)", {
  expect_error(reg_unit(max_dose_rounding = -0.1), "max_dose_rounding")
  expect_error(reg_unit(max_dose_rounding = 1), "max_dose_rounding")
  expect_error(reg_unit(max_dose_rounding = c(0.1, 0.2)), "max_dose_rounding")
})

test_that("max_dose_rounding can't be combined with exact = TRUE", {
  expect_error(reg_unit(exact = TRUE, max_dose_rounding = 0.1),
               "max_dose_rounding can't be used with exact = TRUE")
  expect_silent(reg_unit(exact = TRUE, max_dose_rounding = 0))
})

test_that("the old wastage_threshold argument no longer exists", {
  expect_error(reg_unit(wastage_threshold = 0.1), "unused argument")
})

test_that("no_wastage defaults to FALSE and must be logical", {
  expect_false(reg_unit()$no_wastage)
  expect_true(reg_unit(no_wastage = TRUE)$no_wastage)
  expect_error(reg_unit(no_wastage = NA), "no_wastage")
})

# ===========================================================================
# Fees
# ===========================================================================

test_that("admin_cost and dispensing_cost default to 0 and are stored", {
  reg <- reg_unit()
  expect_equal(reg$admin_cost, 0)
  expect_equal(reg$dispensing_cost, 0)
  reg <- reg_unit(admin_cost = 241, dispensing_cost = 45)
  expect_equal(reg$admin_cost, 241)
  expect_equal(reg$dispensing_cost, 45)
})

test_that("fees must be non-negative scalars", {
  expect_error(reg_unit(admin_cost = -1), "admin_cost")
  expect_error(reg_unit(dispensing_cost = c(1, 2)), "dispensing_cost")
})

# ===========================================================================
# Patient values and distributions
# ===========================================================================

test_that("patient SDs are stored and default to NULL", {
  reg <- reg_unit()
  expect_null(reg$patient_weight_sd)
  expect_null(reg$patient_bsa_sd)
  reg <- reg_unit(dose_basis = "weight", patient_weight_sd = 15)
  expect_equal(reg$patient_weight_sd, 15)
  reg <- reg_unit(dose_basis = "bsa", patient_bsa_sd = 0.2)
  expect_equal(reg$patient_bsa_sd, 0.2)
})

test_that("patient SDs must be positive", {
  expect_error(reg_unit(dose_basis = "weight", patient_weight_sd = -1), "positive")
  expect_error(reg_unit(dose_basis = "weight", patient_weight_sd = 0), "positive")
  expect_error(reg_unit(dose_basis = "bsa", patient_bsa_sd = -0.1), "positive")
})

test_that("unused patient values and SDs are accepted silently", {
  expect_silent(reg_unit(dose_basis = "flat", patient_weight = 80,
                         patient_bsa = 2, patient_weight_sd = 15,
                         patient_bsa_sd = 0.2))
  expect_silent(reg_unit(dose_basis = "weight", patient_bsa_sd = 0.2))
  expect_silent(reg_unit(dose_basis = "bsa", patient_weight_sd = 15))
})

test_that("exact = TRUE can't be used with a patient distribution", {
  expect_error(
    reg_unit(exact = TRUE, dose_basis = "bsa", patient_bsa_sd = 0.2),
    "exact = TRUE can't be used with a patient distribution"
  )
  expect_error(
    reg_unit(exact = TRUE, dose_basis = "weight", patient_weight_sd = 10),
    "exact = TRUE can't be used with a patient distribution"
  )
})

test_that("exact = TRUE with an SD the dose basis doesn't use is fine", {
  expect_silent(reg_unit(exact = TRUE, dose_basis = "flat",
                         patient_bsa_sd = 0.2))
})

# ===========================================================================
# start_day and max_med_cycles
# ===========================================================================

test_that("start_day defaults to 0 and is stored", {
  expect_equal(reg_unit()$start_day, 0)
  expect_equal(reg_unit(start_day = 21)$start_day, 21)
})

test_that("start_day must be a finite non-negative scalar", {
  expect_error(reg_unit(start_day = -1), "start_day")
  expect_error(reg_unit(start_day = Inf), "start_day")
  expect_error(reg_unit(start_day = "a"), "start_day")
  expect_error(reg_unit(start_day = c(0, 1)), "start_day")
})

test_that("max_med_cycles must be positive or Inf", {
  expect_equal(reg_unit()$max_med_cycles, Inf)
  expect_equal(reg_unit(max_med_cycles = 6)$max_med_cycles, 6)
  expect_error(reg_unit(max_med_cycles = 0), "max_med_cycles")
})

# ===========================================================================
# Route is a label only
# ===========================================================================

test_that("route is a label: every route accepts the same arguments", {
  for (r in c("iv", "oral", "sc", "im")) {
    reg <- reg_unit(route = r, exact = TRUE, dispensing_cost = 10,
                    admin_cost = 5)
    expect_equal(reg$route, r)
  }
})

test_that("route does not change the drug cost", {
  costs <- vapply(c("iv", "oral", "sc", "im"), function(r) {
    reg <- reg_unit(route = r, dose_per_admin = 150, unit_size = c(100, 50),
                    unit_cost = c(80, 45))
    calculate_med_costs(reg, model_cycle_length = 21, n_cycles = 1)$drug_cost
  }, numeric(1))
  expect_true(all(costs == costs[1]))
})

test_that("cost_per_day (simple mode) no longer exists", {
  expect_error(define_regimen(cost_per_day = 100, admin_days = 1L),
               "unused argument")
})
