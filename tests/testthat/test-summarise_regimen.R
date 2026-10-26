reg_unit <- function(...) {
  args <- list(name = "Drug A", dose_per_admin = 150, unit_size = c(100, 50),
               unit_cost = c(80, 45), admin_days = 1L, exact = FALSE,
               charge_timing = "per_admin")
  over <- list(...)
  args[names(over)] <- over
  do.call(define_regimen, args)
}

out_of <- function(reg) paste(capture.output(summarise_regimen(reg)),
                              collapse = "\n")

test_that("summarise_regimen returns reg invisibly", {
  reg <- reg_unit()
  capture.output(expect_invisible(summarise_regimen(reg)))
  capture.output(res <- summarise_regimen(reg))
  expect_identical(res, reg)
})

test_that("summarise_regimen errors on non-med_regimen input", {
  expect_error(summarise_regimen(list()))
})

test_that("output contains drug name and route", {
  out <- out_of(reg_unit(name = "Paclitaxel", route = "sc"))
  expect_match(out, "Paclitaxel")
  expect_match(out, "sc")
})

test_that("dose basis labels", {
  expect_match(out_of(reg_unit(dose_per_admin = 5, dose_basis = "weight",
                               patient_weight = 80)), "5 mg/kg")
  expect_match(out_of(reg_unit(dose_per_admin = 5, dose_basis = "weight",
                               patient_weight = 80)), "80 kg")
  expect_match(out_of(reg_unit(dose_per_admin = 75, dose_basis = "bsa",
                               patient_bsa = 1.9)), "75 mg/m2")
  expect_match(out_of(reg_unit(dose_per_admin = 75, dose_basis = "bsa",
                               patient_bsa = 1.9)), "1.9 m2")
  flat <- out_of(reg_unit())
  expect_match(flat, "Prescribed dose:    150 mg\n")
})

test_that("units per administration are listed", {
  out <- out_of(reg_unit())
  expect_match(out, "1 x 100mg")
  expect_match(out, "1 x 50mg")
  expect_match(out, "Drug cost per admin: \\$125")
})

test_that("available units and prices are listed", {
  out <- out_of(reg_unit())
  expect_match(out, "AVAILABLE UNITS\n  100mg @ \\$80 \\(\\$0.8/mg\\)\n  50mg @ \\$45")
})

test_that("no_wastage shows the per-mg pricing instead of units", {
  out <- out_of(reg_unit(no_wastage = TRUE))
  expect_match(out, "Lowest cost per mg: \\$0.8/mg \\(100mg unit\\)")
  expect_match(out, "Costed dose:        150 mg")
  expect_false(grepl("UNITS \\(per administration", out))
  expect_match(out, "Drug cost per admin: \\$120")
})

test_that("leftover and rounding are shown", {
  out <- out_of(reg_unit(dose_per_admin = 120, unit_size = 100, unit_cost = 500))
  expect_match(out, "Left over:          80 mg")
  out <- out_of(reg_unit(dose_per_admin = 110, unit_size = 100, unit_cost = 500,
                         max_dose_rounding = 0.1))
  expect_match(out, "Rounded down by:    10 mg \\(9.09% of the prescribed dose\\)")
  expect_match(out, "Max dose rounding:  10% of the prescribed dose")
})

test_that("costing flags and fees are shown", {
  out <- out_of(reg_unit(exact = TRUE, charge_timing = "per_dispensing",
                         admin_cost = 241, dispensing_cost = 45))
  expect_match(out, "Exact dosing:       yes")
  expect_match(out, "Charge timing:      per_dispensing")
  expect_match(out, "Admin cost:         \\$241 per administration")
  expect_match(out, "Dispensing cost:    \\$45 per medication cycle")
})

test_that("distribution shows SD and expected cost at the mean", {
  out <- out_of(reg_unit(dose_per_admin = 5, dose_basis = "weight",
                         patient_weight = 70, patient_weight_sd = 15))
  expect_match(out, "Weight SD:          15 kg")
  expect_match(out, "UNITS \\(per administration, at the mean\\)")
  expect_match(out, "Expected cost per admin")
})

test_that("no distribution: no expected-cost line", {
  expect_false(grepl("Expected cost per admin", out_of(reg_unit())))
})

test_that("schedule section: max cycles and admin days", {
  out <- out_of(reg_unit(max_med_cycles = 6))
  expect_match(out, "Max medication cycles: 6")
  expect_match(out, "Max treatment duration: 126 days")
  expect_match(out_of(reg_unit()), "unlimited")
  expect_match(out_of(reg_unit(admin_days = c(1L, 8L, 15L))),
               "Administration days: 1, 8, 15")
  expect_match(out_of(reg_unit(admin_days = 1:14)), "1-14 \\(continuous\\)")
})

test_that("start day is shown when positive", {
  expect_match(out_of(reg_unit(start_day = 21)), "Start day: 21")
})

test_that("an unreachable exact dose errors in the summary", {
  reg <- reg_unit(dose_per_admin = 130, exact = TRUE)
  expect_error(capture.output(summarise_regimen(reg)), "cannot be made exactly")
})
