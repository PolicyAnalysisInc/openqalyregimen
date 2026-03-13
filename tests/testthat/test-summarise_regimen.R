test_that("summarise_regimen returns reg invisibly", {
  reg <- define_regimen(vial_size = 100, vial_cost = 500, admin_days = 1L)
  invisible(capture.output(result <- summarise_regimen(reg)))
  expect_identical(result, reg)
})

test_that("summarise_regimen errors on non-med_regimen input", {
  expect_error(summarise_regimen(list(a = 1)))
})

test_that("output contains drug name", {
  reg <- define_regimen(name = "Pembrolizumab",
    vial_size = 100, vial_cost = 500, admin_days = 1L)
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("Pembrolizumab", out)))
})

test_that("output contains route", {
  reg <- define_regimen(route = "iv",
    vial_size = 100, vial_cost = 500, admin_days = 1L)
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("iv", out)))
})

test_that("weight basis shows /kg and patient weight", {
  reg <- define_regimen(
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 80,
    vial_size = 100,
    vial_cost = 500,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("/kg", out)))
  expect_true(any(grepl("80", out)))
})

test_that("BSA basis shows /m2 and patient BSA", {
  reg <- define_regimen(
    dose_per_admin = 75,
    dose_basis = "bsa",
    patient_bsa = 1.9,
    vial_size = 100,
    vial_cost = 500,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("/m2", out)))
  expect_true(any(grepl("1.9", out)))
})

test_that("flat basis shows no unit suffix", {
  reg <- define_regimen(
    dose_per_admin = 100,
    dose_basis = "flat",
    vial_size = 100,
    vial_cost = 500,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  dose_line <- out[grepl("Prescribed dose", out)]
  expect_false(grepl("/kg", dose_line))
  expect_false(grepl("/m2", dose_line))
})

test_that("oral wastage mode: output contains 'ORAL WASTAGE'", {
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 50,
    unit_cost = 10,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("ORAL WASTAGE", out)))
})

test_that("oral no-wastage mode: output contains 'PER-ADMINISTRATION (no oral wastage)'", {
  reg <- define_regimen(
    route = "oral",
    oral_wastage = FALSE,
    dose_per_admin = 100,
    tablet_strength = 50,
    unit_cost = 10,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("PER-ADMINISTRATION \\(no oral wastage\\)", out)))
})

test_that("IV mode: output contains 'PER-ADMINISTRATION (IV)'", {
  reg <- define_regimen(
    route = "iv",
    vial_size = 100,
    vial_cost = 500,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("PER-ADMINISTRATION \\(IV\\)", out)))
})

test_that("finite max_med_cycles: shows number and treatment duration", {
  reg <- define_regimen(
    vial_size = 100,
    vial_cost = 500,
    max_med_cycles = 6,
    med_cycle_length = 21,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("6", out[grepl("Max medication cycles", out)])))
  # 6 * 21 = 126 days
  expect_true(any(grepl("126", out)))
})

test_that("infinite max_med_cycles: shows 'unlimited'", {
  reg <- define_regimen(
    vial_size = 100,
    vial_cost = 500,
    max_med_cycles = Inf,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("unlimited", out)))
})

test_that("short admin_days (<=10): comma-separated list", {
  reg <- define_regimen(
    admin_days = c(1L, 3L, 5L),
    vial_size = 100,
    vial_cost = 500
  )
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("1, 3, 5", out)))
})

test_that("long continuous admin_days (>10): range with '(continuous)'", {
  reg <- define_regimen(
    admin_days = 1:14,
    med_cycle_length = 21,
    vial_size = 100,
    vial_cost = 500
  )
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("1-14", out)))
  expect_true(any(grepl("continuous", out, ignore.case = TRUE)))
})

test_that("tablet rounding waste shown when waste > 0", {
  # 130mg dose with 100mg tablets => 2 tablets = 200mg, waste = 70mg
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 130,
    tablet_strength = 100,
    unit_cost = 10,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  waste_line <- out[grepl("[Tt]ablet rounding waste", out)]
  expect_true(length(waste_line) > 0)
  expect_false(grepl("none", waste_line))
})

test_that("tablet rounding waste shows 'none' when exact", {
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 50,
    unit_cost = 10,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  waste_line <- out[grepl("[Tt]ablet rounding waste", out)]
  expect_true(any(grepl("none", waste_line)))
})


# ===========================================================================
# SC / IM routes
# ===========================================================================

test_that("SC output contains correct route label", {
  reg <- define_regimen(
    route = "sc",
    vial_size = 50,
    vial_cost = 200,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("PER-ADMINISTRATION \\(SC\\)", out)))
})

test_that("IM output contains correct route label", {
  reg <- define_regimen(
    route = "im",
    vial_size = 100,
    vial_cost = 300,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("PER-ADMINISTRATION \\(IM\\)", out)))
})

test_that("SC output contains vial breakdown section", {
  reg <- define_regimen(
    route = "sc",
    dose_per_admin = 100,
    vial_size = 50,
    vial_cost = 200,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("VIAL BREAKDOWN", out)))
  expect_true(any(grepl("Drug waste", out)))
})

test_that("IM output contains vial breakdown section", {
  reg <- define_regimen(
    route = "im",
    dose_per_admin = 100,
    vial_size = 100,
    vial_cost = 300,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("VIAL BREAKDOWN", out)))
})

# ===========================================================================
# Distributional case: no vial/tablet breakdown
# ===========================================================================

test_that("IV with weight SD shows distribution-averaged, not vial breakdown", {
  reg <- define_regimen(
    route = "iv",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 80,
    patient_weight_sd = 10,
    vial_size = 100,
    vial_cost = 500,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("distribution-averaged", out)))
  expect_false(any(grepl("VIAL BREAKDOWN", out)))
  expect_false(any(grepl("Drug waste", out)))
})

test_that("oral with weight SD shows distribution-averaged, not tablet breakdown", {
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 80,
    patient_weight_sd = 10,
    tablet_strength = 50,
    unit_cost = 10,
    oral_wastage = FALSE,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("distribution-averaged", out)))
  expect_false(any(grepl("TABLET BREAKDOWN", out)))
})

test_that("IV without SD still shows vial breakdown (regression guard)", {
  reg <- define_regimen(
    route = "iv",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 80,
    vial_size = 100,
    vial_cost = 500,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  expect_true(any(grepl("VIAL BREAKDOWN", out)))
})

test_that("oral wastage with dist omits Tablets dispensed", {
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 80,
    patient_weight_sd = 10,
    tablet_strength = 50,
    unit_cost = 10,
    oral_wastage = TRUE,
    admin_days = 1L
  )
  out <- capture.output(summarise_regimen(reg))
  expect_false(any(grepl("Tablets dispensed", out)))
})
