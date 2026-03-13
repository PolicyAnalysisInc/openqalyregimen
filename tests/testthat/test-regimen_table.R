test_that("define_regimen_table returns med_regimen for one row", {
  tbl <- data.frame(
    name = "Drug A",
    route = "iv",
    dose = 100,
    dose_basis = "flat",
    med_cycle_length = 21,
    admin_days = "1",
    vial_size = 100,
    vial_cost = 500,
    stringsAsFactors = FALSE
  )

  reg <- define_regimen_table(tbl)
  expect_s3_class(reg, "med_regimen")
  expect_equal(reg$admin_days, 1L)
})

test_that("define_regimen_table returns combo for multiple rows", {
  tbl <- data.frame(
    name = c("Drug A", "Drug B"),
    route = c("iv", "oral"),
    dose = c(100, 50),
    dose_basis = c("flat", "flat"),
    med_cycle_length = c(21, 21),
    admin_days = c("1", "1-14"),
    vial_size = c(100, NA),
    vial_cost = c(500, NA),
    unit_cost = c(NA, 10),
    tablet_strength = c(NA, 50),
    stringsAsFactors = FALSE
  )

  reg <- define_regimen_table(tbl)
  expect_s3_class(reg, "med_regimen_combo")
  expect_equal(length(reg$regimens), 2)
})

test_that("define_regimen_table rejects non-regimen context columns", {
  tbl <- data.frame(
    name = "Drug A",
    route = "iv",
    dose = 100,
    dose_basis = "bsa",
    med_cycle_length = 21,
    admin_days = "1",
    patient_bsa = 1.8,
    vial_size = 100,
    vial_cost = 500,
    stringsAsFactors = FALSE
  )

  expect_error(
    define_regimen_table(tbl),
    "non-regimen context"
  )
})

test_that("define_regimen_table uses define_regimen defaults for blank optional fields", {
  tbl <- data.frame(
    name = "Drug A",
    route = "oral",
    dose = 100,
    dose_basis = "flat",
    med_cycle_length = 21,
    admin_days = "1-14",
    units_per_pack = "",
    oral_wastage = NA,
    unit_cost = 10,
    tablet_strength = 100,
    stringsAsFactors = FALSE
  )

  reg <- define_regimen_table(tbl)
  expect_equal(reg$units_per_pack, 1)
  expect_true(reg$oral_wastage)
})

test_that("calculate_regimen_cost returns medication and administration vectors", {
  tbl <- data.frame(
    name = "Drug A",
    route = "iv",
    dose = 100,
    dose_basis = "flat",
    med_cycle_length = 7,
    admin_days = "1",
    vial_size = 100,
    vial_cost = 500,
    admin_cost = 25,
    stringsAsFactors = FALSE
  )

  reg <- define_regimen_table(tbl)
  med <- calculate_regimen_cost(reg, type = "medication",
                                model_cycle_length = 7, n_cycles = 3)
  admin <- calculate_regimen_cost(reg, type = "administration",
                                  model_cycle_length = 7, n_cycles = 3)

  expect_equal(med, rep(500, 3))
  expect_equal(admin, rep(25, 3))
})

test_that("calculate_regimen_cost uses combo shared_admin by default", {
  tbl <- data.frame(
    name = c("Drug A", "Drug B"),
    route = c("iv", "iv"),
    dose = c(100, 100),
    dose_basis = c("flat", "flat"),
    med_cycle_length = c(7, 7),
    admin_days = c("1", "1"),
    vial_size = c(100, 100),
    vial_cost = c(500, 250),
    admin_cost = c(50, 40),
    stringsAsFactors = FALSE
  )

  reg <- define_regimen_table(tbl, shared_admin = TRUE)
  admin <- calculate_regimen_cost(reg, type = "administration",
                                  model_cycle_length = 7, n_cycles = 2)

  expect_equal(admin, rep(50, 2))
})
