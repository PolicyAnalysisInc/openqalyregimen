# One IV row; columns can be overridden or added.
iv_row <- function(...) {
  base <- list(name = "Drug A", route = "iv", dose = 100, dose_basis = "flat",
               med_cycle_length = 7, admin_days = "1", unit_size = "100",
               unit_cost = "500", exact = FALSE, charge_timing = "per_admin")
  over <- list(...)
  base[names(over)] <- over
  as.data.frame(base, stringsAsFactors = FALSE)
}

test_that("define_regimen_table returns med_regimen for one row", {
  reg <- define_regimen_table(iv_row())
  expect_s3_class(reg, "med_regimen")
  expect_equal(reg$admin_days, 1L)
  expect_equal(reg$unit_size, 100)
  expect_false(reg$exact)
  expect_equal(reg$charge_timing, "per_admin")
})

test_that("define_regimen_table returns combo for multiple rows", {
  tbl <- data.frame(
    name = c("Drug A", "Drug B"),
    route = c("iv", "oral"),
    dose = c(100, 3600),
    dose_basis = c("flat", "flat"),
    med_cycle_length = c(21, 21),
    admin_days = c("1", "1-14"),
    unit_size = c("100", "150,500"),
    unit_cost = c("500", "2.21"),
    exact = c(FALSE, TRUE),
    charge_timing = c("per_admin", "per_dispensing"),
    admin_cost = c(241, NA),
    dispensing_cost = c(NA, 45),
    stringsAsFactors = FALSE
  )
  reg <- define_regimen_table(tbl)
  expect_s3_class(reg, "med_regimen_combo")
  expect_equal(length(reg$regimens), 2)
  expect_equal(reg$regimens[[2]]$unit_size, c(150, 500))
  expect_true(reg$regimens[[2]]$exact)
  expect_equal(reg$regimens[[2]]$dispensing_cost, 45)
  expect_equal(reg$regimens[[2]]$admin_cost, 0)
  expect_equal(reg$regimens[[1]]$admin_cost, 241)
})

test_that("csv unit_cost per size is parsed", {
  reg <- define_regimen_table(iv_row(unit_size = "150,420",
                                     unit_cost = "407.40,1184"))
  expect_equal(reg$unit_cost, c(407.40, 1184))
})

test_that("logical columns accept text values", {
  reg <- define_regimen_table(iv_row(exact = "TRUE", no_wastage = "false"))
  expect_true(reg$exact)
  expect_false(reg$no_wastage)
  expect_error(define_regimen_table(iv_row(exact = "maybe")),
               "invalid logical value in exact")
})

test_that("optional columns map to define_regimen arguments", {
  reg <- define_regimen_table(iv_row(max_cycles = 6, start_day = 21,
                                     max_dose_rounding = 0.1,
                                     admin_cost = 25, dispensing_cost = 10))
  expect_equal(reg$max_med_cycles, 6)
  expect_equal(reg$start_day, 21)
  expect_equal(reg$max_dose_rounding, 0.1)
  expect_equal(reg$admin_cost, 25)
  expect_equal(reg$dispensing_cost, 10)
})

test_that("blank optional fields use define_regimen defaults", {
  reg <- define_regimen_table(iv_row(max_dose_rounding = "", no_wastage = NA))
  expect_equal(reg$max_dose_rounding, 0)
  expect_false(reg$no_wastage)
})

test_that("exact and charge_timing are required in the table too", {
  tbl <- iv_row()
  tbl$exact <- NULL
  expect_error(define_regimen_table(tbl), "exact must be provided")
  tbl <- iv_row()
  tbl$charge_timing <- NULL
  expect_error(define_regimen_table(tbl), "charge_timing must be provided")
})

test_that("unknown columns are ignored", {
  reg <- define_regimen_table(iv_row(component_group = "g", notes = "x"))
  expect_s3_class(reg, "med_regimen")
})

test_that("missing required columns error", {
  tbl <- iv_row()
  tbl$dose <- NULL
  expect_error(define_regimen_table(tbl), "missing required columns: dose")
})

test_that("patient values come from context, not the table", {
  expect_error(define_regimen_table(iv_row(dose_basis = "bsa",
                                           patient_bsa = 1.8)),
               "non-regimen context")
  reg <- define_regimen_table(iv_row(dose = 75, dose_basis = "bsa",
                                     unit_size = "10"),
                              patient_bsa = 1.8, patient_weight = 70)
  expect_equal(reg$actual_dose_mg, 135)
})

test_that("an exact row errors when context gives a matching SD", {
  expect_error(
    define_regimen_table(iv_row(dose = 75, dose_basis = "bsa", exact = TRUE),
                         patient_bsa = 1.8, patient_bsa_sd = 0.2),
    "exact = TRUE can't be used with a patient distribution"
  )
})

test_that("admin_days spec parsing", {
  expect_equal(parse_admin_days_spec("1,8,15"), c(1L, 8L, 15L))
  expect_equal(parse_admin_days_spec("1-3, 8"), c(1L, 2L, 3L, 8L))
  expect_error(parse_admin_days_spec("3-1"), "invalid admin_days")
  expect_error(parse_admin_days_spec("1,1"), "invalid admin_days")
})

# ===========================================================================
# calculate_regimen_cost
# ===========================================================================

test_that("calculate_regimen_cost returns each cost type", {
  reg <- define_regimen_table(iv_row(admin_cost = 25, dispensing_cost = 10))
  args <- list(reg, model_cycle_length = 7, n_cycles = 3)
  expect_equal(do.call(calculate_regimen_cost, c(args, type = "medication")),
               rep(500, 3))
  expect_equal(do.call(calculate_regimen_cost, c(args, type = "administration")),
               rep(25, 3))
  expect_equal(do.call(calculate_regimen_cost, c(args, type = "dispensing")),
               rep(10, 3))
  expect_equal(do.call(calculate_regimen_cost, c(args, type = "total")),
               rep(535, 3))
})

test_that("calculate_regimen_cost rejects unknown types and objects", {
  reg <- define_regimen_table(iv_row())
  expect_error(calculate_regimen_cost(reg, type = "wastage",
                                      model_cycle_length = 7, n_cycles = 3))
  expect_error(calculate_regimen_cost(list(), type = "total",
                                      model_cycle_length = 7, n_cycles = 3),
               "med_regimen")
})

test_that("calculate_regimen_cost time expansion", {
  reg <- define_regimen_table(iv_row(admin_cost = 25))
  base <- calculate_regimen_cost(reg, type = "medication",
                                 model_cycle_length = 7, n_cycles = 3)
  for (time_vec in list(c(7, 14, 21, 7, 14, 21), c(7, 7, 14, 14, 21, 21))) {
    expanded <- calculate_regimen_cost(reg, type = "medication",
                                       model_cycle_length = 7, n_cycles = 3,
                                       time = time_vec)
    expect_equal(expanded, base[round(time_vec / 7)])
  }
})

test_that("calculate_regimen_cost time out of range gives zero", {
  reg <- define_regimen_table(iv_row())
  expanded <- calculate_regimen_cost(reg, type = "medication",
                                     model_cycle_length = 7, n_cycles = 3,
                                     time = c(0, 7, 14, 35))
  expect_equal(expanded, c(0, 500, 500, 0))
})

test_that("combo: types sum across drugs and exclude pre-sharing columns", {
  tbl <- rbind(iv_row(admin_cost = 50, dispensing_cost = 5),
               iv_row(name = "Drug B", unit_cost = "250", admin_cost = 40,
                      dispensing_cost = 8))
  reg <- define_regimen_table(tbl)
  expect_equal(calculate_regimen_cost(reg, type = "medication",
                                      model_cycle_length = 7, n_cycles = 2),
               rep(750, 2))
  expect_equal(calculate_regimen_cost(reg, type = "administration",
                                      model_cycle_length = 7, n_cycles = 2),
               rep(90, 2))
  shared <- define_regimen_table(tbl, shared_admin = TRUE)
  expect_equal(calculate_regimen_cost(shared, type = "administration",
                                      model_cycle_length = 7, n_cycles = 2),
               rep(50, 2))
  expect_equal(calculate_regimen_cost(shared, type = "dispensing",
                                      model_cycle_length = 7, n_cycles = 2),
               rep(8, 2))
  expect_equal(calculate_regimen_cost(shared, type = "total",
                                      model_cycle_length = 7, n_cycles = 2),
               rep(808, 2))
  # Explicit override turns sharing off
  expect_equal(calculate_regimen_cost(shared, type = "administration",
                                      model_cycle_length = 7, n_cycles = 2,
                                      shared_admin = FALSE),
               rep(90, 2))
})

test_that("combo: time expansion works", {
  tbl <- rbind(iv_row(), iv_row(name = "Drug B"))
  reg <- define_regimen_table(tbl)
  base <- calculate_regimen_cost(reg, type = "total", model_cycle_length = 7,
                                 n_cycles = 3)
  expanded <- calculate_regimen_cost(reg, type = "total",
                                     model_cycle_length = 7, n_cycles = 3,
                                     time = c(7, 14, 21, 7))
  expect_equal(expanded, base[c(1, 2, 3, 1)])
})

test_that("an unreachable exact dose errors when costs are calculated", {
  reg <- define_regimen_table(iv_row(dose = 75, dose_basis = "bsa",
                                     unit_size = "20", exact = TRUE),
                              patient_bsa = 1.8)
  expect_error(calculate_regimen_cost(reg, type = "medication",
                                      model_cycle_length = 7, n_cycles = 3),
               "cannot be made exactly")
})
