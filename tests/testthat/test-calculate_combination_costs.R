test_that("calculate_combination_costs errors on non-list input", {
  expect_error(calculate_combination_costs("not_a_list"))
})

test_that("calculate_combination_costs errors on list with non-med_regimen elements", {
  expect_error(calculate_combination_costs(list(1, 2, 3)))
})

test_that("single regimen produces valid output", {
  reg <- define_regimen(
    name = "Drug A",
    vial_size = 100,
    vial_cost = 500
  )
  out <- calculate_combination_costs(list(reg), n_cycles = 5)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 5)
  expect_true("total_cost" %in% names(out))
})

test_that("two regimens: per-drug columns named correctly", {
  reg_a <- define_regimen(
    name = "Drug A",
    vial_size = 100,
    vial_cost = 500
  )
  reg_b <- define_regimen(
    name = "Drug B",
    vial_size = 50,
    vial_cost = 200
  )
  out <- calculate_combination_costs(list(reg_a, reg_b), n_cycles = 3)
  expect_true("Drug_A_drug_cost" %in% names(out))
  expect_true("Drug_A_admin_cost" %in% names(out))
  expect_true("Drug_A_total_cost" %in% names(out))
  expect_true("Drug_B_drug_cost" %in% names(out))
  expect_true("Drug_B_total_cost" %in% names(out))
})

test_that("drug name sanitization: special chars become underscore", {
  reg <- define_regimen(
    name = "Drug-A (combo)",
    vial_size = 100,
    vial_cost = 500
  )
  out <- calculate_combination_costs(list(reg), n_cycles = 3)
  # "Drug-A (combo)" -> "Drug_A__combo_"
  sanitized <- gsub("[^a-zA-Z0-9]", "_", "Drug-A (combo)")
  expect_true(paste0(sanitized, "_total_cost") %in% names(out))
})

test_that("total_cost equals sum of all *_total_cost columns", {
  reg_a <- define_regimen(
    name = "Drug A",
    vial_size = 100,
    vial_cost = 500
  )
  reg_b <- define_regimen(
    name = "Drug B",
    vial_size = 50,
    vial_cost = 200
  )
  out <- calculate_combination_costs(list(reg_a, reg_b), n_cycles = 5)
  cost_cols <- grep("_total_cost$", names(out), value = TRUE)
  cost_cols <- setdiff(cost_cols, "total_cost")
  expect_equal(out$total_cost, rowSums(out[, cost_cols, drop = FALSE]))
})

test_that("per-drug costs match individual calculate_med_costs results", {
  reg_a <- define_regimen(
    name = "Drug A",
    vial_size = 100,
    vial_cost = 500,
    admin_cost = 150
  )
  reg_b <- define_regimen(
    name = "Drug B",
    route = "oral",
    dose_per_admin = 200,
    tablet_strength = 100,
    unit_cost = 20
  )
  combo <- calculate_combination_costs(
    list(reg_a, reg_b),
    model_cycle_length = 7,
    n_cycles = 4
  )
  indiv_a <- calculate_med_costs(reg_a,
    model_cycle_length = 7, n_cycles = 4)
  indiv_b <- calculate_med_costs(reg_b,
    model_cycle_length = 7, n_cycles = 4)
  expect_equal(combo$Drug_A_total_cost, indiv_a$total_cost)
  expect_equal(combo$Drug_B_total_cost, indiv_b$total_cost)
})

test_that("three regimens aggregate correctly", {
  reg_a <- define_regimen(name = "A", vial_size = 100, vial_cost = 500)
  reg_b <- define_regimen(name = "B", vial_size = 50, vial_cost = 200)
  reg_c <- define_regimen(name = "C", vial_size = 25, vial_cost = 100)
  out <- calculate_combination_costs(
    list(reg_a, reg_b, reg_c), n_cycles = 3)
  expect_equal(
    out$total_cost,
    out$A_total_cost + out$B_total_cost + out$C_total_cost
  )
})

test_that("... args pass through (different n_cycles produces different row counts)", {
  reg <- define_regimen(name = "Drug A", vial_size = 100, vial_cost = 500)
  out_5 <- calculate_combination_costs(list(reg), n_cycles = 5)
  out_10 <- calculate_combination_costs(list(reg), n_cycles = 10)
  expect_equal(nrow(out_5), 5)
  expect_equal(nrow(out_10), 10)
})
