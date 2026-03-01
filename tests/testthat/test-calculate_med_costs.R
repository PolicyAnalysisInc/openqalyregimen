test_that("calculate_med_costs errors on non-med_regimen input", {
  expect_error(calculate_med_costs(list(a = 1)))
})

test_that("calculate_med_costs returns a data.frame", {
  reg <- define_regimen(vial_size = 100, vial_cost = 500,
    admin_cost = 150)
  out <- calculate_med_costs(reg)
  expect_s3_class(out, "data.frame")
})

test_that("calculate_med_costs row count equals n_cycles", {
  reg <- define_regimen(vial_size = 100, vial_cost = 500)
  out <- calculate_med_costs(reg, n_cycles = 10)
  expect_equal(nrow(out), 10)
})

test_that("cycle_times overrides n_cycles for row count", {
  reg <- define_regimen(vial_size = 100, vial_cost = 500)
  out <- calculate_med_costs(reg,
    cycle_times = c(0, 7, 14, 21, 28))
  expect_equal(nrow(out), 5)
})

test_that("total_cost equals drug_cost + admin_cost for per-admin path", {
  reg <- define_regimen(vial_size = 100, vial_cost = 500,
    admin_cost = 150)
  out <- calculate_med_costs(reg, n_cycles = 5)
  expect_equal(out$total_cost, out$drug_cost + out$admin_cost)
})

# ===========================================================================
# Simple daily cost mode
# ===========================================================================

test_that("simple daily cost: cost_per_day * model_cycle_length gives correct cost", {
  reg <- define_regimen(
    route = "iv",
    cost_per_day = 100
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 4, simple_daily_cost = TRUE)
  expect_equal(out$drug_cost, rep(700, 4))
  expect_equal(out$total_cost, rep(700, 4))
})

test_that("simple daily cost: max_med_cycles zeroes out costs beyond cap", {
  reg <- define_regimen(
    route = "iv",
    cost_per_day = 100,
    max_med_cycles = 2,
    med_cycle_length = 7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 4, simple_daily_cost = TRUE)
  expect_equal(out$drug_cost[3:4], c(0, 0))
  expect_equal(out$total_cost[3:4], c(0, 0))
})

test_that("simple daily cost: n_administrations is NA", {
  reg <- define_regimen(
    route = "iv",
    cost_per_day = 100
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 2, simple_daily_cost = TRUE)
  expect_true(all(is.na(out$n_administrations)))
})


# ===========================================================================
# Oral wastage path
# ===========================================================================

test_that("oral wastage: dispensing counts match analytic formula", {
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 100,
    unit_cost = 10,
    med_cycle_length = 21
  )
  # model_cycle=7, med_cycle=21: 3 model cycles per med cycle
  out <- calculate_med_costs(reg, model_cycle_length = 7, n_cycles = 6)
  # Dispensing at day 0 (cycle 1), day 21 (cycle 4)
  expect_equal(out$n_dispensing_events[1], 1)
  expect_equal(out$n_dispensing_events[4], 1)
  expect_equal(out$n_dispensing_events[2], 0)
})

test_that("oral wastage: subcycle precision gives same totals on aligned cycles", {
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 100,
    unit_cost = 10,
    med_cycle_length = 21
  )
  out_analytic <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 6, subcycle_precision = FALSE)
  out_precise  <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 6, subcycle_precision = TRUE)
  expect_equal(sum(out_analytic$drug_cost), sum(out_precise$drug_cost))
})

test_that("oral wastage: n_tablets_dispensed = n_dispensing_events * tabs_per_dispensing", {
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 50,
    unit_cost = 10,
    med_cycle_length = 7,
    n_admin_per_cycle = 1
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7, n_cycles = 3)
  # 100mg / 50mg = 2 tablets per admin, 1 admin per cycle => 2 tablets per dispensing
  expect_equal(out$n_tablets_dispensed, out$n_dispensing_events * 2)
})

test_that("oral wastage: drug_cost = n_dispensing_events * dispensing_cost", {
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 100,
    unit_cost = 10,
    med_cycle_length = 7,
    n_admin_per_cycle = 1
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7, n_cycles = 3)
  # dispensing cost = 1 admin * $10 = $10
  expected_drug_cost <- out$n_dispensing_events * 10
  expect_equal(out$drug_cost, expected_drug_cost)
})

test_that("oral wastage: treatment cap prevents dispensing beyond cap", {
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 100,
    unit_cost = 10,
    med_cycle_length = 7,
    max_med_cycles = 1
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7, n_cycles = 4)
  expect_equal(out$n_dispensing_events[1], 1)
  expect_equal(sum(out$n_dispensing_events[2:4]), 0)
})


# ===========================================================================
# Per-admin path (IV / oral no-wastage)
# ===========================================================================

test_that("per-admin: admin counts correct for aligned cycles", {
  # med_cycle=21, model_cycle=21, 1 admin on day 1
  reg <- define_regimen(
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 21,
    n_admin_per_cycle = 1
  )
  out <- calculate_med_costs(reg, model_cycle_length = 21, n_cycles = 3)
  expect_equal(out$n_administrations, c(1, 1, 1))
})

test_that("per-admin: admin counts correct for misaligned cycles", {
  # med_cycle=21 (admin day 1), model_cycle=7
  # admins at days 0, 21, 42
  # model cycles: [0,7), [7,14), [14,21), [21,28), ...
  # day 0 -> cycle 1, day 21 -> cycle 4
  reg <- define_regimen(
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 21,
    n_admin_per_cycle = 1
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 6, subcycle_precision = TRUE)
  expect_equal(out$n_administrations[1], 1)
  expect_equal(out$n_administrations[2], 0)
  expect_equal(out$n_administrations[4], 1)
})

test_that("per-admin: subcycle precision matches analytic on aligned cycles", {
  reg <- define_regimen(
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 21,
    n_admin_per_cycle = 1
  )
  out_analytic <- calculate_med_costs(reg, model_cycle_length = 21,
    n_cycles = 3, subcycle_precision = FALSE)
  out_precise  <- calculate_med_costs(reg, model_cycle_length = 21,
    n_cycles = 3, subcycle_precision = TRUE)
  expect_equal(out_analytic$n_administrations, out_precise$n_administrations)
  expect_equal(out_analytic$total_cost, out_precise$total_cost)
})

test_that("per-admin: treatment cap zeroes out costs beyond cap", {
  reg <- define_regimen(
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7,
    max_med_cycles = 1
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 4, subcycle_precision = TRUE)
  expect_equal(out$n_administrations[1], 1)
  expect_equal(sum(out$n_administrations[2:4]), 0)
})


# ===========================================================================
# Daily detail
# ===========================================================================

test_that("daily detail: one row per day", {
  reg <- define_regimen(
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 3, daily_detail = TRUE)
  expect_equal(nrow(out), 21)
})

test_that("daily detail IV: has vials_used, mg_wasted, mg_from_vials columns", {
  reg <- define_regimen(
    route = "iv",
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 2, daily_detail = TRUE)
  expect_true("vials_used" %in% names(out))
  expect_true("mg_wasted" %in% names(out))
  expect_true("mg_from_vials" %in% names(out))
})

test_that("daily detail oral: has tabs_taken, mg_dispensed columns", {
  reg <- define_regimen(
    route = "oral",
    oral_wastage = FALSE,
    dose_per_admin = 100,
    tablet_strength = 50,
    unit_cost = 10,
    med_cycle_length = 7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 2, daily_detail = TRUE)
  expect_true("tabs_taken" %in% names(out))
  expect_true("mg_dispensed" %in% names(out))
})

test_that("daily detail oral wastage: is_dispensing TRUE only at med cycle starts", {
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 100,
    unit_cost = 10,
    med_cycle_length = 7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 3, daily_detail = TRUE)
  disp_days <- out$day[out$is_dispensing]
  # Med cycle starts at days 0, 7, 14
  expect_equal(disp_days, c(0, 7, 14))
})

test_that("daily detail: aggregating daily drug_cost by model_cycle matches non-daily", {
  reg <- define_regimen(
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7
  )
  out_daily <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 3, daily_detail = TRUE)
  out_cycle <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 3, subcycle_precision = TRUE)
  daily_agg <- tapply(out_daily$drug_cost, out_daily$model_cycle, sum)
  expect_equal(as.numeric(daily_agg), out_cycle$drug_cost)
})

test_that("daily detail: off-treatment days have zero costs and NA med_cycle", {
  reg <- define_regimen(
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7,
    max_med_cycles = 1
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 3, daily_detail = TRUE)
  off_tx <- out[out$day >= 7, ]
  expect_true(all(off_tx$drug_cost == 0))
  expect_true(all(is.na(off_tx$med_cycle)))
})


# ===========================================================================
# Cross-cutting
# ===========================================================================

test_that("daily_detail = TRUE forces subcycle_precision", {
  reg <- define_regimen(
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7
  )
  # daily_detail=TRUE should work even without explicit subcycle_precision
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 2, daily_detail = TRUE)
  expect_equal(nrow(out), 14)
})

test_that("single cycle edge case (n_cycles = 1)", {
  reg <- define_regimen(
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7, n_cycles = 1)
  expect_equal(nrow(out), 1)
  expect_equal(out$n_administrations, 1)
})

test_that("med cycle longer than model cycle works correctly", {
  reg <- define_regimen(
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 28
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 8, subcycle_precision = TRUE)
  # Admin on day 0 (model cycle 1) and day 28 (model cycle 5)
  expect_equal(out$n_administrations[1], 1)
  expect_equal(out$n_administrations[5], 1)
  expect_equal(sum(out$n_administrations), 2)
})
