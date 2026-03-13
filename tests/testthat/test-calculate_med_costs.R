test_that("calculate_med_costs errors on empty call", {
  expect_error(calculate_med_costs(), "At least one")
})

test_that("calculate_med_costs errors on non-med_regimen input", {
  expect_error(calculate_med_costs(list(a = 1)), "med_regimen")
})

test_that("calculate_med_costs returns a data.frame", {
  reg <- define_regimen(admin_days = 1L, vial_size = 100, vial_cost = 500,
    admin_cost = 150)
  out <- calculate_med_costs(reg)
  expect_s3_class(out, "data.frame")
})

test_that("calculate_med_costs row count equals n_cycles", {
  reg <- define_regimen(admin_days = 1L, vial_size = 100, vial_cost = 500)
  out <- calculate_med_costs(reg, n_cycles = 10)
  expect_equal(nrow(out), 10)
})

test_that("cycle_times overrides n_cycles for row count", {
  reg <- define_regimen(admin_days = 1L, vial_size = 100, vial_cost = 500)
  out <- calculate_med_costs(reg,
    cycle_times = c(0, 7, 14, 21, 28))
  expect_equal(nrow(out), 5)
})

test_that("total_cost equals drug_cost + admin_cost for per-admin path", {
  reg <- define_regimen(admin_days = 1L, vial_size = 100, vial_cost = 500,
    admin_cost = 150)
  out <- calculate_med_costs(reg, n_cycles = 5)
  expect_equal(out$total_cost, out$drug_cost + out$admin_cost)
})

# ===========================================================================
# Simple daily cost mode
# ===========================================================================

test_that("simple daily cost: cost_per_day * model_cycle_length gives correct cost", {
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    cost_per_day = 100
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 4, simple_daily_cost = TRUE)
  expect_equal(out$drug_cost, rep(700, 4))
  expect_equal(out$total_cost, rep(700, 4))
})

test_that("simple daily cost: admin_cost distributed evenly across cycle", {
  reg <- define_regimen(admin_days = 1L, 
    route = "oral",
    cost_per_day = 5,
    admin_cost = 28,
    med_cycle_length = 28
  )
  out <- calculate_med_costs(reg, model_cycle_length = 28,
    n_cycles = 4, simple_daily_cost = TRUE)
  # admin_cost = 28 / 28 days = 1/day, so 28-day cycle = 28
  expect_equal(out$admin_cost, rep(28, 4))
  expect_equal(out$drug_cost, rep(5 * 28, 4))
  expect_equal(out$total_cost, out$drug_cost + out$admin_cost)
})

test_that("simple daily cost: admin_cost zeroed before start_day and beyond max_med_cycles", {
  reg <- define_regimen(admin_days = 1L, 
    route = "oral",
    cost_per_day = 10,
    admin_cost = 14,
    med_cycle_length = 7,
    start_day = 7,
    max_med_cycles = 2
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 5, simple_daily_cost = TRUE)
  # Cycle 1 (days 0-6): before start_day => 0
  expect_equal(out$admin_cost[1], 0)
  # Cycles 2-3 (days 7-20): active => 14 each
  expect_equal(out$admin_cost[2], 14)
  expect_equal(out$admin_cost[3], 14)
  # Cycles 4-5 (days 21+): beyond max_med_cycles => 0
  expect_equal(out$admin_cost[4], 0)
  expect_equal(out$admin_cost[5], 0)
})

test_that("simple daily cost: max_med_cycles zeroes out costs beyond cap", {
  reg <- define_regimen(admin_days = 1L, 
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

test_that("simple daily cost: no n_administrations column", {
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    cost_per_day = 100
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 2, simple_daily_cost = TRUE)
  expect_false("n_administrations" %in% names(out))
})


# ===========================================================================
# Oral wastage path
# ===========================================================================

test_that("oral wastage: dispensing counts match analytic formula", {
  reg <- define_regimen(admin_days = 1L, 
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
  reg <- define_regimen(admin_days = 1L, 
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
    admin_days = 1L
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
    admin_days = 1L
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7, n_cycles = 3)
  # dispensing cost = 1 admin * $10 = $10
  expected_drug_cost <- out$n_dispensing_events * 10
  expect_equal(out$drug_cost, expected_drug_cost)
})

test_that("oral wastage: treatment cap prevents dispensing beyond cap", {
  reg <- define_regimen(admin_days = 1L, 
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
    admin_days = 1L
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
    admin_days = 1L
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
    admin_days = 1L
  )
  out_analytic <- calculate_med_costs(reg, model_cycle_length = 21,
    n_cycles = 3, subcycle_precision = FALSE)
  out_precise  <- calculate_med_costs(reg, model_cycle_length = 21,
    n_cycles = 3, subcycle_precision = TRUE)
  expect_equal(out_analytic$n_administrations, out_precise$n_administrations)
  expect_equal(out_analytic$total_cost, out_precise$total_cost)
})

test_that("per-admin: treatment cap zeroes out costs beyond cap", {
  reg <- define_regimen(admin_days = 1L, 
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
  reg <- define_regimen(admin_days = 1L, 
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 3, daily_detail = TRUE)
  expect_equal(nrow(out), 21)
})

test_that("daily detail IV: has vials_used, mg_wasted, mg_from_vials columns", {
  reg <- define_regimen(admin_days = 1L, 
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

test_that("daily detail oral no-wastage: no dispensing/waste columns", {
  reg <- define_regimen(admin_days = 1L, 
    route = "oral",
    oral_wastage = FALSE,
    dose_per_admin = 100,
    tablet_strength = 50,
    unit_cost = 10,
    med_cycle_length = 7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 2, daily_detail = TRUE)
  expect_false("tabs_taken" %in% names(out))
  expect_false("mg_dispensed" %in% names(out))
  expect_false("mg_wasted" %in% names(out))
  expected_cols <- c("day", "model_cycle", "med_cycle", "med_cycle_day",
    "doses_taken", "mg_taken", "drug_cost", "admin_cost", "total_cost")
  expect_equal(names(out), expected_cols)
})

test_that("daily detail oral wastage: is_dispensing TRUE only at med cycle starts", {
  reg <- define_regimen(admin_days = 1L, 
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 100,
    unit_cost = 10,
    med_cycle_length = 7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 3, daily_detail = TRUE)
  disp_days <- out$day[out$doses_dispensed > 0]
  # Med cycle starts at days 0, 7, 14
  expect_equal(disp_days, c(0, 7, 14))
})

test_that("daily detail oral wastage: mg_dispensed shows full cycle amount on dispensing days only", {
  reg <- define_regimen( 
    route = "oral",
    dose_per_admin = 60,
    tablet_strength = 60,
    unit_cost = 10,
    med_cycle_length = 21,
    admin_days = 1:7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 3, daily_detail = TRUE)
  full_cycle_mg <- 60 * 7
  disp_rows <- out[out$doses_dispensed > 0, ]
  non_disp_rows <- out[out$doses_dispensed == 0, ]
  expect_true(all(disp_rows$mg_dispensed == full_cycle_mg))
  expect_true(all(non_disp_rows$mg_dispensed == 0))
})

test_that("daily detail: aggregating daily drug_cost by model_cycle matches non-daily", {
  reg <- define_regimen(admin_days = 1L, 
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
  reg <- define_regimen(admin_days = 1L, 
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
# Oral no-wastage admin_cost: charged per dispensing, not per admin
# ===========================================================================

test_that("oral no-wastage: admin_cost charged once per med cycle, not per admin", {
  reg <- define_regimen( 
    route = "oral",
    oral_wastage = FALSE,
    dose_per_admin = 100,
    tablet_strength = 100,
    unit_cost = 10,
    admin_cost = 25,
    med_cycle_length = 28,
    admin_days = 1:28
  )
  out <- calculate_med_costs(reg, model_cycle_length = 28,
    n_cycles = 3, subcycle_precision = TRUE)
  # One dispensing event per 28-day cycle = $25 admin cost, not 28 * $25
  expect_equal(out$admin_cost, rep(25, 3))
})

test_that("oral no-wastage analytic: admin_cost charged once per med cycle", {
  reg <- define_regimen( 
    route = "oral",
    oral_wastage = FALSE,
    dose_per_admin = 100,
    tablet_strength = 100,
    unit_cost = 10,
    admin_cost = 25,
    med_cycle_length = 28,
    admin_days = 1:28
  )
  out <- calculate_med_costs(reg, model_cycle_length = 28,
    n_cycles = 3, subcycle_precision = FALSE)
  expect_equal(out$admin_cost, rep(25, 3))
})

test_that("oral no-wastage daily: admin_cost non-zero only on day 1 of each med cycle", {
  reg <- define_regimen( 
    route = "oral",
    oral_wastage = FALSE,
    dose_per_admin = 100,
    tablet_strength = 100,
    unit_cost = 10,
    admin_cost = 25,
    med_cycle_length = 7,
    admin_days = 1:7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 2, daily_detail = TRUE)
  # Admin cost should only be non-zero on day 0 and day 7 (med cycle starts)
  admin_days <- out$day[out$admin_cost > 0]
  expect_equal(admin_days, c(0, 7))
  expect_equal(out$admin_cost[out$day == 0], 25)
  expect_equal(out$admin_cost[out$day == 1], 0)
})

test_that("oral no-wastage daily: total_cost = drug_cost + admin_cost per day", {
  reg <- define_regimen( 
    route = "oral",
    oral_wastage = FALSE,
    dose_per_admin = 100,
    tablet_strength = 100,
    unit_cost = 10,
    admin_cost = 25,
    med_cycle_length = 7,
    admin_days = 1:7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 2, daily_detail = TRUE)
  expect_equal(out$total_cost, out$drug_cost + out$admin_cost)
})

test_that("daily detail: no is_admin column in output", {
  # IV
  reg_iv <- define_regimen(admin_days = 1L, 
    vial_size = 100, vial_cost = 500, med_cycle_length = 7
  )
  out_iv <- calculate_med_costs(reg_iv, model_cycle_length = 7,
    n_cycles = 2, daily_detail = TRUE)
  expect_false("is_admin" %in% names(out_iv))

  # Oral wastage
  reg_ow <- define_regimen(admin_days = 1L, 
    route = "oral", dose_per_admin = 100,
    tablet_strength = 100, unit_cost = 10, med_cycle_length = 7
  )
  out_ow <- calculate_med_costs(reg_ow, model_cycle_length = 7,
    n_cycles = 2, daily_detail = TRUE)
  expect_false("is_admin" %in% names(out_ow))

  # Oral no-wastage
  reg_onw <- define_regimen(admin_days = 1L, 
    route = "oral", oral_wastage = FALSE, dose_per_admin = 100,
    tablet_strength = 100, unit_cost = 10, med_cycle_length = 7
  )
  out_onw <- calculate_med_costs(reg_onw, model_cycle_length = 7,
    n_cycles = 2, daily_detail = TRUE)
  expect_false("is_admin" %in% names(out_onw))
})


# ===========================================================================
# Cross-cutting
# ===========================================================================

test_that("daily_detail = TRUE forces subcycle_precision", {
  reg <- define_regimen(admin_days = 1L, 
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
  reg <- define_regimen(admin_days = 1L, 
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7, n_cycles = 1)
  expect_equal(nrow(out), 1)
  expect_equal(out$n_administrations, 1)
})

test_that("med cycle longer than model cycle works correctly", {
  reg <- define_regimen(admin_days = 1L, 
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


# ===========================================================================
# Distribution-averaged cost integration
# ===========================================================================

test_that("distribution cost flows through calculate_med_costs and differs from point estimate", {
  # Mean dose = 4 * 75 = 300mg, exactly at vial boundary
  reg_dist <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 4,
    dose_basis = "weight",
    patient_weight = 75,
    patient_weight_sd = 15,
    vial_size = 100,
    vial_cost = 500
  )
  out_dist <- calculate_med_costs(reg_dist, n_cycles = 3)

  reg_point <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 4,
    dose_basis = "weight",
    patient_weight = 75,
    vial_size = 100,
    vial_cost = 500
  )
  out_point <- calculate_med_costs(reg_point, n_cycles = 3)

  # Output structure should be the same
  expect_equal(names(out_dist), names(out_point))
  expect_equal(nrow(out_dist), nrow(out_point))

  # Drug costs should differ due to distribution averaging
  expect_false(all(out_dist$drug_cost == out_point$drug_cost))
})


# ===========================================================================
# start_day parameter
# ===========================================================================

test_that("start_day = 0 is backward-compatible: identical to omitting it", {
  reg_default <- define_regimen(admin_days = 1L, vial_size = 100, vial_cost = 500,
    med_cycle_length = 7)
  reg_zero <- define_regimen(admin_days = 1L, vial_size = 100, vial_cost = 500,
    med_cycle_length = 7, start_day = 0)
  out_default <- calculate_med_costs(reg_default, model_cycle_length = 7,
    n_cycles = 4, subcycle_precision = TRUE)
  out_zero <- calculate_med_costs(reg_zero, model_cycle_length = 7,
    n_cycles = 4, subcycle_precision = TRUE)
  expect_equal(out_default, out_zero)
})

test_that("per-admin: cycles before start_day get 0 admins", {
  reg <- define_regimen(admin_days = 1L, 
    vial_size = 100, vial_cost = 500,
    med_cycle_length = 7, start_day = 14
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 6, subcycle_precision = TRUE)
  # Cycles 1-2 (days 0-13) should have 0 admins
  expect_equal(out$n_administrations[1], 0)
  expect_equal(out$n_administrations[2], 0)
  # Cycle 3 (days 14-20) should have first admin
  expect_equal(out$n_administrations[3], 1)
})

test_that("per-admin analytic: start_day shifts admin counts", {
  reg <- define_regimen(admin_days = 1L, 
    vial_size = 100, vial_cost = 500,
    med_cycle_length = 7, start_day = 14
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 6, subcycle_precision = FALSE)
  expect_equal(out$n_administrations[1], 0)
  expect_equal(out$n_administrations[2], 0)
  expect_equal(out$n_administrations[3], 1)
})

test_that("per-admin: start_day + max_med_cycles limits treatment window", {
  reg <- define_regimen(admin_days = 1L, 
    vial_size = 100, vial_cost = 500,
    med_cycle_length = 7, start_day = 7, max_med_cycles = 2
  )
  # Treatment window: day 7 to day 21
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 6, subcycle_precision = TRUE)
  expect_equal(out$n_administrations[1], 0)  # days 0-6
  expect_equal(out$n_administrations[2], 1)  # days 7-13
  expect_equal(out$n_administrations[3], 1)  # days 14-20
  expect_equal(out$n_administrations[4], 0)  # days 21-27 (past treat_end)
})

test_that("oral wastage: dispensing events shift with start_day", {
  reg <- define_regimen(admin_days = 1L, 
    route = "oral", dose_per_admin = 100,
    tablet_strength = 100, unit_cost = 10,
    med_cycle_length = 7, start_day = 14
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 6, subcycle_precision = TRUE)
  # Dispensing at day 14 (cycle 3), day 21 (cycle 4), etc.
  expect_equal(out$n_dispensing_events[1], 0)
  expect_equal(out$n_dispensing_events[2], 0)
  expect_equal(out$n_dispensing_events[3], 1)
  expect_equal(out$n_dispensing_events[4], 1)
})

test_that("oral wastage analytic: start_day shifts dispensing counts", {
  reg <- define_regimen(admin_days = 1L, 
    route = "oral", dose_per_admin = 100,
    tablet_strength = 100, unit_cost = 10,
    med_cycle_length = 7, start_day = 14
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 6, subcycle_precision = FALSE)
  expect_equal(out$n_dispensing_events[1], 0)
  expect_equal(out$n_dispensing_events[2], 0)
  expect_equal(out$n_dispensing_events[3], 1)
})

test_that("daily detail per-admin: med_cycle is NA and costs are 0 before start_day", {
  reg <- define_regimen(admin_days = 1L, 
    vial_size = 100, vial_cost = 500,
    med_cycle_length = 7, start_day = 7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 3, daily_detail = TRUE)
  before <- out[out$day < 7, ]
  expect_true(all(is.na(before$med_cycle)))
  expect_true(all(before$drug_cost == 0))
  expect_true(all(before$total_cost == 0))
})

test_that("daily detail oral wastage: no dispensing before start_day", {
  reg <- define_regimen(admin_days = 1L, 
    route = "oral", dose_per_admin = 100,
    tablet_strength = 100, unit_cost = 10,
    med_cycle_length = 7, start_day = 7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 3, daily_detail = TRUE)
  before <- out[out$day < 7, ]
  expect_true(all(before$doses_dispensed == 0))
  expect_true(all(is.na(before$med_cycle)))
  expect_true(all(before$drug_cost == 0))
})

test_that("simple daily cost: start_day zeroes out early cycles", {
  reg <- define_regimen(admin_days = 1L, 
    route = "iv", cost_per_day = 100,
    start_day = 14
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 4, simple_daily_cost = TRUE)
  # Cycles 1-2 (days 0-13) entirely before start_day
  expect_equal(out$drug_cost[1], 0)
  expect_equal(out$drug_cost[2], 0)
  # Cycles 3-4 should have costs
  expect_equal(out$drug_cost[3], 700)
  expect_equal(out$drug_cost[4], 700)
})


# ===========================================================================
# SC / IM routes
# ===========================================================================

test_that("SC per-admin path produces correct admin counts", {
  reg <- define_regimen(admin_days = 1L, 
    route = "sc",
    dose_per_admin = 100,
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 3, subcycle_precision = TRUE)
  expect_equal(out$n_administrations, c(1, 1, 1))
})

test_that("SC total_cost = drug_cost + admin_cost", {
  reg <- define_regimen(admin_days = 1L, 
    route = "sc",
    dose_per_admin = 100,
    vial_size = 100,
    vial_cost = 500,
    admin_cost = 50,
    med_cycle_length = 7
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7, n_cycles = 3)
  expect_equal(out$total_cost, out$drug_cost + out$admin_cost)
})

test_that("SC daily detail has vial columns", {
  reg <- define_regimen(admin_days = 1L, 
    route = "sc",
    dose_per_admin = 100,
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

test_that("IM drug_cost matches equivalent IV setup", {
  reg_im <- define_regimen(admin_days = 1L, 
    route = "im",
    dose_per_admin = 100,
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7
  )
  reg_iv <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 100,
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7
  )
  out_im <- calculate_med_costs(reg_im, model_cycle_length = 7, n_cycles = 3)
  out_iv <- calculate_med_costs(reg_iv, model_cycle_length = 7, n_cycles = 3)
  expect_equal(out_im$drug_cost, out_iv$drug_cost)
})


# ===========================================================================
# Multiple regimens
# ===========================================================================

test_that("multi-regimen: errors on non-regimen in ...", {
  reg <- define_regimen(admin_days = 1L, vial_size = 100, vial_cost = 500)
  expect_error(
    calculate_med_costs(reg, "not_a_regimen", n_cycles = 3),
    "med_regimen"
  )
})

test_that("multi-regimen: wide-format column naming", {
  reg_a <- define_regimen(admin_days = 1L, 
    name = "Drug A", vial_size = 100, vial_cost = 500
  )
  reg_b <- define_regimen(admin_days = 1L, 
    name = "Drug B", vial_size = 50, vial_cost = 200
  )
  out <- calculate_med_costs(reg_a, reg_b, n_cycles = 3)
  expect_true("Drug_A_drug_cost" %in% names(out))
  expect_true("Drug_A_admin_cost" %in% names(out))
  expect_true("Drug_A_total_cost" %in% names(out))
  expect_true("Drug_B_drug_cost" %in% names(out))
  expect_true("Drug_B_admin_cost" %in% names(out))
  expect_true("Drug_B_total_cost" %in% names(out))
  expect_true("total_cost" %in% names(out))
})

test_that("multi-regimen: total_cost = sum of per-drug total_cost columns", {
  reg_a <- define_regimen(admin_days = 1L, 
    name = "Drug A", vial_size = 100, vial_cost = 500
  )
  reg_b <- define_regimen(admin_days = 1L, 
    name = "Drug B", vial_size = 50, vial_cost = 200
  )
  out <- calculate_med_costs(reg_a, reg_b, n_cycles = 5)
  expect_equal(
    out$total_cost,
    out$Drug_A_total_cost + out$Drug_B_total_cost
  )
})

test_that("multi-regimen: per-drug costs match individual single-regimen calls", {
  reg_a <- define_regimen(admin_days = 1L, 
    name = "Drug A", vial_size = 100, vial_cost = 500, admin_cost = 150
  )
  reg_b <- define_regimen(admin_days = 1L, 
    name = "Drug B", route = "oral", dose_per_admin = 200,
    tablet_strength = 100, unit_cost = 20
  )
  combo <- calculate_med_costs(
    reg_a, reg_b, model_cycle_length = 7, n_cycles = 4
  )
  indiv_a <- calculate_med_costs(reg_a, model_cycle_length = 7, n_cycles = 4)
  indiv_b <- calculate_med_costs(reg_b, model_cycle_length = 7, n_cycles = 4)
  expect_equal(combo$Drug_A_total_cost, indiv_a$total_cost)
  expect_equal(combo$Drug_B_total_cost, indiv_b$total_cost)
})

test_that("multi-regimen: daily_detail returns wide-format data.frame", {
  reg_a <- define_regimen(admin_days = 1L, 
    name = "Drug A", vial_size = 100, vial_cost = 500,
    med_cycle_length = 7
  )
  reg_b <- define_regimen(admin_days = 1L, 
    name = "Drug B", route = "oral", dose_per_admin = 100,
    tablet_strength = 100, unit_cost = 10, med_cycle_length = 7
  )
  out <- calculate_med_costs(
    reg_a, reg_b, model_cycle_length = 7, n_cycles = 2,
    daily_detail = TRUE
  )
  expect_s3_class(out, "combo_regimen_daily_detail")
  expect_true(is.data.frame(out))
  expect_true("day" %in% names(out))
  expect_true("model_cycle" %in% names(out))
  expect_true("total_cost" %in% names(out))
  expect_true(any(grepl("^Drug_A_", names(out))))
  expect_true(any(grepl("^Drug_B_", names(out))))
  expect_equal(attr(out, "drug_names"), c("Drug A", "Drug B"))
  expect_true(is.list(attr(out, "drug_columns")))
})

test_that("multi-regimen: three drugs aggregate correctly", {
  reg_a <- define_regimen(admin_days = 1L, name = "A", vial_size = 100, vial_cost = 500)
  reg_b <- define_regimen(admin_days = 1L, name = "B", vial_size = 50, vial_cost = 200)
  reg_c <- define_regimen(admin_days = 1L, name = "C", vial_size = 25, vial_cost = 100)
  out <- calculate_med_costs(reg_a, reg_b, reg_c, n_cycles = 3)
  expect_equal(
    out$total_cost,
    out$A_total_cost + out$B_total_cost + out$C_total_cost
  )
})

test_that("multi-regimen: drug name sanitization", {
  reg <- define_regimen(admin_days = 1L, 
    name = "Drug-A (combo)", vial_size = 100, vial_cost = 500
  )
  reg2 <- define_regimen(admin_days = 1L, 
    name = "Drug B", vial_size = 50, vial_cost = 200
  )
  out <- calculate_med_costs(reg, reg2, n_cycles = 3)
  sanitized <- gsub("[^a-zA-Z0-9]", "_", "Drug-A (combo)")
  expect_true(paste0(sanitized, "_total_cost") %in% names(out))
})


# ===========================================================================
# Shared administration costs
# ===========================================================================

test_that("shared_admin: validation errors on unknown drug name", {
  reg_a <- define_regimen(admin_days = 1L, name = "Drug A", vial_size = 100, vial_cost = 500,
    admin_cost = 100)
  reg_b <- define_regimen(admin_days = 1L, name = "Drug B", vial_size = 50, vial_cost = 200,
    admin_cost = 50)
  expect_error(
    calculate_med_costs(reg_a, reg_b, n_cycles = 3,
      shared_admin = list(c("Drug A", "Drug X"))),
    "Unknown drug name"
  )
})

test_that("shared_admin: validation errors on drug in multiple groups", {
  reg_a <- define_regimen(admin_days = 1L, name = "Drug A", vial_size = 100, vial_cost = 500,
    admin_cost = 100)
  reg_b <- define_regimen(admin_days = 1L, name = "Drug B", vial_size = 50, vial_cost = 200,
    admin_cost = 50)
  reg_c <- define_regimen(admin_days = 1L, name = "Drug C", vial_size = 25, vial_cost = 100,
    admin_cost = 75)
  expect_error(
    calculate_med_costs(reg_a, reg_b, reg_c, n_cycles = 3,
      shared_admin = list(c("Drug A", "Drug B"), c("Drug A", "Drug C"))),
    "multiple shared_admin groups"
  )
})

test_that("shared_admin: validation errors on group with < 2 drugs", {
  reg_a <- define_regimen(admin_days = 1L, name = "Drug A", vial_size = 100, vial_cost = 500,
    admin_cost = 100)
  reg_b <- define_regimen(admin_days = 1L, name = "Drug B", vial_size = 50, vial_cost = 200,
    admin_cost = 50)
  expect_error(
    calculate_med_costs(reg_a, reg_b, n_cycles = 3,
      shared_admin = list(c("Drug A"))),
    "at least 2 drugs"
  )
})

test_that("shared_admin: single regimen warns and ignores", {
  reg <- define_regimen(admin_days = 1L, vial_size = 100, vial_cost = 500, admin_cost = 100)
  expect_warning(
    calculate_med_costs(reg, n_cycles = 3, shared_admin = TRUE),
    "ignored for single-regimen"
  )
})

test_that("shared_admin = TRUE: same-day drugs share, highest wins", {
  # Both drugs admin on day 1 of 21-day cycle, same schedule
  reg_a <- define_regimen( 
    name = "Drug A", vial_size = 100, vial_cost = 500,
    admin_cost = 100, med_cycle_length = 21, admin_days = 1
  )
  reg_b <- define_regimen( 
    name = "Drug B", vial_size = 50, vial_cost = 200,
    admin_cost = 50, med_cycle_length = 21, admin_days = 1
  )

  out_shared <- calculate_med_costs(
    reg_a, reg_b, model_cycle_length = 21, n_cycles = 3,
    shared_admin = TRUE
  )
  out_no_share <- calculate_med_costs(
    reg_a, reg_b, model_cycle_length = 21, n_cycles = 3
  )

  # With sharing, only the higher admin cost ($100) should be charged
  # Without sharing: $100 + $50 = $150 per cycle
  # With sharing: $100 per cycle (Drug A wins)
  expect_equal(out_shared$Drug_A_admin_cost, rep(100, 3))
  expect_equal(out_shared$Drug_B_admin_cost, rep(0, 3))

  # Total cost should be less with sharing
  expect_true(all(out_shared$total_cost < out_no_share$total_cost))

  # Savings should equal Drug B's admin cost each cycle
  savings <- out_no_share$total_cost - out_shared$total_cost
  expect_equal(savings, rep(50, 3))
})

test_that("shared_admin = TRUE: pre_sharing columns present at cycle level", {
  reg_a <- define_regimen( 
    name = "Drug A", vial_size = 100, vial_cost = 500,
    admin_cost = 100, med_cycle_length = 21, admin_days = 1
  )
  reg_b <- define_regimen( 
    name = "Drug B", vial_size = 50, vial_cost = 200,
    admin_cost = 50, med_cycle_length = 21, admin_days = 1
  )
  out <- calculate_med_costs(
    reg_a, reg_b, model_cycle_length = 21, n_cycles = 3,
    shared_admin = TRUE
  )
  expect_true("Drug_A_pre_sharing_admin_cost" %in% names(out))
  expect_true("Drug_B_pre_sharing_admin_cost" %in% names(out))
  expect_equal(out$Drug_A_pre_sharing_admin_cost, rep(100, 3))
  expect_equal(out$Drug_B_pre_sharing_admin_cost, rep(50, 3))
})

test_that("shared_admin: daily detail shows per-day sharing with diagnostic columns", {
  reg_a <- define_regimen( 
    name = "Drug A", vial_size = 100, vial_cost = 500,
    admin_cost = 100, med_cycle_length = 21, admin_days = 1
  )
  reg_b <- define_regimen( 
    name = "Drug B", vial_size = 50, vial_cost = 200,
    admin_cost = 50, med_cycle_length = 21, admin_days = 1
  )
  out <- calculate_med_costs(
    reg_a, reg_b, model_cycle_length = 21, n_cycles = 2,
    daily_detail = TRUE, shared_admin = TRUE
  )

  expect_s3_class(out, "combo_regimen_daily_detail")
  expect_true("Drug_A_pre_sharing_admin_cost" %in% names(out))
  expect_true("Drug_B_pre_sharing_admin_cost" %in% names(out))

  # On admin day (day 0): Drug A pre-sharing = 100, Drug B pre-sharing = 50
  day0 <- out[out$day == 0, ]
  expect_equal(day0$Drug_A_pre_sharing_admin_cost, 100)
  expect_equal(day0$Drug_B_pre_sharing_admin_cost, 50)
  # After sharing: Drug A keeps 100, Drug B zeroed
  expect_equal(day0$Drug_A_admin_cost, 100)
  expect_equal(day0$Drug_B_admin_cost, 0)

  # drug_columns attribute includes pre_sharing columns
  dcols <- attr(out, "drug_columns")
  expect_true("Drug_A_pre_sharing_admin_cost" %in% dcols[["Drug A"]])
  expect_true("Drug_B_pre_sharing_admin_cost" %in% dcols[["Drug B"]])
})

test_that("shared_admin: no overlap means sharing has no effect", {
  # Drug A on day 1, Drug B on day 8 — never overlap
  reg_a <- define_regimen( 
    name = "Drug A", vial_size = 100, vial_cost = 500,
    admin_cost = 100, med_cycle_length = 21, admin_days = 1
  )
  reg_b <- define_regimen( 
    name = "Drug B", vial_size = 50, vial_cost = 200,
    admin_cost = 50, med_cycle_length = 21, admin_days = 8
  )
  out_shared <- calculate_med_costs(
    reg_a, reg_b, model_cycle_length = 21, n_cycles = 3,
    shared_admin = TRUE
  )
  out_no_share <- calculate_med_costs(
    reg_a, reg_b, model_cycle_length = 21, n_cycles = 3
  )
  # Total costs should be equal since drugs never overlap
  expect_equal(out_shared$total_cost, out_no_share$total_cost)
})

test_that("shared_admin: partial groups — 3 drugs, only 2 sharing", {
  reg_a <- define_regimen( 
    name = "Drug A", vial_size = 100, vial_cost = 500,
    admin_cost = 100, med_cycle_length = 21, admin_days = 1
  )
  reg_b <- define_regimen( 
    name = "Drug B", vial_size = 50, vial_cost = 200,
    admin_cost = 50, med_cycle_length = 21, admin_days = 1
  )
  reg_c <- define_regimen( 
    name = "Drug C", vial_size = 25, vial_cost = 100,
    admin_cost = 75, med_cycle_length = 21, admin_days = 1
  )
  out <- calculate_med_costs(
    reg_a, reg_b, reg_c, model_cycle_length = 21, n_cycles = 3,
    shared_admin = list(c("Drug A", "Drug B"))
  )

  # Drug A & B share: A wins ($100 vs $50), B zeroed
  expect_equal(out$Drug_A_admin_cost, rep(100, 3))
  expect_equal(out$Drug_B_admin_cost, rep(0, 3))

  # Drug C is independent, keeps its admin cost
  expect_equal(out$Drug_C_admin_cost, rep(75, 3))

  # Pre-sharing columns only for grouped drugs (A and B), not C
  expect_true("Drug_A_pre_sharing_admin_cost" %in% names(out))
  expect_true("Drug_B_pre_sharing_admin_cost" %in% names(out))
  expect_false("Drug_C_pre_sharing_admin_cost" %in% names(out))
})

test_that("shared_admin = NULL: backward compatible, no pre_sharing columns", {
  reg_a <- define_regimen(admin_days = 1L, 
    name = "Drug A", vial_size = 100, vial_cost = 500,
    admin_cost = 100, med_cycle_length = 21
  )
  reg_b <- define_regimen(admin_days = 1L, 
    name = "Drug B", vial_size = 50, vial_cost = 200,
    admin_cost = 50, med_cycle_length = 21
  )
  out <- calculate_med_costs(reg_a, reg_b, model_cycle_length = 21, n_cycles = 3)
  expect_false(any(grepl("pre_sharing", names(out))))
})

test_that("shared_admin: class and attributes set correctly", {
  reg_a <- define_regimen( 
    name = "Drug A", vial_size = 100, vial_cost = 500,
    admin_cost = 100, med_cycle_length = 21, admin_days = 1
  )
  reg_b <- define_regimen( 
    name = "Drug B", vial_size = 50, vial_cost = 200,
    admin_cost = 50, med_cycle_length = 21, admin_days = 1
  )

  # Cycle-level
  out_cycle <- calculate_med_costs(
    reg_a, reg_b, model_cycle_length = 21, n_cycles = 3,
    shared_admin = TRUE
  )
  expect_s3_class(out_cycle, "combo_regimen_costs")
  expect_equal(attr(out_cycle, "drug_names"), c("Drug A", "Drug B"))
  expect_true(!is.null(attr(out_cycle, "shared_admin")))

  # Daily
  out_daily <- calculate_med_costs(
    reg_a, reg_b, model_cycle_length = 21, n_cycles = 2,
    daily_detail = TRUE, shared_admin = TRUE
  )
  expect_s3_class(out_daily, "combo_regimen_daily_detail")
  expect_equal(attr(out_daily, "drug_names"), c("Drug A", "Drug B"))
  expect_true(!is.null(attr(out_daily, "shared_admin")))
})

test_that("shared_admin: tied admin costs resolved by input order (first wins)", {
  reg_a <- define_regimen( 
    name = "Drug A", vial_size = 100, vial_cost = 500,
    admin_cost = 100, med_cycle_length = 21, admin_days = 1
  )
  reg_b <- define_regimen( 
    name = "Drug B", vial_size = 50, vial_cost = 200,
    admin_cost = 100, med_cycle_length = 21, admin_days = 1
  )
  out <- calculate_med_costs(
    reg_a, reg_b, model_cycle_length = 21, n_cycles = 3,
    daily_detail = TRUE, shared_admin = TRUE
  )
  day0 <- out[out$day == 0, ]
  # Both have $100 admin — first (Drug A) should win
  expect_equal(day0$Drug_A_admin_cost, 100)
  expect_equal(day0$Drug_B_admin_cost, 0)
})


# ===========================================================================
# Non-uniform cycle_times
# ===========================================================================

test_that("non-uniform cycle_times: correct cycle_end_day values", {
  reg <- define_regimen(admin_days = 1L, 
    vial_size = 100, vial_cost = 500,
    med_cycle_length = 7
  )
  out <- calculate_med_costs(
    reg, cycle_times = c(0, 7, 14, 28),
    subcycle_precision = TRUE
  )
  expect_equal(out$cycle_end_day, c(7, 14, 28, 42))
})

test_that("non-uniform cycle_times: correct admin counts for longer cycle", {
  # 7-day med cycle, admin on day 1 only
  # cycle_times = c(0, 7, 14, 28): cycle 3 spans days 14-28 (14 days)
  # => 2 med cycles fit in that model cycle => 2 admins
  reg <- define_regimen( 
    vial_size = 100, vial_cost = 500,
    med_cycle_length = 7, admin_days = 1
  )
  out <- calculate_med_costs(
    reg, cycle_times = c(0, 7, 14, 28),
    subcycle_precision = TRUE
  )
  expect_equal(out$n_administrations[3], 2)
})

test_that("non-uniform cycle_times: simple daily cost uses per-cycle lengths", {
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    cost_per_day = 10
  )
  out <- calculate_med_costs(
    reg, cycle_times = c(0, 7, 14, 28),
    simple_daily_cost = TRUE
  )
  expect_equal(out$drug_cost, c(70, 70, 140, 140))
})
