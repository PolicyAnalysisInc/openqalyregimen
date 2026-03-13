test_that("detail_schedule returns a data.frame", {
  reg <- define_regimen(vial_size = 100, vial_cost = 500, admin_days = 1L)
  out <- detail_schedule(reg, n_cycles = 5)
  expect_s3_class(out, "data.frame")
})

test_that("detail_schedule has all 8 required columns", {
  reg <- define_regimen(vial_size = 100, vial_cost = 500, admin_days = 1L)
  out <- detail_schedule(reg, n_cycles = 5)
  expected_cols <- c("day", "med_cycle", "med_cycle_day",
    "model_cycle", "event_type", "drug_cost",
    "admin_cost", "total_cost")
  expect_true(all(expected_cols %in% names(out)))
})

test_that("detail_schedule total_cost equals drug_cost + admin_cost", {
  reg <- define_regimen(vial_size = 100, vial_cost = 500, admin_days = 1L)
  out <- detail_schedule(reg, n_cycles = 5)
  expect_equal(out$total_cost, out$drug_cost + out$admin_cost)
})

test_that("detail_schedule errors on non-med_regimen input", {
  expect_error(detail_schedule(list(a = 1)))
})

test_that("oral wastage: has both 'dispensing' and 'administration' event types", {
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 100,
    unit_cost = 10,
    med_cycle_length = 7,
    admin_days = 1L
  )
  out <- detail_schedule(reg, n_cycles = 3)
  expect_true("dispensing" %in% out$event_type)
  expect_true("administration" %in% out$event_type)
})

test_that("oral wastage: dispensing events have positive cost, admin events have zero cost", {
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 100,
    unit_cost = 10,
    med_cycle_length = 7,
    admin_days = 1L
  )
  out <- detail_schedule(reg, n_cycles = 3)
  disp_rows <- out[out$event_type == "dispensing", ]
  admin_rows <- out[out$event_type == "administration", ]
  expect_true(all(disp_rows$drug_cost > 0))
  expect_true(all(admin_rows$drug_cost == 0))
  expect_true(all(admin_rows$total_cost == 0))
})

test_that("IV: only 'administration' events, each with positive cost", {
  reg <- define_regimen(
    route = "iv",
    dose_per_admin = 100,
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7,
    admin_days = 1L
  )
  out <- detail_schedule(reg, n_cycles = 3)
  expect_true(all(out$event_type == "administration"))
  expect_true(all(out$drug_cost > 0))
})

test_that("event count matches expected (K dispensing + K*D admin for oral wastage)", {
  # med_cycle_length=7, one administration day, 3 model cycles of 7 days
  # 3 med cycles => K=3 dispensing + 3*1=3 admin = 6 events
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 100,
    unit_cost = 10,
    med_cycle_length = 7,
    admin_days = 1L
  )
  out <- detail_schedule(reg, model_cycle_length = 7, n_cycles = 3)
  expect_equal(sum(out$event_type == "dispensing"), 3)
  expect_equal(sum(out$event_type == "administration"), 3)
  expect_equal(nrow(out), 6)
})

test_that("max_med_cycles caps events", {
  reg <- define_regimen(
    route = "iv",
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7,
    max_med_cycles = 2,
    admin_days = 1L
  )
  out <- detail_schedule(reg, model_cycle_length = 7, n_cycles = 5)
  # Only 2 med cycles, so only 2 admin events
  expect_equal(nrow(out), 2)
  # No events beyond day 14 (2 * 7)
  expect_true(all(out$day < 14))
})

test_that("event days calculated correctly: (mc-1)*mcl + (ad-1)", {
  reg <- define_regimen(
    route = "iv",
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 21,
    admin_days = c(1L, 8L, 15L)
  )
  out <- detail_schedule(reg, model_cycle_length = 21, n_cycles = 2)
  # Med cycle 1: days 0, 7, 14
  # Med cycle 2: days 21, 28, 35
  expect_equal(out$day, c(0, 7, 14, 21, 28, 35))
})

test_that("model_cycle assignment matches findInterval on cycle_starts", {
  reg <- define_regimen(
    route = "iv",
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 21,
    admin_days = 1L
  )
  out <- detail_schedule(reg, model_cycle_length = 7, n_cycles = 6)
  # Admin days: 0, 21
  # cycle_starts: 0, 7, 14, 21, 28, 35
  # Day 0 -> interval 1, Day 21 -> interval 4
  expect_equal(out$model_cycle, c(1, 4))
})

test_that("cycle_times parameter works (custom cycle boundaries)", {
  reg <- define_regimen(
    route = "iv",
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 10,
    admin_days = 1L
  )
  out <- detail_schedule(reg, cycle_times = c(0, 10, 20, 30))
  # Admin at days 0, 10, 20, 30 (horizon = 40)
  expect_equal(out$day, c(0, 10, 20, 30))
  expect_equal(out$model_cycle, c(1, 2, 3, 4))
})

test_that("dispensing events precede administration events within each med cycle", {
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 100,
    unit_cost = 10,
    med_cycle_length = 7,
    admin_days = c(1L, 4L)
  )
  out <- detail_schedule(reg, model_cycle_length = 7, n_cycles = 2)
  for (mc in unique(out$med_cycle)) {
    mc_events <- out[out$med_cycle == mc, ]
    disp_idx <- which(mc_events$event_type == "dispensing")
    admin_idx <- which(mc_events$event_type == "administration")
    if (length(disp_idx) > 0 && length(admin_idx) > 0) {
      expect_true(max(disp_idx) < min(admin_idx))
    }
  }
})


# ===========================================================================
# start_day parameter
# ===========================================================================

test_that("start_day shifts all events; no events before start_day", {
  reg <- define_regimen(
    route = "iv",
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7,
    start_day = 14,
    admin_days = 1L
  )
  out <- detail_schedule(reg, model_cycle_length = 7, n_cycles = 6)
  # No events before day 14

  expect_true(all(out$day >= 14))
  # First event at day 14
  expect_equal(min(out$day), 14)
})

test_that("start_day + max_med_cycles caps events correctly", {
  reg <- define_regimen(
    route = "iv",
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7,
    max_med_cycles = 2,
    start_day = 7,
    admin_days = 1L
  )
  out <- detail_schedule(reg, model_cycle_length = 7, n_cycles = 6)
  # Treatment from day 7 to day 21: admins at day 7, day 14
  expect_equal(nrow(out), 2)
  expect_equal(out$day, c(7, 14))
})

test_that("start_day = 0 gives same results as default", {
  reg_default <- define_regimen(
    route = "iv",
    vial_size = 100, vial_cost = 500,
    med_cycle_length = 7,
    admin_days = 1L
  )
  reg_zero <- define_regimen(
    route = "iv",
    vial_size = 100, vial_cost = 500,
    med_cycle_length = 7, start_day = 0, admin_days = 1L
  )
  out_default <- detail_schedule(reg_default, model_cycle_length = 7,
    n_cycles = 4)
  out_zero <- detail_schedule(reg_zero, model_cycle_length = 7,
    n_cycles = 4)
  expect_equal(out_default, out_zero)
})


# ===========================================================================
# SC / IM routes
# ===========================================================================

test_that("SC produces only administration events (no dispensing)", {
  reg <- define_regimen(
    route = "sc",
    dose_per_admin = 100,
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7,
    admin_days = 1L
  )
  out <- detail_schedule(reg, model_cycle_length = 7, n_cycles = 3)
  expect_true(all(out$event_type == "administration"))
  expect_true(all(out$drug_cost > 0))
})

test_that("IM produces only administration events (no dispensing)", {
  reg <- define_regimen(
    route = "im",
    dose_per_admin = 100,
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7,
    admin_days = 1L
  )
  out <- detail_schedule(reg, model_cycle_length = 7, n_cycles = 3)
  expect_true(all(out$event_type == "administration"))
})


# ===========================================================================
# Non-uniform cycle_times
# ===========================================================================

test_that("non-uniform cycle_times: correct model_cycle assignments and horizon", {
  reg <- define_regimen(
    route = "iv",
    vial_size = 100,
    vial_cost = 500,
    med_cycle_length = 7,
    admin_days = 1L
  )
  # cycle_times = c(0, 7, 14, 28) => cycle_ends = c(7, 14, 28, 42)
  # horizon = 42, so admins at days 0, 7, 14, 21, 28, 35
  out <- detail_schedule(reg, cycle_times = c(0, 7, 14, 28))
  expect_equal(out$day, c(0, 7, 14, 21, 28, 35))
  # model_cycle: day 0->1, day 7->2, day 14->3, day 21->3, day 28->4, day 35->4
  expect_equal(out$model_cycle, c(1, 2, 3, 3, 4, 4))
})
