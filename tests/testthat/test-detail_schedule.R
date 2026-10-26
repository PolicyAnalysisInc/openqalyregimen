reg_iv <- function(...) {
  args <- list(name = "Drug A", dose_per_admin = 100, unit_size = 100,
               unit_cost = 500, med_cycle_length = 21, admin_days = 1L,
               exact = FALSE, charge_timing = "per_admin")
  over <- list(...)
  args[names(over)] <- over
  do.call(define_regimen, args)
}

reg_oral <- function(...) {
  args <- list(name = "Cape", route = "oral", dose_per_admin = 3600,
               unit_size = c(150, 500), unit_cost = 2.21,
               med_cycle_length = 21, admin_days = 1:14, exact = TRUE,
               charge_timing = "per_dispensing")
  over <- list(...)
  args[names(over)] <- over
  do.call(define_regimen, args)
}

test_that("detail_schedule returns a regimen_schedule with expected columns", {
  s <- detail_schedule(reg_iv(), model_cycle_length = 7, n_cycles = 6)
  expect_s3_class(s, "regimen_schedule")
  expect_equal(names(s), c("day", "med_cycle", "med_cycle_day", "model_cycle",
                           "event_type", "drug_cost", "admin_cost",
                           "dispensing_cost", "total_cost"))
})

test_that("detail_schedule errors on non-med_regimen input", {
  expect_error(detail_schedule(list()))
})

test_that("total_cost = drug + admin + dispensing", {
  s <- detail_schedule(reg_oral(admin_cost = 3, dispensing_cost = 45),
                       model_cycle_length = 7, n_cycles = 6)
  expect_equal(s$total_cost, s$drug_cost + s$admin_cost + s$dispensing_cost)
})

test_that("per_admin with no dispensing fee: administration events only", {
  s <- detail_schedule(reg_iv(admin_cost = 50), model_cycle_length = 7,
                       n_cycles = 9)
  expect_equal(unique(s$event_type), "administration")
  expect_equal(nrow(s), 3)
  expect_true(all(s$drug_cost == 500))
  expect_true(all(s$admin_cost == 50))
})

test_that("per_dispensing: one dispensing event per cycle carrying the drug cost", {
  s <- detail_schedule(reg_oral(), model_cycle_length = 7, n_cycles = 6)
  disp <- s[s$event_type == "dispensing", ]
  adm <- s[s$event_type == "administration", ]
  expect_equal(nrow(disp), 2)
  expect_equal(nrow(adm), 28)
  expect_equal(disp$drug_cost, rep(14 * 22.10, 2))
  expect_true(all(adm$drug_cost == 0))
})

test_that("a dispensing fee adds dispensing events under per_admin", {
  s <- detail_schedule(reg_iv(dispensing_cost = 20), model_cycle_length = 21,
                       n_cycles = 2)
  expect_equal(s$event_type, rep(c("dispensing", "administration"), 2))
  expect_equal(s$dispensing_cost, rep(c(20, 0), 2))
  expect_equal(s$drug_cost, rep(c(0, 500), 2))
})

test_that("dispensing precedes administration on the same day", {
  s <- detail_schedule(reg_oral(), model_cycle_length = 7, n_cycles = 3)
  first <- s[s$day == 0, ]
  expect_equal(first$event_type, c("dispensing", "administration"))
})

test_that("event days follow (mc - 1) * mcl + (ad - 1)", {
  s <- detail_schedule(reg_iv(admin_days = c(1L, 8L, 15L)),
                       model_cycle_length = 7, n_cycles = 6)
  expect_equal(s$day, c(0, 7, 14, 21, 28, 35))
  expect_equal(s$med_cycle, rep(1:2, each = 3))
  expect_equal(s$med_cycle_day, rep(c(1, 8, 15), 2))
})

test_that("model_cycle assignment matches findInterval", {
  s <- detail_schedule(reg_iv(admin_days = c(1L, 8L)), model_cycle_length = 10,
                       n_cycles = 9)
  expect_equal(s$model_cycle, findInterval(s$day, seq(0, 80, by = 10)))
})

test_that("max_med_cycles caps events", {
  s <- detail_schedule(reg_iv(max_med_cycles = 2), model_cycle_length = 21,
                       n_cycles = 6)
  expect_equal(nrow(s), 2)
})

test_that("start_day shifts all events", {
  s <- detail_schedule(reg_iv(start_day = 30), model_cycle_length = 7,
                       n_cycles = 12)
  expect_true(all(s$day >= 30))
  expect_equal(s$day[1], 30)
})

test_that("start_day + max_med_cycles caps events", {
  s <- detail_schedule(reg_iv(start_day = 21, max_med_cycles = 1),
                       model_cycle_length = 21, n_cycles = 5)
  expect_equal(s$day, 21)
})

test_that("custom and non-uniform cycle_times", {
  s <- detail_schedule(reg_iv(med_cycle_length = 7), cycle_times = c(0, 7, 21))
  expect_equal(s$model_cycle, c(1, 2, 2, 3, 3))
  expect_equal(max(s$day), 28)
})

test_that("schedule totals match calculate_med_costs", {
  reg <- reg_oral(admin_cost = 3, dispensing_cost = 45)
  s <- detail_schedule(reg, model_cycle_length = 7, n_cycles = 9)
  res <- calculate_med_costs(reg, model_cycle_length = 7, n_cycles = 9)
  expect_equal(sum(s$total_cost), sum(res$total_cost))
})

test_that("route does not change the schedule", {
  a <- detail_schedule(reg_iv(route = "sc"), n_cycles = 6)
  b <- detail_schedule(reg_iv(route = "iv"), n_cycles = 6)
  expect_equal(a, b)
})

