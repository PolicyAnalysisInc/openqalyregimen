# One 100 mg unit at $500 per dose unless overridden.
reg_iv <- function(...) {
  args <- list(name = "Drug A", dose_per_admin = 100, unit_size = 100,
               unit_cost = 500, med_cycle_length = 21, admin_days = 1L,
               exact = FALSE, charge_timing = "per_admin")
  over <- list(...)
  args[names(over)] <- over
  do.call(define_regimen, args)
}

# Tablets: 3,600 mg/day from 150/500 mg tablets ($22.10/day), 14 of 21 days.
reg_oral <- function(...) {
  args <- list(name = "Cape", route = "oral", dose_per_admin = 3600,
               unit_size = c(150, 500), unit_cost = 2.21,
               med_cycle_length = 21, admin_days = 1:14, exact = TRUE,
               charge_timing = "per_dispensing")
  over <- list(...)
  args[names(over)] <- over
  do.call(define_regimen, args)
}

# ===========================================================================
# Basics
# ===========================================================================

test_that("calculate_med_costs errors on empty call or non-regimen input", {
  expect_error(calculate_med_costs(), "At least one")
  expect_error(calculate_med_costs(list(a = 1)), "med_regimen")
  expect_error(calculate_med_costs(reg_iv(), "x"), "med_regimen")
})

test_that("single regimen returns one row per model cycle with expected columns", {
  res <- calculate_med_costs(reg_iv(), model_cycle_length = 7, n_cycles = 10)
  expect_s3_class(res, "regimen_costs")
  expect_equal(nrow(res), 10)
  expect_equal(names(res), c("cycle", "cycle_start_day", "cycle_end_day",
                             "n_administrations", "n_dispensings",
                             "drug_cost", "admin_cost", "dispensing_cost",
                             "total_cost"))
})

test_that("cycle_times overrides n_cycles", {
  res <- calculate_med_costs(reg_iv(), cycle_times = c(0, 7, 14, 21, 28))
  expect_equal(nrow(res), 5)
})

test_that("total_cost = drug + admin + dispensing", {
  res <- calculate_med_costs(reg_oral(admin_cost = 3, dispensing_cost = 45),
                             model_cycle_length = 7, n_cycles = 12)
  expect_equal(res$total_cost,
               res$drug_cost + res$admin_cost + res$dispensing_cost)
})

test_that("single model cycle works", {
  res <- calculate_med_costs(reg_iv(), model_cycle_length = 21, n_cycles = 1)
  expect_equal(res$drug_cost, 500)
})

# ===========================================================================
# Schedule
# ===========================================================================

test_that("aligned cycles: one administration per model cycle", {
  res <- calculate_med_costs(reg_iv(med_cycle_length = 7),
                             model_cycle_length = 7, n_cycles = 6)
  expect_equal(res$n_administrations, rep(1L, 6))
  expect_equal(res$drug_cost, rep(500, 6))
})

test_that("medication cycle longer than model cycle", {
  res <- calculate_med_costs(reg_iv(), model_cycle_length = 7, n_cycles = 9)
  expect_equal(res$n_administrations, rep(c(1L, 0L, 0L), 3))
  expect_equal(res$drug_cost, rep(c(500, 0, 0), 3))
})

test_that("model cycle longer than medication cycle", {
  res <- calculate_med_costs(reg_iv(med_cycle_length = 7),
                             model_cycle_length = 28, n_cycles = 3)
  expect_equal(res$n_administrations, rep(4L, 3))
  expect_equal(res$drug_cost, rep(2000, 3))
})

test_that("misaligned cycles count each administration once", {
  # 21-day medication cycle, days 1 and 8; 10-day model cycles.
  reg <- reg_iv(admin_days = c(1L, 8L))
  res <- calculate_med_costs(reg, model_cycle_length = 10, n_cycles = 9)
  # Administration days: 0, 7, 21, 28, 42, 49, 63, 70, 84 (horizon 90)
  admin <- c(0, 7, 21, 28, 42, 49, 63, 70, 84)
  expect_equal(res$n_administrations,
               as.integer(tabulate(floor(admin / 10) + 1, nbins = 9)))
  expect_equal(sum(res$n_administrations), 9L)
})

test_that("max_med_cycles stops treatment", {
  res <- calculate_med_costs(reg_iv(max_med_cycles = 2),
                             model_cycle_length = 21, n_cycles = 5)
  expect_equal(res$n_administrations, c(1L, 1L, 0L, 0L, 0L))
  expect_equal(res$drug_cost, c(500, 500, 0, 0, 0))
})

test_that("start_day shifts treatment; cycles before it are zero", {
  res <- calculate_med_costs(reg_iv(start_day = 42),
                             model_cycle_length = 21, n_cycles = 5)
  expect_equal(res$n_administrations, c(0L, 0L, 1L, 1L, 1L))
})

test_that("start_day + max_med_cycles limits the treatment window", {
  res <- calculate_med_costs(reg_iv(start_day = 21, max_med_cycles = 2),
                             model_cycle_length = 21, n_cycles = 5)
  expect_equal(res$n_administrations, c(0L, 1L, 1L, 0L, 0L))
})

test_that("start_day = 0 is the default", {
  expect_equal(calculate_med_costs(reg_iv(), n_cycles = 10),
               calculate_med_costs(reg_iv(start_day = 0), n_cycles = 10))
})

test_that("non-uniform cycle_times: boundaries and counts", {
  res <- calculate_med_costs(reg_iv(med_cycle_length = 7),
                             cycle_times = c(0, 7, 21, 28))
  expect_equal(res$cycle_end_day, c(7, 21, 28, 35))
  expect_equal(res$n_administrations, c(1L, 2L, 1L, 1L))
})

# ===========================================================================
# Charge timing
# ===========================================================================

test_that("per_admin puts each dose's cost on its administration day", {
  res <- calculate_med_costs(reg_oral(charge_timing = "per_admin"),
                             model_cycle_length = 7, n_cycles = 6)
  expect_equal(res$drug_cost, rep(c(7, 7, 0), 2) * 22.10)
})

test_that("per_dispensing puts a whole cycle's doses on day 1", {
  res <- calculate_med_costs(reg_oral(), model_cycle_length = 7, n_cycles = 6)
  expect_equal(res$drug_cost, rep(c(14, 0, 0), 2) * 22.10)
  # Administrations are still counted on the days they happen.
  expect_equal(res$n_administrations, rep(c(7L, 7L, 0L), 2))
})

test_that("both timings give the same total over whole medication cycles", {
  a <- calculate_med_costs(reg_oral(charge_timing = "per_admin"),
                           model_cycle_length = 7, n_cycles = 12)
  d <- calculate_med_costs(reg_oral(), model_cycle_length = 7, n_cycles = 12)
  expect_equal(sum(a$drug_cost), sum(d$drug_cost))
})

test_that("per_dispensing charges the full cycle even if the model ends mid-cycle", {
  res <- calculate_med_costs(reg_oral(), model_cycle_length = 7, n_cycles = 1)
  expect_equal(res$drug_cost, 14 * 22.10)
  res <- calculate_med_costs(reg_oral(charge_timing = "per_admin"),
                             model_cycle_length = 7, n_cycles = 1)
  expect_equal(res$drug_cost, 7 * 22.10)
})

test_that("per_dispensing respects start_day and max_med_cycles", {
  res <- calculate_med_costs(reg_oral(start_day = 21, max_med_cycles = 1),
                             model_cycle_length = 21, n_cycles = 4)
  expect_equal(res$drug_cost, c(0, 14 * 22.10, 0, 0))
  expect_equal(res$n_dispensings, c(0L, 1L, 0L, 0L))
})

# ===========================================================================
# Two fees
# ===========================================================================

test_that("admin_cost is charged on every administration", {
  res <- calculate_med_costs(reg_oral(admin_cost = 3),
                             model_cycle_length = 7, n_cycles = 6)
  expect_equal(res$admin_cost, rep(c(21, 21, 0), 2))
})

test_that("dispensing_cost is charged once per medication cycle", {
  res <- calculate_med_costs(reg_oral(dispensing_cost = 45),
                             model_cycle_length = 7, n_cycles = 6)
  expect_equal(res$dispensing_cost, rep(c(45, 0, 0), 2))
  expect_equal(res$n_dispensings, rep(c(1L, 0L, 0L), 2))
})

test_that("fees don't depend on charge_timing", {
  a <- calculate_med_costs(reg_oral(charge_timing = "per_admin", admin_cost = 3,
                                    dispensing_cost = 45),
                           model_cycle_length = 7, n_cycles = 6)
  d <- calculate_med_costs(reg_oral(admin_cost = 3, dispensing_cost = 45),
                           model_cycle_length = 7, n_cycles = 6)
  expect_equal(a$admin_cost, d$admin_cost)
  expect_equal(a$dispensing_cost, d$dispensing_cost)
})

test_that("an IV with both fees charges each on its own schedule", {
  # Days 1, 8, 15 of a 21-day cycle: infusion fee each dose, pharmacy fee once.
  reg <- reg_iv(admin_days = c(1L, 8L, 15L), admin_cost = 241,
                dispensing_cost = 20)
  res <- calculate_med_costs(reg, model_cycle_length = 21, n_cycles = 2)
  expect_equal(res$admin_cost, c(723, 723))
  expect_equal(res$dispensing_cost, c(20, 20))
})

test_that("fees stop with treatment", {
  res <- calculate_med_costs(reg_oral(admin_cost = 3, dispensing_cost = 45,
                                      max_med_cycles = 1),
                             model_cycle_length = 21, n_cycles = 3)
  expect_equal(res$admin_cost, c(42, 0, 0))
  expect_equal(res$dispensing_cost, c(45, 0, 0))
})

# ===========================================================================
# Distribution flows through
# ===========================================================================

test_that("distribution-averaged cost is used per administration", {
  reg <- reg_iv(dose_per_admin = 5, dose_basis = "weight", patient_weight = 70,
                patient_weight_sd = 15, unit_size = c(100, 400),
                unit_cost = c(500, 1800), med_cycle_length = 7)
  res <- calculate_med_costs(reg, model_cycle_length = 7, n_cycles = 3)
  expect_equal(res$drug_cost, rep(cost_single_admin_drug(reg), 3))
  point <- calculate_med_costs(reg_iv(dose_per_admin = 350,
                                      unit_size = c(100, 400),
                                      unit_cost = c(500, 1800),
                                      med_cycle_length = 7),
                               model_cycle_length = 7, n_cycles = 3)
  expect_false(isTRUE(all.equal(res$drug_cost, point$drug_cost)))
})

# ===========================================================================
# Daily detail
# ===========================================================================

test_that("daily detail: one row per day with expected columns", {
  res <- calculate_med_costs(reg_iv(), model_cycle_length = 7, n_cycles = 4,
                             daily_detail = TRUE)
  expect_s3_class(res, "regimen_daily_detail")
  expect_equal(nrow(res), 28)
  expect_equal(names(res), c("day", "model_cycle", "med_cycle",
                             "med_cycle_day", "doses_taken", "mg_taken",
                             "units_dispensed", "mg_dispensed", "drug_cost",
                             "admin_cost", "dispensing_cost", "total_cost"))
})

test_that("daily detail: aggregating by model cycle matches cycle results", {
  reg <- reg_oral(admin_cost = 3, dispensing_cost = 45)
  d <- calculate_med_costs(reg, model_cycle_length = 7, n_cycles = 9,
                           daily_detail = TRUE)
  res <- calculate_med_costs(reg, model_cycle_length = 7, n_cycles = 9)
  for (col in c("drug_cost", "admin_cost", "dispensing_cost", "total_cost")) {
    expect_equal(as.numeric(tapply(d[[col]], d$model_cycle, sum)), res[[col]])
  }
})

test_that("daily detail per_dispensing: units and mg dispensed on day 1 only", {
  d <- calculate_med_costs(reg_oral(), model_cycle_length = 7, n_cycles = 3,
                           daily_detail = TRUE)
  day1 <- d$med_cycle_day == 1
  expect_equal(d$units_dispensed[day1], 14 * 10)
  expect_equal(d$mg_dispensed[day1], 14 * 3600)
  expect_true(all(d$units_dispensed[!day1] == 0))
  expect_equal(sum(d$mg_taken), 14 * 3600)
})

test_that("daily detail per_admin: units dispensed on administration days", {
  d <- calculate_med_costs(reg_oral(charge_timing = "per_admin"),
                           model_cycle_length = 7, n_cycles = 3,
                           daily_detail = TRUE)
  expect_equal(d$units_dispensed, d$doses_taken * 10)
})

test_that("daily detail: off-treatment days have zero costs and NA med_cycle", {
  d <- calculate_med_costs(reg_iv(start_day = 10, admin_cost = 5),
                           model_cycle_length = 7, n_cycles = 3,
                           daily_detail = TRUE)
  pre <- d$day < 10
  expect_true(all(is.na(d$med_cycle[pre])))
  expect_true(all(d$total_cost[pre] == 0))
  expect_equal(d$med_cycle_day[d$day == 10], 1)
})

test_that("daily detail: mg_taken shows the dose given after rounding", {
  reg <- reg_iv(dose_per_admin = 110, max_dose_rounding = 0.1,
                med_cycle_length = 7)
  d <- calculate_med_costs(reg, model_cycle_length = 7, n_cycles = 1,
                           daily_detail = TRUE)
  expect_equal(d$mg_taken[1], 100)
  expect_equal(d$mg_dispensed[1], 100)
})

test_that("daily detail with a distribution omits dose and unit columns", {
  reg <- reg_iv(dose_per_admin = 5, dose_basis = "weight", patient_weight = 70,
                patient_weight_sd = 15, unit_size = c(100, 400),
                unit_cost = c(500, 1800), med_cycle_length = 7)
  d <- calculate_med_costs(reg, model_cycle_length = 7, n_cycles = 1,
                           daily_detail = TRUE)
  expect_false(any(c("mg_taken", "units_dispensed", "mg_dispensed") %in%
                     names(d)))
  expect_equal(d$drug_cost[1], cost_single_admin_drug(reg))
})

test_that("daily detail under no_wastage shows mg costed, not units", {
  reg <- reg_oral(no_wastage = TRUE)
  d <- calculate_med_costs(reg, model_cycle_length = 7, n_cycles = 1,
                           daily_detail = TRUE)
  expect_false("units_dispensed" %in% names(d))
  expect_equal(d$mg_dispensed[1], 14 * 3600)
  expect_equal(d$drug_cost[1], 14 * 3600 * 2.21 / 500)
})

# ===========================================================================
# Multiple regimens
# ===========================================================================

test_that("multi-regimen: wide column naming and class", {
  res <- calculate_med_costs(reg_iv(name = "Drug A"), reg_oral(name = "Drug B"),
                             model_cycle_length = 7, n_cycles = 6)
  expect_s3_class(res, "combo_regimen_costs")
  for (p in c("Drug_A", "Drug_B")) {
    for (s in c("drug_cost", "admin_cost", "dispensing_cost", "total_cost")) {
      expect_true(paste0(p, "_", s) %in% names(res))
    }
  }
  expect_equal(attr(res, "drug_names"), c("Drug A", "Drug B"))
})

test_that("multi-regimen: total = sum of per-drug totals", {
  res <- calculate_med_costs(reg_iv(name = "A", admin_cost = 50),
                             reg_oral(name = "B", dispensing_cost = 45),
                             reg_oral(name = "C", charge_timing = "per_admin", admin_cost = 2),
                             model_cycle_length = 7, n_cycles = 9)
  expect_equal(res$total_cost, res$A_total_cost + res$B_total_cost +
                 res$C_total_cost)
})

test_that("multi-regimen: per-drug costs match single-regimen calls", {
  a <- reg_iv(name = "A", admin_cost = 50)
  b <- reg_oral(name = "B", dispensing_cost = 45)
  s <- reg_oral(name = "C", charge_timing = "per_admin", admin_cost = 2)
  res <- calculate_med_costs(a, b, s, model_cycle_length = 10, n_cycles = 8)
  for (pair in list(list(a, "A"), list(b, "B"), list(s, "C"))) {
    one <- calculate_med_costs(pair[[1]], model_cycle_length = 10, n_cycles = 8)
    for (col in c("drug_cost", "admin_cost", "dispensing_cost", "total_cost")) {
      expect_equal(res[[paste0(pair[[2]], "_", col)]], one[[col]])
    }
  }
})

test_that("multi-regimen: names must be distinct", {
  expect_error(calculate_med_costs(reg_iv(), reg_iv()), "distinct names")
})

test_that("multi-regimen: drug names are sanitized", {
  res <- calculate_med_costs(reg_iv(name = "5-FU (bolus)"), reg_oral(),
                             n_cycles = 3)
  expect_true("5_FU__bolus__drug_cost" %in% names(res))
})

test_that("multi-regimen: daily detail is wide", {
  res <- calculate_med_costs(reg_iv(name = "A"), reg_oral(name = "B"),
                             model_cycle_length = 7, n_cycles = 3,
                             daily_detail = TRUE)
  expect_s3_class(res, "combo_regimen_daily_detail")
  expect_equal(nrow(res), 21)
  expect_true(all(c("A_drug_cost", "B_mg_taken", "total_cost") %in% names(res)))
  expect_equal(res$total_cost, res$A_total_cost + res$B_total_cost)
})

# ===========================================================================
# Shared fees
# ===========================================================================

test_that("shared_admin: validation", {
  a <- reg_iv(name = "A"); b <- reg_iv(name = "B"); c <- reg_iv(name = "C")
  expect_error(calculate_med_costs(a, b, shared_admin = list(c("A", "Z"))),
               "Unknown drug")
  expect_error(calculate_med_costs(a, b, c,
                                   shared_admin = list(c("A", "B"), c("B", "C"))),
               "multiple")
  expect_error(calculate_med_costs(a, b, shared_admin = list("A")),
               "at least 2")
  expect_error(calculate_med_costs(a, b, shared_admin = "A"), "must be NULL")
})

test_that("shared_admin: single regimen warns and ignores", {
  expect_warning(calculate_med_costs(reg_iv(), shared_admin = TRUE), "ignored")
})

test_that("shared_admin: same-day admin fees share, highest wins", {
  a <- reg_iv(name = "A", admin_cost = 100)
  b <- reg_iv(name = "B", admin_cost = 60)
  res <- calculate_med_costs(a, b, model_cycle_length = 21, n_cycles = 2,
                             shared_admin = TRUE)
  expect_equal(res$A_admin_cost, c(100, 100))
  expect_equal(res$B_admin_cost, c(0, 0))
  expect_equal(res$B_pre_sharing_admin_cost, c(60, 60))
})

test_that("shared_admin: dispensing fees share separately from admin fees", {
  a <- reg_iv(name = "A", admin_cost = 100, dispensing_cost = 10)
  b <- reg_oral(name = "B", dispensing_cost = 45)
  res <- calculate_med_costs(a, b, model_cycle_length = 21, n_cycles = 2,
                             shared_admin = TRUE)
  expect_equal(res$A_admin_cost, c(100, 100))
  expect_equal(res$A_dispensing_cost, c(0, 0))
  expect_equal(res$B_dispensing_cost, c(45, 45))
  expect_equal(res$A_pre_sharing_dispensing_cost, c(10, 10))
  expect_equal(res$total_cost, res$A_total_cost + res$B_total_cost)
})

test_that("shared_admin: ties go to the first drug", {
  a <- reg_iv(name = "A", admin_cost = 50)
  b <- reg_iv(name = "B", admin_cost = 50)
  res <- calculate_med_costs(a, b, model_cycle_length = 21, n_cycles = 1,
                             shared_admin = TRUE)
  expect_equal(c(res$A_admin_cost, res$B_admin_cost), c(50, 0))
})

test_that("shared_admin: no same-day overlap means no effect", {
  a <- reg_iv(name = "A", admin_cost = 100)
  b <- reg_iv(name = "B", admin_cost = 60, admin_days = 8L)
  res <- calculate_med_costs(a, b, model_cycle_length = 21, n_cycles = 2,
                             shared_admin = TRUE)
  expect_equal(res$B_admin_cost, c(60, 60))
})

test_that("shared_admin: partial groups leave other drugs alone", {
  a <- reg_iv(name = "A", admin_cost = 100)
  b <- reg_iv(name = "B", admin_cost = 60)
  c <- reg_iv(name = "C", admin_cost = 40)
  res <- calculate_med_costs(a, b, c, model_cycle_length = 21, n_cycles = 1,
                             shared_admin = list(c("A", "B")))
  expect_equal(c(res$A_admin_cost, res$B_admin_cost, res$C_admin_cost),
               c(100, 0, 40))
  expect_false("C_pre_sharing_admin_cost" %in% names(res))
})

test_that("shared_admin = NULL: no pre-sharing columns", {
  res <- calculate_med_costs(reg_iv(name = "A"), reg_iv(name = "B"),
                             n_cycles = 2)
  expect_false(any(grepl("pre_sharing", names(res))))
  expect_null(attr(res, "shared_admin"))
})

test_that("shared_admin: daily detail shows sharing per day", {
  a <- reg_iv(name = "A", admin_cost = 100)
  b <- reg_iv(name = "B", admin_cost = 60)
  d <- calculate_med_costs(a, b, model_cycle_length = 21, n_cycles = 1,
                           daily_detail = TRUE, shared_admin = TRUE)
  expect_equal(d$B_pre_sharing_admin_cost[1], 60)
  expect_equal(d$B_admin_cost[1], 0)
  expect_equal(attr(d, "shared_admin"), list(c("A", "B")))
})
