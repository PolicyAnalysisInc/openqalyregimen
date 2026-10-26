# Locked tests for the pressure-test fixes (#1-#14 in
# openqalyregimen-pressure-test-findings.html). Expected values come from
# the costing rules or hand calculation, not from package output, except #13,
# whose reference values are today's (verified) results.

reg1 <- function(...) {
  args <- list(name = "Drug", dose_per_admin = 100, unit_size = 100,
               unit_cost = 10, exact = FALSE, charge_timing = "per_admin",
               admin_days = 1, med_cycle_length = 21)
  over <- list(...)
  args[names(over)] <- over
  do.call(define_regimen, args)
}

# ===========================================================================
# #1 Column names unique; display names preserved
# ===========================================================================

test_that("#1 colliding names get unique columns and correct totals", {
  a <- reg1(name = "A-B")
  b <- reg1(name = "A_B", dose_per_admin = 200)
  res <- calculate_med_costs(a, b, model_cycle_length = 21, n_cycles = 1)
  expect_equal(res$total_cost, 30)
  expect_equal(attr(res, "drug_names"), c("A-B", "A_B"))
  expect_equal(res$A_B_total_cost, 10)
  expect_equal(res$A_B_1_total_cost, 20)
  expect_equal(names(attr(res, "drug_columns")), c("A-B", "A_B"))
})

test_that("#1 colliding names: daily detail and cost vectors are correct", {
  a <- reg1(name = "Drug 1")
  b <- reg1(name = "Drug-1", dose_per_admin = 200)
  d <- calculate_med_costs(a, b, model_cycle_length = 21, n_cycles = 1,
                           daily_detail = TRUE)
  expect_equal(sum(d$total_cost), 30)
  tbl <- data.frame(name = c("A-B", "A_B"), route = "iv", dose = c(100, 200),
                    dose_basis = "flat", med_cycle_length = 21, admin_days = "1",
                    unit_size = "100", unit_cost = "10", exact = FALSE,
                    charge_timing = "per_admin", stringsAsFactors = FALSE)
  combo <- define_regimen_table(tbl)
  expect_equal(calculate_regimen_cost(combo, type = "medication",
                                      model_cycle_length = 21, n_cycles = 1), 30)
})

test_that("#1 flextables show the real drug names", {
  skip_if_not_installed("flextable")
  for (nm in list(c("A-B", "A_B"), c("5-FU (bolus)", "Cisplatin (IV)"))) {
    a <- reg1(name = nm[1]); b <- reg1(name = nm[2])
    ft <- flextable::as_flextable(calculate_med_costs(a, b, n_cycles = 2))
    top <- unlist(ft$header$dataset[1, ])
    expect_true(all(nm %in% top))
    expect_false(any(grepl("_1$|__", top)))
    ftd <- flextable::as_flextable(calculate_med_costs(a, b, n_cycles = 1,
                                                      daily_detail = TRUE))
    topd <- unlist(ftd$header$dataset[1, ])
    expect_true(all(nm %in% topd))
  }
})

# ===========================================================================
# #2 Fractional days: points in intervals
# ===========================================================================

test_that("#2 fractional med_cycle_length costs every dose", {
  r <- reg1(med_cycle_length = 2.5)
  res <- calculate_med_costs(r, model_cycle_length = 7, n_cycles = 4)
  # doses at 0, 2.5, ..., 27.5: three in each 7-day cycle
  expect_equal(res$n_administrations, c(3L, 3L, 3L, 3L))
  expect_equal(res$drug_cost, c(30, 30, 30, 30))
  expect_equal(res$n_dispensings, c(3L, 3L, 3L, 3L))
})

test_that("#2 fractional start_day costs its doses", {
  r <- reg1(start_day = 0.5)
  res <- calculate_med_costs(r, model_cycle_length = 21, n_cycles = 3)
  expect_equal(res$drug_cost, c(10, 10, 10))
  s <- detail_schedule(r, model_cycle_length = 21, n_cycles = 3)
  expect_equal(s$day, c(0.5, 21.5, 42.5))
})

test_that("#2 30.4375-day medication cycle costs every cycle", {
  r <- reg1(med_cycle_length = 30.4375)
  res <- calculate_med_costs(r, model_cycle_length = 7, n_cycles = 6)
  # doses at 0 and 30.4375 (model cycle 5 = [28, 35))
  expect_equal(res$n_administrations, c(1L, 0L, 0L, 0L, 1L, 0L))
})

test_that("#2 fractional start at a fractional model-cycle boundary", {
  r <- reg1(med_cycle_length = 7, start_day = 2 * 30.4375, admin_cost = 2,
            dispensing_cost = 1)
  res <- calculate_med_costs(r, model_cycle_length = 30.4375, n_cycles = 4)
  # doses at 60.875 + 7k: 5 in [60.875, 91.3125), 4 in [91.3125, 121.75)
  expect_equal(res$n_administrations, c(0L, 0L, 5L, 4L))
  expect_equal(res$drug_cost, c(0, 0, 50, 40))
  d <- calculate_med_costs(r, model_cycle_length = 30.4375, n_cycles = 4,
                           daily_detail = TRUE)
  # 122 day intervals, three cut by a model-cycle boundary (30.4375,
  # 60.875, 91.3125)
  expect_equal(nrow(d), 125)
  day60 <- d[d$day == 60, ]
  expect_equal(day60$model_cycle, c(2L, 3L))
  expect_equal(day60$doses_taken, c(0L, 1L))
  expect_equal(day60$med_cycle_day[2], 1)
  for (col in c("drug_cost", "admin_cost", "dispensing_cost", "total_cost")) {
    expect_equal(as.numeric(tapply(d[[col]], d$model_cycle, sum)), res[[col]])
  }
  s <- detail_schedule(r, model_cycle_length = 30.4375, n_cycles = 4)
  expect_equal(s$day[1], 60.875)
  expect_equal(s$model_cycle[1], 3)
  expect_equal(sum(s$total_cost), sum(res$total_cost))
})

test_that("#2 per_dispensing with a fractional start", {
  r <- reg1(med_cycle_length = 7, admin_days = 1:3, start_day = 0.5,
            charge_timing = "per_dispensing")
  res <- calculate_med_costs(r, model_cycle_length = 7, n_cycles = 2)
  expect_equal(res$drug_cost, c(30, 30))
  expect_equal(res$n_administrations, c(3L, 3L))
  expect_equal(res$n_dispensings, c(1L, 1L))
})

test_that("#2 daily detail sums to cycle totals with fractional everything", {
  r <- reg1(med_cycle_length = 2.5, start_day = 0.25, admin_cost = 1,
            dispensing_cost = 0.5)
  res <- calculate_med_costs(r, model_cycle_length = 3.3, n_cycles = 6)
  d <- calculate_med_costs(r, model_cycle_length = 3.3, n_cycles = 6,
                           daily_detail = TRUE)
  in_model <- !is.na(d$model_cycle)
  for (col in c("drug_cost", "admin_cost", "dispensing_cost", "total_cost")) {
    expect_equal(as.numeric(tapply(d[[col]][in_model], d$model_cycle[in_model],
                                   sum)), res[[col]])
  }
  # doses at 0.25 + 2.5k below 19.8: k = 0..7
  expect_equal(sum(res$n_administrations), 8L)
})

# ===========================================================================
# #3 / #4 admin_days validation
# ===========================================================================

test_that("#3 duplicate admin_days are rejected", {
  expect_error(reg1(admin_days = c(1, 1, 8)), "duplicate")
})

test_that("#4 admin_days must lie within the medication cycle", {
  expect_error(reg1(admin_days = c(1, 30)), "admin_days")
  expect_error(reg1(admin_days = c(1, 8), med_cycle_length = 7), "admin_days")
  expect_silent(reg1(admin_days = c(1, 21)))
  expect_silent(reg1(admin_days = c(1, 3), med_cycle_length = 2.5))
  expect_error(reg1(admin_days = c(1, 4), med_cycle_length = 2.5), "admin_days")
})

# ===========================================================================
# #5 Fee-sharing ties follow input order
# ===========================================================================

test_that("#5 ties go to the first drug in input order, whatever the group order", {
  a <- reg1(name = "A", admin_cost = 5, dispensing_cost = 3)
  b <- reg1(name = "B", admin_cost = 5, dispensing_cost = 3)
  res <- calculate_med_costs(a, b, model_cycle_length = 21, n_cycles = 1,
                             shared_admin = list(c("B", "A")))
  expect_equal(c(res$A_admin_cost, res$B_admin_cost), c(5, 0))
  expect_equal(c(res$A_dispensing_cost, res$B_dispensing_cost), c(3, 0))
})

# ===========================================================================
# #6 Table admin_days must be whole days
# ===========================================================================

test_that("#6 numeric fractional admin_days in a table are rejected", {
  tbl <- data.frame(name = "X", route = "iv", dose = 100, dose_basis = "flat",
                    med_cycle_length = 21, admin_days = 1.5, unit_size = "100",
                    unit_cost = "10", exact = FALSE, charge_timing = "per_admin")
  expect_error(define_regimen_table(tbl), "admin_days")
})

# ===========================================================================
# #7 One tolerance for "on an amount"
# ===========================================================================

test_that("#7 exact dosing accepts a hair either side of a reachable amount", {
  for (d in c(150 - 1e-9, 150 + 1e-9)) {
    r <- reg1(dose_per_admin = d, unit_size = c(50, 0.5), unit_cost = c(10, 0.1),
              exact = TRUE)
    expect_equal(cost_single_dose(d, r)$cost, 30)
  }
  r <- reg1(dose_per_admin = 2.500000001, unit_size = c(1, 0.5),
            unit_cost = c(1, 0.5), exact = TRUE)
  expect_equal(cost_single_dose(2.500000001, r)$cost, 2.5)
})

test_that("#7 exact dosing costs a hair-level dose as the amount it sits on", {
  # Hair-level doses either side of reachable amounts, on grids of scale 1-1000.
  # Every unit costs $1, so the expected cost is the fewest units that make
  # the amount exactly (worked by hand).
  cases <- list(
    list(sizes = c(1, 0.5),     amount = 2.5,    cost = 3),   # 2x1 + 1x0.5
    list(sizes = c(1, 0.5),     amount = 3,      cost = 3),   # 3x1
    list(sizes = c(50, 0.5),    amount = 150,    cost = 3),   # 3x50
    list(sizes = c(50, 0.5),    amount = 150.5,  cost = 4),   # 3x50 + 1x0.5
    list(sizes = c(0.125, 1),   amount = 1.125,  cost = 2),   # 1 + 0.125
    list(sizes = c(0.125, 1),   amount = 2.375,  cost = 5),   # 2x1 + 3x0.125
    list(sizes = c(150, 500),   amount = 3600,   cost = 10),  # 6x500 + 4x150
    list(sizes = c(150, 500),   amount = 650,    cost = 2)    # 500 + 150
  )
  for (cs in cases) {
    for (d in cs$amount * (1 + c(-1, 1) * 1e-10) + c(-1, 1) * 1e-9) {
      r <- reg1(dose_per_admin = d, unit_size = cs$sizes, unit_cost = 1,
                exact = TRUE)
      info <- paste("dose", format(d, digits = 15))
      res <- tryCatch(cost_single_dose(d, r), error = function(e) {
        fail(paste(info, "errored:", conditionMessage(e)))
        NULL
      })
      if (is.null(res)) next
      expect_equal(res$cost, cs$cost, info = info)
      expect_equal(res$given_mg, cs$amount, info = info)
      expect_equal(res$leftover_mg, 0, info = info)
    }
  }
})

test_that("#7 a dose within tolerance of an amount is treated as on it", {
  mk <- function(D) reg1(dose_per_admin = D, unit_size = c(12.5, 2.5, 250),
                         unit_cost = c(2.5, 0.5, 50), max_dose_rounding = 0.5)
  on <- cost_single_dose(500 - 1e-7, mk(500 - 1e-7))
  expect_equal(on$cost, 100)
  expect_equal(on$leftover_mg, 0)
  off <- cost_single_dose(500 - 1e-6, mk(500 - 1e-6))
  expect_equal(off$cost, 99.5)
})

# ===========================================================================
# #8 Combo cost flextable with prefix-sharing names
# ===========================================================================

test_that("#8 combo cost table renders when one name prefixes another", {
  skip_if_not_installed("flextable")
  for (nm in list(c("A", "A_B"), c("Cisplatin", "Cisplatin (IV)"))) {
    res <- calculate_med_costs(reg1(name = nm[1]), reg1(name = nm[2]),
                               n_cycles = 2)
    expect_true(is.list(attr(res, "drug_columns")))
    ft <- flextable::as_flextable(res)
    top <- unlist(ft$header$dataset[1, ])
    expect_equal(sum(top == nm[1]), 4)
    expect_equal(sum(top == nm[2]), 4)
  }
})

# ===========================================================================
# #9 export() on a combination
# ===========================================================================

test_that("#9 export of a combination includes each regimen", {
  tbl <- data.frame(name = c("X", "Y"), route = "iv", dose = 100,
                    dose_basis = "flat", med_cycle_length = 21, admin_days = "1",
                    unit_size = "100", unit_cost = "10", exact = FALSE,
                    charge_timing = "per_admin")
  ex <- export.med_regimen_combo(define_regimen_table(tbl))
  expect_length(ex$regimens, 2)
  expect_equal(ex$regimens[[1]]$type, "med_regimen")
  expect_false(is.null(ex$regimens[[2]]$summary))
})

# ===========================================================================
# #10 Dosing table shows small values
# ===========================================================================

test_that("#10 dosing table never shows a non-zero mg value as zero", {
  skip_if_not_installed("flextable")
  cases <- list(
    reg1(name = "Small", route = "sc", dose_per_admin = 0.52,
         dose_basis = "weight", patient_weight = 1, patient_weight_sd = 0.01,
         unit_size = c(0.1, 1), unit_cost = c(1, 50), max_dose_rounding = 0.05),
    reg1(dose_per_admin = 1, dose_basis = "weight", patient_weight = 2,
         patient_weight_sd = 0.3, unit_size = c(0.125, 1),
         unit_cost = c(0.2, 1.5))
  )
  for (r in cases) {
    dd <- diagnose_dosing(r)
    body <- flextable::as_flextable(dd)$body$dataset
    zero_text <- function(x) grepl("^0(\\.0+)?( – 0(\\.0+)?)?$", x)
    expect_false(any(zero_text(body$shortfall_mg) & dd$shortfall_mg_hi > 0))
    expect_false(any(zero_text(body$leftover_mg) & dd$leftover_mg_hi > 0))
  }
  dd <- diagnose_dosing(cases[[2]])
  body <- flextable::as_flextable(dd)$body$dataset
  expect_true(any(grepl("0.125", body$leftover_mg, fixed = TRUE)))
})

# ===========================================================================
# #11 Schedule total = cost total when cycle_times don't start at 0
# ===========================================================================

test_that("#11 schedule leaves out events before the model", {
  r <- reg1(admin_cost = 3, dispensing_cost = 2)
  s <- detail_schedule(r, cycle_times = c(10, 31, 52), model_cycle_length = 21)
  res <- calculate_med_costs(r, cycle_times = c(10, 31, 52),
                             model_cycle_length = 21)
  expect_equal(sum(s$total_cost), 45)
  expect_equal(sum(s$total_cost), sum(res$total_cost))
  expect_false(any(is.na(s$model_cycle)))
})

# ===========================================================================
# #12 time expansion maps through cycle_times
# ===========================================================================

test_that("#12 time maps to the cycle whose interval (start, end] contains it", {
  r <- reg1()
  args <- list(r, type = "medication", model_cycle_length = 10, n_cycles = 3,
               cycle_times = c(10, 20, 35))
  base <- do.call(calculate_regimen_cost, args)
  expect_equal(base, c(0, 10, 10))
  # openqaly's day = end of each cycle: 20, 35, 50
  expect_equal(do.call(calculate_regimen_cost, c(args, list(time = c(20, 35, 50)))),
               c(0, 10, 10))
  expect_equal(do.call(calculate_regimen_cost, c(args, list(time = c(10, 60)))),
               c(0, 0))
  # uniform cycles unchanged: day = cycle x length
  u <- calculate_regimen_cost(reg1(med_cycle_length = 7), type = "medication",
                              model_cycle_length = 7, n_cycles = 3,
                              time = c(0, 7, 14, 21, 28))
  expect_equal(u, c(0, 10, 10, 10, 0))
})

# ===========================================================================
# #13 Distribution costing speed, identical results
# ===========================================================================

test_that("#13 fine units with a wide distribution are fast and unchanged", {
  reg <- reg1(dose_per_admin = 50, dose_basis = "weight", patient_weight = 70,
              patient_weight_sd = 35, unit_size = c(1, 10), unit_cost = c(5, 40))
  t <- system.time(b <- generate_cost_bins(reg, 70, 35))[["elapsed"]]
  expect_lt(t, 3)
  expect_equal(nrow(b), 47733)
  expect_equal(sum(b$cost * b$probability), 14005.9999828524, tolerance = 1e-10)
  expect_equal(sum(b$cost), 5098626915, tolerance = 1e-12)
  reg2 <- reg1(dose_per_admin = 50, dose_basis = "weight", patient_weight = 70,
               patient_weight_sd = 35, unit_size = c(1, 10),
               unit_cost = c(5, 40), max_dose_rounding = 0.05)
  t2 <- system.time(cost <- cost_single_admin_drug(reg2))[["elapsed"]]
  expect_lt(t2, 3)
  expect_equal(cost, 14001.9999828506, tolerance = 1e-10)
})

# ===========================================================================
# #14 Very large doses give the size error
# ===========================================================================

test_that("#14 a huge dose gives the 50-million-cell error", {
  r <- reg1(dose_per_admin = 3e9, unit_size = 1, unit_cost = 1)
  expect_error(cost_single_dose(3e9, r), "50 million")
})
