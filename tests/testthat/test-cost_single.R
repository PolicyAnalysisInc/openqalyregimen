# Cost one dose with the package's costing rules.
dose_detail <- function(dose, sizes, costs, exact = FALSE, t = 0,
                        no_wastage = FALSE) {
  if (length(costs) == 1L) costs <- rep(costs, length(sizes))
  tab <- unit_table(sizes, costs, dose)
  cost_dose_detail(dose, tab, exact, t, no_wastage)
}

# All whole-unit combinations with total up to `limit` mg: one row per
# combination with its total mg, cost and unit count.
brute_combos <- function(sizes, costs, limit) {
  grids <- lapply(sizes, function(s) 0:floor(limit / s + 1e-9))
  g <- as.matrix(expand.grid(grids))
  data.frame(
    total = round(as.vector(g %*% sizes), 9),
    cost = as.vector(g %*% costs),
    n = rowSums(g)
  )
}

# Cheapest combination making exactly each reachable total.
brute_exact <- function(combos) {
  combos <- combos[order(combos$total, combos$cost, combos$n), ]
  combos[!duplicated(combos$total), ]
}

# Brute-force reference for the costing rules.
brute_cost <- function(dose, sizes, costs, exact, t = 0) {
  combos <- brute_combos(sizes, costs, dose + max(sizes))
  ex <- brute_exact(combos)
  ex <- ex[ex$total > 0, ]
  tol <- 1e-9 * max(1, dose)
  below <- ex[ex$total < dose - tol, ]
  xb <- if (nrow(below)) below[which.max(below$total), ] else NULL
  ok_b <- !is.null(xb) && (dose - xb$total) <= t * dose + tol
  if (exact) {
    hit <- ex[abs(ex$total - dose) <= tol, ]
    if (nrow(hit)) return(hit$cost)
    return(NA_real_)
  }
  cov <- combos[combos$total >= dose - tol, ]
  cov <- cov[order(signif(cov$cost, 12), cov$total, cov$n), ][1, ]
  if (t > 0 && cov$total > dose + tol && ok_b &&
      xb$cost < cov$cost - 1e-9 * max(1, cov$cost)) {
    return(xb$cost)
  }
  cov$cost
}

# ===========================================================================
# Integer grid
# ===========================================================================

test_that("grid scale comes from the unit sizes only", {
  expect_equal(unit_grid_scale(c(50, 100)), 1)
  expect_equal(unit_grid_scale(c(3.5, 10)), 10)
  expect_equal(unit_grid_scale(c(0.25, 1)), 100)
  expect_equal(unit_grid_scale(0.125), 1000)
  expect_error(unit_grid_scale(0.0001), "3 decimal places")
})

test_that("a fractional dose is not rounded onto a coarse grid", {
  # 100.4 mg needs more than one 100 mg vial: 100 + 50 mg = $13.
  d <- dose_detail(100.4, c(50, 100), c(5, 8))
  expect_equal(d$cost, 13)
  expect_equal(d$dispensed_mg, 150)
})

test_that("fractional unit sizes work", {
  d <- dose_detail(2.34, 3.5, 762.38)
  expect_equal(d$cost, 762.38)
  expect_equal(d$leftover_mg, 1.16, tolerance = 1e-9)
})

# ===========================================================================
# Cheapest cover (exact = FALSE)
# ===========================================================================

test_that("the cheapest covering pack beats a dearer exact fill", {
  # 90 mg: 3 x 30 mg = $30 exact; one 100 mg vial = $11.
  d <- dose_detail(90, c(30, 100), c(10, 11))
  expect_equal(d$cost, 11)
  expect_equal(unname(d$units), c(1L, 0L))
  expect_equal(d$leftover_mg, 10)
})

test_that("on equal cost, the pack with less drug wins", {
  # 60 mg: 2 x 30 mg = $10 and one 100 mg = $10. Less drug wins.
  d <- dose_detail(60, c(30, 100), c(5, 10))
  expect_equal(d$cost, 10)
  expect_equal(d$dispensed_mg, 60)
  expect_equal(d$leftover_mg, 0)
})

test_that("on equal cost and drug, fewer units win", {
  # 100 mg: 1 x 100 mg = $10 or 2 x 50 mg = $10.
  d <- dose_detail(100, c(50, 100), c(5, 10))
  expect_equal(unname(d$units), c(1L, 0L))
})

test_that("known min-cost problems give the expected cost", {
  expect_equal(dose_detail(5, 1:5, c(20, 10, 4, 50, 100))$cost, 8)
  expect_equal(dose_detail(5, 1:5, 1:5)$cost, 5)
  expect_equal(dose_detail(6, c(1, 3, 4), 1)$cost, 2)
  expect_equal(dose_detail(40, c(5, 10, 20, 25), 1)$cost, 2)
  expect_equal(dose_detail(7, c(3, 5), c(4, 6))$cost, 10)
  expect_equal(dose_detail(13, c(4, 7), c(3, 5))$cost, 10)
  expect_equal(dose_detail(10, c(3, 5, 7), c(2, 10, 11))$cost, 8)
  expect_equal(dose_detail(100, c(100, 150), c(800, 500))$cost, 500)
})

test_that("units are named '<size>mg' in descending size order", {
  d <- dose_detail(250, c(50, 100.5, 10), c(1, 2, 0.3))
  expect_equal(names(d$units), c("100.5mg", "50mg", "10mg"))
})

test_that("a zero dose costs nothing", {
  d <- dose_detail(0, c(50, 100), c(5, 8))
  expect_equal(d$cost, 0)
  expect_equal(sum(d$units), 0)
})

test_that("a negative or non-finite dose errors", {
  tab <- unit_table(100, 5, 100)
  expect_error(cost_dose_detail(-1, tab, FALSE), "non-negative")
  expect_error(cost_dose_detail(NA_real_, tab, FALSE), "non-negative")
})

# ===========================================================================
# Dose rounding
# ===========================================================================

test_that("shortfall is measured against the prescribed dose", {
  # 110 mg, 100 mg vials. Shortfall 10 mg = 9.1% of 110 mg.
  d <- dose_detail(110, 100, 500, t = 0.1)
  expect_equal(d$cost, 500)
  expect_equal(d$given_mg, 100)
  expect_equal(d$shortfall_mg, 10)
  expect_equal(d$leftover_mg, 0)
  # 112 mg: shortfall 12 mg = 10.7% > 10%, so two vials.
  d <- dose_detail(112, 100, 500, t = 0.1)
  expect_equal(d$cost, 1000)
  expect_equal(d$given_mg, 112)
})

test_that("rounding goes to the nearest exact fill below the dose", {
  # 850 mg with 100/400 mg units at 10%: nearest fill below is 800 mg.
  d <- dose_detail(850, c(100, 400), c(800, 3000), t = 0.10)
  expect_equal(d$given_mg, 800)
  expect_equal(d$cost, 6000)
})

test_that("rounding applies only if strictly cheaper", {
  # 110 mg: 1 x 150 mg = $500 covers. Rounding to 100 mg ($1000) is dearer.
  d <- dose_detail(110, c(100, 150), c(1000, 500), t = 0.5)
  expect_equal(d$cost, 500)
  expect_equal(d$given_mg, 110)
})

test_that("no rounding when the cover has nothing left over", {
  # 200 mg = 2 x 100 mg ($800) is the cheapest cover and fills the dose
  # exactly; 150 mg ($500) is cheaper and within 50%, but isn't used.
  d <- dose_detail(200, c(100, 150), c(400, 500), t = 0.5)
  expect_equal(d$cost, 800)
  expect_equal(d$given_mg, 200)
})

test_that("rounding applies when a cheaper cover overshoots", {
  # 200 mg: cheapest cover 2 x 150 mg ($1000, 100 mg left over). 150 mg
  # ($500) is 25% short, within 50%, and cheaper.
  d <- dose_detail(200, c(100, 150), c(800, 500), t = 0.5)
  expect_equal(d$cost, 500)
  expect_equal(d$given_mg, 150)
})

test_that("rounding never gives a zero dose", {
  d <- dose_detail(840, 1200, 3807.69, t = 0.7)
  expect_equal(d$cost, 3807.69)
  expect_equal(unname(d$units), 1L)
})

test_that("max_dose_rounding = 0 never rounds", {
  d <- dose_detail(101, 100, 500, t = 0)
  expect_equal(d$cost, 1000)
})

# ===========================================================================
# Exact dosing
# ===========================================================================

test_that("exact = TRUE costs the cheapest exact combination", {
  d <- dose_detail(3600, c(150, 500), 2.21, exact = TRUE)
  expect_equal(unname(d$units), c(6L, 4L))
  expect_equal(d$cost, 22.10)
  expect_equal(d$leftover_mg, 0)
})

test_that("exact = TRUE picks exact fill even when a cover is cheaper", {
  # 90 mg exact = 3 x 30 mg ($30), even though one 100 mg is $11.
  d <- dose_detail(90, c(30, 100), c(10, 11), exact = TRUE)
  expect_equal(d$cost, 30)
})

test_that("an unreachable dose errors and names the nearest amounts", {
  expect_error(dose_detail(3620, c(150, 500), 2.21, exact = TRUE),
               "3600 mg and 3650 mg")
  expect_error(dose_detail(3620, c(150, 500), 2.21, exact = TRUE),
               "cannot be made exactly")
})

test_that("exact = TRUE never rounds, even if a rounding value reaches it", {
  # Defined regimens can't combine the two; at the costing level the
  # rounding value is ignored and the unreachable dose still errors.
  expect_error(dose_detail(3620, c(150, 500), 2.21, exact = TRUE, t = 0.5),
               "cannot be made exactly")
  d <- dose_detail(3600, c(150, 500), 2.21, exact = TRUE, t = 0.5)
  expect_equal(d$given_mg, 3600)
})

test_that("a dose below the smallest unit errors under exact = TRUE", {
  expect_error(dose_detail(10, 20, 1, exact = TRUE), "20 mg")
})

test_that("exact error applies to any product, at runtime", {
  reg <- define_regimen(route = "iv", dose_per_admin = 75, dose_basis = "bsa",
                        patient_bsa = 1.8, unit_size = c(20, 80),
                        unit_cost = c(100, 380), admin_days = 1L,
                        exact = TRUE, charge_timing = "per_admin")
  # 75 x 1.8 = 135 mg is not a multiple of 20 mg.
  expect_error(calculate_med_costs(reg, n_cycles = 3), "cannot be made exactly")
  # A different BSA (as in a sensitivity analysis) that is reachable works.
  reg2 <- define_regimen(route = "iv", dose_per_admin = 75, dose_basis = "bsa",
                         patient_bsa = 1.6, unit_size = c(20, 80),
                         unit_cost = c(100, 380), admin_days = 1L,
                         exact = TRUE, charge_timing = "per_admin")
  expect_silent(calculate_med_costs(reg2, n_cycles = 3))
})

# ===========================================================================
# No wastage
# ===========================================================================

test_that("no wastage = lowest cost per mg x prescribed dose", {
  d <- dose_detail(20, c(10, 50, 100), c(2.70, 13.20, 21.38),
                   no_wastage = TRUE)
  expect_equal(d$cost, 20 * min(c(2.70, 13.20, 21.38) / c(10, 50, 100)))
  expect_equal(d$given_mg, 20)
})

test_that("no wastage ignores rounding", {
  d <- dose_detail(110, 100, 500, t = 0.1, no_wastage = TRUE)
  expect_equal(d$cost, 550)
  expect_equal(d$given_mg, 110)
  expect_equal(d$shortfall_mg, 0)
})

test_that("no wastage applies with exact = TRUE too", {
  d <- dose_detail(3600, c(150, 500), 2.21, exact = TRUE, no_wastage = TRUE)
  expect_equal(d$cost, 3600 * 2.21 / 500)
})

test_that("exact = TRUE still errors on an unreachable dose under no wastage", {
  expect_error(dose_detail(3620, c(150, 500), 2.21, exact = TRUE,
                           no_wastage = TRUE), "cannot be made exactly")
})

# ===========================================================================
# Brute-force agreement
# ===========================================================================

test_that("cover and rounding agree with brute force on random problems", {
  set.seed(20261023)
  for (i in 1:150) {
    k <- sample(1:3, 1)
    sizes <- sort(sample(c(1, 2, 5, 10, 20, 25, 40, 50, 100, 150), k))
    costs <- round(runif(k, 1, 20) * sizes / 10, 2)
    dose <- round(runif(1, 1, 300), sample(0:2, 1))
    t <- sample(c(0, 0.05, 0.1, 0.25), 1)
    got <- dose_detail(dose, sizes, costs, exact = FALSE, t = t)$cost
    ref <- brute_cost(dose, sizes, costs, exact = FALSE, t = t)
    expect_equal(got, ref, info = paste(dose, toString(sizes), toString(costs), t))
  }
})

test_that("exact dosing agrees with brute force on random problems", {
  set.seed(4242)
  for (i in 1:150) {
    k <- sample(1:3, 1)
    sizes <- sort(sample(c(2, 5, 10, 20, 25, 40, 50, 100), k))
    costs <- round(runif(k, 1, 20) * sizes / 10, 2)
    dose <- sample(1:300, 1)
    t <- 0
    ref <- brute_cost(dose, sizes, costs, exact = TRUE, t = t)
    if (is.na(ref)) {
      expect_error(dose_detail(dose, sizes, costs, exact = TRUE, t = t),
                   "cannot be made exactly")
    } else {
      got <- dose_detail(dose, sizes, costs, exact = TRUE, t = t)$cost
      expect_equal(got, ref, info = paste(dose, toString(sizes), t))
    }
  }
})

# ===========================================================================
# cost_single_admin_drug and regimen helpers
# ===========================================================================

reg_dist <- function(...) {
  args <- list(dose_per_admin = 5, dose_basis = "weight", patient_weight = 70,
               patient_weight_sd = 15, unit_size = c(100, 400),
               unit_cost = c(500, 1800), admin_days = 1L, exact = FALSE,
               charge_timing = "per_admin")
  over <- list(...)
  args[names(over)] <- over
  do.call(define_regimen, args)
}

test_that("cost_single_admin_drug for a fixed dose is the dose cost", {
  reg <- reg_dist(patient_weight_sd = NULL)
  expect_equal(cost_single_admin_drug(reg), dose_detail(350, c(100, 400),
                                                        c(500, 1800))$cost)
})

test_that("a length-1 unit_cost applies to every size", {
  reg <- reg_dist(patient_weight_sd = NULL, unit_cost = 7)
  expect_equal(regimen_units(reg)$costs, c(7, 7))
})

test_that("regimen_distribution uses the SD matching dose_basis only", {
  expect_null(regimen_distribution(reg_dist(dose_basis = "flat")))
  expect_null(regimen_distribution(reg_dist(dose_basis = "bsa")))
  expect_equal(regimen_distribution(reg_dist()), list(mean = 70, sd = 15))
})

# ===========================================================================
# Distributions
# ===========================================================================

test_that("bin probabilities sum to 1 and bins are contiguous", {
  reg <- reg_dist()
  bins <- generate_cost_bins(reg, 70, 15)
  expect_equal(sum(bins$probability), 1, tolerance = 1e-6)
  expect_equal(bins$dose_lo[-1], bins$dose_hi[-nrow(bins)])
  expect_true(all(bins$dose_lo >= 0))
})

test_that("each bin's cost equals the cost at its midpoint", {
  reg <- reg_dist(max_dose_rounding = 0.1)
  bins <- generate_cost_bins(reg, 70, 15)
  for (i in seq_len(nrow(bins))) {
    mid <- (bins$dose_lo[i] + bins$dose_hi[i]) / 2
    expect_equal(bins$cost[i], cost_single_dose(mid, reg)$cost)
  }
})

test_that("adjacent bins differ in cost or units", {
  # 100 mg and 150 mg units at the same price: doses up to 100 mg and doses
  # from 100 to 150 mg cost the same but use different units.
  reg <- reg_dist(dose_per_admin = 2, unit_size = c(100, 150),
                  unit_cost = c(500, 500))
  bins <- generate_cost_bins(reg, 70, 15, details = TRUE)
  details <- attr(bins, "details")
  keys <- vapply(details, function(d) paste(d$cost, toString(d$units)),
                 character(1))
  expect_gt(length(keys), 1)
  expect_true(all(keys[-1] != keys[-length(keys)]))
  expect_true(any(diff(bins$cost) == 0))
})

test_that("rounding adds breakpoints at amount / (1 - t)", {
  b0 <- generate_cost_bins(reg_dist(), 70, 15)
  b1 <- generate_cost_bins(reg_dist(max_dose_rounding = 0.1), 70, 15)
  expect_true(any(abs(b1$dose_hi - 400 / 0.9) < 1e-6))
  expect_false(isTRUE(all.equal(sum(b0$cost * b0$probability),
                                sum(b1$cost * b1$probability))))
})

test_that("expected cost matches a fine quantile average", {
  reg <- reg_dist(unit_size = c(100, 150), unit_cost = c(500, 700),
                  max_dose_rounding = 0.05)
  expected <- cost_single_admin_drug(reg)
  sigma2 <- log(1 + (15 / 70)^2)
  mu <- log(70) - sigma2 / 2
  n <- 20000
  w <- stats::qlnorm((seq_len(n) - 0.5) / n, mu, sqrt(sigma2))
  tab <- unit_table(c(100, 150), c(500, 700), max(w) * 5)
  costs <- vapply(w * 5, function(d) cost_dose_detail(d, tab, FALSE, 0.05)$cost,
                  numeric(1))
  expect_equal(expected, mean(costs), tolerance = 2e-3)
})

test_that("distribution cost differs from the point estimate", {
  expect_false(isTRUE(all.equal(
    cost_single_admin_drug(reg_dist()),
    cost_single_admin_drug(reg_dist(patient_weight_sd = NULL))
  )))
})

test_that("a very small SD converges to the point estimate", {
  expect_equal(cost_single_admin_drug(reg_dist(patient_weight = 73,
                                               patient_weight_sd = 1e-4)),
               cost_single_admin_drug(reg_dist(patient_weight = 73,
                                               patient_weight_sd = NULL)),
               tolerance = 1e-6)
})

test_that("BSA distributions work", {
  reg <- define_regimen(dose_per_admin = 1000, dose_basis = "bsa",
                        patient_bsa = 1.8, patient_bsa_sd = 0.2,
                        unit_size = c(150, 500), unit_cost = 2.21,
                        admin_days = 1:14, exact = FALSE,
                        charge_timing = "per_admin")
  bins <- generate_cost_bins(reg, 1.8, 0.2)
  expect_equal(sum(bins$probability), 1, tolerance = 1e-6)
  expect_gt(cost_single_admin_drug(reg), 0)
})

test_that("no wastage with a distribution = lowest $/mg x mean dose", {
  reg <- reg_dist(no_wastage = TRUE)
  expect_equal(cost_single_admin_drug(reg), 350 * min(500 / 100, 1800 / 400))
})

# ===========================================================================
# C++ exact-fill table
# ===========================================================================

test_that("exact_fill_table validates its inputs", {
  expect_error(exact_fill_table(c(50L, 100L), 5, 200L), "one value per unit size")
  expect_error(exact_fill_table(c(0L, 100L), c(1, 2), 200L), "positive")
  expect_error(exact_fill_table(100L, 5, -1L), "non-negative")
  expect_error(exact_fill_table(integer(0), numeric(0), 10L), "at least one")
})

test_that("exact_fill_table marks unreachable amounts as Inf", {
  tab <- exact_fill_table(c(3L, 5L), c(4, 6), 8L)
  expect_equal(tab$cost, c(0, Inf, Inf, 4, Inf, 6, 8, Inf, 10))
  expect_equal(tab$units[9], 2L)
})
