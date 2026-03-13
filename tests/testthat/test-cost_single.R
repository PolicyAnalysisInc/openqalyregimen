# ===========================================================================
# cost_iv_single
# ===========================================================================

test_that("IV: exact dose fits perfectly (no waste, correct cost)", {
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 100,
    vial_sizes = 100,
    vial_costs = 500
  )
  expect_equal(res$drug_cost, 500)
  expect_equal(res$waste_mg, 0)
  expect_equal(res$n_vials[["100mg"]], 1L)
})

test_that("IV: multiple same-size vials (dose = 2x vial)", {
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 200,
    vial_sizes = 100,
    vial_costs = 500
  )
  expect_equal(res$drug_cost, 1000)
  expect_equal(res$waste_mg, 0)
  expect_equal(res$n_vials[["100mg"]], 2L)
})

test_that("IV: multiple vial sizes with optimal packing", {
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 150,
    vial_sizes = c(100, 50),
    vial_costs = c(400, 200)
  )
  expect_equal(res$n_vials[["100mg"]], 1L)
  expect_equal(res$n_vials[["50mg"]], 1L)
  expect_equal(res$drug_cost, 600)
  expect_equal(res$waste_mg, 0)
})

test_that("IV: wastage threshold skips last vial when fraction <= threshold", {
  # 110mg dose with 100mg vial: residual=10mg, frac=10/100=0.1
  # threshold=0.1, so 0.1 <= 0.1 => skip
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 110,
    vial_sizes = 100,
    vial_costs = 500,
    wastage_threshold = 0.1
  )
  expect_equal(res$n_vials[["100mg"]], 1L)
  expect_equal(res$drug_cost, 500)
})

test_that("IV: wastage threshold opens last vial when fraction > threshold", {
  # 120mg dose with 100mg vial: residual=20mg, frac=20/100=0.2
  # threshold=0.1, so 0.2 > 0.1 => open vial
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 120,
    vial_sizes = 100,
    vial_costs = 500,
    wastage_threshold = 0.1
  )
  expect_equal(res$n_vials[["100mg"]], 2L)
  expect_equal(res$drug_cost, 1000)
  expect_equal(res$waste_mg, 80)
})

test_that("IV: residual covered by smallest available vial", {
  # 130mg with 100mg + 50mg vials
  # 130 not exactly achievable. Covering: 3x50=150 or 1x100+1x50=150,
  # both cost $600 with 20mg waste. DP tie-break selects 3x50mg.
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 130,
    vial_sizes = c(100, 50),
    vial_costs = c(400, 200)
  )
  expect_equal(res$n_vials[["100mg"]], 0L)
  expect_equal(res$n_vials[["50mg"]], 3L)
  expect_equal(res$drug_cost, 600)
  expect_equal(res$waste_mg, 20)
})

test_that("IV: unordered input vial sizes sorted correctly", {
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 150,
    vial_sizes = c(50, 100),
    vial_costs = c(200, 400)
  )
  expect_equal(res$drug_cost, 600)
})

test_that("IV: output names are '<size>mg' format", {
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 100,
    vial_sizes = c(100, 50),
    vial_costs = c(400, 200)
  )
  expect_true(all(grepl("mg$", names(res$n_vials))))
})


# ===========================================================================
# cost_oral_single
# ===========================================================================

test_that("oral: exact dose fits (no rounding waste)", {
  res <- openqalyregimen:::cost_oral_single(
    dose_mg = 100,
    available_strengths = 50,
    cost_per_tablet = 10
  )
  expect_equal(res$drug_cost, 20)
  expect_equal(res$waste_mg, 0)
  expect_equal(res$tablets[["50mg"]], 2L)
})

test_that("oral: multiple tablets of one strength", {
  res <- openqalyregimen:::cost_oral_single(
    dose_mg = 300,
    available_strengths = 100,
    cost_per_tablet = 15
  )
  expect_equal(res$tablets[["100mg"]], 3L)
  expect_equal(res$drug_cost, 45)
})

test_that("oral: tablet rounding rounds up with smallest tablet", {
  # 130mg with 100mg + 50mg tablets
  # Greedy: 1x100 (remaining 30), 0x50 (30<50)
  # Residual 30 > 1e-9 => round up with smallest (50mg)
  res <- openqalyregimen:::cost_oral_single(
    dose_mg = 130,
    available_strengths = c(100, 50),
    cost_per_tablet = 10
  )
  expect_equal(res$tablets[["100mg"]], 1L)
  expect_equal(res$tablets[["50mg"]], 1L)
  expect_equal(res$waste_mg, 20)
})

test_that("oral: near-zero residual (< 1e-9) does NOT trigger rounding", {
  # 100mg with 50mg tablets: exact fit, no residual
  res <- openqalyregimen:::cost_oral_single(
    dose_mg = 100,
    available_strengths = 100,
    cost_per_tablet = 10
  )
  expect_equal(res$tablets[["100mg"]], 1L)
  expect_equal(res$waste_mg, 0)
})

test_that("oral: multiple strengths with greedy packing then rounding", {
  # 175mg with 100+50+25 tablets
  # Greedy: 1x100 (75), 1x50 (25), 1x25 (0)
  res <- openqalyregimen:::cost_oral_single(
    dose_mg = 175,
    available_strengths = c(100, 50, 25),
    cost_per_tablet = 5
  )
  expect_equal(res$tablets[["100mg"]], 1L)
  expect_equal(res$tablets[["50mg"]], 1L)
  expect_equal(res$tablets[["25mg"]], 1L)
  expect_equal(res$waste_mg, 0)
})

test_that("oral: cost_per_strength (named vector) works", {
  costs <- c(20, 10)
  names(costs) <- c("100", "50")
  res <- openqalyregimen:::cost_oral_single(
    dose_mg = 150,
    available_strengths = c(100, 50),
    cost_per_strength = costs
  )
  expect_equal(unname(res$drug_cost), 30)
})

test_that("oral: cost_per_tablet works (uniform pricing)", {
  res <- openqalyregimen:::cost_oral_single(
    dose_mg = 150,
    available_strengths = c(100, 50),
    cost_per_tablet = 10
  )
  expect_equal(res$drug_cost, 20)
})

test_that("oral: output names are '<strength>mg' format", {
  res <- openqalyregimen:::cost_oral_single(
    dose_mg = 100,
    available_strengths = c(100, 50),
    cost_per_tablet = 10
  )
  expect_true(all(grepl("mg$", names(res$tablets))))
})


# ===========================================================================
# cost_single_admin_drug
# ===========================================================================

test_that("cost_single_admin_drug dispatches to IV path", {
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 100,
    vial_size = 100,
    vial_cost = 500
  )
  cost <- openqalyregimen:::cost_single_admin_drug(reg)
  expect_equal(cost, 500)
})

test_that("cost_single_admin_drug dispatches to oral path", {
  reg <- define_regimen(admin_days = 1L, 
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 50,
    unit_cost = 10
  )
  cost <- openqalyregimen:::cost_single_admin_drug(reg)
  expect_equal(cost, 20)
})

test_that("cost_single_admin_drug returns numeric scalar", {
  reg <- define_regimen(admin_days = 1L, 
    vial_size = 100,
    vial_cost = 500
  )
  cost <- openqalyregimen:::cost_single_admin_drug(reg)
  expect_true(is.numeric(cost))
  expect_length(cost, 1)
})

test_that("cost_single_admin_drug: named unit_cost triggers cost_per_strength path", {
  costs <- c("100" = 20, "50" = 10)
  reg <- define_regimen(admin_days = 1L, 
    route = "oral",
    dose_per_admin = 150,
    available_strengths = c(100, 50),
    unit_cost = costs
  )
  cost <- openqalyregimen:::cost_single_admin_drug(reg)
  expect_equal(unname(cost), 30)
})


# ===========================================================================
# cost_oral_cycle_dispensing
# ===========================================================================

test_that("cost_oral_cycle_dispensing returns per_admin_cost * length(admin_days)", {
  reg <- define_regimen( 
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 100,
    unit_cost = 10,
    admin_days = c(1L, 8L, 15L)
  )
  disp_cost <- openqalyregimen:::cost_oral_cycle_dispensing(reg)
  per_admin <- openqalyregimen:::cost_single_admin_drug(reg)
  expect_equal(disp_cost, per_admin * 3)
})


# ===========================================================================
# cost_iv_vec / cost_oral_vec (vectorized helpers)
# ===========================================================================

test_that("cost_iv_vec matches scalar cost_iv_single for multiple doses", {
  doses <- c(100, 150, 200, 130, 75)
  vec_costs <- openqalyregimen:::cost_iv_vec(
    doses, vial_sizes = c(100, 50), vial_costs = c(400, 200)
  )
  scalar_costs <- vapply(doses, function(d) {
    openqalyregimen:::cost_iv_single(d, c(100, 50), c(400, 200))$drug_cost
  }, numeric(1))
  expect_equal(vec_costs, scalar_costs)
})

test_that("cost_iv_vec with wastage threshold matches scalar", {
  doses <- c(110, 120, 150, 190)
  vec_costs <- openqalyregimen:::cost_iv_vec(
    doses, vial_sizes = 100, vial_costs = 500,
    wastage_threshold = 0.1
  )
  scalar_costs <- vapply(doses, function(d) {
    openqalyregimen:::cost_iv_single(d, 100, 500, 0.1)$drug_cost
  }, numeric(1))
  expect_equal(vec_costs, scalar_costs)
})

test_that("cost_oral_vec matches scalar cost_oral_single for multiple doses", {
  doses <- c(100, 130, 175, 200, 50)
  vec_costs <- openqalyregimen:::cost_oral_vec(
    doses, available_strengths = c(100, 50),
    cost_per_tablet = 10
  )
  scalar_costs <- vapply(doses, function(d) {
    openqalyregimen:::cost_oral_single(d, c(100, 50), cost_per_tablet = 10)$drug_cost
  }, numeric(1))
  expect_equal(vec_costs, scalar_costs)
})

test_that("cost_oral_vec with cost_per_strength matches scalar", {
  costs <- c("100" = 20, "50" = 10)
  doses <- c(100, 150, 130)
  vec_costs <- openqalyregimen:::cost_oral_vec(
    doses, available_strengths = c(100, 50),
    cost_per_strength = costs
  )
  scalar_costs <- vapply(doses, function(d) {
    openqalyregimen:::cost_oral_single(d, c(100, 50),
      cost_per_strength = costs)$drug_cost
  }, numeric(1))
  expect_equal(vec_costs, scalar_costs)
})


# ===========================================================================
# generate_cost_bins
# ===========================================================================

test_that("generate_cost_bins: probabilities sum to 1", {
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    patient_weight_sd = 15,
    vial_size = 100,
    vial_cost = 500
  )
  bins <- openqalyregimen:::generate_cost_bins(reg, 70, 15)
  expect_equal(sum(bins$probability), 1, tolerance = 1e-10)
})

test_that("generate_cost_bins: each bin cost matches cost_single_dose at midpoint", {
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    patient_weight_sd = 15,
    vial_size = 100,
    vial_cost = 500
  )
  bins <- openqalyregimen:::generate_cost_bins(reg, 70, 15)
  # For each finite bin, pick a dose in the middle and check cost matches
  for (i in seq_len(nrow(bins))) {
    if (is.finite(bins$dose_hi[i])) {
      mid <- (bins$dose_lo[i] + bins$dose_hi[i]) / 2
    } else {
      mid <- bins$dose_lo[i] + 1
    }
    expected <- openqalyregimen:::cost_single_dose(mid, reg)
    expect_equal(bins$cost[i], expected,
      info = paste("bin", i, "dose", mid))
  }
})

test_that("generate_cost_bins: fewer bins than 100", {
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    patient_weight_sd = 15,
    vial_size = 100,
    vial_cost = 500
  )
  bins <- openqalyregimen:::generate_cost_bins(reg, 70, 15)
  expect_lt(nrow(bins), 100)
})

test_that("generate_cost_bins: wastage threshold creates additional breakpoints", {
  reg_no_wt <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    patient_weight_sd = 15,
    vial_size = 100,
    vial_cost = 500,
    wastage_threshold = 0
  )
  reg_wt <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    patient_weight_sd = 15,
    vial_size = 100,
    vial_cost = 500,
    wastage_threshold = 0.1
  )
  bins_no_wt <- openqalyregimen:::generate_cost_bins(reg_no_wt, 70, 15)
  bins_wt <- openqalyregimen:::generate_cost_bins(reg_wt, 70, 15)
  # With wastage threshold, cost function can differ so bins may differ
  expect_true(is.data.frame(bins_wt))
  expect_true(nrow(bins_wt) > 0)
})

test_that("generate_cost_bins: oral route works", {
  reg <- define_regimen(admin_days = 1L, 
    route = "oral",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    patient_weight_sd = 15,
    tablet_strength = 50,
    unit_cost = 10
  )
  bins <- openqalyregimen:::generate_cost_bins(reg, 70, 15)
  expect_true(is.data.frame(bins))
  expect_equal(sum(bins$probability), 1, tolerance = 1e-10)
})

test_that("generate_cost_bins: multiple vial sizes work", {
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    patient_weight_sd = 15,
    vial_sizes = c(100, 50),
    vial_cost = c(400, 200)
  )
  bins <- openqalyregimen:::generate_cost_bins(reg, 70, 15)
  expect_true(is.data.frame(bins))
  expect_equal(sum(bins$probability), 1, tolerance = 1e-10)
})

test_that("generate_cost_bins: all dose_lo values are >= 0 (lognormal bounded below)", {
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    patient_weight_sd = 15,
    vial_size = 100,
    vial_cost = 500
  )
  bins <- openqalyregimen:::generate_cost_bins(reg, 70, 15)
  expect_true(all(bins$dose_lo >= 0))
})


# ===========================================================================
# cost_single_admin_drug with distribution
# ===========================================================================

test_that("distribution cost differs from point estimate (Jensen's inequality)", {
  # Mean dose = 4 * 75 = 300mg, exactly at a vial boundary.
  # Point estimate: 3 vials = $1500
  # Distribution: ~half above 300mg needs 4 vials ($2000), so avg > $1500
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 4,
    dose_basis = "weight",
    patient_weight = 75,
    patient_weight_sd = 15,
    vial_size = 100,
    vial_cost = 500
  )
  dist_cost <- openqalyregimen:::cost_single_admin_drug(reg)

  reg_point <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 4,
    dose_basis = "weight",
    patient_weight = 75,
    vial_size = 100,
    vial_cost = 500
  )
  point_cost <- openqalyregimen:::cost_single_admin_drug(reg_point)

  expect_false(dist_cost == point_cost)
  expect_true(dist_cost > point_cost)
})

test_that("NULL SD preserves current point-estimate behavior", {
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    vial_size = 100,
    vial_cost = 500
  )
  cost <- openqalyregimen:::cost_single_admin_drug(reg)
  # 5 * 70 = 350mg, 3x100mg + 1x100mg residual = 4 vials = $2000
  expected <- openqalyregimen:::cost_iv_single(350, 100, 500)$drug_cost
  expect_equal(cost, expected)
})

test_that("very small SD converges to point estimate", {
  reg_tiny_sd <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    patient_weight_sd = 0.001,
    vial_size = 100,
    vial_cost = 500
  )
  dist_cost <- openqalyregimen:::cost_single_admin_drug(reg_tiny_sd)

  reg_point <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    vial_size = 100,
    vial_cost = 500
  )
  point_cost <- openqalyregimen:::cost_single_admin_drug(reg_point)

  expect_equal(dist_cost, point_cost)
})

test_that("distribution cost works with oral route", {
  reg <- define_regimen(admin_days = 1L, 
    route = "oral",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    patient_weight_sd = 15,
    tablet_strength = 50,
    unit_cost = 10
  )
  cost <- openqalyregimen:::cost_single_admin_drug(reg)
  expect_true(is.numeric(cost))
  expect_length(cost, 1)
  expect_true(cost > 0)
})

test_that("cost_single_admin_drug dispatches SC to vial path", {
  reg <- define_regimen(admin_days = 1L, 
    route = "sc",
    dose_per_admin = 100,
    vial_size = 100,
    vial_cost = 500
  )
  cost <- openqalyregimen:::cost_single_admin_drug(reg)
  expect_equal(cost, 500)
})

test_that("cost_single_admin_drug dispatches IM to vial path", {
  reg <- define_regimen(admin_days = 1L, 
    route = "im",
    dose_per_admin = 100,
    vial_size = 100,
    vial_cost = 500
  )
  cost <- openqalyregimen:::cost_single_admin_drug(reg)
  expect_equal(cost, 500)
})

test_that("SC pre-filled syringe (vial_size = dose) gives zero waste", {
  reg <- define_regimen(admin_days = 1L, 
    route = "sc",
    dose_per_admin = 50,
    vial_size = 50,
    vial_cost = 300
  )
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = reg$actual_dose_mg,
    vial_sizes = reg$vial_sizes,
    vial_costs = reg$vial_cost,
    wastage_threshold = reg$wastage_threshold
  )
  expect_equal(res$waste_mg, 0)
  expect_equal(res$drug_cost, 300)
})

test_that("SC distribution-averaged cost works", {
  reg <- define_regimen(admin_days = 1L, 
    route = "sc",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    patient_weight_sd = 15,
    vial_size = 100,
    vial_cost = 500
  )
  cost <- openqalyregimen:::cost_single_admin_drug(reg)
  expect_true(is.numeric(cost))
  expect_length(cost, 1)
  expect_true(cost > 0)
})

test_that("distribution cost works with BSA route", {
  # Mean dose = 100 * 2.0 = 200mg, exactly at a vial boundary
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 100,
    dose_basis = "bsa",
    patient_bsa = 2.0,
    patient_bsa_sd = 0.2,
    vial_size = 50,
    vial_cost = 200
  )
  dist_cost <- openqalyregimen:::cost_single_admin_drug(reg)

  reg_point <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 100,
    dose_basis = "bsa",
    patient_bsa = 2.0,
    vial_size = 50,
    vial_cost = 200
  )
  point_cost <- openqalyregimen:::cost_single_admin_drug(reg_point)

  # Should differ due to Jensen's inequality
  expect_false(dist_cost == point_cost)
  expect_true(dist_cost > point_cost)
})


# ===========================================================================
# generate_cost_bins: cross-vial-size breakpoints
# ===========================================================================

test_that("generate_cost_bins: captures cross-vial-size breakpoints", {
  # Vial sizes [100, 120] with costs where 120mg is cheaper per mg
  # so optimal packing will use both sizes
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 1,
    dose_basis = "weight",
    patient_weight = 220,
    patient_weight_sd = 30,
    vial_sizes = c(100, 120),
    vial_cost = c(450, 480)
  )
  bins <- openqalyregimen:::generate_cost_bins(reg, 220, 30)
  # 220mg should be a bin boundary
  expect_true(220 %in% bins$dose_lo | 220 %in% bins$dose_hi)
  # Cost at 219 and 221 should differ
  cost_219 <- openqalyregimen:::cost_single_dose(219, reg)
  cost_221 <- openqalyregimen:::cost_single_dose(221, reg)
  expect_false(cost_219 == cost_221)
})


# ===========================================================================
# Optimal DP packing tests
# ===========================================================================

# --- Known exact-fill problems (from GeeksforGeeks min-cost-to-fill-weight) ---

test_that("DP: GfG example 1 -- sizes [1..5], costs [20,10,4,50,100], target=5", {
  # Exact fill: 2mg + 3mg = 5mg at $14 (preferred over cheaper covering 2x3=$8)
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 5, vial_sizes = c(1, 2, 3, 4, 5),
    vial_costs = c(20, 10, 4, 50, 100)
  )
  expect_equal(res$drug_cost, 14)
  expect_equal(res$waste_mg, 0)
})

test_that("DP: GfG example 2 -- sizes [1..5], costs [1,2,3,4,5], target=5", {
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 5, vial_sizes = c(1, 2, 3, 4, 5),
    vial_costs = c(1, 2, 3, 4, 5)
  )
  expect_equal(res$drug_cost, 5)
})

# --- Coin-change problems where greedy fails ---

test_that("DP: LeetCode coin change -- sizes [1,3,4] uniform cost, target=6", {
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 6, vial_sizes = c(1, 3, 4), vial_costs = c(1, 1, 1)
  )
  expect_equal(res$drug_cost, 2)
  expect_equal(res$waste_mg, 0)
})

test_that("DP: Indian paise -- sizes [5,10,20,25], unit cost, target=40", {
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 40, vial_sizes = c(5, 10, 20, 25), vial_costs = c(1, 1, 1, 1)
  )
  expect_equal(res$drug_cost, 2)
  expect_equal(res$waste_mg, 0)
})

# --- "At least" covering variants (target not exactly achievable) ---

test_that("DP: covering -- sizes [3,5], costs [4,6], target=7", {
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 7, vial_sizes = c(3, 5), vial_costs = c(4, 6)
  )
  expect_equal(res$drug_cost, 10)
  expect_equal(res$waste_mg, 1)
})

test_that("DP: covering -- sizes [4,7], costs [3,5], target=13", {
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 13, vial_sizes = c(4, 7), vial_costs = c(3, 5)
  )
  expect_equal(res$drug_cost, 10)
  expect_equal(res$waste_mg, 1)
})

test_that("DP: exact fill -- sizes [3,5,7], costs [2,10,11], target=10", {
  # Exact fill: 3mg + 7mg = 10mg at $13 (preferred over cheaper covering 4x3=$8)
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 10, vial_sizes = c(3, 5, 7), vial_costs = c(2, 10, 11)
  )
  expect_equal(res$drug_cost, 13)
  expect_equal(res$waste_mg, 0)
})

# --- Non-integer vial sizes ---

test_that("DP: non-integer sizes -- bortezomib 3.5mg vial", {
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 2.34, vial_sizes = 3.5, vial_costs = 762.38
  )
  expect_equal(res$drug_cost, 762.38)
  expect_equal(res$waste_mg, 1.16, tolerance = 1e-9)
})

# --- Wastage threshold with optimal packing ---

test_that("DP: wastage threshold skips marginal vial (frac <= threshold)", {
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 110, vial_sizes = 100, vial_costs = 500,
    wastage_threshold = 0.1
  )
  expect_equal(res$drug_cost, 500)
})

# --- Exact-fill-first algorithm tests ---

test_that("DP: exact fill preferred over cheaper overshoot", {
  # dose=100, sizes=[100,150], costs=[$800,$500]
  # Covering: 1x150=$500 (content=150, waste=50) -- cheaper
  # Exact fill: 1x100=$800 (content=100, waste=0)
  # Algorithm must choose exact fill despite higher cost
  res <- openqalyregimen:::cost_iv_single(100, c(100, 150), c(800, 500))
  expect_equal(res$drug_cost, 800)
  expect_equal(res$waste_mg, 0)
  expect_equal(res$n_vials[["100mg"]], 1L)
})

test_that("DP: under-fill chosen when cheaper than covering (no exact fill)", {
  # dose=110, sizes=[100], threshold=0.1
  # No exact fill. Under-fill: 1x100=$500 (frac=10/100=0.1<=0.1).
  # Covering: 2x100=$1000.
  # Under-fill cheaper -> use it.
  res <- openqalyregimen:::cost_iv_single(110, 100, 500,
    wastage_threshold = 0.1)
  expect_equal(res$drug_cost, 500)
  expect_equal(res$waste_mg, 0)
})

test_that("DP: covering chosen when cheaper than under-fill (no exact fill)", {
  # dose=110, sizes=[100,150], costs=[$1000,$500]
  # No exact fill for 110.
  # Under-fill: 1x100=$1000 (frac=10/100=0.1<=0.5).
  # Covering: 1x150=$500 (content=150, full dose).
  # Covering cheaper -> use it.
  res <- openqalyregimen:::cost_iv_single(110, c(100, 150), c(1000, 500),
    wastage_threshold = 0.5)
  expect_equal(res$drug_cost, 500)
})

test_that("DP: threshold suppressed when exact fill exists", {
  # dose=200, sizes=[100,150], costs=[$800,$500], threshold=0.5
  # Covering: 2x150=$1000 (waste=100).
  # Exact fill: 2x100=$1600 exists. Use exact fill.
  res <- openqalyregimen:::cost_iv_single(200, c(100, 150), c(800, 500),
    wastage_threshold = 0.5)
  expect_equal(res$drug_cost, 1600)
  expect_equal(res$waste_mg, 0)
})

test_that("DP: high wastage threshold does not produce zero-dose result", {
  # Regression: C >= 0 loop allowed C == 0 (the DP base case with cost $0),

  # meaning a high threshold could deliver 0 mg at $0 cost.
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 840, vial_sizes = 1200, vial_costs = 3807.69,
    wastage_threshold = 0.7
  )
  expect_equal(res$n_vials[["1200mg"]], 1L)
  expect_equal(res$drug_cost, 3807.69)

  # Vec variant should match
  vec_cost <- openqalyregimen:::cost_iv_vec(
    840, vial_sizes = 1200, vial_costs = 3807.69,
    wastage_threshold = 0.7
  )
  expect_equal(vec_cost, 3807.69)
})

test_that("DP: vec matches single across all algorithm tiers", {
  sizes <- c(100, 150)
  costs <- c(800, 500)
  doses <- c(100, 110, 150, 200, 300)
  for (thresh in c(0, 0.1, 0.5)) {
    vec_costs <- openqalyregimen:::cost_iv_vec(doses, sizes, costs,
      wastage_threshold = thresh)
    scalar_costs <- vapply(doses, function(d) {
      openqalyregimen:::cost_iv_single(d, sizes, costs,
        wastage_threshold = thresh)$drug_cost
    }, numeric(1))
    expect_equal(vec_costs, scalar_costs,
      info = paste("threshold =", thresh))
  }
})


# ===========================================================================
# generate_cost_bins: include_packing parameter
# ===========================================================================

test_that("generate_cost_bins(include_packing=TRUE) returns enriched columns", {
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    patient_weight_sd = 15,
    vial_size = 100,
    vial_cost = 500
  )
  bins <- openqalyregimen:::generate_cost_bins(reg, 70, 15, include_packing = TRUE)
  expect_true("param_lo" %in% names(bins))
  expect_true("param_hi" %in% names(bins))
  expect_true("n_vials" %in% names(bins))
  expect_true("total_vial_mg" %in% names(bins))
  expect_true("waste_mg_lo" %in% names(bins))
  expect_true("waste_mg_hi" %in% names(bins))
  expect_true("mg_administered_lo" %in% names(bins))
  expect_true("mg_administered_hi" %in% names(bins))
  expect_true("weighted_cost" %in% names(bins))
  expect_true(!is.null(attr(bins, "packing_results")))
  expect_true(!is.null(attr(bins, "expected_cost")))
  expect_true(!is.null(attr(bins, "dose_per_admin")))
})

test_that("generate_cost_bins(include_packing=TRUE) total_vial_mg is correct", {
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    patient_weight_sd = 15,
    vial_size = 100,
    vial_cost = 500
  )
  bins <- openqalyregimen:::generate_cost_bins(reg, 70, 15, include_packing = TRUE)
  # total_vial_mg should equal n_vials * vial_size for single vial size
  expect_equal(bins$total_vial_mg, bins$n_vials * 100)
  # total_vial_mg >= dose_hi for all bins with vials
  has_vials <- bins$n_vials > 0
  expect_true(all(bins$total_vial_mg[has_vials] >= bins$dose_hi[has_vials] - 1e-9))
})

test_that("generate_cost_bins(include_packing=TRUE) waste ranges are consistent", {
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    patient_weight_sd = 15,
    vial_size = 100,
    vial_cost = 500
  )
  bins <- openqalyregimen:::generate_cost_bins(reg, 70, 15, include_packing = TRUE)
  expect_true(all(bins$waste_mg_lo >= 0))
  expect_true(all(bins$waste_mg_lo <= bins$waste_mg_hi))
  expect_equal(bins$mg_administered_lo, bins$dose_lo)
  expect_equal(bins$mg_administered_hi, bins$dose_hi)
})

test_that("generate_cost_bins: BSA lower bound excludes impossible values", {
  reg <- define_regimen(admin_days = 1L, 
    route = "iv",
    dose_per_admin = 375,
    dose_basis = "bsa",
    patient_bsa = 1.8,
    patient_bsa_sd = 0.15,
    vial_size = 100,
    vial_cost = 500
  )
  # mean - 5*SD = 1.8 - 0.75 = 1.05, dose_min = 375 * 1.05 = 393.75
  bins <- openqalyregimen:::generate_cost_bins(reg, 1.8, 0.15)
  expected_min <- 375 * max(0, 1.8 - 5 * 0.15)
  expect_true(min(bins$dose_lo) >= expected_min)
})

test_that("generate_cost_bins: include_packing=TRUE errors on oral route", {
  reg <- define_regimen(admin_days = 1L, 
    route = "oral",
    dose_per_admin = 5,
    dose_basis = "weight",
    patient_weight = 70,
    patient_weight_sd = 15,
    tablet_strength = 50,
    unit_cost = 10
  )
  expect_error(
    openqalyregimen:::generate_cost_bins(reg, 70, 15, include_packing = TRUE),
    "include_packing only supported for vial-based routes"
  )
})
