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

test_that("IV: multiple vial sizes with greedy packing", {
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
  # Greedy: 1x100 (remaining 30), then 0x50 (30<50)
  # Residual 30: smallest vial covering 30 is 50, frac=30/50=0.6>0 => open
  res <- openqalyregimen:::cost_iv_single(
    dose_mg = 130,
    vial_sizes = c(100, 50),
    vial_costs = c(400, 200)
  )
  expect_equal(res$n_vials[["100mg"]], 1L)
  expect_equal(res$n_vials[["50mg"]], 1L)
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
  reg <- define_regimen(
    route = "iv",
    dose_per_admin = 100,
    vial_size = 100,
    vial_cost = 500
  )
  cost <- openqalyregimen:::cost_single_admin_drug(reg)
  expect_equal(cost, 500)
})

test_that("cost_single_admin_drug dispatches to oral path", {
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 50,
    unit_cost = 10
  )
  cost <- openqalyregimen:::cost_single_admin_drug(reg)
  expect_equal(cost, 20)
})

test_that("cost_single_admin_drug returns numeric scalar", {
  reg <- define_regimen(
    vial_size = 100,
    vial_cost = 500
  )
  cost <- openqalyregimen:::cost_single_admin_drug(reg)
  expect_true(is.numeric(cost))
  expect_length(cost, 1)
})

test_that("cost_single_admin_drug: named unit_cost triggers cost_per_strength path", {
  costs <- c("100" = 20, "50" = 10)
  reg <- define_regimen(
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

test_that("cost_oral_cycle_dispensing returns per_admin_cost * n_admin_per_cycle", {
  reg <- define_regimen(
    route = "oral",
    dose_per_admin = 100,
    tablet_strength = 100,
    unit_cost = 10,
    n_admin_per_cycle = 3,
    admin_days = c(1L, 8L, 15L)
  )
  disp_cost <- openqalyregimen:::cost_oral_cycle_dispensing(reg)
  per_admin <- openqalyregimen:::cost_single_admin_drug(reg)
  expect_equal(disp_cost, per_admin * 3)
})
