# ===========================================================================
# S3 class assignment tests (no flextable needed)
# ===========================================================================

test_that("single regimen gets regimen_costs class", {
  reg <- define_regimen(vial_size = 100, vial_cost = 500, admin_days = 1L)
  out <- calculate_med_costs(reg, n_cycles = 3)
  expect_s3_class(out, "regimen_costs")
  expect_true(is.data.frame(out))
})

test_that("single regimen daily_detail gets regimen_daily_detail class", {
  reg <- define_regimen(vial_size = 100, vial_cost = 500,
    med_cycle_length = 7, admin_days = 1L)
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 2, daily_detail = TRUE)
  expect_s3_class(out, "regimen_daily_detail")
  expect_true(is.data.frame(out))
})

test_that("multi-regimen gets combo_regimen_costs class", {
  reg_a <- define_regimen(name = "A", vial_size = 100, vial_cost = 500, admin_days = 1L)
  reg_b <- define_regimen(name = "B", vial_size = 50, vial_cost = 200, admin_days = 1L)
  out <- calculate_med_costs(reg_a, reg_b, n_cycles = 3)
  expect_s3_class(out, "combo_regimen_costs")
  expect_true(is.data.frame(out))
  expect_equal(attr(out, "drug_names"), c("A", "B"))
})

test_that("multi-regimen daily_detail gets combo_regimen_daily_detail class", {
  reg_a <- define_regimen(name = "A", vial_size = 100, vial_cost = 500,
    med_cycle_length = 7, admin_days = 1L)
  reg_b <- define_regimen(name = "B", vial_size = 50, vial_cost = 200,
    med_cycle_length = 7, admin_days = 1L)
  out <- calculate_med_costs(reg_a, reg_b, model_cycle_length = 7,
    n_cycles = 2, daily_detail = TRUE)
  expect_s3_class(out, "combo_regimen_daily_detail")
  expect_true(is.data.frame(out))
  expect_equal(attr(out, "drug_names"), c("A", "B"))
  expect_true(is.list(attr(out, "drug_columns")))
})

test_that("detail_schedule gets regimen_schedule class", {
  reg <- define_regimen(vial_size = 100, vial_cost = 500,
    med_cycle_length = 7, admin_days = 1L)
  out <- detail_schedule(reg, model_cycle_length = 7, n_cycles = 2)
  expect_s3_class(out, "regimen_schedule")
  expect_true(is.data.frame(out))
})


# ===========================================================================
# Flextable conversion tests
# ===========================================================================

test_that("as_flextable.regimen_costs returns flextable", {
  skip_if_not_installed("flextable")
  reg <- define_regimen(vial_size = 100, vial_cost = 500, admin_days = 1L)
  out <- calculate_med_costs(reg, n_cycles = 3)
  ft <- flextable::as_flextable(out)
  expect_s3_class(ft, "flextable")
})

test_that("as_flextable.regimen_costs works for oral wastage", {
  skip_if_not_installed("flextable")
  reg <- define_regimen(
    route = "oral", dose_per_admin = 100,
    tablet_strength = 100, unit_cost = 10,
    med_cycle_length = 7, admin_days = 1L
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7, n_cycles = 3)
  ft <- flextable::as_flextable(out)
  expect_s3_class(ft, "flextable")
})

test_that("as_flextable.combo_regimen_costs has 2-row header", {
  skip_if_not_installed("flextable")
  reg_a <- define_regimen(name = "A", vial_size = 100, vial_cost = 500, admin_days = 1L)
  reg_b <- define_regimen(name = "B", vial_size = 50, vial_cost = 200, admin_days = 1L)
  out <- calculate_med_costs(reg_a, reg_b, n_cycles = 3)
  ft <- flextable::as_flextable(out)
  expect_s3_class(ft, "flextable")
  expect_equal(nrow(ft$header$dataset), 2)
})

test_that("as_flextable.regimen_daily_detail IV returns flextable", {
  skip_if_not_installed("flextable")
  reg <- define_regimen(
    route = "iv", vial_size = 100, vial_cost = 500,
    med_cycle_length = 7, admin_days = 1L
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 2, daily_detail = TRUE)
  ft <- flextable::as_flextable(out)
  expect_s3_class(ft, "flextable")
  expect_equal(nrow(ft$header$dataset), 2)
})

test_that("as_flextable.regimen_daily_detail oral no-wastage returns flextable", {
  skip_if_not_installed("flextable")
  reg <- define_regimen(
    route = "oral", oral_wastage = FALSE,
    dose_per_admin = 100, tablet_strength = 50,
    unit_cost = 10, med_cycle_length = 7, admin_days = 1L
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 2, daily_detail = TRUE)
  ft <- flextable::as_flextable(out)
  expect_s3_class(ft, "flextable")
})

test_that("as_flextable.regimen_daily_detail oral wastage returns flextable", {
  skip_if_not_installed("flextable")
  reg <- define_regimen(
    route = "oral", dose_per_admin = 100,
    tablet_strength = 100, unit_cost = 10,
    med_cycle_length = 7, admin_days = 1L
  )
  out <- calculate_med_costs(reg, model_cycle_length = 7,
    n_cycles = 2, daily_detail = TRUE)
  ft <- flextable::as_flextable(out)
  expect_s3_class(ft, "flextable")
  expect_equal(nrow(ft$header$dataset), 2)
})

test_that("as_flextable.combo_regimen_daily_detail has 2-row header with drug spans", {
  skip_if_not_installed("flextable")
  reg_a <- define_regimen(
    name = "Drug A", vial_size = 100, vial_cost = 500,
    med_cycle_length = 7, admin_days = 1L
  )
  reg_b <- define_regimen(
    name = "Drug B", vial_size = 50, vial_cost = 200,
    med_cycle_length = 7, admin_days = 1L
  )
  out <- calculate_med_costs(reg_a, reg_b, model_cycle_length = 7,
    n_cycles = 2, daily_detail = TRUE)
  ft <- flextable::as_flextable(out)
  expect_s3_class(ft, "flextable")
  expect_equal(nrow(ft$header$dataset), 2)
})

test_that("as_flextable.regimen_schedule returns flextable", {
  skip_if_not_installed("flextable")
  reg <- define_regimen(
    route = "oral", dose_per_admin = 100,
    tablet_strength = 100, unit_cost = 10,
    med_cycle_length = 7, admin_days = 1L
  )
  out <- detail_schedule(reg, model_cycle_length = 7, n_cycles = 2)
  ft <- flextable::as_flextable(out)
  expect_s3_class(ft, "flextable")
  expect_equal(nrow(ft$header$dataset), 2)
})

test_that("3-drug combo gets correct spanning headers", {
  skip_if_not_installed("flextable")
  reg_a <- define_regimen(name = "A", vial_size = 100, vial_cost = 500, admin_days = 1L)
  reg_b <- define_regimen(name = "B", vial_size = 50, vial_cost = 200, admin_days = 1L)
  reg_c <- define_regimen(name = "C", vial_size = 25, vial_cost = 100, admin_days = 1L)
  out <- calculate_med_costs(reg_a, reg_b, reg_c, n_cycles = 3)
  ft <- flextable::as_flextable(out)
  expect_s3_class(ft, "flextable")
  expect_equal(nrow(ft$header$dataset), 2)
})
