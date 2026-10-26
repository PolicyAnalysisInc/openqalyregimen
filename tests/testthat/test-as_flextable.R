reg_a <- function(...) {
  args <- list(name = "Drug A", dose_per_admin = 100, unit_size = 100,
               unit_cost = 500, med_cycle_length = 7, admin_days = 1L,
               exact = FALSE, charge_timing = "per_admin", admin_cost = 50)
  over <- list(...)
  args[names(over)] <- over
  do.call(define_regimen, args)
}

reg_b <- function(...) {
  args <- list(name = "Drug B", route = "oral", dose_per_admin = 3600,
               unit_size = c(150, 500), unit_cost = 2.21,
               med_cycle_length = 7, admin_days = 1:5, exact = TRUE,
               charge_timing = "per_dispensing", dispensing_cost = 45)
  over <- list(...)
  args[names(over)] <- over
  do.call(define_regimen, args)
}

# ===========================================================================
# S3 classes (no flextable needed)
# ===========================================================================

test_that("output classes", {
  expect_s3_class(calculate_med_costs(reg_a(), n_cycles = 3), "regimen_costs")
  expect_s3_class(calculate_med_costs(reg_a(), n_cycles = 2, daily_detail = TRUE),
                  "regimen_daily_detail")
  combo <- calculate_med_costs(reg_a(), reg_b(), n_cycles = 3)
  expect_s3_class(combo, "combo_regimen_costs")
  cd <- calculate_med_costs(reg_a(), reg_b(), n_cycles = 2, daily_detail = TRUE)
  expect_s3_class(cd, "combo_regimen_daily_detail")
  expect_true(is.list(attr(cd, "drug_columns")))
  expect_s3_class(detail_schedule(reg_a(), n_cycles = 2), "regimen_schedule")
})

# ===========================================================================
# Flextable conversion
# ===========================================================================

test_that("as_flextable.regimen_costs labels both fees", {
  skip_if_not_installed("flextable")
  ft <- flextable::as_flextable(calculate_med_costs(reg_b(), n_cycles = 3))
  expect_s3_class(ft, "flextable")
  labels <- unlist(ft$header$dataset[1, ])
  expect_true("Dispensing Cost" %in% labels || any(grepl("Dispensing", labels)))
})

test_that("as_flextable.combo_regimen_costs has a 2-row header", {
  skip_if_not_installed("flextable")
  ft <- flextable::as_flextable(calculate_med_costs(reg_a(), reg_b(),
                                                    n_cycles = 3))
  expect_equal(nrow(ft$header$dataset), 2)
  expect_true(any(unlist(ft$header$dataset[1, ]) == "Drug A"))
})

test_that("as_flextable.combo_regimen_costs with shared fees", {
  skip_if_not_installed("flextable")
  out <- calculate_med_costs(reg_a(), reg_b(admin_cost = 20), n_cycles = 3,
                             shared_admin = TRUE)
  expect_s3_class(flextable::as_flextable(out), "flextable")
})

test_that("as_flextable.regimen_daily_detail has a 2-row header", {
  skip_if_not_installed("flextable")
  for (r in list(reg_a(), reg_b())) {
    out <- calculate_med_costs(r, model_cycle_length = 7, n_cycles = 2,
                               daily_detail = TRUE)
    ft <- flextable::as_flextable(out)
    expect_s3_class(ft, "flextable")
    expect_equal(nrow(ft$header$dataset), 2)
  }
})

test_that("daily detail flextables work with a distribution and in combos", {
  skip_if_not_installed("flextable")
  w <- define_regimen(name = "W", dose_per_admin = 10, dose_basis = "weight",
                      patient_weight = 70, patient_weight_sd = 15,
                      unit_size = c(500, 100), unit_cost = c(2000, 400),
                      med_cycle_length = 7, admin_days = 1L, exact = FALSE,
                      charge_timing = "per_admin")
  out <- calculate_med_costs(w, model_cycle_length = 7, n_cycles = 1,
                             daily_detail = TRUE)
  expect_s3_class(flextable::as_flextable(out), "flextable")
  out <- calculate_med_costs(w, reg_b(), model_cycle_length = 7, n_cycles = 1,
                             daily_detail = TRUE)
  expect_s3_class(flextable::as_flextable(out), "flextable")
})

test_that("as_flextable.combo_regimen_daily_detail spans drug names", {
  skip_if_not_installed("flextable")
  out <- calculate_med_costs(reg_a(), reg_b(), model_cycle_length = 7,
                             n_cycles = 2, daily_detail = TRUE)
  ft <- flextable::as_flextable(out)
  expect_equal(nrow(ft$header$dataset), 2)
  top <- unlist(ft$header$dataset[1, ])
  expect_true(all(c("Drug A", "Drug B") %in% top))
})

test_that("3-drug combo gets spanning headers", {
  skip_if_not_installed("flextable")
  out <- calculate_med_costs(reg_a(), reg_b(), reg_a(name = "Drug C"),
                             n_cycles = 2)
  top <- unlist(flextable::as_flextable(out)$header$dataset[1, ])
  expect_true(all(c("Drug A", "Drug B", "Drug C") %in% top))
})

test_that("as_flextable.regimen_schedule returns flextable", {
  skip_if_not_installed("flextable")
  ft <- flextable::as_flextable(detail_schedule(reg_b(), n_cycles = 2))
  expect_s3_class(ft, "flextable")
  expect_true("Event Type" %in% unlist(ft$header$dataset[2, ]))
})

test_that("as_flextable.dosing_detail returns flextable with 2-row header", {
  skip_if_not_installed("flextable")
  reg <- define_regimen(name = "W", dose_per_admin = 10, dose_basis = "weight",
                        patient_weight = 70, patient_weight_sd = 15,
                        unit_size = c(500, 100), unit_cost = c(2000, 400),
                        admin_days = 1L, exact = FALSE,
                        charge_timing = "per_admin")
  ft <- flextable::as_flextable(diagnose_dosing(reg))
  expect_s3_class(ft, "flextable")
  expect_equal(nrow(ft$header$dataset), 2)
  expect_equal(nrow(ft$footer$dataset), 2)
  expect_match(ft$footer$dataset[2, 1], "500mg @ \\$2,000.00, 100mg @ \\$400.00")
  expect_true("Rounded Down (mg)" %in% unlist(ft$header$dataset[1, ]))
})
