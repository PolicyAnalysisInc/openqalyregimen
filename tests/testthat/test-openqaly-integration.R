test_that("regimen helpers support correct value and summary patterns in openqaly", {
  skip_if_not_installed("openqaly")
  skip_if_not_installed("tibble")
  skip_if_not_installed("dplyr")

  oral_mono_tbl <- tibble::tibble(
    name = "Capecitabine",
    route = "oral",
    dose = 2000,
    dose_basis = "bsa",
    med_cycle_length = 21,
    admin_days = "1-14",
    max_cycles = 8,
    available_strengths = "150,500",
    unit_cost = 2.21,
    admin_cost = 45
  )

  iv_combo_trastuzumab_tbl <- tibble::tibble(
    name = c("Trastuzumab loading", "Trastuzumab maintenance"),
    route = c("iv", "iv"),
    dose = c(8, 6),
    dose_basis = c("weight", "weight"),
    med_cycle_length = c(21, 21),
    admin_days = c("1", "1"),
    max_cycles = c(1, 7),
    start_day = c(0, 21),
    vial_sizes = c("150,420", "150,420"),
    vial_cost = c("407.40,1184", "407.40,1184"),
    wastage_threshold = c(0.1, 0.1),
    admin_cost = c(241, 241)
  )

  iv_combo_pertuzumab_tbl <- tibble::tibble(
    name = "Pertuzumab",
    route = "iv",
    dose = 420,
    dose_basis = "flat",
    med_cycle_length = 21,
    admin_days = "1",
    max_cycles = 8,
    vial_sizes = "420",
    vial_cost = "2750",
    admin_cost = 241
  )

  base_model <- openqaly::define_model("markov") |>
    openqaly::set_settings(
      n_cycles = 8,
      cycle_length = 21,
      cycle_length_unit = "days"
    ) |>
    openqaly::add_strategy("oral_mono", "Oral Monotherapy") |>
    openqaly::add_strategy("iv_combo", "IV Combination") |>
    openqaly::add_state("alive", initial_prob = 1) |>
    openqaly::add_state("dead", initial_prob = 0) |>
    openqaly::add_transition("alive", "dead", 0.08) |>
    openqaly::add_transition("alive", "alive", C) |>
    openqaly::add_transition("dead", "dead", 1) |>
    openqaly::add_variable("patient_bsa", 1.8) |>
    openqaly::add_variable("patient_weight", 70)

  add_common_values_and_summaries <- function(model) {
    model |>
      openqaly::add_value("capecitabine_med_cost", c_capecitabine_med, state = "alive", type = "cost") |>
      openqaly::add_value("capecitabine_admin_cost", c_capecitabine_admin, state = "alive", type = "cost") |>
      openqaly::add_value("trastuzumab_med_cost", c_trastuzumab_med, state = "alive", type = "cost") |>
      openqaly::add_value("trastuzumab_admin_cost", c_trastuzumab_admin, state = "alive", type = "cost") |>
      openqaly::add_value("pertuzumab_med_cost", c_pertuzumab_med, state = "alive", type = "cost") |>
      openqaly::add_value("pertuzumab_admin_cost", c_pertuzumab_admin, state = "alive", type = "cost") |>
      openqaly::add_value("qaly", 0.8 * cycle_length_years, state = "alive", type = "outcome") |>
      openqaly::add_value("ly", cycle_length_years, state = "alive", type = "outcome") |>
      openqaly::add_summary(
        "total_cost",
        "capecitabine_med_cost,capecitabine_admin_cost,trastuzumab_med_cost,trastuzumab_admin_cost,pertuzumab_med_cost,pertuzumab_admin_cost",
        type = "cost"
      ) |>
      openqaly::add_summary("total_qalys", "qaly", type = "outcome") |>
      openqaly::add_summary("total_lys", "ly", type = "outcome")
  }

  build_raw_model <- function() {
    base_model |>
      openqaly::add_variable(
        "capecitabine_regimen",
        define_regimen(
          name = "Capecitabine",
          route = "oral",
          dose_per_admin = 2000,
          dose_basis = "bsa",
          patient_bsa = patient_bsa,
          med_cycle_length = cycle_length_days,
          admin_days = 1:14,
          max_med_cycles = max(cycle),
          available_strengths = c(150, 500),
          unit_cost = 2.21,
          admin_cost = 45
        ),
        strategy = "oral_mono"
      ) |>
      openqaly::add_variable(
        "trastuzumab_loading_regimen",
        define_regimen(
          name = "Trastuzumab loading",
          route = "iv",
          dose_per_admin = 8,
          dose_basis = "weight",
          patient_weight = patient_weight,
          med_cycle_length = cycle_length_days,
          admin_days = 1,
          max_med_cycles = 1,
          start_day = 0,
          vial_sizes = c(150, 420),
          vial_cost = c(407.40, 1184),
          wastage_threshold = 0.1,
          admin_cost = 241
        ),
        strategy = "iv_combo"
      ) |>
      openqaly::add_variable(
        "trastuzumab_maintenance_regimen",
        define_regimen(
          name = "Trastuzumab maintenance",
          route = "iv",
          dose_per_admin = 6,
          dose_basis = "weight",
          patient_weight = patient_weight,
          med_cycle_length = cycle_length_days,
          admin_days = 1,
          max_med_cycles = max(cycle) - 1,
          start_day = cycle_length_days,
          vial_sizes = c(150, 420),
          vial_cost = c(407.40, 1184),
          wastage_threshold = 0.1,
          admin_cost = 241
        ),
        strategy = "iv_combo"
      ) |>
      openqaly::add_variable(
        "pertuzumab_regimen",
        define_regimen(
          name = "Pertuzumab",
          route = "iv",
          dose_per_admin = 420,
          dose_basis = "flat",
          med_cycle_length = cycle_length_days,
          admin_days = 1,
          max_med_cycles = max(cycle),
          vial_sizes = 420,
          vial_cost = 2750,
          admin_cost = 241
        ),
        strategy = "iv_combo"
      ) |>
      openqaly::add_variable(
        "c_capecitabine_med",
        calculate_regimen_cost(
          capecitabine_regimen,
          type = "medication",
          model_cycle_length = cycle_length_days,
          n_cycles = max(cycle)
        ),
        strategy = "oral_mono"
      ) |>
      openqaly::add_variable(
        "c_capecitabine_admin",
        calculate_regimen_cost(
          capecitabine_regimen,
          type = "administration",
          model_cycle_length = cycle_length_days,
          n_cycles = max(cycle)
        ),
        strategy = "oral_mono"
      ) |>
      openqaly::add_variable("c_trastuzumab_med", rep(0, max(cycle)), strategy = "oral_mono") |>
      openqaly::add_variable("c_trastuzumab_admin", rep(0, max(cycle)), strategy = "oral_mono") |>
      openqaly::add_variable("c_pertuzumab_med", rep(0, max(cycle)), strategy = "oral_mono") |>
      openqaly::add_variable("c_pertuzumab_admin", rep(0, max(cycle)), strategy = "oral_mono") |>
      openqaly::add_variable("c_capecitabine_med", rep(0, max(cycle)), strategy = "iv_combo") |>
      openqaly::add_variable("c_capecitabine_admin", rep(0, max(cycle)), strategy = "iv_combo") |>
      openqaly::add_variable(
        "c_trastuzumab_med",
        calculate_regimen_cost(
          trastuzumab_loading_regimen,
          type = "medication",
          model_cycle_length = cycle_length_days,
          n_cycles = max(cycle)
        ) +
          calculate_regimen_cost(
            trastuzumab_maintenance_regimen,
            type = "medication",
            model_cycle_length = cycle_length_days,
            n_cycles = max(cycle)
          ),
        strategy = "iv_combo"
      ) |>
      openqaly::add_variable(
        "c_trastuzumab_admin",
        calculate_regimen_cost(
          trastuzumab_loading_regimen,
          type = "administration",
          model_cycle_length = cycle_length_days,
          n_cycles = max(cycle)
        ) +
          calculate_regimen_cost(
            trastuzumab_maintenance_regimen,
            type = "administration",
            model_cycle_length = cycle_length_days,
            n_cycles = max(cycle)
          ),
        strategy = "iv_combo"
      ) |>
      openqaly::add_variable(
        "c_pertuzumab_med",
        calculate_regimen_cost(
          pertuzumab_regimen,
          type = "medication",
          model_cycle_length = cycle_length_days,
          n_cycles = max(cycle)
        ),
        strategy = "iv_combo"
      ) |>
      openqaly::add_variable(
        "c_pertuzumab_admin",
        calculate_regimen_cost(
          pertuzumab_regimen,
          type = "administration",
          model_cycle_length = cycle_length_days,
          n_cycles = max(cycle)
        ),
        strategy = "iv_combo"
      ) |>
      add_common_values_and_summaries()
  }

  build_tabular_model <- function() {
    base_model |>
      openqaly::add_table("oral_mono_tbl", oral_mono_tbl) |>
      openqaly::add_table("iv_combo_trastuzumab_tbl", iv_combo_trastuzumab_tbl) |>
      openqaly::add_table("iv_combo_pertuzumab_tbl", iv_combo_pertuzumab_tbl) |>
      openqaly::add_variable(
        "capecitabine_regimen",
        define_regimen_table(
          oral_mono_tbl,
          patient_bsa = patient_bsa
        ),
        strategy = "oral_mono"
      ) |>
      openqaly::add_variable(
        "trastuzumab_regimen",
        define_regimen_table(
          iv_combo_trastuzumab_tbl,
          patient_weight = patient_weight
        ),
        strategy = "iv_combo"
      ) |>
      openqaly::add_variable(
        "pertuzumab_regimen",
        define_regimen_table(iv_combo_pertuzumab_tbl),
        strategy = "iv_combo"
      ) |>
      openqaly::add_variable(
        "c_capecitabine_med",
        calculate_regimen_cost(
          capecitabine_regimen,
          type = "medication",
          model_cycle_length = cycle_length_days,
          n_cycles = max(cycle)
        ),
        strategy = "oral_mono"
      ) |>
      openqaly::add_variable(
        "c_capecitabine_admin",
        calculate_regimen_cost(
          capecitabine_regimen,
          type = "administration",
          model_cycle_length = cycle_length_days,
          n_cycles = max(cycle)
        ),
        strategy = "oral_mono"
      ) |>
      openqaly::add_variable("c_trastuzumab_med", rep(0, max(cycle)), strategy = "oral_mono") |>
      openqaly::add_variable("c_trastuzumab_admin", rep(0, max(cycle)), strategy = "oral_mono") |>
      openqaly::add_variable("c_pertuzumab_med", rep(0, max(cycle)), strategy = "oral_mono") |>
      openqaly::add_variable("c_pertuzumab_admin", rep(0, max(cycle)), strategy = "oral_mono") |>
      openqaly::add_variable("c_capecitabine_med", rep(0, max(cycle)), strategy = "iv_combo") |>
      openqaly::add_variable("c_capecitabine_admin", rep(0, max(cycle)), strategy = "iv_combo") |>
      openqaly::add_variable(
        "c_trastuzumab_med",
        calculate_regimen_cost(
          trastuzumab_regimen,
          type = "medication",
          model_cycle_length = cycle_length_days,
          n_cycles = max(cycle)
        ),
        strategy = "iv_combo"
      ) |>
      openqaly::add_variable(
        "c_trastuzumab_admin",
        calculate_regimen_cost(
          trastuzumab_regimen,
          type = "administration",
          model_cycle_length = cycle_length_days,
          n_cycles = max(cycle)
        ),
        strategy = "iv_combo"
      ) |>
      openqaly::add_variable(
        "c_pertuzumab_med",
        calculate_regimen_cost(
          pertuzumab_regimen,
          type = "medication",
          model_cycle_length = cycle_length_days,
          n_cycles = max(cycle)
        ),
        strategy = "iv_combo"
      ) |>
      openqaly::add_variable(
        "c_pertuzumab_admin",
        calculate_regimen_cost(
          pertuzumab_regimen,
          type = "administration",
          model_cycle_length = cycle_length_days,
          n_cycles = max(cycle)
        ),
        strategy = "iv_combo"
      ) |>
      add_common_values_and_summaries()
  }

  raw_results <- openqaly::run_model(build_raw_model())
  tabular_results <- openqaly::run_model(build_tabular_model())

  raw_totals <- openqaly::get_summaries(
    raw_results,
    summaries = c("total_cost", "total_qalys", "total_lys"),
    discounted = TRUE,
    use_display_names = FALSE
  )
  raw_totals <- raw_totals |>
    dplyr::group_by(strategy, group, summary) |>
    dplyr::summarise(raw_amount = sum(amount), .groups = "drop")

  tabular_totals <- openqaly::get_summaries(
    tabular_results,
    summaries = c("total_cost", "total_qalys", "total_lys"),
    discounted = TRUE,
    use_display_names = FALSE
  )
  tabular_totals <- tabular_totals |>
    dplyr::group_by(strategy, group, summary) |>
    dplyr::summarise(tabular_amount = sum(amount), .groups = "drop")

  totals_comparison <- dplyr::left_join(
    raw_totals,
    tabular_totals,
    by = c("strategy", "group", "summary")
  )

  raw_cost_breakdown <- openqaly::get_summaries(
    raw_results,
    summaries = "total_cost",
    value_type = "cost",
    discounted = TRUE,
    use_display_names = FALSE
  )
  raw_cost_breakdown <- raw_cost_breakdown[, c("strategy", "value", "amount")]
  names(raw_cost_breakdown)[3] <- "raw_amount"

  tabular_cost_breakdown <- openqaly::get_summaries(
    tabular_results,
    summaries = "total_cost",
    value_type = "cost",
    discounted = TRUE,
    use_display_names = FALSE
  )
  tabular_cost_breakdown <- tabular_cost_breakdown[, c("strategy", "value", "amount")]
  names(tabular_cost_breakdown)[3] <- "tabular_amount"

  breakdown_comparison <- dplyr::left_join(
    raw_cost_breakdown,
    tabular_cost_breakdown,
    by = c("strategy", "value")
  )

  expect_true(all(c("total_cost", "total_qalys", "total_lys") %in% totals_comparison$summary))
  expect_true(all(c(
    "capecitabine_med_cost",
    "capecitabine_admin_cost",
    "trastuzumab_med_cost",
    "trastuzumab_admin_cost",
    "pertuzumab_med_cost",
    "pertuzumab_admin_cost"
  ) %in% breakdown_comparison$value))

  oral_trastuzumab <- breakdown_comparison$tabular_amount[
    breakdown_comparison$strategy == "oral_mono" &
      breakdown_comparison$value == "trastuzumab_med_cost"
  ]
  oral_pertuzumab <- breakdown_comparison$tabular_amount[
    breakdown_comparison$strategy == "oral_mono" &
      breakdown_comparison$value == "pertuzumab_med_cost"
  ]
  oral_capecitabine <- breakdown_comparison$tabular_amount[
    breakdown_comparison$strategy == "oral_mono" &
      breakdown_comparison$value == "capecitabine_med_cost"
  ]
  iv_trastuzumab <- breakdown_comparison$tabular_amount[
    breakdown_comparison$strategy == "iv_combo" &
      breakdown_comparison$value == "trastuzumab_med_cost"
  ]
  iv_pertuzumab <- breakdown_comparison$tabular_amount[
    breakdown_comparison$strategy == "iv_combo" &
      breakdown_comparison$value == "pertuzumab_med_cost"
  ]
  iv_capecitabine <- breakdown_comparison$tabular_amount[
    breakdown_comparison$strategy == "iv_combo" &
      breakdown_comparison$value == "capecitabine_med_cost"
  ]

  expect_equal(oral_trastuzumab, 0)
  expect_equal(oral_pertuzumab, 0)
  expect_true(oral_capecitabine > 0)
  expect_true(iv_trastuzumab > 0)
  expect_true(iv_pertuzumab > 0)
  expect_equal(iv_capecitabine, 0)

  expect_equal(totals_comparison$raw_amount, totals_comparison$tabular_amount)
  expect_equal(breakdown_comparison$raw_amount, breakdown_comparison$tabular_amount)
})

test_that("calculate_regimen_cost with time param works in tunnel state model", {
  skip_if_not_installed("openqaly")

  tbl <- data.frame(
    name = "Drug A",
    route = "iv",
    dose = 100,
    dose_basis = "flat",
    med_cycle_length = 7,
    admin_days = "1",
    vial_size = 100,
    vial_cost = 500,
    admin_cost = 25,
    stringsAsFactors = FALSE
  )

  n_cycles <- 4
  state_cycle_limit <- 3

  model <- openqaly::define_model("markov") |>
    openqaly::set_settings(
      n_cycles = n_cycles,
      cycle_length = 7,
      cycle_length_unit = "days"
    ) |>
    openqaly::add_strategy("treat", "Treatment") |>
    openqaly::add_state("alive", initial_prob = 1,
                        state_cycle_limit = state_cycle_limit) |>
    openqaly::add_state("dead", initial_prob = 0) |>
    openqaly::add_transition("alive", "dead", 0.1) |>
    openqaly::add_transition("alive", "alive", C) |>
    openqaly::add_transition("dead", "dead", 1) |>
    openqaly::add_table("drug_tbl", tbl) |>
    openqaly::add_variable(
      "drug_regimen",
      define_regimen_table(drug_tbl)
    ) |>
    openqaly::add_variable(
      "c_drug",
      calculate_regimen_cost(
        drug_regimen,
        type = "medication",
        model_cycle_length = cycle_length_days,
        n_cycles = max(cycle),
        time = day
      )
    ) |>
    openqaly::add_value("drug_cost", c_drug, state = "alive", type = "cost") |>
    openqaly::add_summary("total_cost", "drug_cost", type = "cost")

  results <- openqaly::run_model(model)
  totals <- openqaly::get_summaries(
    results,
    summaries = "total_cost",
    discounted = FALSE,
    use_display_names = FALSE
  )

  expect_true(nrow(totals) > 0)
  expect_true(all(totals$amount >= 0))
})
