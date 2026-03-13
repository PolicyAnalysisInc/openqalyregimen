#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include <limits>

using namespace Rcpp;

// Find scaling factor to convert all values to integers
static double find_scale(const std::vector<double>& values) {
    double scale = 1.0;
    for (double v : values) {
        double tmp = scale;
        while (std::abs(v * tmp - std::round(v * tmp)) > 1e-9 && tmp < 1e12) {
            tmp *= 10.0;
        }
        if (tmp > scale) scale = tmp;
    }
    return scale;
}

// [[Rcpp::export]]
List optimal_vial_cost_single(double dose_mg,
                               NumericVector vial_sizes,
                               NumericVector vial_costs,
                               double wastage_threshold = 0.0) {
    int K = vial_sizes.size();
    if (K == 0) {
        stop("vial_sizes must have at least one element");
    }

    // Scale all values to integers
    std::vector<double> all_vals;
    for (int i = 0; i < K; i++) {
        all_vals.push_back(vial_sizes[i]);
    }
    all_vals.push_back(dose_mg);
    double scale = find_scale(all_vals);

    std::vector<int> int_sizes(K);
    for (int i = 0; i < K; i++) {
        int_sizes[i] = (int)std::round(vial_sizes[i] * scale);
    }
    int int_dose = (int)std::round(dose_mg * scale);

    const double INF = std::numeric_limits<double>::infinity();

    // --- Step 2: Build exact-fill DP ---
    // dp_exact[d] = minimum cost to fill exactly d units
    std::vector<double> dp_exact(int_dose + 1, INF);
    std::vector<int> parent_exact(int_dose + 1, -1);
    dp_exact[0] = 0.0;

    for (int d = 1; d <= int_dose; d++) {
        for (int v = 0; v < K; v++) {
            if (int_sizes[v] <= d &&
                dp_exact[d - int_sizes[v]] + vial_costs[v] < dp_exact[d]) {
                dp_exact[d] = dp_exact[d - int_sizes[v]] + vial_costs[v];
                parent_exact[d] = v;
            }
        }
    }

    // --- Step 3: If exact fill exists, use it ---
    if (dp_exact[int_dose] < INF) {
        IntegerVector n_vials(K, 0);
        int d = int_dose;
        while (d > 0) {
            int v = parent_exact[d];
            n_vials[v]++;
            d -= int_sizes[v];
        }
        return List::create(
            Named("drug_cost") = dp_exact[int_dose],
            Named("n_vials") = n_vials,
            Named("waste_mg") = 0.0
        );
    }

    // --- Step 4: Build covering DP ---
    // dp_cover[d] = minimum cost to cover at least d units
    std::vector<double> dp_cover(int_dose + 1, INF);
    std::vector<int> parent_cover(int_dose + 1, -1);
    dp_cover[0] = 0.0;

    for (int d = 1; d <= int_dose; d++) {
        for (int v = 0; v < K; v++) {
            int prev = d - int_sizes[v];
            if (prev < 0) prev = 0;
            double new_cost = dp_cover[prev] + vial_costs[v];
            if (new_cost < dp_cover[d]) {
                dp_cover[d] = new_cost;
                parent_cover[d] = v;
            } else if (new_cost == dp_cover[d] && parent_cover[d] >= 0) {
                // Tie-break: prefer less overshoot (larger prev = less content)
                int prev_existing = d - int_sizes[parent_cover[d]];
                if (prev > prev_existing) {
                    parent_cover[d] = v;
                }
            }
        }
    }

    // --- Step 5: Under-fill search ---
    if (wastage_threshold > 0) {
        // Sort vial sizes ascending for binary search
        std::vector<double> sorted_sizes(K);
        for (int i = 0; i < K; i++) sorted_sizes[i] = vial_sizes[i];
        std::sort(sorted_sizes.begin(), sorted_sizes.end());

        for (int C = int_dose - 1; C > 0; C--) {
            if (dp_exact[C] < INF) {
                double shortfall_mg = (double)(int_dose - C) / scale;
                // Find smallest vial >= shortfall_mg
                auto it = std::lower_bound(sorted_sizes.begin(),
                                           sorted_sizes.end(),
                                           shortfall_mg - 1e-9);
                if (it != sorted_sizes.end()) {
                    double V = *it;
                    if (shortfall_mg / V <= wastage_threshold + 1e-9) {
                        // Closest acceptable under-fill found
                        if (dp_exact[C] < dp_cover[int_dose]) {
                            // Under-fill is cheaper than covering
                            IntegerVector n_vials(K, 0);
                            int d = C;
                            while (d > 0) {
                                int v = parent_exact[d];
                                n_vials[v]++;
                                d -= int_sizes[v];
                            }
                            return List::create(
                                Named("drug_cost") = dp_exact[C],
                                Named("n_vials") = n_vials,
                                Named("waste_mg") = 0.0
                            );
                        } else {
                            break; // Covering is cheaper, use it
                        }
                    }
                }
            }
        }
    }

    // --- Step 6: Backtrack covering DP ---
    IntegerVector n_vials(K, 0);
    int d = int_dose;
    while (d > 0) {
        int v = parent_cover[d];
        if (v < 0) break;
        n_vials[v]++;
        int prev = d - int_sizes[v];
        if (prev < 0) prev = 0;
        d = prev;
    }

    // Compute waste from actual vial contents
    double total_content = 0.0;
    for (int v = 0; v < K; v++) {
        total_content += (double)n_vials[v] * vial_sizes[v];
    }
    double waste_mg = std::max(total_content - dose_mg, 0.0);

    return List::create(
        Named("drug_cost") = dp_cover[int_dose],
        Named("n_vials") = n_vials,
        Named("waste_mg") = waste_mg
    );
}


// [[Rcpp::export]]
NumericVector optimal_vial_cost_vec(NumericVector dose_mg_vec,
                                     NumericVector vial_sizes,
                                     NumericVector vial_costs,
                                     double wastage_threshold = 0.0) {
    int n = dose_mg_vec.size();
    int K = vial_sizes.size();
    if (K == 0) {
        stop("vial_sizes must have at least one element");
    }

    // Find max dose
    double max_dose = 0;
    for (int i = 0; i < n; i++) {
        if (dose_mg_vec[i] > max_dose) max_dose = dose_mg_vec[i];
    }

    // Scale to integers
    std::vector<double> all_vals;
    for (int i = 0; i < K; i++) {
        all_vals.push_back(vial_sizes[i]);
    }
    all_vals.push_back(max_dose);
    double scale = find_scale(all_vals);

    std::vector<int> int_sizes(K);
    for (int i = 0; i < K; i++) {
        int_sizes[i] = (int)std::round(vial_sizes[i] * scale);
    }

    int int_max_dose = (int)std::round(max_dose * scale);

    const double INF = std::numeric_limits<double>::infinity();

    // Build exact-fill DP (costs only, no parent needed)
    std::vector<double> dp_exact(int_max_dose + 1, INF);
    dp_exact[0] = 0.0;
    for (int d = 1; d <= int_max_dose; d++) {
        for (int v = 0; v < K; v++) {
            if (int_sizes[v] <= d &&
                dp_exact[d - int_sizes[v]] + vial_costs[v] < dp_exact[d]) {
                dp_exact[d] = dp_exact[d - int_sizes[v]] + vial_costs[v];
            }
        }
    }

    // Build covering DP (costs only, no tie-breaking needed for vec)
    std::vector<double> dp_cover(int_max_dose + 1, INF);
    dp_cover[0] = 0.0;
    for (int d = 1; d <= int_max_dose; d++) {
        for (int v = 0; v < K; v++) {
            int prev = d - int_sizes[v];
            if (prev < 0) prev = 0;
            double new_cost = dp_cover[prev] + vial_costs[v];
            if (new_cost < dp_cover[d]) {
                dp_cover[d] = new_cost;
            }
        }
    }

    // Sort vial sizes ascending for binary search (used in under-fill)
    std::vector<double> sorted_sizes(K);
    for (int i = 0; i < K; i++) sorted_sizes[i] = vial_sizes[i];
    std::sort(sorted_sizes.begin(), sorted_sizes.end());

    NumericVector result(n);

    for (int i = 0; i < n; i++) {
        int int_dose = (int)std::round(dose_mg_vec[i] * scale);

        if (dp_exact[int_dose] < INF) {
            // Tier 1: exact fill
            result[i] = dp_exact[int_dose];
        } else if (wastage_threshold > 0) {
            // Tier 2: search for acceptable under-fill
            bool found = false;
            for (int C = int_dose - 1; C > 0; C--) {
                if (dp_exact[C] < INF) {
                    double shortfall_mg = (double)(int_dose - C) / scale;
                    auto it = std::lower_bound(sorted_sizes.begin(),
                                               sorted_sizes.end(),
                                               shortfall_mg - 1e-9);
                    if (it != sorted_sizes.end()) {
                        double V = *it;
                        if (shortfall_mg / V <= wastage_threshold + 1e-9) {
                            if (dp_exact[C] < dp_cover[int_dose]) {
                                result[i] = dp_exact[C];
                            } else {
                                result[i] = dp_cover[int_dose];
                            }
                            found = true;
                            break;
                        }
                    }
                }
            }
            if (!found) {
                // Tier 3: covering
                result[i] = dp_cover[int_dose];
            }
        } else {
            // No threshold, use covering directly
            result[i] = dp_cover[int_dose];
        }
    }

    return result;
}
