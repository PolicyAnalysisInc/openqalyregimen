#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <limits>

using namespace Rcpp;

// Exact-fill table over an integer grid.
//
// For every grid amount x in 0..max_amount, finds the cheapest combination of
// units whose sizes sum to exactly x. Ties in cost (within a relative 1e-9) are
// broken by the fewest units. Unreachable amounts have cost = Inf.
//
// Returns a list with
//   cost   : cheapest cost for each amount (length max_amount + 1)
//   units  : number of units in that combination
//   parent : 0-based index of the last unit added (-1 for amount 0 or unreachable)
//
// [[Rcpp::export]]
List exact_fill_table(IntegerVector int_sizes, NumericVector costs,
                      int max_amount) {
    int K = int_sizes.size();
    if (K == 0) stop("at least one unit size is required");
    if (costs.size() != K) stop("costs must have one value per unit size");
    if (max_amount < 0) stop("max_amount must be non-negative");

    const double INF = std::numeric_limits<double>::infinity();
    NumericVector cost(max_amount + 1, INF);
    IntegerVector units(max_amount + 1, NA_INTEGER);
    IntegerVector parent(max_amount + 1, -1);
    cost[0] = 0.0;
    units[0] = 0;

    for (int x = 1; x <= max_amount; x++) {
        for (int k = 0; k < K; k++) {
            int s = int_sizes[k];
            if (s <= 0) stop("unit sizes must be positive");
            if (s > x) continue;
            double prev = cost[x - s];
            if (!std::isfinite(prev)) continue;
            double c = prev + costs[k];
            int u = units[x - s] + 1;
            double cur = cost[x];
            bool better;
            if (!std::isfinite(cur)) {
                better = true;
            } else {
                double tol = 1e-9 * std::max(1.0, std::max(std::fabs(c), std::fabs(cur)));
                if (c < cur - tol) better = true;
                else if (c > cur + tol) better = false;
                else better = (u < units[x]);
            }
            if (better) {
                cost[x] = c;
                units[x] = u;
                parent[x] = k;
            }
        }
    }

    return List::create(
        Named("cost") = cost,
        Named("units") = units,
        Named("parent") = parent
    );
}

// Cheapest cover at or above every grid amount.
//
// For each amount x in 0..length(cost)-1, the amount y >= x with the lowest
// finite cost; ties (equal to 12 significant digits) go to the smaller y,
// i.e. the least drug. Returns -1 where no amount at or above x is reachable.
//
// [[Rcpp::export]]
IntegerVector cover_best(NumericVector cost) {
    int n = cost.size();
    IntegerVector best(n, -1);
    int cur = -1;
    double cur_cost = std::numeric_limits<double>::infinity();
    for (int y = n - 1; y >= 0; y--) {
        double c = cost[y];
        if (std::isfinite(c)) {
            double tol = 1e-12 * std::max(std::fabs(c), std::fabs(cur_cost));
            if (cur < 0 || c <= cur_cost + tol) {
                cur = y;
                cur_cost = c;
            }
        }
        best[y] = cur;
    }
    return best;
}
