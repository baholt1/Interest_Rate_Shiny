#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calculate_bond_duration_and_convexity_cpp(double coupon_rate, int years_to_maturity, double yield_to_maturity) {
  NumericVector present_values(years_to_maturity);
  double discount_factor = 1.0 / (1.0 + yield_to_maturity);
  
  for (int i = 0; i < years_to_maturity - 1; ++i) {
    present_values[i] = coupon_rate * (1.0 - pow(1.0 + yield_to_maturity, -(years_to_maturity - i))) / yield_to_maturity;
  }
  
  present_values[years_to_maturity - 1] = present_values[years_to_maturity - 1] + discount_factor;
  
  // Calculate duration
  double sum_pv = 0.0;
  double bond_duration = 0.0;
  for (int i = 0; i < years_to_maturity; ++i) {
    sum_pv += present_values[i];
    bond_duration += present_values[i] * (i + 1);
  }
  bond_duration /= sum_pv;
  
  // Calculate convexity
  double bond_convexity = 0.0;
  for (int i = 0; i < years_to_maturity; ++i) {
    bond_convexity += present_values[i] * (i + 1) * (i + 2);
  }
  bond_convexity /= sum_pv * (1 + yield_to_maturity) * (1 + yield_to_maturity);
  
  return NumericVector::create(_["bond_duration"] = bond_duration, _["bond_convexity"] = bond_convexity);
}


NumericMatrix mycppFunction(NumericMatrix x) {
  // Loop through each row of the input matrix x
  for (int i = 0; i < x.nrow(); i++) {
    // Extracting values from the matrix
    double maturity = x(i, 1); // Assuming maturity is in column 1
    double rate = x(i, 2); // Assuming rate is in column 2
  }
  return x; // Return the modified matrix with calculated metrics
}