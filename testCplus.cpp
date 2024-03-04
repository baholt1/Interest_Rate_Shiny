#include <Rcpp.h>
using namespace Rcpp;

// Function to calculate the present value
// [[Rcpp::export]]
double present_value(double rate, double price, double face_value, double coupon_payment, int years_to_maturity, int frequency) {
  int periods = years_to_maturity * frequency;
  double coupon = coupon_payment * (frequency / 2.0);  
  
  // PV coupon
  double pv_coupons = 0.0;
  for (int i = 1; i <= periods; ++i) {
    pv_coupons += coupon / pow(1 + rate / frequency, i);
  }
  
  // PV face value 
  double pv_face_value = face_value / pow(1 + rate / frequency, periods);
  
  // total PV 
  double pv_total = pv_coupons + pv_face_value;
  
  // difference of bond price and present value 
  double diff = price - pv_total;
  
  return diff;
}

// [[Rcpp::export]]
// Function to calculate YTM
double calculate_YTM(double price, double face_value, double coupon_payment, int years_to_maturity, int frequency = 1) {
  // Wrapper function for uniroot
  Environment stats("package:stats");
  Function uniroot = stats["uniroot"];
  
  // Calculate YTM
  NumericVector interval = NumericVector::create(0, 1);
  List uniroot_result = as<List>(uniroot(_["f"] = Function("present_value"), 
                                         _["interval"] = interval,
                                         _["price"] = price,
                                         _["face_value"] = face_value,
                                         _["coupon_payment"] = coupon_payment,
                                         _["years_to_maturity"] = years_to_maturity,
                                         _["frequency"] = frequency));
  double ytm = as<double>(uniroot_result["root"]);
  
  // Convert YTM to percentage
  return ytm * frequency * 100.0;
}

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

// Currently WIP, for some reason I can't get any changes to be made to the dataframe when called in R
// Will implement calculations cohesively once changes can be made
// [[Rcpp::export]]
NumericMatrix mycppFunction(NumericMatrix x) {
  // Loop through each row of the input matrix x
  for (int i = 0; i < x.nrow(); i++) {
    // Extracting values from the matrix
    double maturity = x(i, 1); // Assuming maturity is in column 1
    double rate = x(i, 2); // Assuming rate is in column 2
    // Calculate price by multiplying rate by 100 and adding it as a new column
    x(i, 2) = rate * 100.0;
  }
  return x; // Return the modified matrix with calculated metrics
}