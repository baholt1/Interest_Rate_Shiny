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

// Noteworthy metric: time to load with just mycppFunction: 3.5 seconds, with everything: 3.9 seconds
// Calculation timings for any functions so far in C++ are near instant and negligible

// WIP: moved BPS to be calculated here, price too to help/test
// Notes with 'XX' indicate areas that need to be changed in the process of adding another column of data (3 of them total)
// [[Rcpp::export]]
NumericMatrix mycppFunction(NumericMatrix x) {
  // Resize the input matrix to accommodate the new column for price
  NumericMatrix result(x.nrow(), x.ncol() + 2); // XX: add 1 for each additional column
  
  // Copy the existing columns to the result matrix
  for (int i = 0; i < x.nrow(); i++) {
    for (int j = 0; j < x.ncol(); j++) {
      result(i, j) = x(i, j);
    }
  }
  
  // XX: add each additional column name to the end of this function
  colnames(result) = Rcpp::CharacterVector::create("date", "maturity", "rate", "price", "changeBPS");
  
  // XX: following are calculations for the data of new columns, can be used to template additional columns by adding to the end
  // Calculate and add the price as a new column
  for (int i = 0; i < result.nrow(); i++) {
    double rate = result(i, 2); // Assuming rate is in column 2
    double price = rate * 100.0;
    result(i, 3) = price; // Add the calculated price as the fourth column
  }
  
  // Calculate and add change in BPS as a new column
  for (int i = 1; i < result.nrow(); i++) {
    double rate_prev = result(i - 1, 2);
    double rate_cur = result(i, 2);
    double changeBPS = (rate_cur - rate_prev) * 10000;
    result(i, result.ncol() - 1) = changeBPS; // Add change in BPS as the last column
  }
  
  return result; // Return the modified matrix with the added price column
}
