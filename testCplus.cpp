#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]] THIS LINE EXPORTS THE FUNCTION TO BE CALLED IN R, REMOVE THESE ALL EXCEPT FOR ONES NEEDED
// Function to calculate yield to maturity
double ytm(double PV, double M, double C) {
  double ytm_1 = (C + (100 - PV) / M);
  double ytm_2 = (100 + PV) / 2.0;
  return ytm_1 / ytm_2;
}

// [[Rcpp::export]]
// Function to calculate bond price based on yield to maturity, coupon rate, time to maturity, and periods per year
double bond_price(double ytm, double C, double T2M, int m) {
  double price = 0.0;
  double period = 1.0 / m;
  for (int i = 0; i < T2M * m; ++i) {
    double cf_value = (i == T2M * m - 1) ? (C * 100 / m + 100) : (C * 100 / m);
    double disc_factor = 1 / pow((1 + ytm / m), (i + 1) * period);
    price += cf_value * disc_factor;
  }
  return price;
}

// [[Rcpp::export]]
// Function to calculate bond duration and convexity
List bond_duration_convexity(double ytm, double C, double T2M, int m) {
  double duration = 0.0;
  double convexity = 0.0;
  double price = bond_price(ytm, C, T2M, m); // Calculate bond price
  double period = 1.0 / m;
  
  // Calculate bond duration and convexity
  for (int i = 0; i < T2M * m; ++i) {
    double t_years = (i + 1) * (1.0 / m);
    double cf_value = (i == T2M * m - 1) ? (C * 100 / m + 100) : (C * 100 / m);
    double disc_factor = 1 / pow((1 + ytm / m), (i + 1) * (1.0 / m));
    duration += (cf_value * t_years) / price;
    convexity += (cf_value * t_years * (t_years + 1)) / (price * pow(1 + ytm / m, (i + 1) * period));
  }
  List result = List::create(Named("duration") = duration, Named("convexity") = convexity);
  return result;
}

// Noteworthy metric: time to load with just mycppFunction: 3.5 seconds, with everything: 3.9 seconds
// Calculation timings for any functions so far in C++ are near instant and negligible

// Notes with 'XX' indicate areas that need to be changed in the process of adding another column of data (3 of them total)
// [[Rcpp::export]]
NumericMatrix mycppFunction(NumericMatrix x, double coupon_rate) {
  // Resize the input matrix to accommodate the new column for price
  NumericMatrix result(x.nrow(), x.ncol() + 7); // XX: add 1 for each additional column
  
  // Copy the existing columns to the result matrix
  for (int i = 0; i < x.nrow(); i++) {
    for (int j = 0; j < x.ncol(); j++) {
      result(i, j) = x(i, j);
    }
  }
  
  // XX: add each additional column name to the end of this function
  colnames(result) = Rcpp::CharacterVector::create("date", "maturity", "rate", "value", "changeBPS", "ytm", "delta", "gamma", "duration", "convexity");
  
  // XX: following are calculations for the data of new columns, can be used to template additional columns by adding to the end but before the return
  // Calculate and add the PV as a new column
  for (int i = 0; i < result.nrow(); i++) {
    double rate = result(i, 2); // Assuming rate is in column 2
    double value = 100 - rate * 100.0;
    result(i, 3) = value; // Add the calculated PV as the 4th column
  }
  
  // Calculate and add change in BPS as a new column
  for (int i = 1; i < result.nrow(); i++) {
    double rate_prev = result(i - 1, 2);
    double rate_cur = result(i, 2);
    double changeBPS = (rate_cur - rate_prev) * 10000;
    result(i, 4) = changeBPS; // Add change in BPS as the 5th column
  }
  
  // Calculate the yield to maturity and add it as a new column
  for (int i = 0; i < result.nrow(); i++) {
    double PV = result(i, 3);
    double M = result(i, 1);
    double ytms = ytm(PV, M, coupon_rate);
    result(i, 5) = ytms;
  }
  
  // Step size of 0.0001 set here, MUST BE CHANGED TO IMPLEMENT USER INPUT
  // Calculate PricePlus and PriceMinus and add them as new columns
  for (int i = 0; i < result.nrow(); i++) {
    double PV = result(i, 3);
    double M = result(i, 1);
    double ytms = result(i, 5);
    int m = 2; // Periods per year (hardcoded, replace with user input if needed)
    double PricePlus = bond_price(ytms + 0.0001, coupon_rate, M, m);  // Increment YTM by 0.0001 for PricePlus
    double PriceMinus = bond_price(ytms - 0.0001, coupon_rate, M, m); // Decrement YTM by 0.0001 for PriceMinus
    result(i, 6) = PricePlus;
    result(i, 7) = PriceMinus;
  }
  
  // Step size of 0.0001 set here, MUST BE CHANGED TO IMPLEMENT USER INPUT
  // Calculate and add Delta and Gamma as new columns
  for (int i = 0; i < result.nrow(); i++) {
    double PricePlus = result(i, 6);
    double PriceMinus = result(i, 7);
    double Delta = (PricePlus - PriceMinus) / (2 * 0.01) / 10000; // StepSize is 0.0001
    double Gamma = 0.5 * ((PricePlus - 2 * result(i, 3) + PriceMinus) / pow(0.01, 2)) / pow(10000, 2); // StepSize is 0.0001
    result(i, 6) = Delta; // Overwrites previous PricePlus
    result(i, 7) = Gamma; // Overwrites previous PriceMinus
  }
  
  // Calculate and add Duration and Convexity as new columns
  for (int i = 0; i < result.nrow(); i++) {
    double ytm = result(i, 5); // Yield to maturity
    double T2M = result(i, 1); // Time to maturity
    int m = 2; // Periods per year (hardcoded, replace with user input if needed)
    List metrics = bond_duration_convexity(ytm, coupon_rate, T2M, m); // Calculate bond metrics
    result(i, 8) = as<double>(metrics["duration"]); // Add duration to the result matrix
    result(i, 9) = as<double>(metrics["convexity"]); // Add convexity to the result matrix
  }
  
  return result; // Return the modified matrix with the added price column
}
