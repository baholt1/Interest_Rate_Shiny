#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
// Function to calculate yield to maturity
double ytm(double PV, double M, double C) {
  double ytm_1 = (C + (100 - PV) / M);
  double ytm_2 = (100 + PV) / 2.0;
  return ytm_1 / ytm_2;
}

// Function to calculate bond price based on yield to maturity, coupon rate, time to maturity, and periods per year
double bond_price(double ytm, double C, double T2M, int m) {
  double price = 0.0;
  double period = 1.0 / m;
  double disc_factor;
  for (int i = 0; i < T2M * m; ++i) {
    double t_years = (i + 1) * period; // Compute once outside the loop
    double cf_value = (i == T2M * m - 1) ? (C * 100 / m + 100) : (C * 100 / m);
    disc_factor = 1 / pow((1 + ytm / m), t_years);
    price += cf_value * disc_factor;
  }
  return price;
}

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
    double disc_factor = 1 / pow((1 + ytm / m), t_years);
    duration += (cf_value * disc_factor * t_years) / price;
    convexity += (cf_value * t_years * (t_years + 1)) / (price * pow(1 + ytm / m, t_years));
  }
  List result = List::create(Named("duration") = duration, Named("convexity") = convexity);
  return result;
}

// [[Rcpp::export]]
NumericMatrix mycppFunction(NumericMatrix x, double coupon_rate) {
  int n = x.nrow(); // Number of rows in input matrix
  // Resize the input matrix to accommodate the new column for price
  NumericMatrix result(x.nrow(), x.ncol() + 7); // Resize output matrix dimensions to intended 
  
  // Copy the existing columns to the result matrix
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < x.ncol(); j++) {
      result(i, j) = x(i, j);
    }
  }
  
  // Defining order and column names
  colnames(result) = Rcpp::CharacterVector::create("date", "maturity", "rate", "value", "changeBPS", "ytm", "delta", "gamma", "duration", "convexity");
  
  double period = 0.5; // Periods per year
  
  // Calculations and insertions for each column to be added
  // Calculate and add the PV as a new column
  for (int i = 0; i < n; i++) {
    double rate = result(i, 2);
    result(i, 3) = 100 - rate * 100.0;
  }
  
  // Calculate change in BPS and add it as a new column
  for (int i = 1; i < n; i++) {
    double rate_prev = result(i - 1, 2);
    double rate_cur = result(i, 2);
    result(i, 4) = (rate_cur - rate_prev) * 10000;
  }
  
  // Calculate the yield to maturity and add it as a new column
  for (int i = 0; i < n; i++) {
    double PV = result(i, 3);
    double M = result(i, 1);
    result(i, 5) = ytm(PV, M, coupon_rate);
  }
  
  // Step size of 0.0001 set here
  // Calculate PricePlus and PriceMinus and add them as new columns
  for (int i = 0; i < n; i++) {
    double PV = result(i, 3);
    double M = result(i, 1);
    double ytms = result(i, 2);
    double PricePlus = bond_price(ytms + 0.0001, coupon_rate, M, 2);  
    double PriceMinus = bond_price(ytms - 0.0001, coupon_rate, M, 2);
    result(i, 6) = PricePlus;
    result(i, 7) = PriceMinus;
  }
  
  // Calculate and add Delta and Gamma as new columns
  for (int i = 0; i < n; i++) {
    double PricePlus = result(i, 6);
    double PriceMinus = result(i, 7);
    result(i, 6) = (PricePlus - PriceMinus) / (2 * 0.0001) / 10000; // Delta
    result(i, 7) = 0.5 * ((PricePlus - 2 * result(i, 3) + PriceMinus) / pow(0.0001, 2)) / pow(10000, 2); // Gamma
  }
  
  // Calculate and add Duration and Convexity as new columns
  for (int i = 0; i < n; i++) {
    double ytm = result(i, 2); // Yield to maturity
    double T2M = result(i, 1); // Time to maturity
    List metrics = bond_duration_convexity(ytm, coupon_rate, T2M, 2); // Calculations
    result(i, 8) = as<double>(metrics["duration"]); // Add duration to the result matrix
    result(i, 9) = as<double>(metrics["convexity"]); // Add convexity to the result matrix
  }
  return result; // Return the modified matrix with the added price column
}