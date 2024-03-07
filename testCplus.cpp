#include <Rcpp.h>
using namespace Rcpp;

// Function to calculate yield to maturity
double ytm(double PV, double M, double C) {
  double ytm_1 = (C + (100 - PV) / M);
  double ytm_2 = (100 + PV) / 2.0;
  return ytm_1 / ytm_2;
}

// Function to calculate bond price based on the R code provided
double bond_price(double ytm, double C, double T2M) {
  double price = 0.0;
  int m = 2; // Assuming semi-annual coupon payments ???
  double period = 1.0 / m;
  for (int i = 0; i < T2M * m; ++i) {
    double cf_value = (i == T2M * m - 1) ? (C * 100 / m + 100) : (C * 100 / m);
    double disc_factor = 1 / pow((1 + ytm / m), (i + 1) * period);
    price += cf_value * disc_factor;
  }
  return price;
}

// Noteworthy metric: time to load with just mycppFunction: 3.5 seconds, with everything: 3.9 seconds
// Calculation timings for any functions so far in C++ are near instant and negligible

// Notes with 'XX' indicate areas that need to be changed in the process of adding another column of data (3 of them total)
// [[Rcpp::export]]
NumericMatrix mycppFunction(NumericMatrix x) {
  // Resize the input matrix to accommodate the new column for price
  NumericMatrix result(x.nrow(), x.ncol() + 5); // XX: add 1 for each additional column
  
  // Copy the existing columns to the result matrix
  for (int i = 0; i < x.nrow(); i++) {
    for (int j = 0; j < x.ncol(); j++) {
      result(i, j) = x(i, j);
    }
  }
  
  // XX: add each additional column name to the end of this function
  colnames(result) = Rcpp::CharacterVector::create("date", "maturity", "rate", "value", "changeBPS", "ytm", "delta", "gamma");
  
  // XX: following are calculations for the data of new columns, can be used to template additional columns by adding to the end
  // Calculate and add the PV as a new column
  for (int i = 0; i < result.nrow(); i++) {
    double rate = result(i, 2); // Assuming rate is in column 2
    double value = 100 - rate * 100.0;
    result(i, 3) = value; // Add the calculated price as the 4th column
  }
  
  // Calculate and add change in BPS as a new column
  for (int i = 1; i < result.nrow(); i++) {
    double rate_prev = result(i - 1, 2);
    double rate_cur = result(i, 2);
    double changeBPS = (rate_cur - rate_prev) * 10000;
    result(i, 4) = changeBPS; // Add change in BPS as the 5th column
  }
  
  // Hardcoded value for C, MUST BE CHANGED TO IMPLEMENT USER INPUT
  double C = 0.05;
  
  // Calculate the yield to maturity and add it as a new column
  for (int i = 0; i < result.nrow(); i++) {
    double PV = result(i, 3);
    double M = result(i, 1);
    double ytms = ytm(PV, M, C);
    result(i, 5) = ytms;
  }
  
  // Step size of 0.0001 set here, MUST BE CHANGED TO IMPLEMENT USER INPUT
  // Calculate PricePlus and PriceMinus and add them as new columns
  for (int i = 0; i < result.nrow(); i++) {
    double PV = result(i, 3);
    double M = result(i, 1);
    double ytms = result(i, 5);
    double PricePlus = bond_price(ytms + 0.0001, C, M);  // Increment YTM by 0.0001 for PricePlus
    double PriceMinus = bond_price(ytms - 0.0001, C, M); // Decrement YTM by 0.0001 for PriceMinus
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
    result(i, 6) = Delta;
    result(i, 7) = Gamma;
  }
  return result; // Return the modified matrix with the added price column
}
