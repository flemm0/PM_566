#include <Rcpp.h>

// [[Rcpp::export]]
int fibCpp(int n) {

	if (n < 2) {
		return n;
	}
	
	return fibCpp(n - 1) + fibCpp(n - 2);
}
