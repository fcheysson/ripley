// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
int coinc(const arma::vec x, const arma::vec y, const double delta) {
  int count = 0;

  // Iterators
  arma::vec::const_iterator it_x = x.begin();
  arma::vec::const_iterator it_x_end = x.end();
  arma::vec::const_iterator it_y_lwr = y.begin();
  arma::vec::const_iterator it_y = it_y_lwr;
  arma::vec::const_iterator it_y_end = y.end();

  // Loop on x
  for (; it_x != it_x_end; ++it_x) {
    for (it_y = it_y_lwr; it_y != it_y_end; ++it_y) {
      if (*it_x >= *it_y)
        it_y_lwr++;
      else if ((*it_y - *it_x) <= delta)
        count++;
      else
        break;
    }
  }
  return count;
}

// [[Rcpp::export]]
arma::uvec Coinc(const arma::vec& x, const arma::vec& y, const arma::vec& r) {

  // Output vector
  arma::uvec count(r.n_elem, arma::fill::zeros);

  // Iterators
  arma::vec::const_iterator it_x = x.begin();
  arma::vec::const_iterator it_x_end = x.end();
  arma::vec::const_iterator it_y_lwr = y.begin();
  arma::vec::const_iterator it_y = it_y_lwr;
  arma::vec::const_iterator it_y_end = y.end();
  arma::vec::const_iterator it_r_lwr = r.begin();
  arma::vec::const_iterator it_r = it_r_lwr;
  arma::vec::const_iterator it_r_end = r.end();

  // Temp objects
  double distance;
  bool increased;
  int index;

  // Loop on x
  for (; it_x != it_x_end; ++it_x) {
    it_r_lwr = r.begin();                   // Restart at smallest radius
    // Loop on y
    for (it_y = it_y_lwr; it_y != it_y_end; ++it_y) {
      distance = *it_y - *it_x;
      if (distance <= 0)
        it_y_lwr++;
      else {
        increased = false;
        for (it_r = it_r_lwr; it_r != it_r_end; ++it_r) {
          if (distance <= *it_r) {
            index = it_r - r.begin();
            count(index)++;
            it_r_lwr = it_r;                // Adjust smallest admissible radius (further y start at this radius)
            increased = true;
            break;
          }
        }
        if (!increased)
          break;
      }
    }
  }

  return arma::cumsum(count);

}
