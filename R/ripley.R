#' K-Ripley function
#'
#' Computes the K-Ripley function for a point pattern
#'
#' @param pp A vector of ordered time points; the point pattern
#' @param tMax A double; the end time of the observation window [0,T]
#' @param rmin A double; the minimal radius at which to compute the K-function
#' @param rmax A double; the maximal radius at which to compute the K-function
#' @param step A double; the successive increases between rmin and rmax
#'
#' @returns A tibble object with columns `r` and `ripley`; the value of the K-function at each corresponding radius
#' @export
#'
#' @examples
#' tMax = 1000
#' x = hawkesbow::hawkes(tMax, fun=1, repr=0.5, family="exp", rate=2)
#' x_ripley = ripley(x$p, tMax)
#' ggplot2::ggplot(x_ripley) +
#'   ggplot2::aes(x = r, y = ripley - r) +
#'   ggplot2::geom_line()
ripley = function(pp, tMax, rmin = .1, rmax = 2, step = .1) {

  # ripley
  n = length(pp)
  rs = seq(rmin, rmax, by = step)
  K = as.numeric(Coinc(pp, pp, rs))

  tibble::tibble(r = rs, ripley = tMax * K / (n * (n-1)))

}

#' K-Ripley function for multivariate point patterns
#'
#' Computes the K-Ripley function for a multivariate point pattern
#'
#' @param pp A list of vectors of ordered time points; the point patterns
#' @param tMax A double; the end time of the observation window [0,T]
#' @param rmin A double; the minimal radius at which to compute the K-function
#' @param rmax A double; the maximal radius at which to compute the K-function
#' @param step A double; the successive increases between rmin and rmax
#'
#' @returns A tibble object with columns `i`, `j`, `r`, and `ripley`; the value `ripley` of the K-function between components `i` and `j` at each corresponding radius `r`
#' @export
#'
#' @examples
#' tMax = 1000
#' eta = c(2, 1, 3)
#' repr = matrix(runif(9, 0, 1), 3, 3) %>% {runif(1, .3, .7) * . / invisible(eigen(.))$values[1]} %>% Re()
#' beta = matrix(rexp(9, 1), 3, 3)
#' x = mhawkes(tMax, eta, repr, beta, keep_ancestors = FALSE)
#' x_ripley = mripley(x$pointproc, tMax)
#' ggplot2::ggplot(x_ripley) +
#'   ggplot2::aes(x = r, y = ripley - r) +
#'   ggplot2::geom_line() +
#'   ggplot2::facet_grid(i ~ j)
mripley = function(pp, tMax, rmin = .1, rmax = 2, step = .1) {

  d = length(pp)
  out = tidyr::expand_grid(i = 1:d, j = 1:d)
  rs = seq(rmin, rmax, by = step)
  K = purrr::pmap(out, function(i, j) {
    ni = ifelse(i == j, length(pp[[i]]) - 1, length(pp[i]))
    nj = length(pp[[j]])
    tMax * as.numeric(Coinc(pp[[i]], pp[[j]], rs)) / (ni * nj)
  })

  out %>% dplyr::mutate(r = list(rs), ripley = K) %>% tidyr::unnest(cols = c(r, ripley))

}
