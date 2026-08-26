#'Logit and mlogit functions 
#'
#'Computes vector of beta values from real values for a logit and mlogit link.
#' @param p vector of real probabilities that sum to 1
#' @return vector of beta values with reference category being the last one
#' @author Gary White
#' @export mlogit_from_real logit_from_real
mlogit_from_real <- function(p) 
{
  # p = vector of real probabilities that sum to 1
  if (abs(sum(p) - 1) > 1e-8) stop("Probabilities must sum to 1")
  if (any(p <= 0)) stop("All probabilities must be > 0")
  # reference category is last one
  beta <- log(p[-length(p)] / p[length(p)])
  return(beta)
}
logit_from_real <- function(p)
{
  if (any(p <= 0)|any(p>=1)) stop("All probabilities must be > 0 and <1")
  return(log(p/(1-p)))
}