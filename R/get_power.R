#' Find the power of a group-sequential design
#' 
#' \code{get_power} calculates the power of a group-sequential design for expected z values (of the power-matched fixed design
#' equal to \code{ez}). 
#' 
#' \param{d} A group sequential design object. Usually this is created from \code{gsDesign}.
#' \param{ez} A vector of expected z values. Note this corresponds to the expected z value from a "power-matched" fixed sample 
#' design. 
#' \return A vector. The power at each value of \code{ez}.
#' @export


get_power = function(d, ez){
  
  if (class(d) == "gsDesign"){
    probs = gsProbability(theta = ez, d = d)
    power = colSums(probs$upper$prob)
  }
  else {
    power = pnorm(ez - qnorm(1 - d$alpha))
  }
  power
}

