#' Calculate the expected information fraction of a group-sequential design
#' 
#' \code{get_e_info} calculates the expected information (relative to a fixed design) of a group-sequential design under a range of effect sizes. 
#' 
#' \param{d} A group sequential design object. Usually this is created from \code{gsDesign}.
#' \param{ez} A vector of expected z values. Note this corresponds to the expected z value from a "power-matched" fixed sample 
#' design.
#' \param{include_delay} Should the interim sample sizes take account of the delay between time of recruitment and 
#' time of outcome.
#' \param{d_spec} Design specification that is common to all designs being considered. This is list containing the sample size 
#' for a fixed design, allocation ratio, length of delay, etc. 
#' \return A vector. The expected information fraction (relative to a fixed design) at each value of \code{ez}.
#' @export

get_e_t = function(d, 
                   ez, 
                   d_spec = NULL){

  
  if (class(d_spec) == "time_to_event_design"){
    e_t  = get_pipeline_events(d = d, d_spec = d_spec)$e_t
  }
  else {
    e_t = get_pipeline(d = d, d_spec = d_spec)$e_t
  }
  
  
  ### for survival, also supply "get_pipeline" with ez
  
  if (class(d) == "gsDesign"){
    probs = gsProbability(theta = ez, d = d)
    
    if (is.null(probs$lower)){
      p_stop = probs$upper$prob[1:(d$k - 1),]
    }
    else {
      p_stop = probs$upper$prob[1:(d$k - 1),] + probs$lower$prob[1:(d$k - 1),]
    }
    
    if (d$k == 2){
      
      e_t_1 = p_stop * e_t[1]
      e_t_2 = (1 - p_stop) * e_t[2]
      
    }
    else{
      e_t_1 = colSums(p_stop * e_t[1:(d$k - 1)])
      e_t_2 = (1 - colSums(p_stop)) * e_t[d$k]
    }
    e_t = e_t_1 + e_t_2
  }
  else {
    e_t = rep(e_t, length(ez))
  }
  
  e_t
  
}