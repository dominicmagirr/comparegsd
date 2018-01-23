#' Get the sample size at each interim analysis, with and without delay.
#' 
#' \code{get_pipeline} calculates how many patients have been recruited at each interim analysis, and how many patients have
#' completed.
#' \param{d} A group sequential design object. Usually this is created from \code{gsDesign}.
#' \param{d_spec} Design specification that is common to all designs being considered. This is list containing the sample size 
#' for a fixed design, allocation ratio, length of delay, etc. 
#' \return A list containing the number of patients completed, the number of patients in the pipeline, the expected time of decision,
#' the number of patients recruited and the fraction of patients recruited (relative to a fixed sample size).
#' @export



get_pipeline = function(d, d_spec){
  
  if (class(d) == "gsDesign"){
    
    n_no_delay = d$n.I * d_spec$n_control_arm * (1 + d_spec$R)
    
    n_in_pipeline = sapply(n_no_delay, 
                           find_pipeline_when_n_complete,
                           delay = d_spec$delay,
                           rec_period = d_spec$rec_period * max(d$n.I),
                           total_n = max(d$n.I) * d_spec$n_control_arm * (1 + d_spec$R),
                           k = d_spec$k)
    
    e_t = sapply(n_no_delay, 
                 find_t_when_n_complete,
                 delay = d_spec$delay,
                 rec_period = d_spec$rec_period * max(d$n.I),
                 total_n = max(d$n.I) * d_spec$n_control_arm * (1 + d_spec$R),
                 k = d_spec$k)
    
    n_with_delay = n_no_delay + n_in_pipeline
    n_int = n_with_delay / (d_spec$n_control_arm * (1 + d_spec$R))
  }
  else{
    n_no_delay = d$n.I * d_spec$n_control_arm * (1 + d_spec$R)
    n_in_pipeline = 0
    
    e_t = find_t_when_n_complete(n = n_no_delay,
                                 delay = d_spec$delay,
                                 rec_period = d_spec$rec_period * max(d$n.I),
                                 total_n = max(d$n.I) * d_spec$n_control_arm * (1 + d_spec$R),
                                 k = d_spec$k)
    
    
    n_with_delay = n_no_delay
    n_int = 1 
  }
  
  list(n_no_delay = n_no_delay,
       n_in_pipeline = n_in_pipeline,
       e_t = e_t,
       n_with_delay = n_with_delay,
       n_int = n_int)
}
#######################################
# Helper functions...
#######################################
e_n_recruited = function(t, rec_period, total_n, k){
  
  total_n * (min(t, rec_period) / rec_period) ^ k
  
} 
#################################
e_n_completed = function(t, delay, rec_period, total_n, k){
  
  total_n * (min(max(t - delay, 0), rec_period) / rec_period) ^ k
  
} 
#################################
find_t_when_n_complete = function(n, delay, rec_period, total_n, k){
  
  uniroot(function(t,...) e_n_completed(t,...) - n, 
          c(0, rec_period + delay + 1),
          delay = delay,
          rec_period = rec_period,
          total_n = total_n,
          k = k)$root
  
}
#################################
find_pipeline_when_n_complete = function(n, delay, rec_period, total_n, k){
  
  t_star = find_t_when_n_complete(n = n, 
                                  delay = delay, 
                                  rec_period = rec_period, 
                                  total_n = total_n, 
                                  k = k)
  
  e_n_recruited(t = t_star, 
                rec_period = rec_period,
                total_n = total_n,
                k = k) - n
  
}
#################################

