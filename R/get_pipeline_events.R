#' Get the number of events at each interim analysis, and the number of patients that have been recruited up to that point.
#' 
#' \code{get_pipeline_events} calculates the number of events each interim analysis, and how many patients have
#' been recruited up to that point.
#' \param{d} A group sequential design object. Usually this is created from \code{gsDesign}.
#' \param{d_spec} Design specification that is common to all designs being considered. This is list containing the sample size 
#' for a fixed design, allocation ratio, length of delay, etc. 
#' \return A list containing the number of target events, the expected time of decision,
#' the number of patients in the pipeline, the number of patients
#' recruited and the fraction of patients recruited (relative to a fixed sample size).
#' @export 

get_pipeline_events = function(d, d_spec){
  
  if (class(d) == "gsDesign"){
    
    n_target_events = d_spec$target_events * d$n.I
    
    if (d_spec$increase_n){
      
      n_in_pipeline = sapply(n_target_events, 
                             find_pipeline_when_n_events,
                             median_control = d_spec$median_control,
                             hr = d_spec$hr,
                             rec_period = d_spec$rec_period * max(d$n.I),
                             n_control_arm = d_spec$n_control_arm * max(d$n.I),
                             R = d_spec$R,
                             k = d_spec$k)
      
      
      
      e_t = sapply(n_target_events, 
                   find_t_when_n_events,
                   median_control = d_spec$median_control,
                   hr = d_spec$hr,
                   rec_period = d_spec$rec_period * max(d$n.I),
                   n_control_arm = d_spec$n_control_arm * max(d$n.I),
                   R = d_spec$R,
                   k = d_spec$k)
      
    }
    else {
      
      n_in_pipeline = sapply(n_target_events, 
                             find_pipeline_when_n_events,
                             median_control = d_spec$median_control,
                             hr = d_spec$hr,
                             rec_period = d_spec$rec_period,
                             n_control_arm = d_spec$n_control_arm,
                             R = d_spec$R,
                             k = d_spec$k)
      
    
      
      e_t = sapply(n_target_events, 
                   find_t_when_n_events,
                   median_control = d_spec$median_control,
                   hr = d_spec$hr,
                   rec_period = d_spec$rec_period,
                   n_control_arm = d_spec$n_control_arm,
                   R = d_spec$R,
                   k = d_spec$k)
      
    }
    
    
    n_recruited = n_target_events + n_in_pipeline
    
    n_int = n_recruited / (d_spec$n_control_arm * (1 + d_spec$R))
  }
  else{
    n_target_events = d_spec$target_events * d$n.I
    
    e_t = find_t_when_n_events(n = n_target_events,
                               median_control = d_spec$median_control,
                               hr = d_spec$hr,
                               rec_period = d_spec$rec_period,
                               n_control_arm = d_spec$n_control_arm,
                               R = d_spec$R,
                               k = d_spec$k)
    
    n_in_pipeline = 0
    n_recruited = d$n.I * d_spec$n_control_arm * (1 + d_spec$R)
    n_int = 1 
  }
  
  list(n_target_events = n_target_events,
       e_t = e_t,
       n_in_pipeline = n_in_pipeline,
       n_recruited = n_recruited,
       n_int = n_int)
}

#################################
# Helper functions...
#################################
# time-to-event functions (generalize to piecewise exp with dropout)
#################################
p_event_by_t_given_r = function(t, r, lambda) 1 - exp(-lambda * pmax(0, t - r))
p_r = function(r, rec_period, k) k * (r / rec_period) ^ (k - 1) / rec_period
p_event_r = function(r, t, rec_period, lambda, k)  p_event_by_t_given_r(t, r, lambda) * p_r(r, rec_period, k)
#################################
#################################
e_n_events = function(t, median_control, hr, rec_period, n_control_arm, R, k){
  
  n_active_arm = R * n_control_arm
  
  lambda_control = log(2) / median_control
  lambda_active = lambda_control * hr
  
  p_event_by_t_control = integrate(p_event_r, 0, rec_period, 
                                   t = t,
                                   rec_period = rec_period,
                                   lambda = lambda_control,
                                   k  = k)$value 
  
  p_event_by_t_active = integrate(p_event_r, 0, rec_period, 
                                  t = t,
                                  rec_period = rec_period,
                                  lambda = lambda_active,
                                  k  = k)$value 
  
  n_active_arm * p_event_by_t_active + n_control_arm * p_event_by_t_control
  
} 
#################################
find_t_when_n_events = function(n, median_control, hr, rec_period, n_control_arm, R, k){
  
  uniroot(function(t,...) e_n_events(t,...) - n, 
          c(0, rec_period + 2 * median_control / hr),
          median_control = median_control,
          hr = hr,
          rec_period = rec_period,
          n_control_arm = n_control_arm,
          R = R,
          k = k)$root
  
}
#################################
#################################
#################################
find_pipeline_when_n_events = function(n, 
                                       median_control,
                                       hr, 
                                       rec_period, 
                                       n_control_arm, 
                                       R, 
                                       k){
  
  t_star = find_t_when_n_events(n = n, 
                                median_control = median_control,
                                hr = hr,
                                rec_period = rec_period,
                                n_control_arm = n_control_arm,
                                R = R,
                                k = k)
  
  total_n = n_control_arm * (1 + R)
  
  e_n_recruited(t = t_star, 
                rec_period = rec_period,
                total_n = total_n,
                k = k) - n
  
}

