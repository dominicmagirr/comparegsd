get_pipeline = function(d, d_spec){
  
  if (class(d) == "gsDesign"){
    
    n_no_delay = d$n.I * d_spec$n_control_arm * (1 + d_spec$R)
    
    n_in_pipeline = sapply(n_no_delay, 
                           find_pipeline_when_n_complete,
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
    n_with_delay = n_no_delay
    n_int = 1 
  }
  
  list(n_no_delay = n_no_delay,
       n_in_pipeline = n_in_pipeline,
       n_with_delay = n_with_delay,
       n_int = n_int)
}
#######################################
#######################################
get_pipeline_events = function(d, d_spec, ez){
  
  if (class(d) == "gsDesign"){
    
    n_no_delay = d$n.I * d_spec$n_control_arm * (1 + d_spec$R)
    
    n_in_pipeline = sapply(n_no_delay, 
                           find_pipeline_when_n_complete,
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
    n_with_delay = n_no_delay
    n_int = 1 
  }
  
  list(n_no_delay = n_no_delay,
       n_in_pipeline = n_in_pipeline,
       n_with_delay = n_with_delay,
       n_int = n_int)
}