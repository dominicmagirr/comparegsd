e_n_recruited = function(t, rec_period, total_n, k){
  
  total_n * (min(t, rec_period) / rec_period) ^ k
  
} 
#################################
#################################
e_n_completed = function(t, delay, rec_period, total_n, k){
  
  total_n * (min(max(t - delay, 0), rec_period) / rec_period) ^ k
  
} 
#################################
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
#################################
find_pipeline_when_n_events = function(n, median_control, hr, rec_period, n_control_arm, R, k){
  
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















