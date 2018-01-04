get_e_info = function(d, 
                      ez, 
                      include_delay = FALSE, 
                      d_spec = NULL){

  n_int = d$n.I
  if (include_delay) n_int = get_pipeline(d = d, d_spec = d_spec)$n_int
  
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
      
      e_info_1 = p_stop * n_int[1]
      e_info_2 = (1 - p_stop) * n_int[2]
      
    }
    else{
      e_info_1 = colSums(p_stop * n_int[1:(d$k - 1)])
      e_info_2 = (1 - colSums(p_stop)) * n_int[d$k]
    }
    e_info = e_info_1 + e_info_2
  }
  else {
    e_info = rep(1, length(ez))
  }
  
  e_info
  
}

#df_e_info_i = data.frame(e_info = e_info, ez = probs$theta)

draw_e_info = function(d_list, 
                       d_spec, 
                       ez = seq(0, 5, length.out = 100),
                       graph_params = list(z_scale = TRUE,
                                           z_limits = c(-4,4), 
                                           n_x_points = 400,
                                           hazard_scale = FALSE),
                       include_delay = FALSE){
  
  n_control_arm = d_spec$n_control_arm
  R = d_spec$R
  
  z_scale = graph_params$z_scale
  z_limits = graph_params$z_limits
  n_x_points = graph_params$n_x_points
  hazard_scale = graph_params$hazard_scale
  
  if(z_scale && hazard_scale) stop("z_scale and hazard_scale cannot both be TRUE")
  
  
  df_e_info = NULL
  for (i in seq_along(d_list)){
    
    df_e_info_i = data.frame(e_info = get_e_info(d = d_list[[i]], 
                                                 ez = ez,
                                                 include_delay = include_delay,
                                                 d_spec = d_spec),
                             ez = ez)

    df_e_info_i$design = i
    
    df_e_info = rbind(df_e_info, df_e_info_i)
  }
  
  df_e_info$design = factor(df_e_info$design)
  
  df_e_info$e_info = df_e_info$e_info * n_control_arm * (1 + R) 
  
  if(!z_scale) df_e_info$ez = df_e_info$ez / sqrt(n_control_arm * R / (R + 1))
  if(hazard_scale) df_e_info$ez = exp(-df_e_info$ez)
  
  n_scale = n_control_arm * (1 + R)
  
  if(hazard_scale){
    y_lab = ifelse(include_delay, 
                   "E(Total n), including pipeline", 
                   "E(Total events)")
  }
  else{
    y_lab = ifelse(include_delay, 
                   "E(Total n), including pipeline", 
                   "E(Total n), excluding pipeline")
  }
  
  p = ggplot(data = df_e_info,
             mapping = aes(x = ez,
                           y = e_info,
                           colour = design)) +
    geom_line() +
    scale_y_continuous(limits = c(0,1.5) * n_scale)+
    theme_bw() +
    xlab(ifelse(z_scale,
                "E(Z_fixed)",
                ifelse(hazard_scale,
                       "True hazard ratio",
                       "True effect (standardized)"))) +
    ylab(y_lab) 
    ########################
  if (hazard_scale) p = p + scale_x_log10(breaks = seq(0.5,2,0.1))
  p
  
}