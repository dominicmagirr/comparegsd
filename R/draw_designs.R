get_design_points = function(d, 
                             z_limits, 
                             n_x_points, 
                             include_delay = FALSE, 
                             d_spec = NULL){
  
  n_int = d$n.I
  if (include_delay) n_int = get_pipeline(d = d, d_spec = d_spec)$n_int
  
  z_range = abs(diff(z_limits))
  
  if (class(d) == "gsDesign"){
    
    k = d$k
    
    x = seq(z_limits[1], z_limits[2], length.out = n_x_points)
    y = rep(n_int[k], n_x_points)
    int = rep("f", n_x_points)
    
    upr_bnds = d$upper$bound[-k]
    for (j in seq_along(upr_bnds)){
      
      x_u_j = seq(upr_bnds[j], z_limits[2],  length.out = abs(diff(c(z_limits[2],  upr_bnds[j]))) / z_range * n_x_points)
      
      y_u_j = rep(n_int[j], length(x_u_j))
      
      int_u_j = rep(paste(j,"_u",sep=""), length(x_u_j))
      
      x = c(x, x_u_j)
      y = c(y, y_u_j)
      int = c(int, int_u_j)
    }
    
    if (!is.null(d$lower)){
      lwr_bnds = d$lower$bound[-k]
      for (j in seq_along(lwr_bnds)){
        x_l_j = seq(z_limits[1], lwr_bnds[j], length.out = abs(diff(c(z_limits[1], lwr_bnds[j]))) / z_range * n_x_points)
        
        y_l_j = rep(n_int[j], length(x_l_j))
        
        int_l_j = rep(paste(j,"_l",sep=""), length(x_l_j))
        
        x = c(x, x_l_j)
        y = c(y, y_l_j)
        int = c(int, int_l_j)
      }
    }
  }
  else{
    x = seq(z_limits[1], z_limits[2], length.out = n_x_points)
    y = rep(n_int, n_x_points)
    int = rep("1", n_x_points)
  }
  
  list(x = x, y = y, int = int)
  
}


draw_designs = function(d_list, 
                        d_spec, 
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
  
  df = NULL
  
  for (i in 1:length(d_list)){
    
    d = d_list[[i]]
    
    design_points = get_design_points(d = d, 
                                      z_limits = z_limits, 
                                      n_x_points = n_x_points,
                                      include_delay = include_delay,
                                      d_spec = d_spec)
    
    df_i = data.frame(x = design_points$x, y = design_points$y, int = design_points$int)
    df_i$design = i
    
    df= rbind(df, df_i)
  }
  
  df$design = factor(df$design)
  df$design_int = paste(df$design, df$int)
  
  df$y = df$y * n_control_arm * (R + 1)
  if(!z_scale) df$x = df$x / sqrt(n_control_arm * R / (R + 1))
  if(hazard_scale) df$x = exp(-df$x)
  
  n_scale =  n_control_arm * (R + 1)
  
  if(hazard_scale){
    y_lab = ifelse(include_delay, 
                   "Total n, including pipeline", 
                   "Total events")
  }
  else{
    y_lab = ifelse(include_delay, 
                   "Total n, including pipeline", 
                   "Total n, excluding pipeline")
  }
  
  p =  ggplot(data = df,
              mapping = aes(x = x, 
                            y = y, 
                            group = design_int,
                            colour = design)) +
    geom_line( position = position_jitter(height = 0.01 * n_scale)) +
    #geom_line(linetype = 1,  position = position_jitter(height = 0.02)) +
    xlab(ifelse(z_scale,
                "Observed Z",
                ifelse(hazard_scale,
                       "Observed hazard ratio",
                       "Observed effect (standardized)"))) +
    ylab(y_lab) + 
    theme_bw() +
    scale_y_continuous(limits = c(0,1.5) * n_scale)
  
  if (hazard_scale) p = p + scale_x_log10(breaks = seq(0.5,2,0.1)) 
  p
  
}
