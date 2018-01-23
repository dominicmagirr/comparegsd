#' Plot the expected sample size of several group-sequential designs on one plot, for comparison.
#' 
#' \code{draw_e_info} calls \code{get_e_info} for a list of group-sequential designs and creates a ggplot2 object. 
#' Some aspects of the plot can be controlled with \code{graph_params}.
#' 
#' \param{d_list} A list of group-sequential design objects. Usually these are created from \code{gsDesign}.
#' \param{d_spec} Design specification that is common to all designs being considered. This is list containing the sample size 
#' for a fixed design, allocation ratio, length of delay, etc. 
#' \param{ez} The expected z values to calculate power under.
#' \param{graph_params} A list of graphical parameters to control the plot: \code{z_scale} (default = \code{TRUE}, if false will plot
#' on the treatment effect scale), \code{z_limits}, \code{n_x_points}, \code{hazard_scale} (default = \code{FALSE}, if true will overide 
#' \code{z_scale} and plot on hazard ratio scale).
#' \return A ggplot2 object.
#' @export

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
  
  
  if (class(d_spec) == "time_to_event_design"){
    
    if(!z_scale) df_e_info$ez = df_e_info$ez / sqrt(d_spec$target_events * R / (R + 1) ^ 2)
    if(hazard_scale) df_e_info$ez = exp(-df_e_info$ez)
    
    
    if (!include_delay){
      
      df_e_info$e_info = df_e_info$e_info * d_spec$target_events
      n_scale = d_spec$target_events
      
    }
    else {
      
      df_e_info$e_info = df_e_info$e_info * n_control_arm * (1 + R) 
      n_scale = n_control_arm * (1 + R)
      
    }
    
    y_lab = ifelse(include_delay, 
                   "E(Total n), including pipeline", 
                   "E(Total events)")
    
  }
  else {
    
    df_e_info$e_info = df_e_info$e_info * n_control_arm * (1 + R) 
    
    if(!z_scale) df_e_info$ez = df_e_info$ez / sqrt(n_control_arm * R / (R + 1))
    if(hazard_scale) df_e_info$ez = exp(-df_e_info$ez)
    
    n_scale = n_control_arm * (1 + R)
    
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