#' Plot the power of several group-sequential designs on one plot, for comparison.
#' 
#' \code{draw_power} calls \code{get_power} for a list of group-sequential designs and creates a ggplot2 object. 
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

draw_power = function(d_list, 
                      d_spec, 
                      ez = seq(0, 5, length.out = 100),
                      graph_params = list(z_scale = TRUE,
                                          z_limits = c(-4,4), 
                                          n_x_points = 400,
                                          hazard_scale = FALSE)){
  
  n_control_arm = d_spec$n_control_arm
  R = d_spec$R
  
  z_scale = graph_params$z_scale
  z_limits = graph_params$z_limits
  n_x_points = graph_params$n_x_points
  hazard_scale = graph_params$hazard_scale
  
  if(z_scale && hazard_scale) stop("z_scale and hazard_scale cannot both be TRUE")
  
  df_pow = NULL
  for (i in seq_along(d_list)){
    
    d = d_list[[i]]
    
    df_pow_i = data.frame(power = get_power(d, ez = ez),
                          ez = ez)
    
    df_pow_i$design = i
    
    df_pow = rbind(df_pow, df_pow_i)
  }
  
  df_pow$design = factor(df_pow$design)
  
  
  if (class(d_spec) == "time_to_event_design"){
    
    if(!z_scale) df_pow$ez = df_pow$ez / sqrt(d_spec$target_events * R / (R + 1) ^ 2)
    if(hazard_scale) df_pow$ez = exp(-df_pow$ez)
    
  }
  
  else {
    
    if(!z_scale) df_pow$ez = df_pow$ez / sqrt(n_control_arm * R / (R + 1))
    if(hazard_scale) df_pow$ez = exp(-df_pow$ez)
    
  }
  
  p = ggplot(data = df_pow,
             mapping = aes(x = ez,
                           y = power,
                           colour = design)) +
    geom_line() +
    scale_y_continuous(limits = c(0,1))+
    theme_bw() +
    xlab(ifelse(z_scale,
                "E(Z_fixed)",
                ifelse(hazard_scale,
                       "True hazard ratio",
                       "True effect (standardized)"))) +
    ylab("Power")

  if (hazard_scale) p = p + scale_x_log10(breaks = seq(0.5,2,0.1)) 
  p
  
}