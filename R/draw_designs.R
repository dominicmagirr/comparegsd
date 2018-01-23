#' Draw several group-sequential design on one plot, for easy comparison
#' 
#' \code{draw_designs} calls \code{get_design_points} for a list of group-sequential designs and creates a ggplot2 object. 
#' Some aspects of the plot can be controlled with \code{graph_params}.
#' 
#' \param{d_list} A list of group-sequential design objects. Usually these are created from \code{gsDesign}.
#' \param{d_spec} Design specification that is common to all designs being considered. This is list containing the sample size 
#' for a fixed design, allocation ratio, length of delay, etc. 
#' \param{graph_params} A list of graphical parameters to control the plot: \code{z_scale} (default = \code{TRUE}, if false will plot
#' on the treatment effect scale), \code{z_limits}, \code{n_x_points}, \code{hazard_scale} (default = \code{FALSE}, if true will overide 
#' \code{z_scale} and plot on hazard ratio scale).
#' \param{include_delay} Should the interim sample sizes take account of the delay between time of recruitment and 
#' time of outcome.
#' \return A ggplot2 object.
#' @export


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
  
  if (class(d_spec) == "time_to_event_design"){
    
    
    if(!z_scale) df$x = df$x / sqrt(d_spec$target_events * R / (R + 1) ^ 2)
    if(hazard_scale) df$x = exp(-df$x)
    
    if (!include_delay){
    
      df$y = df$y * d_spec$target_events
      n_scale = d_spec$target_events
      
    }
    else {
      
      df$y = df$y * n_control_arm * (R + 1)
      n_scale =  n_control_arm * (R + 1)
      
    }
    
    y_lab = ifelse(include_delay, 
                   "Total n, including pipeline", 
                   "Total events")
     
  }
  else{
    
    df$y = df$y * n_control_arm * (R + 1)
    if(!z_scale) df$x = df$x / sqrt(n_control_arm * R / (R + 1))
    
    
    n_scale =  n_control_arm * (R + 1)
    
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
