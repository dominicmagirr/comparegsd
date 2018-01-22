#' Get the coordintes to plot a picture of a group-sequential design
#' 
#' \code{get_design_points} Provides a list of co-ordinates that can be used to visualize a group-sequential design.
#' 
#' \param{d} A group sequential design object. Usually this is created from \code{gsDesign}.
#' \param{z_limits} The plot limits on the z-scale. A vector of length 2. 
#' Make sure that \code{z_limits[1]} is less than the lower stopping boundary, and that \code{z_limits[2]} is more than
#' the upper stopping boundary.
#' \param{n_x_points} The number of points to include on the x-axis.
#' \param{include_delay} Should the interim sample sizes take account of the delay between time of recruitment and 
#' time of outcome.
#' \param{d_spec} Design specification that is common to all designs being considered. This is list containing the sample size 
#' for a fixed design, allocation ratio, length of delay, etc. 
#' \return A list containing x cooridates, y coordinates, and which analysis they apply to.
#' @export


get_design_points = function(d, 
                             z_limits, 
                             n_x_points, 
                             include_delay = FALSE, 
                             d_spec = NULL){
  
  n_int = d$n.I # info fraction
  
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

