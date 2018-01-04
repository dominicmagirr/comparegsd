rm(list = ls())
###################################
library(gsDesign)
library(dplyr)
library(ggplot2)
###################################
d0 = list(n.I = 1, alpha = 0.025)
class(d0) = "fixedDesign"
###################################
d1 = gsDesign(k = 2,
              test.type = 6,
              timing = c(0.5, 1),
              #n.I = c(0.5, 1),
              sfu = sfLDPocock,
              sfl = sfPoints,
              sflpar = c(0.5 / 0.975))
###################################
d2 = gsDesign(k = 3,
               test.type = 6,
               timing = c(0.5,0.7,1),
               #n.I = c(0.5,0.7,1),
               sfu = sfLDPocock,
               sfl = sfPoints,
               sflpar = c(0.5 / 0.975, 0.6 / 0.975))
###################################
d3 = gsDesign(k = 3,
               test.type = 1,
               timing = c(0.5,0.7,1),
               sfu = sfLDPocock)
####################################
draw_designs = function(d_list){

  df = NULL
  
  for (i in 1:length(d_list)){
    
    d = d_list[[i]]
    
    if (class(d) == "gsDesign"){
    
      k = d$k
      
      lwr_bnds = d$lower$bound[-k] 
      upr_bnds = d$upper$bound[-k] 

      x = seq(-4, 4, length.out = 400)
      y = rep(d$n.I[k], 400)
      int = rep("f", 400)

      for (j in seq_along(lwr_bnds)){
        x_l_j = seq(-4, lwr_bnds[j], length.out = abs(diff(c(-4, lwr_bnds[j]))) / 8 * 400)
        x_u_j = seq(upr_bnds[j], 4,  length.out = abs(diff(c(4,  upr_bnds[j]))) / 8 * 400)
        y_l_j = rep(d$n.I[j], length(x_l_j))
        y_u_j = rep(d$n.I[j], length(x_u_j))
        int_l_j = rep(paste(j,"_l",sep=""), length(x_l_j))
        int_u_j = rep(paste(j,"_u",sep=""), length(x_u_j))
        x = c(x, x_l_j, x_u_j)
        y = c(y, y_l_j, y_u_j)
        int = c(int, int_l_j, int_u_j)
      }
    }
    else{
      x = seq(-4, 4, length.out = 400)
      y = rep(d$n.I, 400)
      int = rep("1", 400)
    }
    df_i = data.frame(x = x, y = y, int = int)
    df_i$design = i
    
    df= rbind(df, df_i)
  }
  
  df$design = factor(df$design)
  df$design_int = paste(df$design, df$int)
  p =  ggplot(data = df,
              mapping = aes(x = x, 
                            y = y, 
                            group = design_int,
                            colour = design)) +
      geom_line( position = position_jitter(height = 0.01)) +
      #geom_line(linetype = 1,  position = position_jitter(height = 0.02)) +
      xlab("Z") +
      ylab("I") + 
      theme_bw() +
    scale_y_continuous(limits = c(0,1.5))
  
  p

}
p_designs = draw_designs(d_list = list(d0, d1, d2, d3))
p_designs
###################################################


###################################################
# compare power

draw_power = function(d_list){
  
  df_pow = NULL
  for (i in seq_along(d_list)){
    
    probs = gsProbability(theta = seq(0, 5, length.out = 100), d = d_list[[i]])
    
    df_pow_i = data.frame(power = colSums(probs$upper$prob),
                          ez = probs$theta)
    
    df_pow_i$design = i
    
    df_pow = rbind(df_pow, df_pow_i)
  }
  #########################
  p = ggplot(data = df_pow,
         mapping = aes(x = ez,
                       y = power,
                       colour = factor(design))) +
    geom_line() +
    scale_y_continuous(limits = c(0,1))+
    theme_bw() +
    xlab("E(Z_final)") +
    ylab("Power")
  ##########################
  
  p

}
draw_power(d_list = list(d0, d1, d2, d3))

###################################################
###################################################
# compare E(I)
draw_e_info = function(d_list){
  
  df_e_info = NULL
  for (i in seq_along(d_list)){
    
    probs = gsProbability(theta = seq(0, 5, length.out = 100), d = d_list[[i]])
    
    if (is.null(probs$lower)){
      p_stop = probs$upper$prob[1:(d_list[[i]]$k - 1),]
    }
    else {
      p_stop = probs$upper$prob[1:(d_list[[i]]$k - 1),] + probs$lower$prob[1:(d_list[[i]]$k - 1),]
    }
    
    if (d_list[[i]]$k == 2){
      
      e_info_1 = p_stop * d_list[[i]]$n.I[1]
      e_info_2 = (1 - p_stop) * d_list[[i]]$n.I[2]
      
    }
    else{
      e_info_1 = colSums(p_stop * d_list[[i]]$n.I[1:(d_list[[i]]$k - 1)])
      e_info_2 = (1 - colSums(p_stop)) * d_list[[i]]$n.I[d_list[[i]]$k]
    }
    e_info = e_info_1 + e_info_2
    
    df_e_info_i = data.frame(e_info = e_info,
                             ez = probs$theta)
    
    df_e_info_i$design = i
    
    df_e_info = rbind(df_e_info, df_e_info_i)
  }
  #########################
  p = ggplot(data = df_e_info,
             mapping = aes(x = ez,
                           y = e_info,
                           colour = factor(design))) +
    geom_line() +
    scale_y_continuous(limits = c(0,1))+
    theme_bw() +
    xlab("E(Z_final)") +
    ylab("E(I)")
  ##########################
  
  p
  
}
draw_e_info(d_list = list(d0, d1, d2, d3))

