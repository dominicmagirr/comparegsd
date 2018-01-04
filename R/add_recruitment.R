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
              sfu = sfLDPocock,
              sfl = sfPoints,
              sflpar = c(0.5 / 0.975))
###################################
d2 = gsDesign(k = 3,
              test.type = 6,
              timing = c(0.5,0.7,1),
              sfu = sfLDPocock,
              sfl = sfPoints,
              sflpar = c(0.5 / 0.975, 0.6 / 0.975))
###################################
d3 = gsDesign(k = 3,
              test.type = 1,
              timing = c(0.5,0.7,1),
              sfu = sfLDPocock)
####################################
d_spec = list(n_control_arm  = 100, 
              R = 2, 
              delay = 6, 
              rec_period = 36, 
              k = 2)

graph_params = list(z_scale = FALSE,
                    z_limits = c(-4,4), 
                    n_x_points = 400,
                    hazard_scale = TRUE)


source("draw_designs.R")
source("draw_power.R")
source("draw_e_info.R")
source("recruitment.R")
source("get_pipeline.R")
draw_designs(d_list = list(d0, d1, d2, d3), d_spec = d_spec, graph_params = graph_params)
draw_designs(d_list = list(d0, d1, d2, d3), d_spec = d_spec, graph_params = graph_params, include_delay = TRUE)
draw_power(d_list = list(d0, d1, d2, d3), d_spec = d_spec, graph_params = graph_params)
draw_e_info(d_list = list(d0, d1, d2, d3), d_spec = d_spec, graph_params = graph_params, include_delay = FALSE) 
draw_e_info(d_list = list(d0, d1, d2, d3), d_spec = d_spec, graph_params = graph_params, include_delay = TRUE) 
####################################













