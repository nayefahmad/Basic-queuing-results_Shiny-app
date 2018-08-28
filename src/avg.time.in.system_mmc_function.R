
#*********************************************************
# FUNCTION: calculate W, the steady-state avg time in system 
# for M/M/c queue 
#*********************************************************
# 2018-08-14
# Nayef Ahmad 

library("here")


# function defn: ------------
avg.tis_mmc <- function(lambda, 
                        mu, 
                        c,  # num servers 
                        p_0.function = p_0.prob  # function used to calculate p_0 
                        ){
      
      # output: average time in system (TIS) including time in queue and 
      #     time in service 
      # Note: for ED modelling, if we define service time as start to 
      #     disposition time, then we expect most of the TIS to fall under time in service 
      #     The division between "queue" and "service" can be arbitrarily selected for this system 
      
      # define vars: 
      r <-  lambda/mu 
      rho <- lambda/(c*mu)
      
      W = 1/mu + 
            (((r^c)/(factorial(c) * (c*mu) * ((1-rho)^2))) * 
            p_0.function(lambda, mu, c))
      
      return(W)
      
}


#********************************************
# test the fn: ---------------
#********************************************

# > Example from Gross, p72: ----------
p_0.prob(6, 3, 3) # 1/9; this is correct 

avg.tis_mmc(6, 3, 3)  # 0.4814815 hours = 28.88889 minutes; this is correct




# > Examples from VCH data: ----------

# todo: obviously, all of the below should be done in a purrr:pmap( ) loop 

# Example: VGH, calendar 2017 ------------ 
avg.tis_mmc(267, 4.08, 143) * 24  # 5.882353 hours 
# data from cube: 5.89 hours. 
# This is effectively equal! <1 minute difference!

# note: c*mu = 4.08*143 = 583.44; if lambda reaches this value, system breaks down!!

# effect of increasing visits:
df <- data.frame(visits = 267:583, 
           tis = map_dbl(267:583, 
                         avg.tis_mmc, 
                         c = 143, 
                         mu = 4.08, 
                         p_0.function = p_0.prob) *24) %>% 
      mutate(rho = visits/(143*4.08))
# looks like system performance starts to deteriorate sharply when rho 
#     reaches about 0.90 

scale.factor <- 1  # used in approximation for non-Markovian multi-server queue (p371 of Patient Flow book)
p1.vgh <- 
      df %>% ggplot(aes(x=rho, y=tis*scale.factor)) + 
      geom_line() + 
      scale_y_continuous(limits = c(0, 24)) +
      scale_x_continuous(breaks = seq(0.4, 1.0, 0.1)) + 
      geom_vline(xintercept = 0.9, 
                 colour = "red") + 
      labs(title = "VGH - Average time in ED versus ED bed utilization rate (rho)", 
           subtitle = "Model shows that avg time in ED starts to increase rapidly after rho reaches 0.90", 
           x = "ED bed utilization", 
           y = "Time in ED (hours)") + 
      theme_classic(base_size = 12); p1.vgh

# save plot: 
ggsave(here("results", 
            "output from src", 
            "2018-08-18_rgnl_vgh_time-in-ed-vs-utilization.pdf"))



# Example: SPH, calendar 2017 -------------   
avg.tis_mmc(242, 4.57, 64) * 24  # 5.297288 hours 
# data from cube: 5.25 hours. 
# Pretty damn close: 3 minutes difference (<1% diff)

# effect of increasing visits:
df <- data.frame(visits = 242:295, 
           tis = map_dbl(242:295, 
                         avg.tis_mmc, 
                         c = 64, 
                         mu = 4.57, 
                         p_0.function = p_0.prob) *24) %>% 
           mutate(rho = visits/(64*4.57))
# looks like system performance starts to deteriorate sharply when rho 
#     reaches about 0.90 

scale.factor <- 1  # used in approximation for non-Markovian multi-server queue (p371 of Patient Flow book)
p2.sph <- 
      df %>% ggplot(aes(x=rho, y=tis*scale.factor)) + 
      geom_line() + 
      scale_y_continuous(limits = c(0, 15)) +
      scale_x_continuous(breaks = seq(0.8, 1.0, 0.1)) + 
      # coord_cartesian(xlim = c(0.7, 1.0)) + 
      geom_vline(xintercept = 0.9, 
                 colour = "red") + 
      labs(title = "SPH - Average time in ED versus ED bed utilization rate (rho)", 
           subtitle = "Model shows that avg time in ED starts to increase rapidly after rho reaches 0.90", 
           x = "ED bed utilization", 
           y = "Time in ED (hours)") + 
      theme_classic(base_size = 12); p2.sph 

# save plot: 
ggsave(here("results", 
            "output from src", 
            "2018-08-18_rgnl_sph_time-in-ed-vs-utilization.pdf"))



# Example: LGH, calendar 2017 -----------    
avg.tis_mmc(176, 5.19, 81) * 24  # 4.624277 hours 
# data from cube: 4.63 hours. 
# This is effectively equal! <1 minute difference!

# effect of increasing visits:
df <- data.frame(visits = 176:round(5.19*81, 0), 
                 tis = seq_along(176:round(5.19*81, 0))) %>% 
      mutate(tis =map_dbl(visits,
                          avg.tis_mmc, 
                          c = 81, 
                          mu = 5.19, 
                          p_0.function = p_0.prob) *24) %>% 
      mutate(rho = visits/(81*5.19))
# looks like system performance starts to deteriorate sharply when rho 
#     reaches about 0.90 

scale.factor <- 1  # used in approximation for non-Markovian multi-server queue (p371 of Patient Flow book)
p3.lgh <- 
      df %>% ggplot(aes(x=rho, y=tis*scale.factor)) + 
      geom_line() + 
      scale_y_continuous(limits = c(0, 24)) +
      scale_x_continuous(breaks = seq(0.4, 1.0, 0.1)) + 
      geom_vline(xintercept = 0.9, 
                 colour = "red") + 
      labs(title = "LGH - Average time in ED versus ED bed utilization rate (rho)", 
           subtitle = "Model shows that avg time in ED starts to increase rapidly after rho reaches 0.90", 
           x = "ED bed utilization", 
           y = "Time in ED (hours)") + 
      theme_classic(base_size = 12); p3.lgh

# save plot: 
ggsave(here("results", 
            "output from src", 
            "2018-08-18_rgnl_LGH_time-in-ed-vs-utilization.pdf"))







# Example: RHS, calendar 2017 ------------  
avg.tis_mmc(156, 4.25, 67) * 24  # 5.64706 hours 
# data from cube: 5.65 hours. 
# This is effectively equal! <1 minute difference!


# effect of increasing visits:
df <- data.frame(visits = 156:round(4.25*67, 0), 
                 tis = seq_along(156:round(4.25*67, 0))) %>% 
      mutate(tis =map_dbl(visits,
                          avg.tis_mmc, 
                          c = 67, 
                          mu = 4.25, 
                          p_0.function = p_0.prob) *24) %>% 
      mutate(rho = visits/(67*4.25))
# looks like system performance starts to deteriorate sharply when rho 
#     reaches about 0.90 

scale.factor <- 1  # used in approximation for non-Markovian multi-server queue (p371 of Patient Flow book)
p4.rhs <- 
      df %>% ggplot(aes(x=rho, y=tis*scale.factor)) + 
      geom_line() + 
      scale_y_continuous(limits = c(0, 24)) +
      scale_x_continuous(breaks = seq(0.4, 1.0, 0.1)) + 
      geom_vline(xintercept = 0.9, 
                 colour = "red") + 
      labs(title = "RHS - Average time in ED versus ED bed utilization rate (rho)", 
           subtitle = "Model shows that avg time in ED starts to increase rapidly after rho reaches 0.90", 
           x = "ED bed utilization", 
           y = "Time in ED (hours)") + 
      theme_classic(base_size = 12); p4.rhs

# save plot: 
ggsave(here("results", 
            "output from src", 
            "2018-08-18_rgnl_RHS_time-in-ed-vs-utilization.pdf"))






# Example: MSJ, calendar 2017  ------------  
avg.tis_mmc(88, 8.11, 45) * 24  # 2.959309 hours 
# data from cube: 2.96 hours. 
# This is effectively equal! <1 minute difference!

# effect of increasing visits:
df <- data.frame(visits = 88:round(8.11*45, 0), 
                 tis = seq_along(88:round(8.11*45, 0))) %>% 
      mutate(tis =map_dbl(visits,
                          avg.tis_mmc, 
                          c = 45, 
                          mu = 8.11, 
                          p_0.function = p_0.prob) *24) %>% 
      mutate(rho = visits/(8.11*45))
# looks like system performance starts to deteriorate sharply when rho 
#     reaches about 0.90 

scale.factor <- 1  # used in approximation for non-Markovian multi-server queue (p371 of Patient Flow book)
p5.msj <- 
      df %>% ggplot(aes(x=rho, y=tis*scale.factor)) + 
      geom_line() + 
      scale_y_continuous(limits = c(0, 24)) +
      scale_x_continuous(breaks = seq(0.2, 1.0, 0.1)) + 
      geom_vline(xintercept = 0.9, 
                 colour = "red") + 
      labs(title = "MSJ - Average time in ED versus ED bed utilization rate (rho)", 
           subtitle = "Model shows that avg time in ED starts to increase rapidly after rho reaches 0.90", 
           x = "ED bed utilization", 
           y = "Time in ED (hours)") + 
      theme_classic(base_size = 12); p5.msj

# save plot: 
ggsave(here("results", 
            "output from src", 
            "2018-08-18_rgnl_MSJ_time-in-ed-vs-utilization.pdf"))






# Example: UBC, calendar 2017  ------------   
avg.tis_mmc(60, 10.45, 79) * 24  # 2.296651 hours 
# data from cube: 2.30 hours. 
# This is effectively equal! <1 minute difference!

# effect of increasing visits:
df <- data.frame(visits = 60:round(10.45*79, 0), 
                 tis = seq_along(60:round(10.45*79, 0))) %>% 
      mutate(tis =map_dbl(visits,
                          avg.tis_mmc, 
                          c = 79, 
                          mu = 10.45, 
                          p_0.function = p_0.prob) *24) %>% 
      mutate(rho = visits/(10.45*79))
# looks like system performance starts to deteriorate sharply when rho 
#     reaches about 0.90 

scale.factor <- 1  # used in approximation for non-Markovian multi-server queue (p371 of Patient Flow book)
p6.ubc <- 
      df %>% ggplot(aes(x=rho, y=tis*scale.factor)) + 
      geom_line() + 
      scale_y_continuous(limits = c(0, 24)) +
      scale_x_continuous(breaks = seq(0.0, 1.0, 0.1)) + 
      geom_vline(xintercept = 0.9, 
                 colour = "red") + 
      labs(title = "UBC - Average time in ED versus ED bed utilization rate (rho)", 
           subtitle = "Model shows that avg time in ED starts to increase rapidly after rho reaches 0.90", 
           x = "ED bed utilization", 
           y = "Time in ED (hours)") + 
      theme_classic(base_size = 12); p6.ubc

# save plot: 
ggsave(here("results", 
            "output from src", 
            "2018-08-18_rgnl_UBC_time-in-ed-vs-utilization.pdf"))









# Example: VCH ex C.Rural, calendar 2017 
# avg.tis_mmc(989, 4.81, 552) * 24  # todo: c=552 is too large to compute factorial! 



#***********************************************************
# >> Example: VGH, calendar 2016 -------------
avg.tis_mmc(262, 4.13, 143) * 24  # 5.811138 hours 
# data from cube: 5.81 hours. 
# This is effectively equal! <1 minute difference!


# >> Example: SPH, calendar 2016 ------------
avg.tis_mmc(237, 4.38, 64) * 24  # 5.552992 hours 
# data from cube: 5.48 hours. 
# This is pretty close: 4.4 minute differencet (~1.3% diff)


# >> Example: LGH, calendar 2016 ------------
avg.tis_mmc(177, 4.07, 154) * 24  # 5.896806 hours 
# data from cube: 5.89 hours. 
# This is effectively equal! <1 minute difference!


# >> Example: RHS, calendar 2016 ------------
avg.tis_mmc(154, 4.22, 67) * 24  # 5.687205 hours 
# data from cube: 5.68 hours. 
# This is effectively equal! <1 minute difference!


# >> Example: MSJ, calendar 2016 ------------
avg.tis_mmc(85, 7.97, 45) * 24  # 3.011292 hours 
# data from cube: 3.01 hours. 
# This is effectively equal! <1 minute difference!


# >> Example: UBC, calendar 2016 ------------
avg.tis_mmc(59, 11.63, 79) * 24  # 2.063629 hours 
# data from cube: 32.06 hours. 
# This is effectively equal! <1 minute difference!





#**************************************************
# WRITE ALL GRAPHS: -------
#**************************************************

pdf(here("results", 
         "output from src", 
         "2018-08-19_rgnl_all-sites_time-in-ed-vs-utilization.pdf"))
p1.vgh
p2.sph
p3.lgh
p4.rhs
p5.msj
p6.ubc
dev.off()

