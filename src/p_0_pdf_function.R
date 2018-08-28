

#*********************************************************
# FUNCTION: prob OF p_0, the steady-state prob of M/M/c
# queue system being in state zero 
#*********************************************************
# 2018-08-14
# Nayef Ahmad 

# Function defn: 
p_0.prob <- function(lambda,  # arrival rate 
                    mu,  # service rate 
                    c){  # num servers 
      
      # output: p_0(n), the pdf of the r.v. n representing the 
      
      library("purrr")
      library("tidyverse")
                    
      # define vars: 
      r <-  lambda/mu 
      rho <- lambda/(c*mu) 
      state.list <- 0:(c-1)
      
      
      first.term <- r^c/(factorial(c) * (1-rho))
      
      second.term <- 
            map_dbl(state.list, function(x){
                  r^x/factorial(x)
                  }) %>% 
            sum
            
      return(1/(first.term + second.term))

}



# test function: 
p_0.prob(10, 6, 2)  # 0.09090909; matches with ex. on p7 https://nptel.ac.in/courses/110106046/Module%209/Lecture%204.pdf 
