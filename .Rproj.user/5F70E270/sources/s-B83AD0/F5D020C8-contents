

#*******************************************
# READING INPUT DATA: TREATMENT VOLUMES, DURATIONS  
#*******************************************

library("tidyr")
library("ggplot2")
library("magrittr")
library("dplyr")
library("reshape2")

# rm(list = ls())

source("expand.hist_function.R")

# TODO: ---------------
# > need a duration value for IV Antibiotics (see file "VGH MDC information... .pptx")

#******************************************
# READ IN CURRENT DURATIONS DATA: ------------
data.path <- "H:/VCH files - Nayef/2017-12-04_vgh_mdc-capacity-plan/results/clean data"

df1.durations <- read.csv(file=paste0(data.path, 
                                      "/2017-12-05_vgh_mdc-treatment-durations.csv"))

# str(df1.durations)                          



# data wranglingn: ------------
# replace NAs with zero: 
df1.durations[is.na(df1.durations)] <- 0 

# str(df1.durations); nrow(df1.durations)

# create full data from input histograms in df1: -------
list1.durations <- lapply(2:19, 
                          expand.hist, 
                          df = df1.durations) 

names(list1.durations) <- names(df1.durations)[-1]

# examine the data: -----------------
# str(list1.durations)
# summary(list1.durations)  # check totals 



#*********************************************
# READ IN CURRENT VOLUMES DATA: ---------
df2.volumes <- read.csv(file=paste0(data.path, 
                                    "/2017-12-07_vgh_mdc-volumes-data-fy17.csv")) 
df2.volumes %<>%
      mutate(code = as.character(code))
# df2.volumes %<>% filter(!(treatment %in% c("Phlebotomy", "Cytotoxic")))  

# str(df2.volumes)




#**************************************
# READ IN FUTURE DURATIONS DATA: -----------
df3.durations <- read.csv(file=paste0(data.path, 
                                      "/2017-12-08_vgh_mdc-treatment-durations-future-state.csv"))

# data wranglingn: ------------
# replace NAs with zero: 
df3.durations[is.na(df3.durations)] <- 0 

# create full data from input histograms in df1: -------
list2.durations <- lapply(2:20, 
                          expand.hist, 
                          df = df3.durations) 

names(list2.durations) <- names(df3.durations)[-1]

# examine the data: -----------------
# str(list2.durations)
# summary(list2.durations)  # check totals



#**************************************
# READ IN FUTURE VOLUMES DATA: ADD VGH ED AND UBC ----------
df4.volumes <- read.csv(file=paste0(data.path, 
                                    "/2017-12-08_vgh_mdc-volumes-data-future-state.csv")) 
df4.volumes %<>%
      mutate(code = as.character(code))








#**************************************
# WRITE OUTPUTS: -----------
durations.summary <- data.frame(code = names(list1.durations),
                                volume = sapply(list1.durations, length), 
                                mean = sapply(list1.durations, mean), 
                                median = sapply(list1.durations, median)) 
durations.summary %<>% mutate(skew = mean - median) %>% arrange(volume)  # %>% print 

output.path <- "H:/VCH files - Nayef/2017-12-04_vgh_mdc-capacity-plan/results/output from src"

write.csv(durations.summary, 
          file=paste0(output.path, "/2017-12-07_vgh_mdc-durations-summary.csv"), 
          row.names = FALSE)
