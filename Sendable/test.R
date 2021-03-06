####################################################
## Test file for the Set Point Proximity Algorithm
##
## Version 1.0
## Author Paolo Aguilar
####################################################


#set working directory
setwd("~/Documents/07_canary/Sendable") #asume GUI manager does that

#clear Environment used while testing/developing the code
rm(list=ls())

source("setPointAlgv07.R")
beta_expo = "exponential"
csvdata = read.csv("test_station_b.csv")
sigmaPar = 1.0
column_select <- list("B_CL2_VAL","B_TURB_VAL","B_PH_VAL","B_TOC_VAL","B_COND_VAL","B_TEMP_VAL","B_PLNT_PH_VAL","B_PLNT_TURB_VAL","B_PLNT_CL2_VAL")

setPoint_results <- setPointAlg(beta_expo,sigmaPar,csvdata,column_select)

write.csv(setPoint_results, file = "outputTest.csv")
            