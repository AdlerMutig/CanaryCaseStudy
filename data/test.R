
#set working directory
setwd("~/Documents/07_canary/") #asume GUI manager does that

#clear Environment used while testing/developing the code
rm(list=ls())

source("data/setPointAlgv07.R")
beta_expo = 1
csvdata = read.csv("data/test_station_b.csv")
sigmaPar = 1
column_select <- list("B_CL2_VAL","B_TURB_VAL","B_PH_VAL","B_TOC_VAL","B_COND_VAL","B_TEMP_VAL","B_PLNT_PH_VAL","B_PLNT_TURB_VAL","B_PLNT_CL2_VAL")

setPoint_results <<- setPointAlg(beta_expo,csvdata,sigmaPar,column_select)