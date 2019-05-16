####################################################
## Structure of the offline mode of Canary
##
## Version 0.1
## Author Paolo Aguilar
####################################################
# Inputs
#    values     data (signal) values for the current time  (1xm vector)
#    precision  precision values for each sensor (1xm vector) Read from csv?
#    set_pt_lo   minimum set point values for each signal (1xm vector)
#    set_pt_hi   maximum set point values for each signal (1xm vector)
#    nprec      number of precision steps away from either set pt. limit where P(event) decays to zero
#    prob_dist  probability distribution to use (1 = exponential; 2 = beta)

#set working directory
setwd("~/Documents/07_canary/")
source("data/finiteData.R")
source("data/exponentialFunction.R")

#clear Environment (maybe not best idea later if used as a funtion and called from another R script)
#just used while testing/developing the code
rm(list=ls())

#load csv, load data set 
values_raw = read.csv("data/test_station_b.csv")

#Precision values for each sensor (1xm vector) Read from csv? ATM filled as data frame with same value
#precision = .1
#
vals <- list("B_CL2_VAL","B_TURB_VAL","B_PH_VAL","B_TOC_VAL","B_COND_VAL","B_TEMP_VAL","B_PLNT_PH_VAL","B_PLNT_TURB_VAL","B_PLNT_CL2_VAL")
#create empty character as placeholder for later used string for creating the data.frame of the residuals
used_names_res <- character()
#fill character with needed names for the residuals data.frame
for(i in 1:length(vals)){
  #assign name for first element in vals
  if(i==1){
    used_names_res <- paste0(used_names_res,paste0(vals[[i]],"_precision"))
  }
  #assign name for second and more element vals
  if(i>=2){
    used_names_res <- paste0(used_names_res,paste0(",",paste0(vals[[i]],"_precision")))
  }
}

#create data.frame for precisions
precision <- read.csv(text=used_names_res)
precision[1,] <- as.numeric(.1)


#Minimum set point values for each signal (1xm vector)
#set_pt_lo = .1
used_names_res <- character()
#fill character with needed names for the residuals data.frame
for(i in 1:length(vals)){
  #assign name for first element in vals
  if(i==1){
    used_names_res <- paste0(used_names_res,paste0(vals[[i]],"_low"))
  }
  #assign name for second and more element vals
  if(i>=2){
    used_names_res <- paste0(used_names_res,paste0(",",paste0(vals[[i]],"_low")))
  }
}

#create data.frame for precisions
set_pt_lo <- read.csv(text=used_names_res)

#All to 0.1
#set_pt_lo[1,] <- as.numeric(.1)
#Hard code it
set_pt_lo[1,1] <- as.numeric(0.0)
set_pt_lo[1,2] <- as.numeric(0.102)
set_pt_lo[1,3] <- as.numeric(6.55)
set_pt_lo[1,4] <- as.numeric(3.1)
set_pt_lo[1,5] <- as.numeric(214.3)
set_pt_lo[1,6] <- as.numeric(52.5)
set_pt_lo[1,7] <- as.numeric(4.0)
set_pt_lo[1,8] <- as.numeric(0.0)
set_pt_lo[1,9] <- as.numeric(0.0)

#maximum set point values for each signal (1xm vector)
#set_pt_hi = 10
used_names_res <- character()
#fill character with needed names for the residuals data.frame
for(i in 1:length(vals)){
  #assign name for first element in vals
  if(i==1){
    used_names_res <- paste0(used_names_res,paste0(vals[[i]],"_high"))
  }
  #assign name for second and more element vals
  if(i>=2){
    used_names_res <- paste0(used_names_res,paste0(",",paste0(vals[[i]],"_high")))
  }
}

#create data.frame for precisions
set_pt_hi <- read.csv(text=used_names_res)
#set_pt_hi[1,] <- as.numeric(10)
#Manually set max
set_pt_hi[1,1] <- as.numeric(4.02)
set_pt_hi[1,2] <- as.numeric(0.998)
set_pt_hi[1,3] <- as.numeric(8.14)
set_pt_hi[1,4] <- as.numeric(19.66)
set_pt_hi[1,5] <- as.numeric(263)
set_pt_hi[1,6] <- as.numeric(82.2)
set_pt_hi[1,7] <- as.numeric(8.75)
set_pt_hi[1,8] <- as.numeric(4.028)
set_pt_hi[1,9] <- as.numeric(4.89)


#Probability vector
#pp
used_names_res <- character()
#fill character with needed names for the residuals data.frame
for(i in 1:length(vals)){
  #assign name for first element in vals
  if(i==1){
    used_names_res <- paste0(used_names_res,paste0(vals[[i]],"_prob"))
  }
  #assign name for second and more element vals
  if(i>=2){
    used_names_res <- paste0(used_names_res,paste0(",",paste0(vals[[i]],"_prob")))
  }
}

#create data.frame for precisions
pp <- read.csv(text=used_names_res)
pp[1,] <- as.numeric(0)


#number of precision steps away from either set pt. limit where P(event) decays to zero
nprec = 1

#probability distribution to use (1 = exponential; 2 = beta)
prob_dist = 1 # (1 = exponential; 2 = beta)


# Initialize
range = set_pt_hi - set_pt_lo;           # range between set points
qtr_range = range / 3.0; 
prec_dist = nprec * precision;

source("data/finiteData.R")
ispinf = is.finite.data.frame(set_pt_hi);
isninf = is.finite.data.frame(set_pt_lo);

range[isninf] = abs(set_pt_hi[isninf]*2.0);
range[ispinf] = abs(set_pt_lo[ispinf]*2.0);

half_range = range/2.0;               # half distance of range
ctr_line = half_range + set_pt_lo;      # center value of range
ctr_line[isninf] = 0;
ctr_line[ispinf] = 0;
qtr_range_prec = qtr_range / nprec;
precision[qtr_range < prec_dist] = qtr_range_prec[qtr_range < prec_dist];



# Check for overlapping regions of non-zero P(event)
err_code = 0;
idx = which(precision*nprec>half_range);
if ((sum(idx)) > 0.0){
  err_code = 1;
}

#get one line of values
#if its stupid and it words, its not stupid
subset_values_raw <- subset(values_raw, select = -Time_Step)
subset_values_raw <- subset(subset_values_raw, select = -B_PRES_OP)
subset_values_raw <- subset(subset_values_raw, select = -B_PLNT_FLOW_OP)
subset_values_raw <- subset(subset_values_raw, select = -B_PLNT_PRES_OP)


#done with one, actually should be iterated over loop
# Calculate distance from set point normalized by precision
ndist = half_range - abs((subset_values_raw[1,] - ctr_line));  # raw distance from nearest set point
ndist[ispinf] = (subset_values_raw[1,ispinf] - set_pt_lo[ispinf]);
ndist[isninf] = (set_pt_hi[isninf] - subset_values_raw[1, isninf]);
ndist = ndist / precision;                     # number of precision units in the raw distance
ndist = ndist / nprec

#pp[1,1] = expon_prob(ndist[1,1]);

source("data/exponentialFunction.R")
# Call probability function with normalized distance
if (prob_dist == 1){
  #Call exponential
  for(i in 1:length(ndist)){
    pp[1,i] = expon_prob(ndist[1,i]);
  }
} else{
  #Call beta
  pp[1,1] = beta_prob(ndist[1,1]);
}

#pp(values<ctr_line) = -pp(values<ctr_line);
#pp(isinf(range)) = 0.0;

