####################################################
## Structure of the offline mode of Canary
##
## Version 0.1
## Author Popolson Baggins
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

#clear Environment (maybe not best idea later if used as a funtion and called from another R script)
#just used while testing/developing the code
rm(list=ls())

#define window size
window_size <- 1990

#load csv, load data set 
values_raw = read.csv("data/test_station_b.csv")

#Precision values for each sensor (1xm vector) Read from csv? ATM filled as data frame with same value
#precision = .1
precision = read.csv(text="Precs") #Create emtpy data frame
for(row in 1:9){
  #fill column Precs with .1 until real values are read
  precision[row,]=.1
}

#Minimum set point values for each signal (1xm vector)
#set_pt_lo = .1
set_pt_lo = read.csv(text="SPLow") #Create emtpy data frame
for(row in 1:9){
  #fill column SPLow with .1 until having real values
  set_pt_lo[row,]=.1
}

#maximum set point values for each signal (1xm vector)
#set_pt_hi = 10
set_pt_hi = read.csv(text="SPHigh") #Create emtpy data frame
for(row in 1:9){
  #fill column SPHigh with 10 until real values
  set_pt_hi[row,]=10
}

#number of precision steps away from either set pt. limit where P(event) decays to zero
nprec = 1

#probability distribution to use (1 = exponential; 2 = beta)
prob_dist = 1 # (1 = exponential; 2 = beta)

# Initialize
range = set_pt_hi - set_pt_lo;           # range between set points
qtr_range = range / 3.0; 
prec_dist = nprec * precision;

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
subset_values_raw <- subset(values_raw, select = -Time_Step)
subset_values_raw <- subset(subset_values_raw, select = -B_PRES_OP)
subset_values_raw <- subset(subset_values_raw, select = -B_PLNT_FLOW_OP)
subset_values_raw <- subset(subset_values_raw, select = -B_PLNT_PRES_OP)

# Calculate distance from set point normalized by precision
ndist = half_range - abs((values - ctr_line));  # raw distance from nearest set point
ndist(ispinf) = (values(ispinf) - set_pt_lo(ispinf));
ndist(isninf) = (set_pt_hi(isninf) - values(isninf));
ndist = ndist / precision;                     # number of precision units in the raw distance
ndist = ndist / nprec


