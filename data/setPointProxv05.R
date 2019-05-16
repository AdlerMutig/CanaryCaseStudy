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


#clear Environment 
#just used while testing/developing the code
rm(list=ls())
source("data/finiteData.R")
source("data/exponentialFunction.R")

#Standard deviation received as parameter form GUI guy
stdDev = 2

#probability distribution to use (1 = exponential; 2 = beta)
prob_dist = 1 # (1 = exponential; 2 = beta)

#Load csv with data set. Also will be done through parameters
values_raw = read.csv("data/test_station_b.csv")

#Create empty data.frame for time_stamps
subset_used_timestamps = values_raw[0,1, drop = FALSE]

#Precision values for each sensor (1xm vector) Read from csv? ATM filled as data frame with same value
#precision = .1

#Used column names. To be determined by GUI
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

#Create standard deviation data frame

#Empty string placeholder for column names
used_names_res <- character()
#fill character with needed names for the residuals data.frame
for(i in 1:length(vals)){
  #assign name for first element in vals
  if(i==1){
    used_names_res <- paste0(used_names_res,paste0(vals[[i]],"_stdDev"))
  }
  #assign name for second and more element vals
  if(i>=2){
    used_names_res <- paste0(used_names_res,paste0(",",paste0(vals[[i]],"_stdDev")))
  }
}


#create data.frame for standard deviations
std_dev_df <- read.csv(text=used_names_res)
std_dev_df[1,1] <- sd(values_raw[["B_CL2_VAL"]])
std_dev_df[1,2] <- sd(values_raw[["B_TURB_VAL"]])
std_dev_df[1,3] <- sd(values_raw[["B_PH_VAL"]])
std_dev_df[1,4] <- sd(values_raw[["B_TOC_VAL"]])
std_dev_df[1,5] <- sd(values_raw[["B_COND_VAL"]])
std_dev_df[1,6] <- sd(values_raw[["B_TEMP_VAL"]])
std_dev_df[1,7] <- sd(values_raw[["B_PLNT_PH_VAL"]])
std_dev_df[1,8] <- sd(values_raw[["B_PLNT_TURB_VAL"]])
std_dev_df[1,9] <- sd(values_raw[["B_PLNT_CL2_VAL"]])


#Create means data frame

#Empty string placeholder for column names
used_names_res <- character()
#fill character with needed names for the residuals data.frame
for(i in 1:length(vals)){
  #assign name for first element in vals
  if(i==1){
    used_names_res <- paste0(used_names_res,paste0(vals[[i]],"_stdDev"))
  }
  #assign name for second and more element vals
  if(i>=2){
    used_names_res <- paste0(used_names_res,paste0(",",paste0(vals[[i]],"_mean")))
  }
}


#create data.frame for means
mean_df <- read.csv(text=used_names_res)
mean_df[1,1] <- mean(values_raw[["B_CL2_VAL"]])
mean_df[1,2] <- mean(values_raw[["B_TURB_VAL"]])
mean_df[1,3] <- mean(values_raw[["B_PH_VAL"]])
mean_df[1,4] <- mean(values_raw[["B_TOC_VAL"]])
mean_df[1,5] <- mean(values_raw[["B_COND_VAL"]])
mean_df[1,6] <- mean(values_raw[["B_TEMP_VAL"]])
mean_df[1,7] <- mean(values_raw[["B_PLNT_PH_VAL"]])
mean_df[1,8] <- mean(values_raw[["B_PLNT_TURB_VAL"]])
mean_df[1,9] <- mean(values_raw[["B_PLNT_CL2_VAL"]])


#Minimum set point values for each signal (1xm vector)

#Empty string placeholder for column names
used_names_res <- character()

#fill character with needed names for the lower limits data.frame
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

#create data.frame for low limits
set_pt_lo <- read.csv(text=used_names_res)


#set_pt_lo[1,] <- as.numeric(.1)
#Hard code it
set_pt_lo[1,1] <- mean_df[1,1]- (stdDev * std_dev_df[1,1])
set_pt_lo[1,2] <- mean_df[1,2]- (stdDev * std_dev_df[1,2])
set_pt_lo[1,3] <- mean_df[1,3]- (stdDev * std_dev_df[1,3])
set_pt_lo[1,4] <- mean_df[1,4]- (stdDev * std_dev_df[1,4])
set_pt_lo[1,5] <- mean_df[1,5]- (stdDev * std_dev_df[1,5])
set_pt_lo[1,6] <- mean_df[1,6]- (stdDev * std_dev_df[1,6])
set_pt_lo[1,7] <- mean_df[1,7]- (stdDev * std_dev_df[1,7])
set_pt_lo[1,8] <- mean_df[1,8]- (stdDev * std_dev_df[1,8])
set_pt_lo[1,9] <- mean_df[1,9]- (stdDev * std_dev_df[1,9])

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

#create data.frame for high limits
set_pt_hi <- read.csv(text=used_names_res)
#set_pt_hi[1,] <- as.numeric(10)
#Manually set max
set_pt_hi[1,1] <- mean_df[1,1]+ stdDev*std_dev_df[1,1]
set_pt_hi[1,2] <- mean_df[1,1]+ stdDev*std_dev_df[1,1]
set_pt_hi[1,3] <- mean_df[1,1]+ stdDev*std_dev_df[1,1]
set_pt_hi[1,4] <- mean_df[1,1]+ stdDev*std_dev_df[1,1]
set_pt_hi[1,5] <- mean_df[1,1]+ stdDev*std_dev_df[1,1]
set_pt_hi[1,6] <- mean_df[1,1]+ stdDev*std_dev_df[1,1]
set_pt_hi[1,7] <- mean_df[1,1]+ stdDev*std_dev_df[1,1]
set_pt_hi[1,8] <- mean_df[1,1]+ stdDev*std_dev_df[1,1]
set_pt_hi[1,9] <- mean_df[1,1]+ stdDev*std_dev_df[1,1]


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




# Initialize
range = set_pt_hi - set_pt_lo;           # range between set points
qtr_range = range / 3.0; 
prec_dist = nprec * precision;

#source("data/finiteData.R")
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

for(row in 1:nrow(subset_values_raw)){
#row=1
  #done with one, actually should be iterated over loop
  # Calculate distance from set point normalized by precision
    ndist = half_range - abs((subset_values_raw[row,] - ctr_line));  # raw distance from nearest set point
    ndist[ispinf] = (subset_values_raw[row,ispinf] - set_pt_lo[ispinf]);
    ndist[isninf] = (set_pt_hi[isninf] - subset_values_raw[row, isninf]);
    ndist = ndist / precision;                     # number of precision units in the raw distance
    ndist = ndist / nprec
    
    #pp[1,1] = expon_prob(ndist[1,1]);
    #add new timestamp to each row
    subset_used_timestamps = rbind(subset_used_timestamps,values_raw[row,1,drop = FALSE])
    #source("data/exponentialFunction.R")
    # Call probability function with normalized distance
    if (prob_dist == 1){
      #Call exponential
      pp[row,1] = values_raw[row,1]
      for(i in 1:length(ndist)){
        pp[row,i] = expon_prob(ndist[1,i]);
      }
    } else{
      #Call beta
      pp[row,i] = beta_prob(ndist[1,i]);
    }
}
results = cbind(subset_used_timestamps,pp)


