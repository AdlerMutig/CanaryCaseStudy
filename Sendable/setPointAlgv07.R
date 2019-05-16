####################################################
## Structure of the offline mode of Canary
##
## Version 7.0
## Author Paolo Aguilar
####################################################
# Inputs
#    beta_expo      1 for exponential function, 2 for beta function
#    csvdata        data frame with the information read from a csv file
#    sigmaPar       how many sigmas we define as upper and lower limit
#    column_select  columns wanted in the analysis
#
setPointAlg <- function(beta_expo,sigmaPar,csvdata,column_select){

  #Add sources of the auxiliary probability functions calculation and the (in)finite boolean array function
  source("data/finiteData.R")
  source("data/exponentialFunction.R")
  source("data/betaFunction.R")
  
  #Standard deviation received as parameter form GUI
  stdDev = sigmaPar
  
  #probability distribution to use ("beta" / "exponential")
  prob_dist = beta_expo 
  
  #Load csv with data set. Done through parameters
  values_raw = csvdata
  
  #Create empty data.frame for time_stamps
  subset_used_timestamps = values_raw[0,1, drop = FALSE]
  
  #Create empty data frame for event column
  event_column <- read.csv(text="Event_Column")
  event_column[1,] <- as.numeric(.1)
  
  #Used column names for variables to be processed. Determined by GUI
  vals <- column_select
  
  #Create empty character list as placeholder for column names in Precision Data frame
  used_names_res <- character()
  
  #Fill empty character list with names form the variables + _precision
  for(i in 1:length(vals)){
    #assign name for first element in vals
    if(i==1){
      used_names_res <- paste0(used_names_res,paste0(vals[[i]],"_precision"))
    }
    #assign name for second and more element vals. Comma needed to separate values
    if(i>=2){
      used_names_res <- paste0(used_names_res,paste0(",",paste0(vals[[i]],"_precision")))
    }
  }
  
  #Create Data frame for precisions using the above created column names
  precision <- read.csv(text=used_names_res)
  precision[1,] <- as.numeric(.1)
  
  #
  #STANDARD DEVIATION
  #
  
  #Create empty character list as placeholder for column names in Standard Deviation Data frame
  used_names_res <- character()
  
  ##Fill empty character list with names form the variables + _stdDev
  for(i in 1:length(vals)){
    #assign name for first element in vals
    if(i==1){
      used_names_res <- paste0(used_names_res,paste0(vals[[i]],"_stdDev"))
    }
    #assign name for second and more element vals. Comma needed to separate values
    if(i>=2){
      used_names_res <- paste0(used_names_res,paste0(",",paste0(vals[[i]],"_stdDev")))
    }
  }
  
  #create data.frame for standard deviations
  std_dev_df <- read.csv(text=used_names_res)

  #Calculate standard deviation from input data
  for(i in 1:length(vals)){
    std_dev_df[1,i] <- sd(values_raw[[vals[[i]]]], na.rm=TRUE)
  }
 
  
  #
  # MEAN
  #
  
  #Create empty character list as placeholder for column names in Mean Data frame
  used_names_res <- character()
  
  #Fill empty character list with names form the variables + _mean
  for(i in 1:length(vals)){
    #assign name for first element in vals
    if(i==1){
      used_names_res <- paste0(used_names_res,paste0(vals[[i]],"_mean"))
    }
    #assign name for second and more element vals. Comma needed to separate values
    if(i>=2){
      used_names_res <- paste0(used_names_res,paste0(",",paste0(vals[[i]],"_mean")))
    }
  }
  
  
  #create data.frame for Mean and fill it with the calculated mean
  mean_df <- read.csv(text=used_names_res)
  for(i in 1:length(vals)){
    mean_df[1,i] <- mean(values_raw[[vals[[i]]]],na.rm=TRUE)
  }
  
    
  #
  # Set Point
  #
  
  #Create empty character list as placeholder for column names in Minimum set points Data frame
  used_names_res <- character()
  
  #Fill empty character list with names form the variables + _low
  for(i in 1:length(vals)){
    #assign name for first element in vals
    if(i==1){
      used_names_res <- paste0(used_names_res,paste0(vals[[i]],"_low"))
    }
    #assign name for second and more element vals. Comma needed to separate values
    if(i>=2){
      used_names_res <- paste0(used_names_res,paste0(",",paste0(vals[[i]],"_low")))
    }
  }
  
  #create data.frame for low limits
  set_pt_lo <- read.csv(text=used_names_res)
  
  #Calculate lower limit by substracting the parametric sigma to the mean
  for(i in 1:length(vals)){
    set_pt_lo[1,i] <- mean_df[1,i]- (stdDev * std_dev_df[1,i])
  }
  
  #Create empty character list as placeholder for column names in Maximum set points Data frame
  used_names_res <- character()
  
  #Fill empty character list with names form the variables + _high
  for(i in 1:length(vals)){
    #assign name for first element in vals
    if(i==1){
      used_names_res <- paste0(used_names_res,paste0(vals[[i]],"_high"))
    }
    #assign name for second and more element vals. Comma needed to separate values
    if(i>=2){
      used_names_res <- paste0(used_names_res,paste0(",",paste0(vals[[i]],"_high")))
    }
  }
  
  #create data.frame for high limits
  set_pt_hi <- read.csv(text=used_names_res)

  #Calculate higher limit by adding the parametric sigma to the mean
  for(i in 1:length(vals)){
    set_pt_hi[1,i] <- mean_df[1,i]+ (stdDev * std_dev_df[1,i])
  }
  
  #
  # Probability Vector
  #
  
  #Create empty character list as placeholder for column names in Event Probabilities Data frame
  used_names_res <- character()
  
  #Fill empty character list with names form the variables + _prob
  for(i in 1:length(vals)){
    #assign name for first element in vals
    if(i==1){
      used_names_res <- paste0(used_names_res,paste0(vals[[i]],"_prob"))
    }
    #assign name for second and more element vals. Comma needed to separate values
    if(i>=2){
      used_names_res <- paste0(used_names_res,paste0(",",paste0(vals[[i]],"_prob")))
    }
  }
  
  #Create and initialize data.frame for probabilities
  pp <- read.csv(text=used_names_res)
  pp[1,] <- as.numeric(0)
  
  
  #number of precision steps away from either set pt. limit where P(event) decays to zero. Should be parametric
  nprec = 1
  
  
  # Initialize
  range = set_pt_hi - set_pt_lo;           # range between set points
  qtr_range = range / 3.0; 
  prec_dist = nprec * precision;
  
  #Determine if some elements were NA
  ispinf = is.finite.data.frame(set_pt_hi);
  isninf = is.finite.data.frame(set_pt_lo);
  
  #Calculate ranges for only existing values
  range[isninf] = abs(set_pt_hi[isninf]*2.0);
  range[ispinf] = abs(set_pt_lo[ispinf]*2.0);
  
  #Calculate half distance of the range
  half_range = range/2.0;
  #Determine center value of range
  ctr_line = half_range + set_pt_lo;
  #Set to cero non existing values
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
  
  #Rid the data frame with the raw data from the deselected columns
  for(i in 1:length(column_select)){
    if(i == 1){
      subset_values_raw <- subset(values_raw, select = -Time_Step)
      subset_values_raw_sub = subset(subset_values_raw, select=c(column_select[[i]]))
    }else{
      subset_values_raw_sub_temp= subset(subset_values_raw, select=c(column_select[[i]]))
      subset_values_raw_sub = cbind(subset_values_raw_sub, subset_values_raw_sub_temp)
    }
  }
  
  
  #Calculate probability of each coming value
  for(row in 1:nrow(subset_values_raw_sub)){

    # Calculate distance from set point normalized by precision
    ndist = half_range - abs((subset_values_raw_sub[row,] - ctr_line));  # raw distance from nearest set point
    ndist[ispinf] = (subset_values_raw_sub[row,ispinf] - set_pt_lo[ispinf]);
    ndist[isninf] = (set_pt_hi[isninf] - subset_values_raw_sub[row, isninf]);
    ndist = ndist / precision;                     # number of precision units in the raw distance
    ndist = ndist / nprec
    

    #add new timestamp to each row
    subset_used_timestamps = rbind(subset_used_timestamps,values_raw[row,1,drop = FALSE])
    
    # Call probability function with normalized distance
    if (prob_dist == "exponential"){
      
      #Call exponential
      pp[row,1] = values_raw[row,1]
      for(i in 1:length(ndist)){
        pp[row,i] = expon_prob(ndist[1,i]);
      }
    } else{
      #Call beta
      pp[row,i] = beta_prob(ndist[1,i]);
    }
    if(1 %in% pp[row,]){
      event_column[row,1] = 1
    }else{
      event_column[row,1] = 0
    }
  }

  results = cbind(subset_used_timestamps,pp, event_column)
  
  write.csv(results, file = "probEvent.csv")
  return(results)
}
