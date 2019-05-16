####################################################
## Structure of the offline mode of Canary
##
## Version 0.62
## Author Popolson Baggins
####################################################

#set working directory
setwd("~/Documents/07_canary/")

#clear Environment (maybe not best idea)
rm(list=ls())

#load csv : Save as data frame, handled like name array
loaded_data = read.csv("data/test_station_b.csv")

#define window size (SHOULD BE RECEIVED AS A PARAMETER)
window_size = 1990


#define treshold for outlier or not depending on sigma
treshold = 1.5

#define sigma (in this case one because SD of normalized data is 1)
sigma=1

#remove time (first column) of the loaded_data
subset_loaded_data = subset(loaded_data, select = -Time_Step)

#create empty data.frame for the normalization window
subset_used = subset_loaded_data[0,]

#create empty data.frame for time_stamps
#subset_used_timestamps = loaded_data[0,1]
subset_used_timestamps = loaded_data[0,1, drop = FALSE]
print(str(subset_used_timestamps))

#list of VALs available in the loaded_data
vals = list("B_CL2_VAL","B_TURB_VAL","B_PH_VAL","B_TOC_VAL","B_COND_VAL","B_TEMP_VAL","B_PLNT_PH_VAL","B_PLNT_TURB_VAL","B_PLNT_CL2_VAL")

#create data.frame for outlier detection saving
events = read.csv(text="Event") #Create emtpy data frame
for(row in 1:window_size){
  #fill column Event with NA until first calcuation (depending on the window size) substituting the logical default one
  events[row,]=NA
}

#create data.frame for residuals
calc_residuals = read.csv(text="B_CL2_VAL_residual,B_TURB_VAL_residual,B_PH_VAL_residual,B_TOC_VAL_residual,B_COND_VAL_residual,B_TEMP_VAL_residual,B_PLNT_PH_VAL_residual,B_PLNT_TURB_VAL_residual,B_PLNT_CL2_VAL_residual")
for(row in 1:window_size){
  #fill columns XXX_residual with NA until first calcuation (depending on the window size)
  calc_residuals[row,]= as.numeric(NA)
}

###### loop over all rows in the data.frame

#for(row in 1:nrow(subset_loaded_data)){
for(row in 1:2000){
  
  #add new row to subset_used
  subset_used = rbind(subset_used,subset_loaded_data[row,])
  
  #add new timestamp to each row
  subset_used_timestamps = rbind(subset_used_timestamps,loaded_data[row,1,drop = FALSE])
  
  #if windows is filled with enough observations start  
  if(nrow(subset_used)>=window_size){
    
    #create empty list for temp_residuals
    temp_residuals = numeric(length(vals))

    #take the last observations of subsed_used which fit the window_size
    defined_window = tail(subset_used,window_size)
    
    #normalize data (what happens with NA?); scale returns a matrix object, therefore the object is transformed back to a data.frame
    scaled_defined_window = as.data.frame(scale(defined_window))
    
    #remove last row
    scaled_defined_window_last_row_rm = head(scaled_defined_window, -1)
    #str(scaled_subset_a1_last_row_rm)
    
    #exclude the last row
    scaled_defined_window_last_row = tail(scaled_defined_window, 1)
    
    #calculations for the residual and event for all VAL (which are in vals[])
    for(i in 1:length(vals)){
      #assign name
      used_name = vals[[i]]
      # train linear model linear models formula has to be inserted as ONE text, therefore paste is used

      lmodel = lm(paste0(used_name, " ~ . "),data = scaled_defined_window_last_row_rm)
      pred_value = predict(lmodel, scaled_defined_window_last_row)
      #calculate residual between estimate and real value. unname() is used because return is a named num and abs() is used to get absolut value
      pred_residual = abs(unname(pred_value-scaled_defined_window_last_row[,used_name]))
      #assign pred_residual to the apppropriate column in the data.frame 
      calc_residuals[row,paste0(used_name, "_residual")] = pred_residual
      #assign the pred_residual to a temp variable which will be used to find the biggest residual
      temp_residuals[i] = pred_residual
      
      #remove unused variables to clean up workspace after each loop
      rm(pred_value)
      rm(pred_residual)
      rm(lmodel)
      rm(used_name)
    }
    #find biggest residual and compare it with treshold
    #sort numeric decreasing
    sorted_temp_residuals = sort(temp_residuals, decreasing = TRUE)
    #check if outlier or not
    #check for NA values because they break the if statement
    if(!is.na(sorted_temp_residuals[1])){
      #outlier
      if(sorted_temp_residuals[1]>treshold*sigma){
        #print("outlier")
        #add event detection to events data.frame
        events[row,"Event"]=TRUE
      }
      #backround
      if(sorted_temp_residuals[1]<=treshold*sigma){
        #print("background")
        #add event detection to events data.frame
        events[row,"Event"]=FALSE
      }
    }
    rm(temp_residuals)
    rm(sorted_temp_residuals)
  }
}

#combine all data.frames to one big for the results
# timestamps as VALs are untouched, residuals are calculated from the scaled values and events are evaluated as event TRUE or FALSE
results = cbind(subset_used_timestamps,subset_used,calc_residuals,events)

tail(results,25)