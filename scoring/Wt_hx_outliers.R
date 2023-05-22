
#selects record_id, height, current weight, lowest weight, highest weight, current bmi, highest bmi, lowest bmi, current weight suppression (from highest), and difference between lowest ever weight and current weight 


WtHx<- EDGI_exercise_cleaned|> 
  select(record_id, height, wt_cur_lb, wt_lo_lb, wt_hi_lb, currentbmi, highestbmi, lowestbmi, ED100k_wt_suppress_high_current, ED100k_wt_suppress_curr_low) 


## Identifies outlier values and filters dataset to include those which meet one of the following critueria
WtHx_outliers <- WtHx |> 
  filter(currentbmi > 70 | #current bmi > 70
           currentbmi < 10 |  # current bmi < 10
           ED100k_wt_suppress_curr_low > 250 |  # difference between current weight and lowest ever weight > 250 lb
           ED100k_wt_suppress_high_current > 250 # current weight suppression (difference between highest ever weight and current weight > 250 lb)
           ED100k_wt_suppress_high_current < 0 | # current weight reported to be higher than highest ever weight
           ED100k_wt_suppress_curr_low < 0) # lowest ever weight reported to be higher than current weight


write.csv(WtHx_outliers, file = 'data/EDGI_WtHx_outliers.csv') #Save data file -- 'weight history outliers' 


