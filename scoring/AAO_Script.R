library(haven)
library(dplyr)
edgi_us <- haven::read_sas('data/exercise.sas7bdat')
an <- edgi_us |> 
  dplyr::filter(an_case == 1)

median(an$age_lowt)

median(edgi_us$vom_age, na.rm = TRUE)
median(edgi_us$fast_age, na.rm = TRUE)
median(edgi_us$be_agefirst, na.rm = TRUE)
mean(an$wt_lo_age)
