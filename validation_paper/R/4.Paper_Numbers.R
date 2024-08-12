load(RData_file) # should NOT need to change if you did scoring step correctly

EDGI_exercise_cleaned <- EDGI_exercise_cleaned |> 
  mutate(ex_dur_3m = case_when(ED100k_ex_dur > 2 ~ 1,
                             ED100k_ex_dur <= 2 ~ 0)) |> 
  mutate(ex_age_1 = if_else(ED100k_ex_compulsive_current2 == 2, age, ex_age_last))

Q1Broad.1 <- EDGI_exercise_cleaned |> 
  filter(ED100k_exercise_icb > 0)
Q1Narrow.2 <- EDGI_exercise_cleaned |> 
  filter(ED100k_exercise_icb == 2) 
Compulsive.3 <- EDGI_exercise_cleaned |> 
  filter(ED100k_ex3_compulsive_broad >0)

# Table for paper
Q1_frq <- sjmisc::frq(EDGI_exercise_cleaned$ED100k_ex1_Q1_broad)
Compulsive_frq <- sjmisc::frq(Q1Broad.1$ED100k_ex3_compulsive_broad)
AAO.3 <- psych::describe(EDGI_exercise_cleaned$ex_age)
Age_last.3 <- psych::describe(EDGI_exercise_cleaned$ex_age_last)
Compulsive_current.3 <- sjmisc::frq(Compulsive.3$ED100k_ex_compulsive_current2)
Compulsive_current_full <- sjmisc::frq(EDGI_exercise_cleaned$ED100k_ex_compulsive_current2)
Ex_dur.2 <- sjmisc::frq(Q1Narrow.2$ex_dur_3m)
Ex_dur_full <- sjmisc::frq(EDGI_exercise_cleaned$ex_dur_3m)


ct3<- table(EDGI_exercise_cleaned$ED100k_ex_compulsive_current2, EDGI_exercise_cleaned$cet_clinical)
ex_current_cet <- prop.table (ct3, 1)
CET_ED100k_raw_table <- as.data.frame(ex_current_cet) |>
  rename(`Current Driven Exercise [ED100k]` = Var1) |>
  rename(`CET Clinical Met` = Var2)   |>
  filter (`CET Clinical Met` == 1) %>%
  mutate(Freq = round(Freq*100,2))

CET_ED100k <- EDGI_exercise_cleaned |>
  select(c(ED100k_ex_compulsive_current2, cet_clinical, ED100k_exercise_icb)) |>
  mutate(ED100k_ex_compulsive_current2 = as.factor (ED100k_ex_compulsive_current2))
CET_ED100k$ED100k_ex_compulsive_current2 <- relevel(CET_ED100k$ED100k_ex_compulsive_current2, ref = "0")

CET_ED100k_multinomial_lr <- nnet::multinom(cet_clinical ~ ED100k_ex_compulsive_current2, data = CET_ED100k)
CET_ED100k_ORs <- exp(coef(CET_ED100k_multinomial_lr))
CET_ED100k_ORs <- as.data.frame(CET_ED100k_ORs) |>
  mutate(CET_ED100k_ORs = round(CET_ED100k_ORs,2))

dx_frqs <- as.data.frame(frq(EDGI_exercise_cleaned$case_status))


# Collect objects above into a list with each item named for itself
manuscript_info <- list(Q1_frq = Q1_frq, 
                      Compulsive_frq = Compulsive_frq, 
                      AAO.3 = AAO.3, 
                      Age_last.3 = Age_last.3, 
                      Compulsive_current.3 = Compulsive_current.3, 
                      Compulsive_current_full = Compulsive_current_full, 
                      Ex_dur.2 = Ex_dur.2, 
                      Ex_dur_full = Ex_dur_full, 
                      CET_ED100k_raw_table = CET_ED100k_raw_table, 
                      CET_ED100k_multinomial_lr = CET_ED100k_multinomial_lr,
                      CET_ED100k_ORs = CET_ED100k_ORs, 
                      dx_frqs = dx_frqs)

resave(manuscript_info, file = df_file)
