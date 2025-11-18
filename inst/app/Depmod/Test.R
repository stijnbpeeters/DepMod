
# Base case ---------------------------------------------------------------

# Prevention: Sub-clinical depressie
df_prev_sub <- data.frame(
  cov = 0.15,
  adh = 0.56,
  "1-RR" = 0.21,
  N = 30,
  "healthcare costs" = 160,
  check.names = FALSE
)




# Treatment: Mild depression
df_tr_mild <- data.frame(
  cov = c(0.02, 0.17),
  adh = c(0.43 , 0.56),
  d = c(0.33, 0.51),
  n = c(30, 30),
  "healthcare costs" = c(265, 962),
  check.names = FALSE
)

tr_mild_profilactic <- sum(df_tr_mild$cov * df_tr_mild$adh, na.rm = TRUE) * 0.25



# Treatment: Moderate depression
df_tr_mod <- data.frame(
  cov = c(0.02, 0.16),
  adh = c(0.43, 0.56),
  d = c(0.33, 0.51),
  n = c(30, 30),
  "healthcare costs" = c(248, 901),
  check.names = FALSE
)

tr_mod_profilactic <- sum(df_tr_mod$cov * df_tr_mod$adh, na.rm = TRUE) * 0.25
# Treatment: Severe depression
df_tr_sev <- data.frame(
  cov = c(0.18, 0.20, 0.20),
  adh = c(0.68, 0.44, 0.56),
  d = c(0.51, 0.3, 0.51),
  n = c(30, 30, 30),
  "healthcare costs" = c(1984, 607, 679),
  check.names = FALSE)

tr_sev_profilactic <- sum(df_tr_sev$cov * df_tr_sev$adh, na.rm = TRUE) * 0.25

# Prevention: Recurrent depression
df_prev_rec <- data.frame(
  cov = c(0.15, 0.15, 0.15),
  adh = c(0.42, 0.68, 0.56),
  '1-RR' = c(0.25, 0.34, 0.32),
  n = c(30, 30, 30),
  "healthcare costs" = c(1002, 361, 429),
  check.names = FALSE
)

sim_runs = 1000

# Other parameters
death_rate <- 0.00198371022638523
exces_mort <- 1.65
retirement_rate <- 0.02128

leavemodel <- (1 - death_rate *  1.65) * (1 - retirement_rate)

increased_relapse_1 <- 0.5
increased_relapse_2 <- 0.699999988079071
increased_relapse_3 <- 0.899999976158142
increased_relapse_4 <- 0.949999988079071
increased_relapse_5 <- 0.990000009536743


mean_dur_chron <- 3.96
dw_conversion_fact <- 0.172

total_population <- 10518000
incidence_no_history <- 0.0128446472713444
discount_rate_daly <- 0.0149999996647239
scale_shape_gamma_cost <- 6
disc_rate_cost <- 0.0399999991059303
