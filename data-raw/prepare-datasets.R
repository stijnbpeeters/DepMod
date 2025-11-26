
# Base case ---------------------------------------------------------------

# Prevention: Sub-clinical depression
data_prev_sub_base <- data.frame(
  cov = 0.15,
  adh = 0.56,
  "1-RR" = 0.21,
  N = 30,
  "healthcare costs" = 160,
  check.names = FALSE
)


# Treatment: Mild depression
data_tr_mild_base <- data.frame(
  cov = c(0.02, 0.17),
  adh = c(0.43 , 0.56),
  d = c(0.33, 0.51),
  n = c(30, 30),
  "healthcare costs" = c(265, 962),
  check.names = FALSE
)

# Treatment: Moderate depression
data_tr_mod_base <- data.frame(
  cov = c(0.02, 0.16),
  adh = c(0.43, 0.56),
  d = c(0.33, 0.51),
  n = c(30, 30),
  "healthcare costs" = c(248, 901),
  check.names = FALSE
)

# Treatment: Severe depression
data_tr_sev_base <- data.frame(
  cov = c(0.18, 0.20, 0.20),
  adh = c(0.68, 0.44, 0.56),
  d = c(0.51, 0.3, 0.51),
  n = c(30, 30, 30),
  "healthcare costs" = c(1984, 607, 679),
  check.names = FALSE)



# Prevention: Recurrent depression
data_prev_rec_base <- data.frame(
  cov = c(0.15, 0.15, 0.15),
  adh = c(0.42, 0.68, 0.56),
  '1-RR' = c(0.25, 0.34, 0.32),
  n = c(30, 30, 30),
  "healthcare costs" = c(1002, 361, 429),
  check.names = FALSE
)


# Alt case ----------------------------------------------------------------

# Prevention: Sub-clinical depression
data_prev_sub_alt <- data.frame(
  cov = 0.15,
  adh = 0.56,
  "1-RR" = 0.21,
  N = 30,
  "healthcare costs" = 160,
  check.names = FALSE
)
  
# Treatment: Mild depression
  data_tr_mild_alt <- data.frame(
    cov = c(0.02, 0.17),
    adh = c(0.43 , 0.56),
    d = c(0.33, 0.51),
    n = c(30, 30),
    "healthcare costs" = c(265, 962),
    check.names = FALSE
  )

# Treatment: Moderate depression
data_tr_mod_alt <- data.frame(
  cov = c(0.02, 0.16),
  adh = c(0.43, 0.56),
  d = c(0.33, 0.51),
  n = c(30, 30),
  "healthcare costs" = c(248, 901),
  check.names = FALSE
)

# Treatment: Severe depression
data_tr_sev_alt <- data.frame(
  cov = c(0.18, 0.20, 0.20),
  adh = c(0.68, 0.44, 0.56),
  d = c(0.51, 0.3, 0.51),
  n = c(30, 30, 30),
  "healthcare costs" = c(1984, 607, 679),
  check.names = FALSE)



# Prevention: Recurrent depression
data_prev_rec_alt <- data.frame(
  cov = c(0.15, 0.15, 0.15),
  adh = c(0.42, 0.68, 0.56),
  '1-RR' = c(0.25, 0.34, 0.32),
  n = c(30, 30, 30),
  "healthcare costs" = c(1002, 361, 429),
  check.names = FALSE
)

usethis::use_data(
  data_prev_sub_base,
  data_prev_sub_alt,
  data_tr_mild_base,
  data_tr_mild_alt,
  data_tr_mod_base,
  data_tr_mod_alt,
  data_tr_sev_base,
  data_tr_sev_alt,
  data_prev_rec_base,
  data_prev_rec_alt,
  overwrite = TRUE
)

