

# Input -------------------------------------------------------------------
`Total population` <- 10518000
`Nr of simulation runs` <- 1000
`WTP per QALY averted` <- 20000

# Decision between base case or alternative case

retrieve <- "base case"

# Base case  --------------------------------------------------------------

## (Hier niet de percentages meegenomen, dit later wel doen)
# Prevention: sub-clinical depression

df_b_sub_clin <- data.frame(
  Intervention = "Online CBT (e-health)",
  cov = 0.00,
  adh = 0.56,
  `1-RR` = 0.21,
  n = 30,
  `healthcare costs` = 160,
  `societal costs` = 297,
  stringsAsFactors = FALSE,
  check.names = FALSE
) 

df_b_sub_clin_Covered <- sum(df_b_sub_clin$cov)
df_b_sub_clin_Effectively <- sum(df_b_sub_clin$cov * df_b_sub_clin$adh)

#Treatment: mild-depression

df_b_mild <- data.frame(
  Intervention = c("supplementary e-health", "individual CBT"),
  cov = c(0.02, 0.17),
  adh = c(0.43, 0.56),
  d = c(0.33, 0.51),
  n = c(30, 30),
  `healthcare costs` = c(265, 962),
  `societal costs` = c(441, 1491),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

df_b_mild_Covered <- sum(df_b_mild$cov)
df_b_mild_Effectively <- sum(df_b_mild$cov * df_b_mild$adh)
df_b_mild_Profilactic <- sum(df_b_mild[2, "cov"] * df_b_mild[2, "adh"]) * 0.25     #Dit later herschrijven zodat men meerdere rijen kan pakken (naar eigen invulling)

#Treatment: moderate depression

df_b_mod <- data.frame(
  Intervention = c("supplementary e-health", "individual CBT"),
  cov = c(0.02, 0.16),
  adh = c(0.43, 0.56),
  d = c(0.33, 0.51),
  n = c(30, 30),
  `healthcare costs` = c(248, 901),
  `societal costs` = c(441, 1491),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

df_b_mod_Covered <- sum(df_b_mod$cov)
df_b_mod_Effectively <- sum(df_b_mod$cov * df_b_mod$adh)
df_b_mod_Profilactic <- sum(df_b_mod[2, "cov"] * df_b_mod[2, "adh"]) * 0.25     #Dit later herschrijven zodat men meerdere rijen kan pakken (naar eigen invulling)


#Treatment: Severe depression

df_b_sev <- data.frame(
  Intervention = c("Individual psychotherapy (8-24 sessions)", "antidep med (12 mo)", "antidep med plus PHO"),
  cov = c(0.18, 0.20, 0.20),
  adh = c(0.68, 0.44, 0.56),
  d = c(0.51, 0.3, 0.51),
  n = c(30, 30, 30),
  `healthcare costs` = c(1984, 607, 679),
  `societal costs` = c(1652, 358, 415),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

df_b_sev_Covered <- sum(df_b_sev$cov)
df_b_sev_Effectively <- sum(df_b_sev$cov * df_b_sev$adh)
df_b_sev_Profilactic <- (df_b_sev[1, "cov"] * df_b_sev[1, "adh"] + df_b_sev[3, "cov"] * df_b_sev[3, "adh"]) * 0.25     #Dit later herschrijven zodat men meerdere rijen kan pakken (naar eigen invulling)


# Prevention: Recurrent depression

df_b_rec_dep <- data.frame(
  Intervention = c("Clinical management with maintenance medication", "Mindfulness-based CT", "Cognitive (behaviour) therapy"),
  cov = c(0.0, 0.0, 0.0),
  adh = c(0.42, 0.68, 0.56),
  `1-RR` = c(0.25, 0.34, 0.32),
  n = c(30, 30, 30),
  `healthcare costs` = c(1002, 361, 429),
  `societal costs` = c(680, 540, 537),
  stringsAsFactors = FALSE,
  check.names = FALSE) 
  
df_b_rec_dep_Covered <- sum(df_b_rec_dep$cov)
df_b_rec_dep_Effectively <- sum(df_b_rec_dep$cov * df_b_rec_dep$adh)













# Alternative case  --------------------------------------------------------------

## (Hier niet de percentages meegenomen, dit later wel doen)
# Prevention: sub-clinical depression

df_a_sub_clin <- data.frame(
  Intervention = "Online CBT (e-health)",
  cov = 0.02,
  adh = 0.56,
  `1-RR` = 0.21,
  n = 30,
  `healthcare costs` = 160,
  `societal costs` = 297,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

df_a_sub_clin_Covered <- sum(df_a_sub_clin$cov)
df_a_sub_clin_Effectively <- sum(df_a_sub_clin$cov * df_a_sub_clin$adh)


#Treatment: mild-depression

df_a_mild <- data.frame(
  Intervention = c("supplementary e-health", "individual CBT"),
  cov = c(0.02, 0.17),
  adh = c(0.43, 0.56),
  d = c(0.33, 0.51),
  n = c(30, 30),
  `healthcare costs` = c(265, 962),
  `societal costs` = c(441, 1491),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

df_a_mild_Covered <- sum(df_a_mild$cov)
df_a_mild_Effectively <- sum(df_a_mild$cov * df_a_mild$adh)
df_a_mild_Profilactic <- sum(df_a_mild[2, "cov"] * df_a_mild[2, "adh"]) * 0.25     #Dit later herschrijven zodat men meerdere rijen kan pakken (naar eigen invulling)




#Treatment: moderate depression

df_a_mod <- data.frame(
  Intervention = c("supplementary e-health", "individual CBT"),
  cov = c(0.02, 0.16),
  adh = c(0.43, 0.56),
  d = c(0.33, 0.51),
  n = c(30, 30),
  `healthcare costs` = c(248, 901),
  `societal costs` = c(441, 1491),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

df_a_mod_Covered <- sum(df_a_mod$cov)
df_a_mod_Effectively <- sum(df_a_mod$cov * df_a_mod$adh)
df_a_mod_Profilactic <- sum(df_a_mod[2, "cov"] * df_a_mod[2, "adh"]) * 0.25     #Dit later herschrijven zodat men meerdere rijen kan pakken (naar eigen invulling)




#Treatment: Severe depression

df_a_sev <- data.frame(
  Intervention = c("Individual psychotherapy (8-24 sessions)", "antidep med (12 mo)", "antidep med plus PHO"),
  cov = c(0.18, 0.20, 0.20),
  adh = c(0.68, 0.44, 0.56),
  d = c(0.51, 0.3, 0.51),
  n = c(30, 30, 30),
  `healthcare costs` = c(1984, 607, 679),
  `societal costs` = c(1652, 358, 415),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

df_a_sev_Covered <- sum(df_a_sev$cov)
df_a_sev_Effectively <- sum(df_a_sev$cov * df_a_sev$adh)
df_a_sev_Profilactic <- (df_a_sev[1, "cov"] * df_a_sev[1, "adh"] + df_a_sev[3, "cov"] * df_a_sev[3, "adh"]) * 0.25     #Dit later herschrijven zodat men meerdere rijen kan pakken (naar eigen invulling)


# Prevention: Recurrent depression

df_a_rec_dep <- data.frame(
  Intervention = c("Clinical management with maintenance medication", "Mindfulness-based CT", "Cognitive (behaviour) therapy"),
  cov = c(0.0, 0.0, 0.0),
  adh = c(0.42, 0.68, 0.56),
  `1-RR` = c(0.25, 0.34, 0.32),
  n = c(30, 30, 30),
  `healthcare costs` = c(1002, 361, 429),
  `societal costs` = c(680, 540, 537),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

df_a_rec_dep_Covered <- sum(df_a_rec_dep$cov)
df_a_rec_dep_Effectively <- sum(df_a_rec_dep$cov * df_a_rec_dep$adh)




# Store in a list and retrieve base case and alternative case

dataframes <- list(base_case = list(df_b_sub_clin, df_b_mild, df_b_mod, df_b_sev, df_b_rec_dep),
                   alternative = list(df_a_sub_clin, df_a_mild, df_a_mod, df_a_sev, df_a_rec_dep))




if(retrieve == "base case"){
  selected_df <- dataframes$base_case
  
  mildcuration <- df_b_mild_Profilactic
  moderatecuration <- df_b_mod_Profilactic
  severecuration <- df_b_sev_Profilactic
  
}else{
  selected_df <- dataframes$alternative
  
  mildcuration <- df_a_mild_Profilactic
  moderatecuration <- df_a_mod_Profilactic
  severecuration <- df_a_sev_Profilactic
  
  
}
