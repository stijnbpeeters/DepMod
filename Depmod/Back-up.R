
# Source datascripts ------------------------------------------------------

source("~/Boost/Depmod/Input.R")  #Input
source("~/Boost/Depmod/Parameters.R") #Parameters
source("~/Boost/Depmod/Transition tree.R") #Transition tree


# Calculations based on input parameters ----------------------------------

`Incidence` <- `Total population` * `Incidence no history`

`preventionfactor` <- 0
`moderatecuration` <- 0.0187500007450581
`severecuration` <- 0.025000000372529 
`relapsefactor` <- 0

`moderatecure` <- 1
`severecure` <-  1
`relapsecure` <-  1

`dwmild fixed` <- 0.14
`dwmoderate fixed` <- 0.35
`dwsevere fixed` <- 0.76

yldmild <- 0.25
yldmoderate <- 0.50
yldsevere <-  0.75

# Average DW improvement
mild <- sum(apply(selected_df[[2]][, c("cov", "adh", "d")], 1, prod)) * `dw conversion factor`
moderate <- sum(apply(selected_df[[3]][, c("cov", "adh", "d")], 1, prod)) * `dw conversion factor`
severe <- sum(apply(selected_df[[4]][, c("cov", "adh", "d")], 1, prod)) * `dw conversion factor`

prob_mild <-  0.5
prob_moderate <-  0.5
prob_severe <-  0.5


# Improvement DW per 
#1-RR including profilactic (coefficients after intervention) --> base case
preventionfactor_base <- sum(apply(selected_df[[1]][, c("cov", "adh", "1-RR")], 1, prod))
mildcuration_int_base <- qnorm(0.5,0.25,0.025) * df_a_mild_Profilactic/0.25
modcuration_int_base <- qnorm(0.5,0.25,0.025) * df_a_mod_Profilactic/0.25
sevcuration_int_base <- qnorm(0.5,0.25,0.025) * df_a_sev_Profilactic/0.25
relapsefactor_base <- sum(apply(selected_df[[5]][, c("cov", "adh", "1-RR")], 1, prod))


#1-RR including profilactic (coefficients after intervention) --> alternative case
preventionfactor_alt <- sum(apply(selected_df[[1]][, c("cov", "adh", "1-RR")], 1, prod))
mildcuration_int_alt <- qnorm(0.5,0.25,0.025) * df_a_mild_Profilactic/0.25
modcuration_int_alt <- qnorm(0.5,0.25,0.025) * df_a_mod_Profilactic/0.25
sevcuration_int_alt <- qnorm(0.5,0.25,0.025) * df_a_sev_Profilactic/0.25
relapsefactor_alt <- sum(apply(selected_df[[5]][, c("cov", "adh", "1-RR")], 1, prod))


`employment rate` <- 0.95
`fte factor` <-  0.80

#Relapse division 
`the next year` <- 0.40
`after two years` <- 0.30
`after three years` <- 0.20
`after four years` <- 0.07
`after five years`  <- 0.03


# Calibration

`Fifth episode to chronic` <- 0.3523

#DW reduction per year


# Transition matrix -------------------------------------------------------
# State names

base_states <- c("incidence", 
                 "cured", 
                 "1x_1_year", "1x_2_year", "1x_3_year", "1x_4_year", "1x_5_year", 
                 "2x_1_year", "2x_2_year", "2x_3_year", "2x_4_year", "2x_5_year", 
                 "3x_1_year", "3x_2_year", "3x_3_year", "3x_4_year", "3x_5_year",
                 "4x_1_year", "4x_2_year", "4x_3_year", "4x_4_year", "4x_5_year",
                 "5x_1_year", "5x_2_year", "5x_3_year", "5x_4_year", "5x_5_year",
                 "chronic",
                 "mortality")

severities <-  c("mild", "moderate", "severe")

states <- c(outer(severities, base_states, paste, sep = "_"))

# Build matrix
transition_matrix <- array(0, 
                           dim = c(length(states), length(states), 47),
                           dimnames = list(from = states,
                                           to = states,
                                           year = 0:46))

# Apply probabilities to the transition matrix ----------------------------

## Transitions mild incidence
transition_matrix["mild_incidence", "mild_incidence",] <- 1
transition_matrix["mild_incidence", "mild_cured",] <- leavemodel/pmild * ((pmild * mildrecovery * (1-mildrecoveryrelapse*relapsecure) + pmild * mildpartial * (1-mildpartialrelapse * relapsecure)))
transition_matrix["mild_incidence", "mild_1x_1_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.4
transition_matrix["mild_incidence", "mild_1x_2_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.3
transition_matrix["mild_incidence", "mild_1x_3_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.2
transition_matrix["mild_incidence", "mild_1x_4_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.07
transition_matrix["mild_incidence", "mild_1x_5_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.03
transition_matrix["mild_incidence", "mild_chronic", ] <- leavemodel/pmild * ((pmild * mildchronic))
transition_matrix["mild_incidence", "mild_mortality", ] <- 1- leavemodel

## Transitions moderate incidence
transition_matrix["moderate_incidence", "moderate_incidence", ] <- 1
transition_matrix["moderate_incidence", "moderate_cured", ] <- leavemodel/pmoderate * ((pmoderate * moderaterecovery * (1-moderaterecoveryrelapse*relapsecure*moderatecure) + pmoderate*moderatepartial * (1-moderatepartialrelapse*relapsecure*moderatecure)))
transition_matrix["moderate_incidence", "moderate_1x_1_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse * moderatecure + pmoderate * moderatepartial * moderatepartialrelapse * moderatecure)) * 0.4
transition_matrix["moderate_incidence", "moderate_1x_2_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse * moderatecure + pmoderate * moderatepartial * moderatepartialrelapse * moderatecure)) * 0.3
transition_matrix["moderate_incidence", "moderate_1x_3_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse * moderatecure + pmoderate * moderatepartial * moderatepartialrelapse * moderatecure)) * 0.2
transition_matrix["moderate_incidence", "moderate_1x_4_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse * moderatecure + pmoderate * moderatepartial * moderatepartialrelapse * moderatecure)) * 0.07
transition_matrix["moderate_incidence", "moderate_1x_5_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse * moderatecure + pmoderate * moderatepartial * moderatepartialrelapse * moderatecure)) * 0.03
transition_matrix["moderate_incidence", "moderate_chronic", ] <- leavemodel/pmoderate * ((pmoderate * moderatechronic))
transition_matrix["moderate_incidence", "moderate_mortality", ] <- 1-leavemodel

## Transitions severe incidence
transition_matrix["severe_incidence", "severe_incidence", ] <- 1
transition_matrix["severe_incidence", "severe_cured", ] <- leavemodel/psevere*((psevere * severerecovery * (1-severerecoveryrelapse * relapsecure * severecure) + psevere * severepartial * (1-severepartialrelapse*relapsecure*severecure)))
transition_matrix["severe_incidence", "severe_1x_1_year", ] <- leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse * severecure + psevere * severepartial * severepartialrelapse * severecure)) * 0.4
transition_matrix["severe_incidence", "severe_1x_2_year", ] <- leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse * severecure + psevere * severepartial * severepartialrelapse * severecure)) * 0.3
transition_matrix["severe_incidence", "severe_1x_3_year", ] <- leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse * severecure + psevere * severepartial * severepartialrelapse * severecure)) * 0.2
transition_matrix["severe_incidence", "severe_1x_4_year", ] <- leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse * severecure + psevere * severepartial * severepartialrelapse * severecure)) * 0.07
transition_matrix["severe_incidence", "severe_1x_5_year", ] <- leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse * severecure + psevere * severepartial * severepartialrelapse * severecure)) * 0.03
transition_matrix["severe_incidence", "severe_chronic", ] <- leavemodel/psevere * ((psevere * severechronic))
transition_matrix["severe_incidence", "severe_mortality", ] <- 1 - leavemodel

## Transitions cured 
transition_matrix["mild_cured", "mild_cured", ] <- leavemodel
transition_matrix["mild_cured", "mild_mortality", ] <- 1- leavemodel
transition_matrix["moderate_cured", "moderate_cured", ] <- leavemodel
transition_matrix["moderate_cured", "moderate_mortality", ] <- 1- leavemodel
transition_matrix["severe_cured", "severe_cured", ] <- leavemodel
transition_matrix["severe_cured", "severe_mortality", ] <- 1- leavemodel

## Transitions 1x after 1 year - mild 

transition_matrix["mild_1x_1_year", "mild_cured", ] <- leavemodel/pmild * ((pmild * mildrecovery * (1-(1-(mildrecoverycured*(1-`increased relapse 1`))) * relapsecure) + pmild * mildpartial * (1-(1-(mildpartialcured * (1- `increased relapse 1`)))* relapsecure)))
transition_matrix["mild_1x_1_year", "mild_2x_1_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.4
transition_matrix["mild_1x_1_year", "mild_2x_2_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.3
transition_matrix["mild_1x_1_year", "mild_2x_3_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.2
transition_matrix["mild_1x_1_year", "mild_2x_4_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.07
transition_matrix["mild_1x_1_year", "mild_2x_5_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.03
transition_matrix["mild_1x_1_year", "mild_chronic", ] <- leavemodel/pmild * ((pmild*mildchronic))
transition_matrix["mild_1x_1_year", "mild_mortality", ] <- 1- leavemodel

## Transitions 1x after 1 year - moderate
transition_matrix["moderate_1x_1_year", "moderate_cured", ] <- leavemodel/pmoderate * ((pmoderate * moderaterecovery * (1-(1-(moderaterecoverycured * (1 - `increased relapse 1`))) * relapsecure * moderatecure) + pmoderate * moderatepartial * (1-(1-(moderatepartialcured*(1 - `increased relapse 1`))) * relapsecure * moderatecure)))
transition_matrix["moderate_1x_1_year", "moderate_2x_1_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.4
transition_matrix["moderate_1x_1_year", "moderate_2x_2_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.3
transition_matrix["moderate_1x_1_year", "moderate_2x_3_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.2
transition_matrix["moderate_1x_1_year", "moderate_2x_4_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.07
transition_matrix["moderate_1x_1_year", "moderate_2x_5_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.03
transition_matrix["moderate_1x_1_year", "moderate_chronic", ] <-  leavemodel/pmoderate * ((pmoderate * moderatechronic))
transition_matrix["moderate_1x_1_year", "moderate_mortality", ] <-  1 - leavemodel


## Transitions 1x after 1 year - severe
transition_matrix["severe_1x_1_year", "severe_cured", ] <- leavemodel / psevere * ((psevere * severerecovery * (1-(1-(severerecoverycured * (1- `increased relapse 1`))) * relapsecure * severecure) + psevere * severepartial * (1-(1-(severepartialcured * (1 - `increased relapse 1`))) * relapsecure * severecure)))
transition_matrix["severe_1x_1_year", "severe_2x_1_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 1`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 1`))) * severecure)) * 0.4
transition_matrix["severe_1x_1_year", "severe_2x_2_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 1`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 1`))) * severecure)) * 0.3
transition_matrix["severe_1x_1_year", "severe_2x_3_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 1`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 1`))) * severecure)) * 0.2
transition_matrix["severe_1x_1_year", "severe_2x_4_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 1`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 1`))) * severecure)) * 0.07
transition_matrix["severe_1x_1_year", "severe_2x_5_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 1`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 1`))) * severecure)) * 0.03
transition_matrix["severe_1x_1_year", "severe_chronic", ] <- leavemodel / psevere * ((psevere * severechronic))
transition_matrix["severe_1x_1_year", "severe_mortality", ] <-  1 - leavemodel


## Transitions 1x after 2 years - mild
transition_matrix["mild_1x_2_year", "mild_1x_1_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_1x_2_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 1x after 2 years - moderate
transition_matrix["moderate_1x_2_year", "moderate_1x_1_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_1x_2_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 1x after 2 years - severe
transition_matrix["severe_1x_2_year", "severe_1x_1_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_1x_2_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)



## Transitions 1x after 3 years - mild
transition_matrix["mild_1x_3_year", "mild_1x_2_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_1x_3_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 1x after 3 years - moderate
transition_matrix["moderate_1x_3_year", "moderate_1x_2_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_1x_3_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 1x after 3 years - severe
transition_matrix["severe_1x_3_year", "severe_1x_2_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_1x_3_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)



## Transitions 1x after 4 years - mild
transition_matrix["mild_1x_4_year", "mild_1x_3_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_1x_4_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 1x after 4 years - moderate
transition_matrix["moderate_1x_4_year", "moderate_1x_3_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_1x_4_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 1x after 4 years - severe
transition_matrix["severe_1x_4_year", "severe_1x_3_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_1x_4_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)



## Transitions 1x after 5 years - mild
transition_matrix["mild_1x_5_year", "mild_1x_4_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_1x_5_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 1x after 5 years - moderate
transition_matrix["moderate_1x_5_year", "moderate_1x_4_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_1x_5_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 1x after 5 years - severe
transition_matrix["severe_1x_5_year", "severe_1x_4_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_1x_5_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)




## Transitions 2x after 1 year - mild
transition_matrix["mild_2x_1_year", "mild_cured", ] <- leavemodel/pmild * ((pmild * mildrecovery * (1-(1-(mildrecoverycured*(1-`increased relapse 2`))) * relapsecure) + pmild * mildpartial * (1-(1-(mildpartialcured * (1- `increased relapse 2`)))* relapsecure)))
transition_matrix["mild_2x_1_year", "mild_3x_1_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.4
transition_matrix["mild_2x_1_year", "mild_3x_2_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.3
transition_matrix["mild_2x_1_year", "mild_3x_3_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.2
transition_matrix["mild_2x_1_year", "mild_3x_4_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.07
transition_matrix["mild_2x_1_year", "mild_3x_5_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.03
transition_matrix["mild_2x_1_year", "mild_chronic", ] <- leavemodel/pmild * ((pmild*mildchronic))
transition_matrix["mild_2x_1_year", "mild_mortality", ] <- 1- leavemodel

## Transitions 2x after 1 year - moderate
transition_matrix["moderate_2x_1_year", "moderate_cured", ] <- leavemodel/pmoderate * ((pmoderate * moderaterecovery * (1-(1-(moderaterecoverycured * (1 - `increased relapse 2`))) * relapsecure * moderatecure) + pmoderate * moderatepartial * (1-(1-(moderatepartialcured*(1 - `increased relapse 2`))) * relapsecure * moderatecure)))
transition_matrix["moderate_2x_1_year", "moderate_3x_1_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.4
transition_matrix["moderate_2x_1_year", "moderate_3x_2_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.3
transition_matrix["moderate_2x_1_year", "moderate_3x_3_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.2
transition_matrix["moderate_2x_1_year", "moderate_3x_4_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.07
transition_matrix["moderate_2x_1_year", "moderate_3x_5_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.03
transition_matrix["moderate_2x_1_year", "moderate_chronic", ] <-  leavemodel/pmoderate * ((pmoderate * moderatechronic))
transition_matrix["moderate_2x_1_year", "moderate_mortality", ] <-  1 - leavemodel

## Transitions 2x after 1 year - severe 
transition_matrix["severe_2x_1_year", "severe_cured", ] <- leavemodel / psevere * ((psevere * severerecovery * (1-(1-(severerecoverycured * (1- `increased relapse 2`))) * relapsecure * severecure) + psevere * severepartial * (1-(1-(severepartialcured * (1 - `increased relapse 2`))) * relapsecure * severecure)))
transition_matrix["severe_2x_1_year", "severe_3x_1_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 2`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 2`))) * severecure)) * 0.4
transition_matrix["severe_2x_1_year", "severe_3x_2_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 2`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 2`))) * severecure)) * 0.3
transition_matrix["severe_2x_1_year", "severe_3x_3_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 2`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 2`))) * severecure)) * 0.2
transition_matrix["severe_2x_1_year", "severe_3x_4_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 2`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 2`))) * severecure)) * 0.07
transition_matrix["severe_2x_1_year", "severe_3x_5_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 2`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 2`))) * severecure)) * 0.03
transition_matrix["severe_2x_1_year", "severe_chronic", ] <- leavemodel / psevere * ((psevere * severechronic))
transition_matrix["severe_2x_1_year", "severe_mortality", ] <-  1 - leavemodel



## Transitions 2x after 2 years - mild
transition_matrix["mild_2x_2_year", "mild_2x_1_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_2x_2_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 2x after 2 years - moderate
transition_matrix["moderate_2x_2_year", "moderate_2x_1_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_2x_2_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 2x after 2 years - severe
transition_matrix["severe_2x_2_year", "severe_2x_1_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_2x_2_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 2x after 3 years - mild
transition_matrix["mild_2x_3_year", "mild_2x_2_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_2x_3_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 2x after 3 years - moderate
transition_matrix["moderate_2x_3_year", "moderate_2x_2_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_2x_3_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 1x after 3 years - severe
transition_matrix["severe_2x_3_year", "severe_2x_2_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_2x_3_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)


## Transitions 2x after 4 years - mild
transition_matrix["mild_2x_4_year", "mild_2x_3_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_2x_4_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 2x after 4 years - moderate
transition_matrix["moderate_2x_4_year", "moderate_2x_3_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_2x_4_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 2x after 4 years - severe
transition_matrix["severe_2x_4_year", "severe_2x_3_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_2x_4_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 2x after 5 years - mild
transition_matrix["mild_2x_5_year", "mild_2x_4_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_2x_5_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 2x after 5 years - moderate
transition_matrix["moderate_2x_5_year", "moderate_2x_4_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_2x_5_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 2x after 5 years - severe
transition_matrix["severe_2x_5_year", "severe_2x_4_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_2x_5_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)



## Transitions 3x after 1 year - mild
transition_matrix["mild_3x_1_year", "mild_cured", ] <- leavemodel/pmild * ((pmild * mildrecovery * (1-(1-(mildrecoverycured*(1-`increased relapse 3`))) * relapsecure) + pmild * mildpartial * (1-(1-(mildpartialcured * (1- `increased relapse 3`)))* relapsecure)))
transition_matrix["mild_3x_1_year", "mild_4x_1_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.4
transition_matrix["mild_3x_1_year", "mild_4x_2_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.3
transition_matrix["mild_3x_1_year", "mild_4x_3_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.2
transition_matrix["mild_3x_1_year", "mild_4x_4_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.07
transition_matrix["mild_3x_1_year", "mild_4x_5_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.03
transition_matrix["mild_3x_1_year", "mild_chronic", ] <- leavemodel/pmild * ((pmild*mildchronic))
transition_matrix["mild_3x_1_year", "mild_mortality", ] <- 1- leavemodel

## Transitions 3x after 1 year - moderate
transition_matrix["moderate_3x_1_year", "moderate_cured", ] <- leavemodel/pmoderate * ((pmoderate * moderaterecovery * (1-(1-(moderaterecoverycured * (1 - `increased relapse 3`))) * relapsecure * moderatecure) + pmoderate * moderatepartial * (1-(1-(moderatepartialcured*(1 - `increased relapse 3`))) * relapsecure * moderatecure)))
transition_matrix["moderate_3x_1_year", "moderate_4x_1_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.4
transition_matrix["moderate_3x_1_year", "moderate_4x_2_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.3
transition_matrix["moderate_3x_1_year", "moderate_4x_3_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.2
transition_matrix["moderate_3x_1_year", "moderate_4x_4_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.07
transition_matrix["moderate_3x_1_year", "moderate_4x_5_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.03
transition_matrix["moderate_3x_1_year", "moderate_chronic", ] <-  leavemodel/pmoderate * ((pmoderate * moderatechronic))
transition_matrix["moderate_3x_1_year", "moderate_mortality", ] <-  1 - leavemodel

## Transitions 3x after 1 year - severe 
transition_matrix["severe_3x_1_year", "severe_cured", ] <- leavemodel / psevere * ((psevere * severerecovery * (1-(1-(severerecoverycured * (1- `increased relapse 3`))) * relapsecure * severecure) + psevere * severepartial * (1-(1-(severepartialcured * (1 - `increased relapse 3`))) * relapsecure * severecure)))
transition_matrix["severe_3x_1_year", "severe_4x_1_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 3`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 3`))) * severecure)) * 0.4
transition_matrix["severe_3x_1_year", "severe_4x_2_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 3`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 3`))) * severecure)) * 0.3
transition_matrix["severe_3x_1_year", "severe_4x_3_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 3`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 3`))) * severecure)) * 0.2
transition_matrix["severe_3x_1_year", "severe_4x_4_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 3`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 3`))) * severecure)) * 0.07
transition_matrix["severe_3x_1_year", "severe_4x_5_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 3`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 3`))) * severecure)) * 0.03
transition_matrix["severe_3x_1_year", "severe_chronic", ] <- leavemodel / psevere * ((psevere * severechronic))
transition_matrix["severe_3x_1_year", "severe_mortality", ] <-  1 - leavemodel


## Transitions 3x after 2 years - mild
transition_matrix["mild_3x_2_year", "mild_3x_1_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_3x_2_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 3x after 2 years - moderate
transition_matrix["moderate_3x_2_year", "moderate_3x_1_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_3x_2_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 3x after 2 years - severe
transition_matrix["severe_3x_2_year", "severe_3x_1_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_3x_2_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 3x after 3 years - mild
transition_matrix["mild_3x_3_year", "mild_3x_2_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_3x_3_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 3x after 3 years - moderate
transition_matrix["moderate_3x_3_year", "moderate_3x_2_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_3x_3_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 3x after 3 years - severe
transition_matrix["severe_3x_3_year", "severe_3x_2_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_3x_3_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)


## Transitions 3x after 4 years - mild
transition_matrix["mild_3x_4_year", "mild_3x_3_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_3x_4_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 3x after 4 years - moderate
transition_matrix["moderate_3x_4_year", "moderate_3x_3_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_3x_4_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 3x after 4 years - severe
transition_matrix["severe_3x_4_year", "severe_3x_3_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_3x_4_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 3x after 5 years - mild
transition_matrix["mild_3x_5_year", "mild_3x_4_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_3x_5_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 3x after 5 years - moderate
transition_matrix["moderate_3x_5_year", "moderate_3x_4_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_3x_5_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 3x after 5 years - severe
transition_matrix["severe_3x_5_year", "severe_3x_4_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_3x_5_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)



## Transitions 4x after 1 year - mild
transition_matrix["mild_4x_1_year", "mild_cured", ] <- leavemodel/pmild * ((pmild * mildrecovery * (1-(1-(mildrecoverycured*(1-`increased relapse 4`))) * relapsecure) + pmild * mildpartial * (1-(1-(mildpartialcured * (1- `increased relapse 4`)))* relapsecure)))
transition_matrix["mild_4x_1_year", "mild_5x_1_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.4
transition_matrix["mild_4x_1_year", "mild_5x_2_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.3
transition_matrix["mild_4x_1_year", "mild_5x_3_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.2
transition_matrix["mild_4x_1_year", "mild_5x_4_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.07
transition_matrix["mild_4x_1_year", "mild_5x_5_year", ] <- leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.03
transition_matrix["mild_4x_1_year", "mild_chronic", ] <- leavemodel/pmild * ((pmild*mildchronic))
transition_matrix["mild_4x_1_year", "mild_mortality", ] <- 1- leavemodel

## Transitions 4x after 1 year - moderate
transition_matrix["moderate_4x_1_year", "moderate_cured", ] <- leavemodel/pmoderate * ((pmoderate * moderaterecovery * (1-(1-(moderaterecoverycured * (1 - `increased relapse 4`))) * relapsecure * moderatecure) + pmoderate * moderatepartial * (1-(1-(moderatepartialcured*(1 - `increased relapse 4`))) * relapsecure * moderatecure)))
transition_matrix["moderate_4x_1_year", "moderate_5x_1_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.4
transition_matrix["moderate_4x_1_year", "moderate_5x_2_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.3
transition_matrix["moderate_4x_1_year", "moderate_5x_3_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.2
transition_matrix["moderate_4x_1_year", "moderate_5x_4_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.07
transition_matrix["moderate_4x_1_year", "moderate_5x_5_year", ] <-  leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.03
transition_matrix["moderate_4x_1_year", "moderate_chronic", ] <-  leavemodel/pmoderate * ((pmoderate * moderatechronic))
transition_matrix["moderate_4x_1_year", "moderate_mortality", ] <-  1 - leavemodel

## Transitions 4x after 1 year - severe 
transition_matrix["severe_4x_1_year", "severe_cured", ] <- leavemodel / psevere * ((psevere * severerecovery * (1-(1-(severerecoverycured * (1- `increased relapse 4`))) * relapsecure * severecure) + psevere * severepartial * (1-(1-(severepartialcured * (1 - `increased relapse 4`))) * relapsecure * severecure)))
transition_matrix["severe_4x_1_year", "severe_5x_1_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 4`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 4`))) * severecure)) * 0.4
transition_matrix["severe_4x_1_year", "severe_5x_2_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 4`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 4`))) * severecure)) * 0.3
transition_matrix["severe_4x_1_year", "severe_5x_3_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 4`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 4`))) * severecure)) * 0.2
transition_matrix["severe_4x_1_year", "severe_5x_4_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 4`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 4`))) * severecure)) * 0.07
transition_matrix["severe_4x_1_year", "severe_5x_5_year", ] <- leavemodel / psevere * ((relapsecure * (psevere * severerecovery * (1-(severerecoverycured * (1- `increased relapse 4`))) * severecure + psevere * severepartial * (1- severepartialcured * (1- `increased relapse 4`))) * severecure)) * 0.03
transition_matrix["severe_4x_1_year", "severe_chronic", ] <- leavemodel / psevere * ((psevere * severechronic))
transition_matrix["severe_4x_1_year", "severe_mortality", ] <-  1 - leavemodel



## Transitions 4x after 2 years - mild
transition_matrix["mild_4x_2_year", "mild_4x_1_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_4x_2_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 4x after 2 years - moderate
transition_matrix["moderate_4x_2_year", "moderate_4x_1_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_4x_2_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 4x after 2 years - severe
transition_matrix["severe_4x_2_year", "severe_4x_1_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_4x_2_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 4x after 3 years - mild
transition_matrix["mild_4x_3_year", "mild_4x_2_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_4x_3_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 4x after 3 years - moderate
transition_matrix["moderate_4x_3_year", "moderate_4x_2_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_4x_3_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 4x after 3 years - severe
transition_matrix["severe_4x_3_year", "severe_4x_2_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_4x_3_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)


## Transitions 4x after 4 years - mild
transition_matrix["mild_4x_4_year", "mild_4x_3_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_4x_4_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 4x after 4 years - moderate
transition_matrix["moderate_4x_4_year", "moderate_4x_3_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_4x_4_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 4x after 4 years - severe
transition_matrix["severe_4x_4_year", "severe_4x_3_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_4x_4_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 4x after 5 years - mild
transition_matrix["mild_4x_5_year", "mild_4x_4_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_4x_5_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 4x after 5 years - moderate
transition_matrix["moderate_4x_5_year", "moderate_4x_4_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_4x_5_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 4x after 5 years - severe
transition_matrix["severe_4x_5_year", "severe_4x_4_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_4x_5_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)



## Transitions 5x after 1 year - mild
transition_matrix["mild_5x_1_year", "mild_cured", ] <-  leavemodel/pmild * ((pmild * mildrecovery * (1-(1-(mildrecoverycured*(1-`increased relapse 5`))) * relapsecure) + pmild * mildpartial * (1-(1-(mildpartialcured * (1- `increased relapse 5`)))* relapsecure))) + `Fifth episode to chronic`
transition_matrix["mild_5x_1_year", "mild_mortality", ] <- 1-leavemodel
transition_matrix["mild_5x_1_year", "mild_chronic", ] <-  1 - (1-leavemodel) - (leavemodel/pmild * ((pmild * mildrecovery * (1-(1-(mildrecoverycured*(1-`increased relapse 5`))) * relapsecure) + pmild * mildpartial * (1-(1-(mildpartialcured * (1- `increased relapse 5`)))* relapsecure))) + `Fifth episode to chronic`)

## Transitions 5x after 1 year - moderate
transition_matrix["moderate_5x_1_year", "moderate_cured", ] <- leavemodel/pmoderate * ((pmoderate * moderaterecovery * (1-(1-(moderaterecoverycured * (1 - `increased relapse 5`))) * relapsecure * moderatecure) + pmoderate * moderatepartial * (1-(1-(moderatepartialcured*(1 - `increased relapse 5`))) * relapsecure * moderatecure))) + `Fifth episode to chronic`
transition_matrix["moderate_5x_1_year", "moderate_mortality", ] <- 1-leavemodel
transition_matrix["moderate_5x_1_year", "moderate_chronic", ] <- 1- (1-leavemodel) - (leavemodel/pmoderate * ((pmoderate * moderaterecovery * (1-(1-(moderaterecoverycured * (1 - `increased relapse 5`))) * relapsecure * moderatecure) + pmoderate * moderatepartial * (1-(1-(moderatepartialcured*(1 - `increased relapse 5`))) * relapsecure * moderatecure))) + `Fifth episode to chronic`)

## Transitions 5x after 1 year - severe
transition_matrix["severe_5x_1_year", "severe_cured", ] <- `Fifth episode to chronic`
transition_matrix["severe_5x_1_year", "severe_mortality", ] <- 1-leavemodel
transition_matrix["severe_5x_1_year", "severe_chronic", ] <- 1- (1-leavemodel) - (`Fifth episode to chronic`)

## Transitions 5x after 2 years - mild
transition_matrix["mild_5x_2_year", "mild_5x_1_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_5x_2_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 5x after 2 years - moderate
transition_matrix["moderate_5x_2_year", "moderate_5x_1_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_5x_2_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 5x after 2 years - severe
transition_matrix["severe_5x_2_year", "severe_5x_1_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_5x_2_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 5x after 3 years - mild
transition_matrix["mild_5x_3_year", "mild_5x_2_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_5x_3_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 5x after 3 years - moderate
transition_matrix["moderate_5x_3_year", "moderate_5x_2_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_5x_3_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 5x after 3 years - severe
transition_matrix["severe_5x_3_year", "severe_5x_2_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_5x_3_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 5x after 4 years - mild
transition_matrix["mild_5x_4_year", "mild_5x_3_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_5x_4_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 5x after 4 years - moderate
transition_matrix["moderate_5x_4_year", "moderate_5x_3_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_5x_4_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 5x after 4 years - severe
transition_matrix["severe_5x_4_year", "severe_5x_3_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_5x_4_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 5x after 5 years - mild
transition_matrix["mild_5x_5_year", "mild_5x_4_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["mild_5x_5_year", "mild_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 5x after 5 years - moderate
transition_matrix["moderate_5x_5_year", "moderate_5x_4_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["moderate_5x_5_year", "moderate_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)

## Transitions 5x after 5 years - severe
transition_matrix["severe_5x_5_year", "severe_5x_4_year", ] <- (1- `retirement rate`) * (1- `death rate`)
transition_matrix["severe_5x_5_year", "severe_mortality", ] <-  1 - (1- `retirement rate`) * (1- `death rate`)


## Transitions chronic - mild
transition_matrix["mild_chronic", "mild_cured", ] <- (1-2^(-1/(`mean duration of chronicity (year)` * log(2)))) *  (leavemodel)
transition_matrix["mild_chronic", "mild_chronic", ] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
transition_matrix["mild_chronic", "mild_mortality", ] <- 1 - leavemodel

## Transitions chronic - moderate
transition_matrix["moderate_chronic", "moderate_cured", ] <- (1-2^(-1/(`mean duration of chronicity (year)` * log(2)))) *  (leavemodel)
transition_matrix["moderate_chronic", "moderate_chronic", ] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
transition_matrix["moderate_chronic", "moderate_mortality", ] <- 1 - leavemodel

## Transitions chronic - severe
transition_matrix["severe_chronic", "severe_cured", ] <- (1-2^(-1/(`mean duration of chronicity (year)` * log(2)))) *  (leavemodel)
transition_matrix["severe_chronic", "severe_chronic", ] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
transition_matrix["severe_chronic", "severe_mortality", ] <- 1 - leavemodel


## Transitions mortality 
transition_matrix["mild_mortality", "mild_mortality", ] <- 1
transition_matrix["moderate_mortality", "moderate_mortality", ] <- 1
transition_matrix["severe_mortality", "severe_mortality", ] <- 1



func_sim_tm <- function(transition_matrix,retrieve){
  
  if(retrieve == "base_case"){
    prevention_factor <- preventionfactor_base
    mildcuration_int <-  mildcuration_int_base
    modcuration_int <- modcuration_int_base
    sevcuration_int <- sevcuration_int_base
    relapsefactor <- relapsefactor_base
  }else{
    prevention_factor <- preventionfactor_alt
    mildcuration_int <-  mildcuration_int_alt
    modcuration_int <- modcuration_int_alt
    sevcuration_int <- sevcuration_int_alt
    relapsefactor <- relapsefactor_alt
  }
  
  
  # Transitions change because of influence of intervention 
  
  ## Year 42 (year 1) probabilities
  prevention_random_42 <-  runif(1,0,1)
  mildcuration_random_42 <- runif(1,0,1)
  moderatecuration_random_42 <- runif(1,0,1)
  severecuration_random_42 <- runif(1,0,1)
  relapseprevention_random_42 <- runif(1,0,1)
  
  prevention_rate_42 <- ifelse(preventionfactor == 0, 0, qbeta(prevention_random_42, preventionfactor*30,30-preventionfactor))
  mildcuration_rate_42 <- ifelse(mildcuration_int == 0,0, qbeta(mildcuration_random_42, mildcuration_int*30,30-mildcuration_int))
  moderatecuration_rate_42 <- ifelse(modcuration_int == 0,0, qbeta(moderatecuration_random_42, modcuration_int*30, 30-modcuration_int))
  severecuration_rate_42 <- ifelse(sevcuration_int == 0,0, qbeta(severecuration_random_42, sevcuration_int*30, 30-sevcuration_int))
  relapseprevention_rate_42 <- ifelse(relapsefactor == 0,0, qbeta(relapseprevention_random_42, relapsefactor*30,30-relapsefactor))
  
  ## Mild
  
  ### Mild 1x
  transition_matrix["mild_incidence", "mild_1x_1_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.4) * (1 - mildcuration_rate_42)
  transition_matrix["mild_incidence", "mild_1x_2_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.3) * (1 - mildcuration_rate_42)
  transition_matrix["mild_incidence", "mild_1x_3_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.2) * (1 - mildcuration_rate_42)
  transition_matrix["mild_incidence", "mild_1x_4_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.07) * (1 - mildcuration_rate_42)
  transition_matrix["mild_incidence", "mild_1x_5_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.03) * (1 - mildcuration_rate_42)
  
  ### Mild 2x
  transition_matrix["mild_1x_1_year", "mild_2x_1_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.4) * (1 - mildcuration_rate_42)
  transition_matrix["mild_1x_1_year", "mild_2x_2_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.3) * (1 - mildcuration_rate_42)
  transition_matrix["mild_1x_1_year", "mild_2x_3_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.2) * (1 - mildcuration_rate_42)
  transition_matrix["mild_1x_1_year", "mild_2x_4_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.07) * (1 - mildcuration_rate_42)
  transition_matrix["mild_1x_1_year", "mild_2x_5_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.03) * (1 - mildcuration_rate_42)
  
  ### Mild 3x
  transition_matrix["mild_2x_1_year", "mild_3x_1_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.4) * (1 - mildcuration_rate_42)
  transition_matrix["mild_2x_1_year", "mild_3x_2_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.3) * (1 - mildcuration_rate_42)
  transition_matrix["mild_2x_1_year", "mild_3x_3_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.2) * (1 - mildcuration_rate_42)
  transition_matrix["mild_2x_1_year", "mild_3x_4_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.07) * (1 - mildcuration_rate_42)
  transition_matrix["mild_2x_1_year", "mild_3x_5_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.03) * (1 - mildcuration_rate_42)
  
  ### Mild 4x
  transition_matrix["mild_3x_1_year", "mild_4x_1_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.4) * (1 - mildcuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["mild_3x_1_year", "mild_4x_2_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.3) * (1 - mildcuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["mild_3x_1_year", "mild_4x_3_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.2) * (1 - mildcuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["mild_3x_1_year", "mild_4x_4_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.07) * (1 - mildcuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["mild_3x_1_year", "mild_4x_5_year", 42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.03) * (1 - mildcuration_rate_42) * (1 - relapseprevention_rate_42)
  
  ### Mild 5x
  transition_matrix["mild_4x_1_year", "mild_5x_1_year",42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.4) * (1 - mildcuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["mild_4x_1_year", "mild_5x_2_year",42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.3) * (1 - mildcuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["mild_4x_1_year", "mild_5x_3_year",42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.2) * (1 - mildcuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["mild_4x_1_year", "mild_5x_4_year",42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.07) * (1 - mildcuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["mild_4x_1_year", "mild_5x_5_year",42] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.03) * (1 - mildcuration_rate_42) * (1 - relapseprevention_rate_42)
  
  ### Mild - chronic
  transition_matrix["mild_incidence", "mild_chronic", 42] <- (leavemodel/pmild * ((pmild * mildchronic))) * (1 - mildcuration_rate_42)
  transition_matrix["mild_1x_1_year", "mild_chronic", 42] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1 - mildcuration_rate_42)
  transition_matrix["mild_2x_1_year", "mild_chronic", 42] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1- mildcuration_rate_42)
  transition_matrix["mild_3x_1_year", "mild_chronic", 42] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1 - mildcuration_rate_42) * (1-relapseprevention_rate_42)
  transition_matrix["mild_4x_1_year", "mild_chronic", 42] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1 - mildcuration_rate_42) * (1-relapseprevention_rate_42)
  transition_matrix["mild_5x_1_year", "mild_chronic", 42] <-  (1 - (1-leavemodel) - (leavemodel/pmild * ((pmild * mildrecovery * (1-(1-(mildrecoverycured*(1-`increased relapse 5`))) * relapsecure) + pmild * mildpartial * (1-(1-(mildpartialcured * (1- `increased relapse 5`)))* relapsecure))) + `Fifth episode to chronic`)) * (1 - relapseprevention_rate_42) * (1 - mildcuration_rate_42)
  transition_matrix["mild_chronic", "mild_chronic",42] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
  
  ## Moderate
  
  ### Moderate 1x
  transition_matrix["moderate_incidence", "moderate_1x_1_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.4) * (1 - moderatecuration_rate_42)
  transition_matrix["moderate_incidence", "moderate_1x_2_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.3) * (1 - moderatecuration_rate_42)
  transition_matrix["moderate_incidence", "moderate_1x_3_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.2) * (1 - moderatecuration_rate_42)
  transition_matrix["moderate_incidence", "moderate_1x_4_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.07) * (1 - moderatecuration_rate_42)
  transition_matrix["moderate_incidence", "moderate_1x_5_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.03) * (1 - moderatecuration_rate_42)
  
  ### moderate 2x
  transition_matrix["moderate_1x_1_year", "moderate_2x_1_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.4) * (1 - moderatecuration_rate_42)
  transition_matrix["moderate_1x_1_year", "moderate_2x_2_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.3) * (1 - moderatecuration_rate_42)
  transition_matrix["moderate_1x_1_year", "moderate_2x_3_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.2) * (1 - moderatecuration_rate_42)
  transition_matrix["moderate_1x_1_year", "moderate_2x_4_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.07) * (1 - moderatecuration_rate_42)
  transition_matrix["moderate_1x_1_year", "moderate_2x_5_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.03) * (1 - moderatecuration_rate_42)
  
  ### moderate 3x
  transition_matrix["moderate_2x_1_year", "moderate_3x_1_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.4) * (1 - moderatecuration_rate_42)
  transition_matrix["moderate_2x_1_year", "moderate_3x_2_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.3) * (1 - moderatecuration_rate_42)
  transition_matrix["moderate_2x_1_year", "moderate_3x_3_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.2) * (1 - moderatecuration_rate_42)
  transition_matrix["moderate_2x_1_year", "moderate_3x_4_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.07) * (1 - moderatecuration_rate_42)
  transition_matrix["moderate_2x_1_year", "moderate_3x_5_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.03) * (1 - moderatecuration_rate_42)
  
  ### moderate 4x
  transition_matrix["moderate_3x_1_year", "moderate_4x_1_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.4) * (1 - moderatecuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["moderate_3x_1_year", "moderate_4x_2_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.3) * (1 - moderatecuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["moderate_3x_1_year", "moderate_4x_3_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.2) * (1 - moderatecuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["moderate_3x_1_year", "moderate_4x_4_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.07) * (1 - moderatecuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["moderate_3x_1_year", "moderate_4x_5_year", 42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.03) * (1 - moderatecuration_rate_42) * (1 - relapseprevention_rate_42)
  
  ### moderate 5x
  transition_matrix["moderate_4x_1_year", "moderate_5x_1_year",42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.4) * (1 - moderatecuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["moderate_4x_1_year", "moderate_5x_2_year",42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.3) * (1 - moderatecuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["moderate_4x_1_year", "moderate_5x_3_year",42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.2) * (1 - moderatecuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["moderate_4x_1_year", "moderate_5x_4_year",42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.07) * (1 - moderatecuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["moderate_4x_1_year", "moderate_5x_5_year",42] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.03) * (1 - moderatecuration_rate_42) * (1 - relapseprevention_rate_42)
  
  ### moderate - chronic
  transition_matrix["moderate_incidence", "moderate_chronic", 42] <- (leavemodel/pmoderate * ((pmoderate * moderatechronic))) * (1 - moderatecuration_rate_42)
  transition_matrix["moderate_1x_1_year", "moderate_chronic", 42] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1 - moderatecuration_rate_42)
  transition_matrix["moderate_2x_1_year", "moderate_chronic", 42] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1- moderatecuration_rate_42)
  transition_matrix["moderate_3x_1_year", "moderate_chronic", 42] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1 - moderatecuration_rate_42) * (1-relapseprevention_rate_42)
  transition_matrix["moderate_4x_1_year", "moderate_chronic", 42] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1 - moderatecuration_rate_42) * (1-relapseprevention_rate_42)
  transition_matrix["moderate_5x_1_year", "moderate_chronic", 42] <-  (1 - (1-leavemodel) - (leavemodel/pmoderate * ((pmoderate * moderaterecovery * (1-(1-(moderaterecoverycured*(1-`increased relapse 5`))) * relapsecure) + pmoderate * moderatepartial * (1-(1-(moderatepartialcured * (1- `increased relapse 5`)))* relapsecure))) + `Fifth episode to chronic`)) * (1 - relapseprevention_rate_42) * (1 - moderatecuration_rate_42)
  transition_matrix["moderate_chronic", "moderate_chronic",42] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
  
  ## Severe
  
  ### severe 1x
  transition_matrix["severe_incidence", "severe_1x_1_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.4) * (1 - severecuration_rate_42)
  transition_matrix["severe_incidence", "severe_1x_2_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.3) * (1 - severecuration_rate_42)
  transition_matrix["severe_incidence", "severe_1x_3_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.2) * (1 - severecuration_rate_42)
  transition_matrix["severe_incidence", "severe_1x_4_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.07) * (1 - severecuration_rate_42)
  transition_matrix["severe_incidence", "severe_1x_5_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.03) * (1 - severecuration_rate_42)
  
  ### severe 2x
  transition_matrix["severe_1x_1_year", "severe_2x_1_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.4) * (1 - severecuration_rate_42)
  transition_matrix["severe_1x_1_year", "severe_2x_2_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.3) * (1 - severecuration_rate_42)
  transition_matrix["severe_1x_1_year", "severe_2x_3_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.2) * (1 - severecuration_rate_42)
  transition_matrix["severe_1x_1_year", "severe_2x_4_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.07) * (1 - severecuration_rate_42)
  transition_matrix["severe_1x_1_year", "severe_2x_5_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.03) * (1 - severecuration_rate_42)
  
  ### severe 3x
  transition_matrix["severe_2x_1_year", "severe_3x_1_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.4) * (1 - severecuration_rate_42)
  transition_matrix["severe_2x_1_year", "severe_3x_2_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.3) * (1 - severecuration_rate_42)
  transition_matrix["severe_2x_1_year", "severe_3x_3_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.2) * (1 - severecuration_rate_42)
  transition_matrix["severe_2x_1_year", "severe_3x_4_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.07) * (1 - severecuration_rate_42)
  transition_matrix["severe_2x_1_year", "severe_3x_5_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.03) * (1 - severecuration_rate_42)
  
  ### severe 4x
  transition_matrix["severe_3x_1_year", "severe_4x_1_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.4) * (1 - severecuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["severe_3x_1_year", "severe_4x_2_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.3) * (1 - severecuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["severe_3x_1_year", "severe_4x_3_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.2) * (1 - severecuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["severe_3x_1_year", "severe_4x_4_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.07) * (1 - severecuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["severe_3x_1_year", "severe_4x_5_year", 42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.03) * (1 - severecuration_rate_42) * (1 - relapseprevention_rate_42)
  
  ### severe 5x
  transition_matrix["severe_4x_1_year", "severe_5x_1_year",42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.4) * (1 - severecuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["severe_4x_1_year", "severe_5x_2_year",42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.3) * (1 - severecuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["severe_4x_1_year", "severe_5x_3_year",42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.2) * (1 - severecuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["severe_4x_1_year", "severe_5x_4_year",42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.07) * (1 - severecuration_rate_42) * (1 - relapseprevention_rate_42)
  transition_matrix["severe_4x_1_year", "severe_5x_5_year",42] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.03) * (1 - severecuration_rate_42) * (1 - relapseprevention_rate_42)
  
  ### severe - chronic
  transition_matrix["severe_incidence", "severe_chronic", 42] <- (leavemodel/psevere * ((psevere * severechronic))) * (1 - severecuration_rate_42)
  transition_matrix["severe_1x_1_year", "severe_chronic", 42] <- (leavemodel/psevere * ((psevere*severechronic))) * (1 - severecuration_rate_42)
  transition_matrix["severe_2x_1_year", "severe_chronic", 42] <- (leavemodel/psevere * ((psevere*severechronic))) * (1- severecuration_rate_42)
  transition_matrix["severe_3x_1_year", "severe_chronic", 42] <- (leavemodel/psevere * ((psevere*severechronic))) * (1 - severecuration_rate_42) * (1-relapseprevention_rate_42)
  transition_matrix["severe_4x_1_year", "severe_chronic", 42] <- (leavemodel/psevere * ((psevere*severechronic))) * (1 - severecuration_rate_42) * (1-relapseprevention_rate_42)
  transition_matrix["severe_5x_1_year", "severe_chronic", 42] <-  (1- (1-leavemodel) - (`Fifth episode to chronic`)) * (1 - relapseprevention_rate_42) * (1 - severecuration_rate_42)
  transition_matrix["severe_chronic", "severe_chronic",42] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
  
  
  ##Year 43 probabilities
  prevention_random_43 <-  runif(1,0,1)
  mildcuration_random_43 <- runif(1,0,1)
  moderatecuration_random_43 <- runif(1,0,1)
  severecuration_random_43 <- runif(1,0,1)
  relapseprevention_random_43 <- runif(1,0,1)
  
  prevention_rate_43 <- ifelse(preventionfactor == 0, 0, qbeta(prevention_random_43, preventionfactor*30,30-preventionfactor))
  mildcuration_rate_43 <- ifelse(mildcuration_int == 0,0, qbeta(mildcuration_random_43, mildcuration_int*30,30-mildcuration_int))
  moderatecuration_rate_43 <- ifelse(modcuration_int == 0,0, qbeta(moderatecuration_random_43, modcuration_int*30, 30-modcuration_int))
  severecuration_rate_43 <- ifelse(sevcuration_int == 0,0, qbeta(severecuration_random_43, sevcuration_int*30, 30-sevcuration_int))
  relapseprevention_rate_43 <- ifelse(relapsefactor == 0,0, qbeta(relapseprevention_random_43, relapsefactor*30,30-relapsefactor))
  
  ## Mild
  
  ### Mild 1x
  transition_matrix["mild_incidence", "mild_1x_1_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.4) * (1 - mildcuration_rate_43)
  transition_matrix["mild_incidence", "mild_1x_2_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.3) * (1 - mildcuration_rate_43)
  transition_matrix["mild_incidence", "mild_1x_3_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.2) * (1 - mildcuration_rate_43)
  transition_matrix["mild_incidence", "mild_1x_4_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.07) * (1 - mildcuration_rate_43)
  transition_matrix["mild_incidence", "mild_1x_5_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.03) * (1 - mildcuration_rate_43)
  
  ### Mild 2x
  transition_matrix["mild_1x_1_year", "mild_2x_1_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.4) * (1 - mildcuration_rate_43)
  transition_matrix["mild_1x_1_year", "mild_2x_2_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.3) * (1 - mildcuration_rate_43)
  transition_matrix["mild_1x_1_year", "mild_2x_3_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.2) * (1 - mildcuration_rate_43)
  transition_matrix["mild_1x_1_year", "mild_2x_4_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.07) * (1 - mildcuration_rate_43)
  transition_matrix["mild_1x_1_year", "mild_2x_5_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.03) * (1 - mildcuration_rate_43)
  
  ### Mild 3x
  transition_matrix["mild_2x_1_year", "mild_3x_1_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.4) * (1 - mildcuration_rate_43)
  transition_matrix["mild_2x_1_year", "mild_3x_2_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.3) * (1 - mildcuration_rate_43)
  transition_matrix["mild_2x_1_year", "mild_3x_3_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.2) * (1 - mildcuration_rate_43)
  transition_matrix["mild_2x_1_year", "mild_3x_4_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.07) * (1 - mildcuration_rate_43)
  transition_matrix["mild_2x_1_year", "mild_3x_5_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.03) * (1 - mildcuration_rate_43)
  
  ### Mild 4x
  transition_matrix["mild_3x_1_year", "mild_4x_1_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.4) * (1 - mildcuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["mild_3x_1_year", "mild_4x_2_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.3) * (1 - mildcuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["mild_3x_1_year", "mild_4x_3_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.2) * (1 - mildcuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["mild_3x_1_year", "mild_4x_4_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.07) * (1 - mildcuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["mild_3x_1_year", "mild_4x_5_year", 43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.03) * (1 - mildcuration_rate_43) * (1 - relapseprevention_rate_43)
  
  ### Mild 5x
  transition_matrix["mild_4x_1_year", "mild_5x_1_year",43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.4) * (1 - mildcuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["mild_4x_1_year", "mild_5x_2_year",43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.3) * (1 - mildcuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["mild_4x_1_year", "mild_5x_3_year",43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.2) * (1 - mildcuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["mild_4x_1_year", "mild_5x_4_year",43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.07) * (1 - mildcuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["mild_4x_1_year", "mild_5x_5_year",43] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.03) * (1 - mildcuration_rate_43) * (1 - relapseprevention_rate_43)
  
  ### Mild - chronic
  transition_matrix["mild_incidence", "mild_chronic", 43] <- (leavemodel/pmild * ((pmild * mildchronic))) * (1 - mildcuration_rate_43)
  transition_matrix["mild_1x_1_year", "mild_chronic", 43] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1 - mildcuration_rate_43)
  transition_matrix["mild_2x_1_year", "mild_chronic", 43] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1- mildcuration_rate_43)
  transition_matrix["mild_3x_1_year", "mild_chronic", 43] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1 - mildcuration_rate_43) * (1-relapseprevention_rate_43)
  transition_matrix["mild_4x_1_year", "mild_chronic", 43] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1 - mildcuration_rate_43) * (1-relapseprevention_rate_43)
  transition_matrix["mild_5x_1_year", "mild_chronic", 43] <-  (1 - (1-leavemodel) - (leavemodel/pmild * ((pmild * mildrecovery * (1-(1-(mildrecoverycured*(1-`increased relapse 5`))) * relapsecure) + pmild * mildpartial * (1-(1-(mildpartialcured * (1- `increased relapse 5`)))* relapsecure))) + `Fifth episode to chronic`)) * (1 - relapseprevention_rate_43) * (1 - mildcuration_rate_43)
  transition_matrix["mild_chronic", "mild_chronic",43] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
  
  ## Moderate
  
  ### Moderate 1x
  transition_matrix["moderate_incidence", "moderate_1x_1_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.4) * (1 - moderatecuration_rate_43)
  transition_matrix["moderate_incidence", "moderate_1x_2_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.3) * (1 - moderatecuration_rate_43)
  transition_matrix["moderate_incidence", "moderate_1x_3_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.2) * (1 - moderatecuration_rate_43)
  transition_matrix["moderate_incidence", "moderate_1x_4_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.07) * (1 - moderatecuration_rate_43)
  transition_matrix["moderate_incidence", "moderate_1x_5_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.03) * (1 - moderatecuration_rate_43)
  
  ### moderate 2x
  transition_matrix["moderate_1x_1_year", "moderate_2x_1_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.4) * (1 - moderatecuration_rate_43)
  transition_matrix["moderate_1x_1_year", "moderate_2x_2_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.3) * (1 - moderatecuration_rate_43)
  transition_matrix["moderate_1x_1_year", "moderate_2x_3_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.2) * (1 - moderatecuration_rate_43)
  transition_matrix["moderate_1x_1_year", "moderate_2x_4_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.07) * (1 - moderatecuration_rate_43)
  transition_matrix["moderate_1x_1_year", "moderate_2x_5_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.03) * (1 - moderatecuration_rate_43)
  
  ### moderate 3x
  transition_matrix["moderate_2x_1_year", "moderate_3x_1_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.4) * (1 - moderatecuration_rate_43)
  transition_matrix["moderate_2x_1_year", "moderate_3x_2_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.3) * (1 - moderatecuration_rate_43)
  transition_matrix["moderate_2x_1_year", "moderate_3x_3_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.2) * (1 - moderatecuration_rate_43)
  transition_matrix["moderate_2x_1_year", "moderate_3x_4_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.07) * (1 - moderatecuration_rate_43)
  transition_matrix["moderate_2x_1_year", "moderate_3x_5_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.03) * (1 - moderatecuration_rate_43)
  
  ### moderate 4x
  transition_matrix["moderate_3x_1_year", "moderate_4x_1_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.4) * (1 - moderatecuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["moderate_3x_1_year", "moderate_4x_2_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.3) * (1 - moderatecuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["moderate_3x_1_year", "moderate_4x_3_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.2) * (1 - moderatecuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["moderate_3x_1_year", "moderate_4x_4_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.07) * (1 - moderatecuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["moderate_3x_1_year", "moderate_4x_5_year", 43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.03) * (1 - moderatecuration_rate_43) * (1 - relapseprevention_rate_43)
  
  ### moderate 5x
  transition_matrix["moderate_4x_1_year", "moderate_5x_1_year",43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.4) * (1 - moderatecuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["moderate_4x_1_year", "moderate_5x_2_year",43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.3) * (1 - moderatecuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["moderate_4x_1_year", "moderate_5x_3_year",43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.2) * (1 - moderatecuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["moderate_4x_1_year", "moderate_5x_4_year",43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.07) * (1 - moderatecuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["moderate_4x_1_year", "moderate_5x_5_year",43] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.03) * (1 - moderatecuration_rate_43) * (1 - relapseprevention_rate_43)
  
  ### moderate - chronic
  transition_matrix["moderate_incidence", "moderate_chronic", 43] <- (leavemodel/pmoderate * ((pmoderate * moderatechronic))) * (1 - moderatecuration_rate_43)
  transition_matrix["moderate_1x_1_year", "moderate_chronic", 43] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1 - moderatecuration_rate_43)
  transition_matrix["moderate_2x_1_year", "moderate_chronic", 43] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1- moderatecuration_rate_43)
  transition_matrix["moderate_3x_1_year", "moderate_chronic", 43] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1 - moderatecuration_rate_43) * (1-relapseprevention_rate_43)
  transition_matrix["moderate_4x_1_year", "moderate_chronic", 43] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1 - moderatecuration_rate_43) * (1-relapseprevention_rate_43)
  transition_matrix["moderate_5x_1_year", "moderate_chronic", 43] <-  (1 - (1-leavemodel) - (leavemodel/pmoderate * ((pmoderate * moderaterecovery * (1-(1-(moderaterecoverycured*(1-`increased relapse 5`))) * relapsecure) + pmoderate * moderatepartial * (1-(1-(moderatepartialcured * (1- `increased relapse 5`)))* relapsecure))) + `Fifth episode to chronic`)) * (1 - relapseprevention_rate_43) * (1 - moderatecuration_rate_43)
  transition_matrix["moderate_chronic", "moderate_chronic",43] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
  
  ## Severe
  
  ### severe 1x
  transition_matrix["severe_incidence", "severe_1x_1_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.4) * (1 - severecuration_rate_43)
  transition_matrix["severe_incidence", "severe_1x_2_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.3) * (1 - severecuration_rate_43)
  transition_matrix["severe_incidence", "severe_1x_3_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.2) * (1 - severecuration_rate_43)
  transition_matrix["severe_incidence", "severe_1x_4_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.07) * (1 - severecuration_rate_43)
  transition_matrix["severe_incidence", "severe_1x_5_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.03) * (1 - severecuration_rate_43)
  
  ### severe 2x
  transition_matrix["severe_1x_1_year", "severe_2x_1_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.4) * (1 - severecuration_rate_43)
  transition_matrix["severe_1x_1_year", "severe_2x_2_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.3) * (1 - severecuration_rate_43)
  transition_matrix["severe_1x_1_year", "severe_2x_3_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.2) * (1 - severecuration_rate_43)
  transition_matrix["severe_1x_1_year", "severe_2x_4_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.07) * (1 - severecuration_rate_43)
  transition_matrix["severe_1x_1_year", "severe_2x_5_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.03) * (1 - severecuration_rate_43)
  
  ### severe 3x
  transition_matrix["severe_2x_1_year", "severe_3x_1_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.4) * (1 - severecuration_rate_43)
  transition_matrix["severe_2x_1_year", "severe_3x_2_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.3) * (1 - severecuration_rate_43)
  transition_matrix["severe_2x_1_year", "severe_3x_3_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.2) * (1 - severecuration_rate_43)
  transition_matrix["severe_2x_1_year", "severe_3x_4_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.07) * (1 - severecuration_rate_43)
  transition_matrix["severe_2x_1_year", "severe_3x_5_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.03) * (1 - severecuration_rate_43)
  
  ### severe 4x
  transition_matrix["severe_3x_1_year", "severe_4x_1_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.4) * (1 - severecuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["severe_3x_1_year", "severe_4x_2_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.3) * (1 - severecuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["severe_3x_1_year", "severe_4x_3_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.2) * (1 - severecuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["severe_3x_1_year", "severe_4x_4_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.07) * (1 - severecuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["severe_3x_1_year", "severe_4x_5_year", 43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.03) * (1 - severecuration_rate_43) * (1 - relapseprevention_rate_43)
  
  ### severe 5x
  transition_matrix["severe_4x_1_year", "severe_5x_1_year",43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.4) * (1 - severecuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["severe_4x_1_year", "severe_5x_2_year",43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.3) * (1 - severecuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["severe_4x_1_year", "severe_5x_3_year",43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.2) * (1 - severecuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["severe_4x_1_year", "severe_5x_4_year",43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.07) * (1 - severecuration_rate_43) * (1 - relapseprevention_rate_43)
  transition_matrix["severe_4x_1_year", "severe_5x_5_year",43] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.03) * (1 - severecuration_rate_43) * (1 - relapseprevention_rate_43)
  
  ### severe - chronic
  transition_matrix["severe_incidence", "severe_chronic", 43] <- (leavemodel/psevere * ((psevere * severechronic))) * (1 - severecuration_rate_43)
  transition_matrix["severe_1x_1_year", "severe_chronic", 43] <- (leavemodel/psevere * ((psevere*severechronic))) * (1 - severecuration_rate_43)
  transition_matrix["severe_2x_1_year", "severe_chronic", 43] <- (leavemodel/psevere * ((psevere*severechronic))) * (1- severecuration_rate_43)
  transition_matrix["severe_3x_1_year", "severe_chronic", 43] <- (leavemodel/psevere * ((psevere*severechronic))) * (1 - severecuration_rate_43) * (1-relapseprevention_rate_43)
  transition_matrix["severe_4x_1_year", "severe_chronic", 43] <- (leavemodel/psevere * ((psevere*severechronic))) * (1 - severecuration_rate_43) * (1-relapseprevention_rate_43)
  transition_matrix["severe_5x_1_year", "severe_chronic", 43] <-  (1- (1-leavemodel) - (`Fifth episode to chronic`)) * (1 - relapseprevention_rate_43) * (1 - severecuration_rate_43)
  transition_matrix["severe_chronic", "severe_chronic",43] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
  
  
  
  
  
  ##Year 44 probabilities
  prevention_random_44 <-  runif(1,0,1)
  mildcuration_random_44 <- runif(1,0,1)
  moderatecuration_random_44 <- runif(1,0,1)
  severecuration_random_44 <- runif(1,0,1)
  relapseprevention_random_44 <- runif(1,0,1)
  
  prevention_rate_44 <- ifelse(preventionfactor == 0, 0, qbeta(prevention_random_44, preventionfactor*30,30-preventionfactor))
  mildcuration_rate_44 <- ifelse(mildcuration_int == 0,0, qbeta(mildcuration_random_44, mildcuration_int*30,30-mildcuration_int))
  moderatecuration_rate_44 <- ifelse(modcuration_int == 0,0, qbeta(moderatecuration_random_44, modcuration_int*30, 30-modcuration_int))
  severecuration_rate_44 <- ifelse(sevcuration_int == 0,0, qbeta(severecuration_random_44, sevcuration_int*30, 30-sevcuration_int))
  relapseprevention_rate_44 <- ifelse(relapsefactor == 0,0, qbeta(relapseprevention_random_44, relapsefactor*30,30-relapsefactor))
  
  ## Mild
  
  ### Mild 1x
  transition_matrix["mild_incidence", "mild_1x_1_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.4) * (1 - mildcuration_rate_44)
  transition_matrix["mild_incidence", "mild_1x_2_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.3) * (1 - mildcuration_rate_44)
  transition_matrix["mild_incidence", "mild_1x_3_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.2) * (1 - mildcuration_rate_44)
  transition_matrix["mild_incidence", "mild_1x_4_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.07) * (1 - mildcuration_rate_44)
  transition_matrix["mild_incidence", "mild_1x_5_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.03) * (1 - mildcuration_rate_44)
  
  ### Mild 2x
  transition_matrix["mild_1x_1_year", "mild_2x_1_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.4) * (1 - mildcuration_rate_44)
  transition_matrix["mild_1x_1_year", "mild_2x_2_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.3) * (1 - mildcuration_rate_44)
  transition_matrix["mild_1x_1_year", "mild_2x_3_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.2) * (1 - mildcuration_rate_44)
  transition_matrix["mild_1x_1_year", "mild_2x_4_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.07) * (1 - mildcuration_rate_44)
  transition_matrix["mild_1x_1_year", "mild_2x_5_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.03) * (1 - mildcuration_rate_44)
  
  ### Mild 3x
  transition_matrix["mild_2x_1_year", "mild_3x_1_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.4) * (1 - mildcuration_rate_44)
  transition_matrix["mild_2x_1_year", "mild_3x_2_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.3) * (1 - mildcuration_rate_44)
  transition_matrix["mild_2x_1_year", "mild_3x_3_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.2) * (1 - mildcuration_rate_44)
  transition_matrix["mild_2x_1_year", "mild_3x_4_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.07) * (1 - mildcuration_rate_44)
  transition_matrix["mild_2x_1_year", "mild_3x_5_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.03) * (1 - mildcuration_rate_44)
  
  ### Mild 4x
  transition_matrix["mild_3x_1_year", "mild_4x_1_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.4) * (1 - mildcuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["mild_3x_1_year", "mild_4x_2_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.3) * (1 - mildcuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["mild_3x_1_year", "mild_4x_3_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.2) * (1 - mildcuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["mild_3x_1_year", "mild_4x_4_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.07) * (1 - mildcuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["mild_3x_1_year", "mild_4x_5_year", 44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.03) * (1 - mildcuration_rate_44) * (1 - relapseprevention_rate_44)
  
  ### Mild 5x
  transition_matrix["mild_4x_1_year", "mild_5x_1_year",44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.4) * (1 - mildcuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["mild_4x_1_year", "mild_5x_2_year",44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.3) * (1 - mildcuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["mild_4x_1_year", "mild_5x_3_year",44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.2) * (1 - mildcuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["mild_4x_1_year", "mild_5x_4_year",44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.07) * (1 - mildcuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["mild_4x_1_year", "mild_5x_5_year",44] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.03) * (1 - mildcuration_rate_44) * (1 - relapseprevention_rate_44)
  
  ### Mild - chronic
  transition_matrix["mild_incidence", "mild_chronic", 44] <- (leavemodel/pmild * ((pmild * mildchronic))) * (1 - mildcuration_rate_44)
  transition_matrix["mild_1x_1_year", "mild_chronic", 44] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1 - mildcuration_rate_44)
  transition_matrix["mild_2x_1_year", "mild_chronic", 44] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1- mildcuration_rate_44)
  transition_matrix["mild_3x_1_year", "mild_chronic", 44] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1 - mildcuration_rate_44) * (1-relapseprevention_rate_44)
  transition_matrix["mild_4x_1_year", "mild_chronic", 44] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1 - mildcuration_rate_44) * (1-relapseprevention_rate_44)
  transition_matrix["mild_5x_1_year", "mild_chronic", 44] <-  (1 - (1-leavemodel) - (leavemodel/pmild * ((pmild * mildrecovery * (1-(1-(mildrecoverycured*(1-`increased relapse 5`))) * relapsecure) + pmild * mildpartial * (1-(1-(mildpartialcured * (1- `increased relapse 5`)))* relapsecure))) + `Fifth episode to chronic`)) * (1 - relapseprevention_rate_44) * (1 - mildcuration_rate_44)
  transition_matrix["mild_chronic", "mild_chronic",44] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
  
  ## Moderate
  
  ### Moderate 1x
  transition_matrix["moderate_incidence", "moderate_1x_1_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.4) * (1 - moderatecuration_rate_44)
  transition_matrix["moderate_incidence", "moderate_1x_2_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.3) * (1 - moderatecuration_rate_44)
  transition_matrix["moderate_incidence", "moderate_1x_3_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.2) * (1 - moderatecuration_rate_44)
  transition_matrix["moderate_incidence", "moderate_1x_4_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.07) * (1 - moderatecuration_rate_44)
  transition_matrix["moderate_incidence", "moderate_1x_5_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.03) * (1 - moderatecuration_rate_44)
  
  ### moderate 2x
  transition_matrix["moderate_1x_1_year", "moderate_2x_1_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.4) * (1 - moderatecuration_rate_44)
  transition_matrix["moderate_1x_1_year", "moderate_2x_2_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.3) * (1 - moderatecuration_rate_44)
  transition_matrix["moderate_1x_1_year", "moderate_2x_3_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.2) * (1 - moderatecuration_rate_44)
  transition_matrix["moderate_1x_1_year", "moderate_2x_4_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.07) * (1 - moderatecuration_rate_44)
  transition_matrix["moderate_1x_1_year", "moderate_2x_5_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.03) * (1 - moderatecuration_rate_44)
  
  ### moderate 3x
  transition_matrix["moderate_2x_1_year", "moderate_3x_1_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.4) * (1 - moderatecuration_rate_44)
  transition_matrix["moderate_2x_1_year", "moderate_3x_2_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.3) * (1 - moderatecuration_rate_44)
  transition_matrix["moderate_2x_1_year", "moderate_3x_3_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.2) * (1 - moderatecuration_rate_44)
  transition_matrix["moderate_2x_1_year", "moderate_3x_4_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.07) * (1 - moderatecuration_rate_44)
  transition_matrix["moderate_2x_1_year", "moderate_3x_5_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.03) * (1 - moderatecuration_rate_44)
  
  ### moderate 4x
  transition_matrix["moderate_3x_1_year", "moderate_4x_1_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.4) * (1 - moderatecuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["moderate_3x_1_year", "moderate_4x_2_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.3) * (1 - moderatecuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["moderate_3x_1_year", "moderate_4x_3_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.2) * (1 - moderatecuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["moderate_3x_1_year", "moderate_4x_4_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.07) * (1 - moderatecuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["moderate_3x_1_year", "moderate_4x_5_year", 44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.03) * (1 - moderatecuration_rate_44) * (1 - relapseprevention_rate_44)
  
  ### moderate 5x
  transition_matrix["moderate_4x_1_year", "moderate_5x_1_year",44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.4) * (1 - moderatecuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["moderate_4x_1_year", "moderate_5x_2_year",44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.3) * (1 - moderatecuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["moderate_4x_1_year", "moderate_5x_3_year",44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.2) * (1 - moderatecuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["moderate_4x_1_year", "moderate_5x_4_year",44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.07) * (1 - moderatecuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["moderate_4x_1_year", "moderate_5x_5_year",44] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.03) * (1 - moderatecuration_rate_44) * (1 - relapseprevention_rate_44)
  
  ### moderate - chronic
  transition_matrix["moderate_incidence", "moderate_chronic", 44] <- (leavemodel/pmoderate * ((pmoderate * moderatechronic))) * (1 - moderatecuration_rate_44)
  transition_matrix["moderate_1x_1_year", "moderate_chronic", 44] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1 - moderatecuration_rate_44)
  transition_matrix["moderate_2x_1_year", "moderate_chronic", 44] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1- moderatecuration_rate_44)
  transition_matrix["moderate_3x_1_year", "moderate_chronic", 44] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1 - moderatecuration_rate_44) * (1-relapseprevention_rate_44)
  transition_matrix["moderate_4x_1_year", "moderate_chronic", 44] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1 - moderatecuration_rate_44) * (1-relapseprevention_rate_44)
  transition_matrix["moderate_5x_1_year", "moderate_chronic", 44] <-  (1 - (1-leavemodel) - (leavemodel/pmoderate * ((pmoderate * moderaterecovery * (1-(1-(moderaterecoverycured*(1-`increased relapse 5`))) * relapsecure) + pmoderate * moderatepartial * (1-(1-(moderatepartialcured * (1- `increased relapse 5`)))* relapsecure))) + `Fifth episode to chronic`)) * (1 - relapseprevention_rate_44) * (1 - moderatecuration_rate_44)
  transition_matrix["moderate_chronic", "moderate_chronic",44] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
  
  ## Severe
  
  ### severe 1x
  transition_matrix["severe_incidence", "severe_1x_1_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.4) * (1 - severecuration_rate_44)
  transition_matrix["severe_incidence", "severe_1x_2_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.3) * (1 - severecuration_rate_44)
  transition_matrix["severe_incidence", "severe_1x_3_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.2) * (1 - severecuration_rate_44)
  transition_matrix["severe_incidence", "severe_1x_4_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.07) * (1 - severecuration_rate_44)
  transition_matrix["severe_incidence", "severe_1x_5_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.03) * (1 - severecuration_rate_44)
  
  ### severe 2x
  transition_matrix["severe_1x_1_year", "severe_2x_1_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.4) * (1 - severecuration_rate_44)
  transition_matrix["severe_1x_1_year", "severe_2x_2_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.3) * (1 - severecuration_rate_44)
  transition_matrix["severe_1x_1_year", "severe_2x_3_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.2) * (1 - severecuration_rate_44)
  transition_matrix["severe_1x_1_year", "severe_2x_4_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.07) * (1 - severecuration_rate_44)
  transition_matrix["severe_1x_1_year", "severe_2x_5_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.03) * (1 - severecuration_rate_44)
  
  ### severe 3x
  transition_matrix["severe_2x_1_year", "severe_3x_1_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.4) * (1 - severecuration_rate_44)
  transition_matrix["severe_2x_1_year", "severe_3x_2_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.3) * (1 - severecuration_rate_44)
  transition_matrix["severe_2x_1_year", "severe_3x_3_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.2) * (1 - severecuration_rate_44)
  transition_matrix["severe_2x_1_year", "severe_3x_4_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.07) * (1 - severecuration_rate_44)
  transition_matrix["severe_2x_1_year", "severe_3x_5_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.03) * (1 - severecuration_rate_44)
  
  ### severe 4x
  transition_matrix["severe_3x_1_year", "severe_4x_1_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.4) * (1 - severecuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["severe_3x_1_year", "severe_4x_2_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.3) * (1 - severecuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["severe_3x_1_year", "severe_4x_3_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.2) * (1 - severecuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["severe_3x_1_year", "severe_4x_4_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.07) * (1 - severecuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["severe_3x_1_year", "severe_4x_5_year", 44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.03) * (1 - severecuration_rate_44) * (1 - relapseprevention_rate_44)
  
  ### severe 5x
  transition_matrix["severe_4x_1_year", "severe_5x_1_year",44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.4) * (1 - severecuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["severe_4x_1_year", "severe_5x_2_year",44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.3) * (1 - severecuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["severe_4x_1_year", "severe_5x_3_year",44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.2) * (1 - severecuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["severe_4x_1_year", "severe_5x_4_year",44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.07) * (1 - severecuration_rate_44) * (1 - relapseprevention_rate_44)
  transition_matrix["severe_4x_1_year", "severe_5x_5_year",44] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.03) * (1 - severecuration_rate_44) * (1 - relapseprevention_rate_44)
  
  ### severe - chronic
  transition_matrix["severe_incidence", "severe_chronic", 44] <- (leavemodel/psevere * ((psevere * severechronic))) * (1 - severecuration_rate_44)
  transition_matrix["severe_1x_1_year", "severe_chronic", 44] <- (leavemodel/psevere * ((psevere*severechronic))) * (1 - severecuration_rate_44)
  transition_matrix["severe_2x_1_year", "severe_chronic", 44] <- (leavemodel/psevere * ((psevere*severechronic))) * (1- severecuration_rate_44)
  transition_matrix["severe_3x_1_year", "severe_chronic", 44] <- (leavemodel/psevere * ((psevere*severechronic))) * (1 - severecuration_rate_44) * (1-relapseprevention_rate_44)
  transition_matrix["severe_4x_1_year", "severe_chronic", 44] <- (leavemodel/psevere * ((psevere*severechronic))) * (1 - severecuration_rate_44) * (1-relapseprevention_rate_44)
  transition_matrix["severe_5x_1_year", "severe_chronic", 44] <-  (1- (1-leavemodel) - (`Fifth episode to chronic`)) * (1 - relapseprevention_rate_44) * (1 - severecuration_rate_44)
  transition_matrix["severe_chronic", "severe_chronic",44] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
  
  ##Year 45 probabilities
  prevention_random_45 <-  runif(1,0,1)
  mildcuration_random_45 <- runif(1,0,1)
  moderatecuration_random_45 <- runif(1,0,1)
  severecuration_random_45 <- runif(1,0,1)
  relapseprevention_random_45 <- runif(1,0,1)
  
  prevention_rate_45 <- ifelse(preventionfactor == 0, 0, qbeta(prevention_random_45, preventionfactor*30,30-preventionfactor))
  mildcuration_rate_45 <- ifelse(mildcuration_int == 0,0, qbeta(mildcuration_random_45, mildcuration_int*30,30-mildcuration_int))
  moderatecuration_rate_45 <- ifelse(modcuration_int == 0,0, qbeta(moderatecuration_random_45, modcuration_int*30, 30-modcuration_int))
  severecuration_rate_45 <- ifelse(sevcuration_int == 0,0, qbeta(severecuration_random_45, sevcuration_int*30, 30-sevcuration_int))
  relapseprevention_rate_45 <- ifelse(relapsefactor == 0,0, qbeta(relapseprevention_random_45, relapsefactor*30,30-relapsefactor))
  
  ## Mild
  
  ### Mild 1x
  transition_matrix["mild_incidence", "mild_1x_1_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.4) * (1 - mildcuration_rate_45)
  transition_matrix["mild_incidence", "mild_1x_2_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.3) * (1 - mildcuration_rate_45)
  transition_matrix["mild_incidence", "mild_1x_3_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.2) * (1 - mildcuration_rate_45)
  transition_matrix["mild_incidence", "mild_1x_4_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.07) * (1 - mildcuration_rate_45)
  transition_matrix["mild_incidence", "mild_1x_5_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.03) * (1 - mildcuration_rate_45)
  
  ### Mild 2x
  transition_matrix["mild_1x_1_year", "mild_2x_1_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.4) * (1 - mildcuration_rate_45)
  transition_matrix["mild_1x_1_year", "mild_2x_2_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.3) * (1 - mildcuration_rate_45)
  transition_matrix["mild_1x_1_year", "mild_2x_3_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.2) * (1 - mildcuration_rate_45)
  transition_matrix["mild_1x_1_year", "mild_2x_4_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.07) * (1 - mildcuration_rate_45)
  transition_matrix["mild_1x_1_year", "mild_2x_5_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.03) * (1 - mildcuration_rate_45)
  
  ### Mild 3x
  transition_matrix["mild_2x_1_year", "mild_3x_1_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.4) * (1 - mildcuration_rate_45)
  transition_matrix["mild_2x_1_year", "mild_3x_2_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.3) * (1 - mildcuration_rate_45)
  transition_matrix["mild_2x_1_year", "mild_3x_3_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.2) * (1 - mildcuration_rate_45)
  transition_matrix["mild_2x_1_year", "mild_3x_4_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.07) * (1 - mildcuration_rate_45)
  transition_matrix["mild_2x_1_year", "mild_3x_5_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.03) * (1 - mildcuration_rate_45)
  
  ### Mild 4x
  transition_matrix["mild_3x_1_year", "mild_4x_1_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.4) * (1 - mildcuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["mild_3x_1_year", "mild_4x_2_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.3) * (1 - mildcuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["mild_3x_1_year", "mild_4x_3_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.2) * (1 - mildcuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["mild_3x_1_year", "mild_4x_4_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.07) * (1 - mildcuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["mild_3x_1_year", "mild_4x_5_year", 45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.03) * (1 - mildcuration_rate_45) * (1 - relapseprevention_rate_45)
  
  ### Mild 5x
  transition_matrix["mild_4x_1_year", "mild_5x_1_year",45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.4) * (1 - mildcuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["mild_4x_1_year", "mild_5x_2_year",45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.3) * (1 - mildcuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["mild_4x_1_year", "mild_5x_3_year",45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.2) * (1 - mildcuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["mild_4x_1_year", "mild_5x_4_year",45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.07) * (1 - mildcuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["mild_4x_1_year", "mild_5x_5_year",45] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.03) * (1 - mildcuration_rate_45) * (1 - relapseprevention_rate_45)
  
  ### Mild - chronic
  transition_matrix["mild_incidence", "mild_chronic", 45] <- (leavemodel/pmild * ((pmild * mildchronic))) * (1 - mildcuration_rate_45)
  transition_matrix["mild_1x_1_year", "mild_chronic", 45] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1 - mildcuration_rate_45)
  transition_matrix["mild_2x_1_year", "mild_chronic", 45] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1- mildcuration_rate_45)
  transition_matrix["mild_3x_1_year", "mild_chronic", 45] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1 - mildcuration_rate_45) * (1-relapseprevention_rate_45)
  transition_matrix["mild_4x_1_year", "mild_chronic", 45] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1 - mildcuration_rate_45) * (1-relapseprevention_rate_45)
  transition_matrix["mild_5x_1_year", "mild_chronic", 45] <-  (1 - (1-leavemodel) - (leavemodel/pmild * ((pmild * mildrecovery * (1-(1-(mildrecoverycured*(1-`increased relapse 5`))) * relapsecure) + pmild * mildpartial * (1-(1-(mildpartialcured * (1- `increased relapse 5`)))* relapsecure))) + `Fifth episode to chronic`)) * (1 - relapseprevention_rate_45) * (1 - mildcuration_rate_45)
  transition_matrix["mild_chronic", "mild_chronic",45] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
  
  ## Moderate
  
  ### Moderate 1x
  transition_matrix["moderate_incidence", "moderate_1x_1_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.4) * (1 - moderatecuration_rate_45)
  transition_matrix["moderate_incidence", "moderate_1x_2_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.3) * (1 - moderatecuration_rate_45)
  transition_matrix["moderate_incidence", "moderate_1x_3_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.2) * (1 - moderatecuration_rate_45)
  transition_matrix["moderate_incidence", "moderate_1x_4_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.07) * (1 - moderatecuration_rate_45)
  transition_matrix["moderate_incidence", "moderate_1x_5_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.03) * (1 - moderatecuration_rate_45)
  
  ### moderate 2x
  transition_matrix["moderate_1x_1_year", "moderate_2x_1_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.4) * (1 - moderatecuration_rate_45)
  transition_matrix["moderate_1x_1_year", "moderate_2x_2_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.3) * (1 - moderatecuration_rate_45)
  transition_matrix["moderate_1x_1_year", "moderate_2x_3_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.2) * (1 - moderatecuration_rate_45)
  transition_matrix["moderate_1x_1_year", "moderate_2x_4_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.07) * (1 - moderatecuration_rate_45)
  transition_matrix["moderate_1x_1_year", "moderate_2x_5_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.03) * (1 - moderatecuration_rate_45)
  
  ### moderate 3x
  transition_matrix["moderate_2x_1_year", "moderate_3x_1_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.4) * (1 - moderatecuration_rate_45)
  transition_matrix["moderate_2x_1_year", "moderate_3x_2_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.3) * (1 - moderatecuration_rate_45)
  transition_matrix["moderate_2x_1_year", "moderate_3x_3_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.2) * (1 - moderatecuration_rate_45)
  transition_matrix["moderate_2x_1_year", "moderate_3x_4_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.07) * (1 - moderatecuration_rate_45)
  transition_matrix["moderate_2x_1_year", "moderate_3x_5_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.03) * (1 - moderatecuration_rate_45)
  
  ### moderate 4x
  transition_matrix["moderate_3x_1_year", "moderate_4x_1_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.4) * (1 - moderatecuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["moderate_3x_1_year", "moderate_4x_2_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.3) * (1 - moderatecuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["moderate_3x_1_year", "moderate_4x_3_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.2) * (1 - moderatecuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["moderate_3x_1_year", "moderate_4x_4_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.07) * (1 - moderatecuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["moderate_3x_1_year", "moderate_4x_5_year", 45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.03) * (1 - moderatecuration_rate_45) * (1 - relapseprevention_rate_45)
  
  ### moderate 5x
  transition_matrix["moderate_4x_1_year", "moderate_5x_1_year",45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.4) * (1 - moderatecuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["moderate_4x_1_year", "moderate_5x_2_year",45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.3) * (1 - moderatecuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["moderate_4x_1_year", "moderate_5x_3_year",45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.2) * (1 - moderatecuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["moderate_4x_1_year", "moderate_5x_4_year",45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.07) * (1 - moderatecuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["moderate_4x_1_year", "moderate_5x_5_year",45] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.03) * (1 - moderatecuration_rate_45) * (1 - relapseprevention_rate_45)
  
  ### moderate - chronic
  transition_matrix["moderate_incidence", "moderate_chronic", 45] <- (leavemodel/pmoderate * ((pmoderate * moderatechronic))) * (1 - moderatecuration_rate_45)
  transition_matrix["moderate_1x_1_year", "moderate_chronic", 45] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1 - moderatecuration_rate_45)
  transition_matrix["moderate_2x_1_year", "moderate_chronic", 45] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1- moderatecuration_rate_45)
  transition_matrix["moderate_3x_1_year", "moderate_chronic", 45] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1 - moderatecuration_rate_45) * (1-relapseprevention_rate_45)
  transition_matrix["moderate_4x_1_year", "moderate_chronic", 45] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1 - moderatecuration_rate_45) * (1-relapseprevention_rate_45)
  transition_matrix["moderate_5x_1_year", "moderate_chronic", 45] <-  (1 - (1-leavemodel) - (leavemodel/pmoderate * ((pmoderate * moderaterecovery * (1-(1-(moderaterecoverycured*(1-`increased relapse 5`))) * relapsecure) + pmoderate * moderatepartial * (1-(1-(moderatepartialcured * (1- `increased relapse 5`)))* relapsecure))) + `Fifth episode to chronic`)) * (1 - relapseprevention_rate_45) * (1 - moderatecuration_rate_45)
  transition_matrix["moderate_chronic", "moderate_chronic",45] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
  
  ## Severe
  
  ### severe 1x
  transition_matrix["severe_incidence", "severe_1x_1_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.4) * (1 - severecuration_rate_45)
  transition_matrix["severe_incidence", "severe_1x_2_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.3) * (1 - severecuration_rate_45)
  transition_matrix["severe_incidence", "severe_1x_3_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.2) * (1 - severecuration_rate_45)
  transition_matrix["severe_incidence", "severe_1x_4_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.07) * (1 - severecuration_rate_45)
  transition_matrix["severe_incidence", "severe_1x_5_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.03) * (1 - severecuration_rate_45)
  
  ### severe 2x
  transition_matrix["severe_1x_1_year", "severe_2x_1_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.4) * (1 - severecuration_rate_45)
  transition_matrix["severe_1x_1_year", "severe_2x_2_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.3) * (1 - severecuration_rate_45)
  transition_matrix["severe_1x_1_year", "severe_2x_3_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.2) * (1 - severecuration_rate_45)
  transition_matrix["severe_1x_1_year", "severe_2x_4_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.07) * (1 - severecuration_rate_45)
  transition_matrix["severe_1x_1_year", "severe_2x_5_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.03) * (1 - severecuration_rate_45)
  
  ### severe 3x
  transition_matrix["severe_2x_1_year", "severe_3x_1_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.4) * (1 - severecuration_rate_45)
  transition_matrix["severe_2x_1_year", "severe_3x_2_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.3) * (1 - severecuration_rate_45)
  transition_matrix["severe_2x_1_year", "severe_3x_3_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.2) * (1 - severecuration_rate_45)
  transition_matrix["severe_2x_1_year", "severe_3x_4_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.07) * (1 - severecuration_rate_45)
  transition_matrix["severe_2x_1_year", "severe_3x_5_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.03) * (1 - severecuration_rate_45)
  
  ### severe 4x
  transition_matrix["severe_3x_1_year", "severe_4x_1_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.4) * (1 - severecuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["severe_3x_1_year", "severe_4x_2_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.3) * (1 - severecuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["severe_3x_1_year", "severe_4x_3_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.2) * (1 - severecuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["severe_3x_1_year", "severe_4x_4_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.07) * (1 - severecuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["severe_3x_1_year", "severe_4x_5_year", 45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.03) * (1 - severecuration_rate_45) * (1 - relapseprevention_rate_45)
  
  ### severe 5x
  transition_matrix["severe_4x_1_year", "severe_5x_1_year",45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.4) * (1 - severecuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["severe_4x_1_year", "severe_5x_2_year",45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.3) * (1 - severecuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["severe_4x_1_year", "severe_5x_3_year",45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.2) * (1 - severecuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["severe_4x_1_year", "severe_5x_4_year",45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.07) * (1 - severecuration_rate_45) * (1 - relapseprevention_rate_45)
  transition_matrix["severe_4x_1_year", "severe_5x_5_year",45] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.03) * (1 - severecuration_rate_45) * (1 - relapseprevention_rate_45)
  
  ### severe - chronic
  transition_matrix["severe_incidence", "severe_chronic", 45] <- (leavemodel/psevere * ((psevere * severechronic))) * (1 - severecuration_rate_45)
  transition_matrix["severe_1x_1_year", "severe_chronic", 45] <- (leavemodel/psevere * ((psevere*severechronic))) * (1 - severecuration_rate_45)
  transition_matrix["severe_2x_1_year", "severe_chronic", 45] <- (leavemodel/psevere * ((psevere*severechronic))) * (1- severecuration_rate_45)
  transition_matrix["severe_3x_1_year", "severe_chronic", 45] <- (leavemodel/psevere * ((psevere*severechronic))) * (1 - severecuration_rate_45) * (1-relapseprevention_rate_45)
  transition_matrix["severe_4x_1_year", "severe_chronic", 45] <- (leavemodel/psevere * ((psevere*severechronic))) * (1 - severecuration_rate_45) * (1-relapseprevention_rate_45)
  transition_matrix["severe_5x_1_year", "severe_chronic", 45] <-  (1- (1-leavemodel) - (`Fifth episode to chronic`)) * (1 - relapseprevention_rate_45) * (1 - severecuration_rate_45)
  transition_matrix["severe_chronic", "severe_chronic",45] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
  
  ##Year 46 probabilities
  prevention_random_46 <-  runif(1,0,1)
  mildcuration_random_46 <- runif(1,0,1)
  moderatecuration_random_46 <- runif(1,0,1)
  severecuration_random_46 <- runif(1,0,1)
  relapseprevention_random_46 <- runif(1,0,1)
  
  prevention_rate_46 <- ifelse(preventionfactor == 0, 0, qbeta(prevention_random_46, preventionfactor*30,30-preventionfactor))
  mildcuration_rate_46 <- ifelse(mildcuration_int == 0,0, qbeta(mildcuration_random_46, mildcuration_int*30,30-mildcuration_int))
  moderatecuration_rate_46 <- ifelse(modcuration_int == 0,0, qbeta(moderatecuration_random_46, modcuration_int*30, 30-modcuration_int))
  severecuration_rate_46 <- ifelse(sevcuration_int == 0,0, qbeta(severecuration_random_46, sevcuration_int*30, 30-sevcuration_int))
  relapseprevention_rate_46 <- ifelse(relapsefactor == 0,0, qbeta(relapseprevention_random_46, relapsefactor*30,30-relapsefactor))
  
  ## Mild
  
  ### Mild 1x
  transition_matrix["mild_incidence", "mild_1x_1_year", 46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.4) * (1 - mildcuration_rate_46)
  transition_matrix["mild_incidence", "mild_1x_2_year", 46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.3) * (1 - mildcuration_rate_46)
  transition_matrix["mild_incidence", "mild_1x_3_year", 46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.2) * (1 - mildcuration_rate_46)
  transition_matrix["mild_incidence", "mild_1x_4_year", 46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.07) * (1 - mildcuration_rate_46)
  transition_matrix["mild_incidence", "mild_1x_5_year", 46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * mildrecoveryrelapse + pmild * mildpartial * mildpartialrelapse)) * 0.03) * (1 - mildcuration_rate_46)
  
  ### Mild 2x
  transition_matrix["mild_1x_1_year", "mild_2x_1_year", 46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.4) * (1 - mildcuration_rate_46)
  transition_matrix["mild_1x_1_year", "mild_2x_2_year", 46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.3) * (1 - mildcuration_rate_46)
  transition_matrix["mild_1x_1_year", "mild_2x_3_year", 46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.2) * (1 - mildcuration_rate_46)
  transition_matrix["mild_1x_1_year", "mild_2x_4_year", 46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.07) * (1 - mildcuration_rate_46)
  transition_matrix["mild_1x_1_year", "mild_2x_5_year", 46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 1`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 1`))))) * 0.03) * (1 - mildcuration_rate_46)
  
  ### Mild 3x
  transition_matrix["mild_2x_1_year", "mild_3x_1_year", 46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.4) * (1 - mildcuration_rate_46)
  transition_matrix["mild_2x_1_year", "mild_3x_2_year", 46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.3) * (1 - mildcuration_rate_46)
  transition_matrix["mild_2x_1_year", "mild_3x_3_year", 46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.2) * (1 - mildcuration_rate_46)
  transition_matrix["mild_2x_1_year", "mild_3x_4_year", 46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.07) * (1 - mildcuration_rate_46)
  transition_matrix["mild_2x_1_year", "mild_3x_5_year", 46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 2`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 2`))))) * 0.03) * (1 - mildcuration_rate_46)
  
  ### Mild 4x
  transition_matrix["mild_3x_1_year", "mild_4x_1_year",46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.4) * (1 - mildcuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["mild_3x_1_year", "mild_4x_2_year",46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.3) * (1 - mildcuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["mild_3x_1_year", "mild_4x_3_year",46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.2) * (1 - mildcuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["mild_3x_1_year", "mild_4x_4_year",46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.07) * (1 - mildcuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["mild_3x_1_year", "mild_4x_5_year",46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 3`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 3`))))) * 0.03) * (1 - mildcuration_rate_46) * (1 - relapseprevention_rate_46)
  
  ### Mild 5x
  transition_matrix["mild_4x_1_year", "mild_5x_1_year",46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.4) * (1 - mildcuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["mild_4x_1_year", "mild_5x_2_year",46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.3) * (1 - mildcuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["mild_4x_1_year", "mild_5x_3_year",46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.2) * (1 - mildcuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["mild_4x_1_year", "mild_5x_4_year",46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.07) * (1 - mildcuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["mild_4x_1_year", "mild_5x_5_year",46] <- (leavemodel/pmild * (relapsecure * (pmild * mildrecovery * (1-(mildrecoverycured*(1-`increased relapse 4`))) + pmild * mildpartial * (1-(mildpartialcured * (1-`increased relapse 4`))))) * 0.03) * (1 - mildcuration_rate_46) * (1 - relapseprevention_rate_46)
  
  ### Mild - chronic
  transition_matrix["mild_incidence", "mild_chronic", 46] <- (leavemodel/pmild * ((pmild * mildchronic))) * (1 - mildcuration_rate_46)
  transition_matrix["mild_1x_1_year", "mild_chronic", 46] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1 - mildcuration_rate_46)
  transition_matrix["mild_2x_1_year", "mild_chronic", 46] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1- mildcuration_rate_46)
  transition_matrix["mild_3x_1_year", "mild_chronic", 46] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1 - mildcuration_rate_46) * (1-relapseprevention_rate_46)
  transition_matrix["mild_4x_1_year", "mild_chronic", 46] <- (leavemodel/pmild * ((pmild*mildchronic))) * (1 - mildcuration_rate_46) * (1-relapseprevention_rate_46)
  transition_matrix["mild_5x_1_year", "mild_chronic", 46] <-  (1 - (1-leavemodel) - (leavemodel/pmild * ((pmild * mildrecovery * (1-(1-(mildrecoverycured*(1-`increased relapse 5`))) * relapsecure) + pmild * mildpartial * (1-(1-(mildpartialcured * (1- `increased relapse 5`)))* relapsecure))) + `Fifth episode to chronic`)) * (1 - relapseprevention_rate_46) * (1 - mildcuration_rate_46)
  transition_matrix["mild_chronic", "mild_chronic",46] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
  
  ## Moderate
  
  ### Moderate 1x
  transition_matrix["moderate_incidence", "moderate_1x_1_year", 46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.4) * (1 - moderatecuration_rate_46)
  transition_matrix["moderate_incidence", "moderate_1x_2_year", 46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.3) * (1 - moderatecuration_rate_46)
  transition_matrix["moderate_incidence", "moderate_1x_3_year", 46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.2) * (1 - moderatecuration_rate_46)
  transition_matrix["moderate_incidence", "moderate_1x_4_year", 46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.07) * (1 - moderatecuration_rate_46)
  transition_matrix["moderate_incidence", "moderate_1x_5_year", 46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * moderaterecoveryrelapse + pmoderate * moderatepartial * moderatepartialrelapse)) * 0.03) * (1 - moderatecuration_rate_46)
  
  ### moderate 2x
  transition_matrix["moderate_1x_1_year", "moderate_2x_1_year", 46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.4) * (1 - moderatecuration_rate_46)
  transition_matrix["moderate_1x_1_year", "moderate_2x_2_year", 46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.3) * (1 - moderatecuration_rate_46)
  transition_matrix["moderate_1x_1_year", "moderate_2x_3_year", 46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.2) * (1 - moderatecuration_rate_46)
  transition_matrix["moderate_1x_1_year", "moderate_2x_4_year", 46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.07) * (1 - moderatecuration_rate_46)
  transition_matrix["moderate_1x_1_year", "moderate_2x_5_year", 46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 1`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 1`))))) * 0.03) * (1 - moderatecuration_rate_46)
  
  ### moderate 3x
  transition_matrix["moderate_2x_1_year", "moderate_3x_1_year", 46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.4) * (1 - moderatecuration_rate_46)
  transition_matrix["moderate_2x_1_year", "moderate_3x_2_year", 46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.3) * (1 - moderatecuration_rate_46)
  transition_matrix["moderate_2x_1_year", "moderate_3x_3_year", 46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.2) * (1 - moderatecuration_rate_46)
  transition_matrix["moderate_2x_1_year", "moderate_3x_4_year", 46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.07) * (1 - moderatecuration_rate_46)
  transition_matrix["moderate_2x_1_year", "moderate_3x_5_year", 46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 2`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 2`))))) * 0.03) * (1 - moderatecuration_rate_46)
  
  ### moderate 4x
  transition_matrix["moderate_3x_1_year", "moderate_4x_1_year",46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.4) * (1 - moderatecuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["moderate_3x_1_year", "moderate_4x_2_year",46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.3) * (1 - moderatecuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["moderate_3x_1_year", "moderate_4x_3_year",46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.2) * (1 - moderatecuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["moderate_3x_1_year", "moderate_4x_4_year",46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.07) * (1 - moderatecuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["moderate_3x_1_year", "moderate_4x_5_year",46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 3`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 3`))))) * 0.03) * (1 - moderatecuration_rate_46) * (1 - relapseprevention_rate_46)
  
  ### moderate 5x
  transition_matrix["moderate_4x_1_year", "moderate_5x_1_year",46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.4) * (1 - moderatecuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["moderate_4x_1_year", "moderate_5x_2_year",46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.3) * (1 - moderatecuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["moderate_4x_1_year", "moderate_5x_3_year",46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.2) * (1 - moderatecuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["moderate_4x_1_year", "moderate_5x_4_year",46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.07) * (1 - moderatecuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["moderate_4x_1_year", "moderate_5x_5_year",46] <- (leavemodel/pmoderate * (relapsecure * (pmoderate * moderaterecovery * (1-(moderaterecoverycured*(1-`increased relapse 4`))) + pmoderate * moderatepartial * (1-(moderatepartialcured * (1-`increased relapse 4`))))) * 0.03) * (1 - moderatecuration_rate_46) * (1 - relapseprevention_rate_46)
  
  ### moderate - chronic
  transition_matrix["moderate_incidence", "moderate_chronic", 46] <- (leavemodel/pmoderate * ((pmoderate * moderatechronic))) * (1 - moderatecuration_rate_46)
  transition_matrix["moderate_1x_1_year", "moderate_chronic", 46] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1 - moderatecuration_rate_46)
  transition_matrix["moderate_2x_1_year", "moderate_chronic", 46] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1- moderatecuration_rate_46)
  transition_matrix["moderate_3x_1_year", "moderate_chronic", 46] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1 - moderatecuration_rate_46) * (1-relapseprevention_rate_46)
  transition_matrix["moderate_4x_1_year", "moderate_chronic", 46] <- (leavemodel/pmoderate * ((pmoderate*moderatechronic))) * (1 - moderatecuration_rate_46) * (1-relapseprevention_rate_46)
  transition_matrix["moderate_5x_1_year", "moderate_chronic", 46] <-  (1 - (1-leavemodel) - (leavemodel/pmoderate * ((pmoderate * moderaterecovery * (1-(1-(moderaterecoverycured*(1-`increased relapse 5`))) * relapsecure) + pmoderate * moderatepartial * (1-(1-(moderatepartialcured * (1- `increased relapse 5`)))* relapsecure))) + `Fifth episode to chronic`)) * (1 - relapseprevention_rate_46) * (1 - moderatecuration_rate_46)
  transition_matrix["moderate_chronic", "moderate_chronic",46] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
  
  ## Severe
  
  ### severe 1x
  transition_matrix["severe_incidence", "severe_1x_1_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.4) * (1 - severecuration_rate_46)
  transition_matrix["severe_incidence", "severe_1x_2_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.3) * (1 - severecuration_rate_46)
  transition_matrix["severe_incidence", "severe_1x_3_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.2) * (1 - severecuration_rate_46)
  transition_matrix["severe_incidence", "severe_1x_4_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.07) * (1 - severecuration_rate_46)
  transition_matrix["severe_incidence", "severe_1x_5_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * severerecoveryrelapse + psevere * severepartial * severepartialrelapse)) * 0.03) * (1 - severecuration_rate_46)
  
  ### severe 2x
  transition_matrix["severe_1x_1_year", "severe_2x_1_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.4) * (1 - severecuration_rate_46)
  transition_matrix["severe_1x_1_year", "severe_2x_2_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.3) * (1 - severecuration_rate_46)
  transition_matrix["severe_1x_1_year", "severe_2x_3_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.2) * (1 - severecuration_rate_46)
  transition_matrix["severe_1x_1_year", "severe_2x_4_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.07) * (1 - severecuration_rate_46)
  transition_matrix["severe_1x_1_year", "severe_2x_5_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 1`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 1`))))) * 0.03) * (1 - severecuration_rate_46)
  
  ### severe 3x
  transition_matrix["severe_2x_1_year", "severe_3x_1_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.4) * (1 - severecuration_rate_46)
  transition_matrix["severe_2x_1_year", "severe_3x_2_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.3) * (1 - severecuration_rate_46)
  transition_matrix["severe_2x_1_year", "severe_3x_3_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.2) * (1 - severecuration_rate_46)
  transition_matrix["severe_2x_1_year", "severe_3x_4_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.07) * (1 - severecuration_rate_46)
  transition_matrix["severe_2x_1_year", "severe_3x_5_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 2`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 2`))))) * 0.03) * (1 - severecuration_rate_46)
  
  ### severe 4x
  transition_matrix["severe_3x_1_year", "severe_4x_1_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.4) * (1 - severecuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["severe_3x_1_year", "severe_4x_2_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.3) * (1 - severecuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["severe_3x_1_year", "severe_4x_3_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.2) * (1 - severecuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["severe_3x_1_year", "severe_4x_4_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.07) * (1 - severecuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["severe_3x_1_year", "severe_4x_5_year", 46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 3`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 3`))))) * 0.03) * (1 - severecuration_rate_46) * (1 - relapseprevention_rate_46)
  
  ### severe 5x
  transition_matrix["severe_4x_1_year", "severe_5x_1_year",46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.4) * (1 - severecuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["severe_4x_1_year", "severe_5x_2_year",46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.3) * (1 - severecuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["severe_4x_1_year", "severe_5x_3_year",46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.2) * (1 - severecuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["severe_4x_1_year", "severe_5x_4_year",46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.07) * (1 - severecuration_rate_46) * (1 - relapseprevention_rate_46)
  transition_matrix["severe_4x_1_year", "severe_5x_5_year",46] <- (leavemodel/psevere * (relapsecure * (psevere * severerecovery * (1-(severerecoverycured*(1-`increased relapse 4`))) + psevere * severepartial * (1-(severepartialcured * (1-`increased relapse 4`))))) * 0.03) * (1 - severecuration_rate_46) * (1 - relapseprevention_rate_46)
  
  ### severe - chronic
  transition_matrix["severe_incidence", "severe_chronic", 46] <- (leavemodel/psevere * ((psevere * severechronic))) * (1 - severecuration_rate_46)
  transition_matrix["severe_1x_1_year", "severe_chronic", 46] <- (leavemodel/psevere * ((psevere*severechronic))) * (1 - severecuration_rate_46)
  transition_matrix["severe_2x_1_year", "severe_chronic", 46] <- (leavemodel/psevere * ((psevere*severechronic))) * (1- severecuration_rate_46)
  transition_matrix["severe_3x_1_year", "severe_chronic", 46] <- (leavemodel/psevere * ((psevere*severechronic))) * (1 - severecuration_rate_46) * (1-relapseprevention_rate_46)
  transition_matrix["severe_4x_1_year", "severe_chronic", 46] <- (leavemodel/psevere * ((psevere*severechronic))) * (1 - severecuration_rate_46) * (1-relapseprevention_rate_46)
  transition_matrix["severe_5x_1_year", "severe_chronic", 46] <-  (1- (1-leavemodel) - (`Fifth episode to chronic`)) * (1 - relapseprevention_rate_46) * (1 - severecuration_rate_46)
  transition_matrix["severe_chronic", "severe_chronic",46] <- 2^(-1/(`mean duration of chronicity (year)` * log(2))) * (leavemodel)
  
  return(transition_matrix)
}

# Population to year 41 
population <- array(0,
                    dim = c(47, length(states)),
                    dimnames = list(Year = 0:46, state = states))

population[1, "mild_incidence"] <- Incidence * pmild
population[1, "moderate_incidence"] <- Incidence * pmoderate 
population[1, "severe_incidence"] <- Incidence * psevere


for(i in 2:47){
  population[i, ] <- population[i-1, ] %*% transition_matrix[, , i-1]
}

## Take the incidences of the population at year 41 to start the 5 year model
incidence_population_start <- population[42,]


# Calculate the transition probabilities for the 5 years for the base and alternative case --------

## Base case transition probabilities
simulation_tm_results_base <- vector("list", `Nr of simulation runs`)


for(i in 1:`Nr of simulation runs`){
  simulation_tm_results_base[[i]] <- func_sim_tm(transition_matrix = transition_matrix, retrieve = "base_case")
}

### Take only the last five years of probabilities (year 42 - year 46)

simulation_tm_results_base_5 <- lapply(simulation_tm_results_base, 
                                       function(x) 
                                         x[,,43:47])

## Alternative case transition probabilities
simulation_tm_results_alt <- vector("list", `Nr of simulation runs`)


for(i in 1:`Nr of simulation runs`){
  simulation_tm_results_alt[[i]] <- func_sim_tm(transition_matrix = transition_matrix, retrieve = "alt_case")
}

### Take only the last five years of probabilities (year 42 - year 46)
simulation_tm_results_alt_5 <- lapply(simulation_tm_results_alt, 
                                      function(x) 
                                        x[,,43:47])



# Now simulate the 5 years of the model 1000 times over the base and alternative model -------------------------

## Base population
base_population <- array(0,
                         dim = c(6, length(states)),
                         dimnames = list(Year = 0:5, state = states))


base_population[1, ] <- incidence_population_start

base_population_sim <- lapply(1:`Nr of simulation runs`, function(x){
  base_population
})

for(sim in 1:`Nr of simulation runs`){
  population_matrix <- base_population_sim[[sim]]
  transition_array <- simulation_tm_results_base_5[[sim]]
  
  for(i in 2:6){
    population_matrix[i,] <- population_matrix[i-1, ] %*% transition_array[,,i-1]
  }
  
  
}

## Alternative population
alt_population <- array(0,
                        dim = c(6, length(states)),
                        dimnames = list(Year = 0:5, state = states))


alt_population[1, ] <- incidence_population_start

alt_population_sim <- lapply(1:`Nr of simulation runs`, function(x){
  alt_population
})

for(sim in 1:`Nr of simulation runs`){
  population_matrix_alt <- alt_population_sim[[sim]]
  transition_array_alt <- simulation_tm_results_alt_5[[sim]]
  
  for(i in 2:6){
    population_matrix_alt[i,] <- population_matrix_alt[i-1, ] %*% transition_array_alt[,,i-1]
  }
  
  alt_population_sim[[sim]] <- population_matrix_alt
}



# Store costs and qalys in a list -----------------------------------------
## Calculate the  population, dw, costs & QALYs averted per year per state
### Base population 
base_population_sim <- lapply(base_population_sim, function(base_population){
  mild_population <- rowSums(base_population[, c("mild_incidence", 
                                                 "mild_1x_1_year", 
                                                 "mild_2x_1_year", 
                                                 "mild_3x_1_year", 
                                                 "mild_4x_1_year",
                                                 "mild_5x_1_year",
                                                 "mild_chronic")])
  
  moderate_population <- rowSums(base_population[, c("moderate_incidence", 
                                                     "moderate_1x_1_year", 
                                                     "moderate_2x_1_year", 
                                                     "moderate_3x_1_year", 
                                                     "moderate_4x_1_year",
                                                     "moderate_5x_1_year",
                                                     "moderate_chronic")])
  
  severe_population <- rowSums(base_population[, c("severe_incidence", 
                                                   "severe_1x_1_year", 
                                                   "severe_2x_1_year", 
                                                   "severe_3x_1_year", 
                                                   "severe_4x_1_year",
                                                   "severe_5x_1_year",
                                                   "severe_chronic")])
  
  population_matrix <- cbind(population_matrix, 
                             "mild_population" = mild_population,
                             "moderate_population" = moderate_population,
                             "severe_population" = severe_population)
})

### Random probabilities for dw array
dw_p_array <- array(NA_real_,
                    dim = c(3,6),
                    dimnames = list(State = c("mild", "moderate", "severe"),
                                    Year = 0:5))

### Simulate random probabilities based on nr of simulations
dw_p_array_sim <- replicate(`Nr of simulation runs`, {
  
  dw_p_array["mild",] <- runif(6, 0, 1)
  dw_p_array["moderate",] <- runif(6, 0, 1)
  dw_p_array["severe",] <- runif(6, 0, 1)
  
  return(dw_p_array)
}, simplify = FALSE)

### Simulate dw reduction coefficients based on nr of simulations
dw_red_array <- array(NA_real_,
                      dim = c(3,6),
                      dimnames = list(State = c("mild", "moderate", "severe"),
                                      Year = 0:5))

dw_red_array_sim <- lapply(dw_p_array_sim, function(dw_p_array){
  
  dw_red_array["mild", 1] <- qnorm(dw_p_array["mild",1], mild, (mild/6))
  dw_red_array["mild", 2] <- qnorm(dw_p_array["mild",2], mild, (mild/6))
  dw_red_array["mild", 3] <- qnorm(dw_p_array["mild",3], mild, (mild/6))
  dw_red_array["mild", 4] <- qnorm(dw_p_array["mild",4], mild, (mild/6))
  dw_red_array["mild", 5] <- qnorm(dw_p_array["mild",5], mild, (mild/6))
  dw_red_array["mild", 6] <- qnorm(dw_p_array["mild",6], mild, (mild/6))
  
  dw_red_array["moderate", 1] <- qnorm(dw_p_array["moderate",1], moderate, (moderate/6))
  dw_red_array["moderate", 2] <- qnorm(dw_p_array["moderate",2], moderate, (moderate/6))
  dw_red_array["moderate", 3] <- qnorm(dw_p_array["moderate",3], moderate, (moderate/6))
  dw_red_array["moderate", 4] <- qnorm(dw_p_array["moderate",4], moderate, (moderate/6))
  dw_red_array["moderate", 5] <- qnorm(dw_p_array["moderate",5], moderate, (moderate/6))
  dw_red_array["moderate", 6] <- qnorm(dw_p_array["moderate",6], moderate, (moderate/6))
  
  dw_red_array["severe", 1] <- qnorm(dw_p_array["severe",1], severe, (severe/6))
  dw_red_array["severe", 2] <- qnorm(dw_p_array["severe",2], severe, (severe/6))
  dw_red_array["severe", 3] <- qnorm(dw_p_array["severe",3], severe, (severe/6))
  dw_red_array["severe", 4] <- qnorm(dw_p_array["severe",4], severe, (severe/6))
  dw_red_array["severe", 5] <- qnorm(dw_p_array["severe",5], severe, (severe/6))
  dw_red_array["severe", 6] <- qnorm(dw_p_array["severe",6], severe, (severe/6))
  
  return(dw_red_array)
  
})

change_in_daly_base <-  array(NA_real_,
                              dim = c(6,8),
                              dimnames = list(Year = 0:5,
                                              DW = c("dwmild", "dwmoderate", "dwsevere", "mild", "moderate", "severe", "averted", "total dalys")))


change_in_daly_base_sim <- lapply(1:length(base_population_sim), function(i){
  
  
  base_population <- base_population_sim[[i]]
  dw_red_array <- dw_red_array_sim[[i]]
  
  #DW
  change_in_daly_base[1, "dwmild"] <- base_population[1, "mild_population"] * `dwmild fixed` * yldmild
  change_in_daly_base[2, "dwmild"] <- base_population[2, "mild_population"] * `dwmild fixed` * yldmild
  change_in_daly_base[3, "dwmild"] <- base_population[3, "mild_population"] * `dwmild fixed` * yldmild
  change_in_daly_base[4, "dwmild"] <- base_population[4, "mild_population"] * `dwmild fixed` * yldmild
  change_in_daly_base[5, "dwmild"] <- base_population[5, "mild_population"] * `dwmild fixed` * yldmild
  change_in_daly_base[6, "dwmild"] <- base_population[6, "mild_population"] * `dwmild fixed` * yldmild
  
  
  change_in_daly_base[1, "dwmoderate"] <- base_population[1, "moderate_population"] * `dwmoderate fixed` * yldmoderate
  change_in_daly_base[2, "dwmoderate"] <- base_population[2, "moderate_population"] * `dwmoderate fixed` * yldmoderate
  change_in_daly_base[3, "dwmoderate"] <- base_population[3, "moderate_population"] * `dwmoderate fixed` * yldmoderate
  change_in_daly_base[4, "dwmoderate"] <- base_population[4, "moderate_population"] * `dwmoderate fixed`* yldmoderate
  change_in_daly_base[5, "dwmoderate"] <- base_population[5, "moderate_population"] * `dwmoderate fixed` * yldmoderate
  change_in_daly_base[6, "dwmoderate"] <- base_population[6, "moderate_population"] * `dwmoderate fixed` * yldmoderate
  
  
  change_in_daly_base[1, "dwsevere"] <- base_population[1, "severe_population"] * `dwsevere fixed` * yldsevere
  change_in_daly_base[2, "dwsevere"] <- base_population[2, "severe_population"] * `dwsevere fixed` * yldsevere
  change_in_daly_base[3, "dwsevere"] <- base_population[3, "severe_population"] * `dwsevere fixed` * yldsevere
  change_in_daly_base[4, "dwsevere"] <- base_population[4, "severe_population"] * `dwsevere fixed` * yldsevere
  change_in_daly_base[5, "dwsevere"] <- base_population[5, "severe_population"] * `dwsevere fixed` * yldsevere
  change_in_daly_base[6, "dwsevere"] <- base_population[5, "severe_population"] * `dwsevere fixed` * yldsevere
  
  #DALYs
  
  change_in_daly_base[2, "mild"] <- base_population[2, "mild_population"] * dw_red_array["mild", 2] * yldmild/ ((1 + `discount rate daly averted`) ^ 0.50)
  change_in_daly_base[3, "mild"] <- base_population[3, "mild_population"] * dw_red_array["mild", 3] * yldmild/ ((1 + `discount rate daly averted`) ^ 1.50)
  change_in_daly_base[4, "mild"] <- base_population[4, "mild_population"] * dw_red_array["mild", 4] * yldmild/ ((1 + `discount rate daly averted`) ^ 2.50)
  change_in_daly_base[5, "mild"] <- base_population[5, "mild_population"] * dw_red_array["mild", 5] * yldmild/ ((1 + `discount rate daly averted`) ^ 3.50)
  change_in_daly_base[6, "mild"] <- base_population[6, "mild_population"] * dw_red_array["mild", 6] * yldmild/ ((1 + `discount rate daly averted`) ^ 4.50)
  
  change_in_daly_base[2, "moderate"] <- base_population[2, "moderate_population"] * dw_red_array["moderate", 2] * yldmoderate/ ((1 + `discount rate daly averted`) ^ 0.50)
  change_in_daly_base[3, "moderate"] <- base_population[3, "moderate_population"] * dw_red_array["moderate", 3] * yldmoderate/ ((1 + `discount rate daly averted`) ^ 1.50)
  change_in_daly_base[4, "moderate"] <- base_population[4, "moderate_population"] * dw_red_array["moderate", 4] * yldmoderate/ ((1 + `discount rate daly averted`) ^ 2.50)
  change_in_daly_base[5, "moderate"] <- base_population[5, "moderate_population"] * dw_red_array["moderate", 5] * yldmoderate/ ((1 + `discount rate daly averted`) ^ 3.50)
  change_in_daly_base[6, "moderate"] <- base_population[6, "moderate_population"] * dw_red_array["moderate", 6] * yldmoderate/ ((1 + `discount rate daly averted`) ^ 4.50)
  
  change_in_daly_base[2, "severe"] <- base_population[2, "severe_population"] * dw_red_array["severe", 2] * yldsevere/ ((1 + `discount rate daly averted`) ^ 0.50)
  change_in_daly_base[3, "severe"] <- base_population[3, "severe_population"] * dw_red_array["severe", 3] * yldsevere/ ((1 + `discount rate daly averted`) ^ 1.50)
  change_in_daly_base[4, "severe"] <- base_population[4, "severe_population"] * dw_red_array["severe", 4] * yldsevere/ ((1 + `discount rate daly averted`) ^ 2.50)
  change_in_daly_base[5, "severe"] <- base_population[5, "severe_population"] * dw_red_array["severe", 5] * yldsevere/ ((1 + `discount rate daly averted`) ^ 3.50)
  change_in_daly_base[6, "severe"] <- base_population[6, "severe_population"] * dw_red_array["severe", 6] * yldsevere/ ((1 + `discount rate daly averted`) ^ 4.50)
  
  change_in_daly_base[2, "averted"] <- -(change_in_daly_base[2, "dwmild"] - change_in_daly_base[1,"dwmild"] + change_in_daly_base[2, "dwmoderate"] - change_in_daly_base[1,"dwmoderate"] + change_in_daly_base[2,"dwsevere"] - change_in_daly_base[1,"dwsevere"]) / (1 + `discount rate daly averted`)^0.50
  change_in_daly_base[3, "averted"] <- -(change_in_daly_base[3, "dwmild"] - change_in_daly_base[1,"dwmild"] + change_in_daly_base[3, "dwmoderate"] - change_in_daly_base[1,"dwmoderate"] + change_in_daly_base[3,"dwsevere"] - change_in_daly_base[1,"dwsevere"]) / (1 + `discount rate daly averted`)^1.50
  change_in_daly_base[4, "averted"] <- -(change_in_daly_base[4, "dwmild"] - change_in_daly_base[1,"dwmild"] + change_in_daly_base[4, "dwmoderate"] - change_in_daly_base[1,"dwmoderate"] + change_in_daly_base[4,"dwsevere"] - change_in_daly_base[1,"dwsevere"]) / (1 + `discount rate daly averted`)^2.50
  change_in_daly_base[5, "averted"] <- -(change_in_daly_base[5, "dwmild"] - change_in_daly_base[1,"dwmild"] + change_in_daly_base[5, "dwmoderate"] - change_in_daly_base[1,"dwmoderate"] + change_in_daly_base[5,"dwsevere"] - change_in_daly_base[1,"dwsevere"]) / (1 + `discount rate daly averted`)^3.50
  change_in_daly_base[6, "averted"] <- -(change_in_daly_base[6, "dwmild"] - change_in_daly_base[1,"dwmild"] + change_in_daly_base[6, "dwmoderate"] - change_in_daly_base[1,"dwmoderate"] + change_in_daly_base[6,"dwsevere"] - change_in_daly_base[1,"dwsevere"]) / (1 + `discount rate daly averted`)^4.50
  
  change_in_daly_base[2, "averted"] <- -(change_in_daly_base[2, "dwmild"] - change_in_daly_base[1,"dwmild"] + change_in_daly_base[2, "dwmoderate"] - change_in_daly_base[1,"dwmoderate"] + change_in_daly_base[2,"dwsevere"] - change_in_daly_base[1,"dwsevere"]) / (1 + `discount rate daly averted`)^0.50
  change_in_daly_base[3, "averted"] <- -(change_in_daly_base[3, "dwmild"] - change_in_daly_base[1,"dwmild"] + change_in_daly_base[3, "dwmoderate"] - change_in_daly_base[1,"dwmoderate"] + change_in_daly_base[3,"dwsevere"] - change_in_daly_base[1,"dwsevere"]) / (1 + `discount rate daly averted`)^1.50
  change_in_daly_base[4, "averted"] <- -(change_in_daly_base[4, "dwmild"] - change_in_daly_base[1,"dwmild"] + change_in_daly_base[4, "dwmoderate"] - change_in_daly_base[1,"dwmoderate"] + change_in_daly_base[4,"dwsevere"] - change_in_daly_base[1,"dwsevere"]) / (1 + `discount rate daly averted`)^2.50
  change_in_daly_base[5, "averted"] <- -(change_in_daly_base[5, "dwmild"] - change_in_daly_base[1,"dwmild"] + change_in_daly_base[5, "dwmoderate"] - change_in_daly_base[1,"dwmoderate"] + change_in_daly_base[5,"dwsevere"] - change_in_daly_base[1,"dwsevere"]) / (1 + `discount rate daly averted`)^3.50
  change_in_daly_base[6, "averted"] <- -(change_in_daly_base[6, "dwmild"] - change_in_daly_base[1,"dwmild"] + change_in_daly_base[6, "dwmoderate"] - change_in_daly_base[1,"dwmoderate"] + change_in_daly_base[6,"dwsevere"] - change_in_daly_base[1,"dwsevere"]) / (1 + `discount rate daly averted`)^4.50
  
  change_in_daly_base[2, "total dalys"] <- change_in_daly_base[2, "mild"] + change_in_daly_base[2, "moderate"] + change_in_daly_base[2, "severe"] + change_in_daly_base[2, "averted"]
  change_in_daly_base[3, "total dalys"] <- change_in_daly_base[3, "mild"] + change_in_daly_base[3, "moderate"] + change_in_daly_base[3, "severe"] + change_in_daly_base[3, "averted"]
  change_in_daly_base[4, "total dalys"] <- change_in_daly_base[4, "mild"] + change_in_daly_base[4, "moderate"] + change_in_daly_base[4, "severe"] + change_in_daly_base[4, "averted"]
  change_in_daly_base[5, "total dalys"] <- change_in_daly_base[5, "mild"] + change_in_daly_base[5, "moderate"] + change_in_daly_base[5, "severe"] + change_in_daly_base[5, "averted"]
  change_in_daly_base[6, "total dalys"] <- change_in_daly_base[6, "mild"] + change_in_daly_base[6, "moderate"] + change_in_daly_base[6, "severe"] + change_in_daly_base[6, "averted"]
  
  
  
  return(change_in_daly_base)
})


# Calculate costs per year ------------------------------------------------

## Array costs per intervention
cost_p_array <- array(NA_real_,
                      dim = c(10,10),
                      dimnames = list(c(1:10),
                                      P = c("prev_sub", "prev_sub_costs", 
                                            "t_mild", "t_mild_costs",
                                            "t_mod", "t_mod_costs",
                                            "t_sev", "t_sev_costs",
                                            "prev_rec", "prev_rec_costs")))


cost_p_array_sim <- replicate(`Nr of simulation runs`, {
  cost_p_array[1:10, "prev_sub"] <- runif(10, 0, 1)
  cost_p_array[1:10, "t_mild"] <- runif(10, 0, 1)
  cost_p_array[1:10, "t_mod"] <- runif(10, 0, 1)
  cost_p_array[1:10, "t_sev"] <- runif(10, 0, 1)
  cost_p_array[1:10, "prev_rec"] <- runif(10, 0, 1)
  
  
  for(i in 1:10){
    cost_p_array[i, "prev_sub_costs"] <- ifelse(df_b_sub_clin[i, "healthcare costs"] == 0,0, qgamma(p = cost_p_array[i, "prev_sub"], shape = `Scale/shape Gamma cost distribution` * sqrt(df_b_sub_clin[i, "healthcare costs"]/`Scale/shape Gamma cost distribution`), scale = sqrt(df_b_sub_clin[i, "healthcare costs"]/`Scale/shape Gamma cost distribution`)))
    cost_p_array[i, "t_mild_costs"] <- ifelse(df_b_mild[i, "healthcare costs"] == 0,0, qgamma(p = cost_p_array[i, "t_mild"], shape = `Scale/shape Gamma cost distribution` * sqrt(df_b_mild[i, "healthcare costs"]/`Scale/shape Gamma cost distribution`), scale = sqrt(df_b_mild[i, "healthcare costs"]/`Scale/shape Gamma cost distribution`)))
    cost_p_array[i, "t_mod_costs"] <- ifelse(df_b_mod[i, "healthcare costs"] == 0,0, qgamma(p = cost_p_array[i, "t_mod"], shape = `Scale/shape Gamma cost distribution` * sqrt(df_b_mod[i, "healthcare costs"]/`Scale/shape Gamma cost distribution`), scale = sqrt(df_b_mod[i, "healthcare costs"]/`Scale/shape Gamma cost distribution`)))
    cost_p_array[i, "t_sev_costs"] <- ifelse(df_b_sev[i, "healthcare costs"] == 0,0, qgamma(p = cost_p_array[i, "t_sev"], shape = `Scale/shape Gamma cost distribution` * sqrt(df_b_sev[i, "healthcare costs"]/`Scale/shape Gamma cost distribution`), scale = sqrt(df_b_sev[i, "healthcare costs"]/`Scale/shape Gamma cost distribution`)))
    cost_p_array[i, "prev_rec_costs"] <- ifelse(df_b_rec_dep[i, "healthcare costs"] == 0,0, qgamma(p = cost_p_array[i, "prev_rec"], shape = `Scale/shape Gamma cost distribution` * sqrt(df_b_rec_dep[i, "healthcare costs"]/`Scale/shape Gamma cost distribution`), scale = sqrt(df_b_rec_dep[i, "healthcare costs"]/`Scale/shape Gamma cost distribution`)))
    
  }
  
  return(cost_p_array)
}, simplify = FALSE)


change_in_costs_base <-  array(NA_real_,
                               dim = c(6,4),
                               dimnames = list(Year = 0:5,
                                               Severity = c("mild", "moderate", "severe", "total")))

change_in_costs_base_sim <- lapply(1:length(base_population_sim), function(i){
  
  base_population <- base_population_sim[[i]]
  cost_p_array <- cost_p_array_sim[[i]]
  
  change_in_costs_base[2,"mild"] <- (base_population[2, "mild_population"]) * (sum(df_b_mild$cov * cost_p_array[ ,"t_mild_costs"], na.rm = TRUE)) / ((1+ `discount rate costs`)^0.5)
  change_in_costs_base[3,"mild"] <- (base_population[3, "mild_population"]) * (sum(df_b_mild$cov * cost_p_array[ ,"t_mild_costs"], na.rm = TRUE)) / ((1+ `discount rate costs`)^1.5)
  change_in_costs_base[4,"mild"] <- (base_population[4, "mild_population"]) * (sum(df_b_mild$cov * cost_p_array[ ,"t_mild_costs"], na.rm = TRUE)) / ((1+ `discount rate costs`)^2.5)
  change_in_costs_base[5,"mild"] <- (base_population[5, "mild_population"]) * (sum(df_b_mild$cov * cost_p_array[ ,"t_mild_costs"], na.rm = TRUE)) / ((1+ `discount rate costs`)^3.5)
  change_in_costs_base[6,"mild"] <- (base_population[6, "mild_population"]) * (sum(df_b_mild$cov * cost_p_array[ ,"t_mild_costs"], na.rm = TRUE)) / ((1+ `discount rate costs`)^4.5)
  
  change_in_costs_base[2,"moderate"] <- (base_population[2, "moderate_population"]) * (sum(df_b_mod$cov * cost_p_array[ ,"t_mod_costs"], na.rm = TRUE)) / ((1+ `discount rate costs`)^0.5)
  change_in_costs_base[3,"moderate"] <- (base_population[3, "moderate_population"]) * (sum(df_b_mod$cov * cost_p_array[ ,"t_mod_costs"], na.rm = TRUE)) / ((1+ `discount rate costs`)^1.5)
  change_in_costs_base[4,"moderate"] <- (base_population[4, "moderate_population"]) * (sum(df_b_mod$cov * cost_p_array[ ,"t_mod_costs"], na.rm = TRUE)) / ((1+ `discount rate costs`)^2.5)
  change_in_costs_base[5,"moderate"] <- (base_population[5, "moderate_population"]) * (sum(df_b_mod$cov * cost_p_array[ ,"t_mod_costs"], na.rm = TRUE)) / ((1+ `discount rate costs`)^3.5)
  change_in_costs_base[6,"moderate"] <- (base_population[6, "moderate_population"]) * (sum(df_b_mod$cov * cost_p_array[ ,"t_mod_costs"], na.rm = TRUE)) / ((1+ `discount rate costs`)^4.5)
  
  change_in_costs_base[2,"severe"] <- (base_population[2, "severe_population"]) * (sum(df_b_sev$cov * cost_p_array[ ,"t_sev_costs"], na.rm = TRUE)) / ((1+ `discount rate costs`)^0.5)
  change_in_costs_base[3,"severe"] <- (base_population[3, "severe_population"]) * (sum(df_b_sev$cov * cost_p_array[ ,"t_sev_costs"], na.rm = TRUE)) / ((1+ `discount rate costs`)^1.5)
  change_in_costs_base[4,"severe"] <- (base_population[4, "severe_population"]) * (sum(df_b_sev$cov * cost_p_array[ ,"t_sev_costs"], na.rm = TRUE)) / ((1+ `discount rate costs`)^2.5)
  change_in_costs_base[5,"severe"] <- (base_population[5, "severe_population"]) * (sum(df_b_sev$cov * cost_p_array[ ,"t_sev_costs"], na.rm = TRUE)) / ((1+ `discount rate costs`)^3.5)
  change_in_costs_base[6,"severe"] <- (base_population[6, "severe_population"]) * (sum(df_b_sev$cov * cost_p_array[ ,"t_sev_costs"], na.rm = TRUE)) / ((1+ `discount rate costs`)^4.5)
  
  change_in_costs_base[,"total"] <- rowSums(change_in_costs_base[,c("mild", "moderate", "severe")])
  
  return(change_in_costs_base)
})

# Eventual QALYs and costs for the 1000 simulations ------------------------------------------------
base_case_array <- array(NA_real_,
                         dim = c(`Nr of simulation runs`, 2),
                         dimnames = list(Run = 1:1000,
                                         Outcome = c("Cost", "QALYs")))

for(i in 1:`Nr of simulation runs`){
  change_in_costs_base <- change_in_costs_base_sim[[i]]
  change_in_daly_base <- change_in_daly_base_sim[[i]]
  
  base_case_array[i,"Cost"] <- sum(change_in_costs_base[,"total"], na.rm = TRUE)
  base_case_array[i, "QALYs"] <- sum(change_in_daly_base[,"total dalys"], na.rm = TRUE)
}
