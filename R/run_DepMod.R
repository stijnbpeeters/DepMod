#' Run base and alternative simulation models
#'
#' Wrapper for running the DepMod decision-analytic model under both base and
#' alternative scenarios. The function first builds the transition matrix using
#' \code{func_first_part_model()} and then runs \code{fun_sim_model()} for each
#' scenario.
#'
#' @param parameters Named list of model parameters (see Details).
#' @param sim_runs Integer. Number of simulation runs. Default is 1000.
#' @param total_population Integer. Total population size used in the simulation.
#'   Default is 10518000.
#' @param df_prev_sub_base Data frame for base scenario prevention
#'   (sub-clinical depression).
#' @param df_tr_mild_base Data frame for base scenario treatment (mild depression).
#' @param df_tr_mod_base Data frame for base scenario treatment
#'   (moderate depression).
#' @param df_tr_sev_base Data frame for base scenario treatment
#'   (severe depression).
#' @param df_prev_rec_base Data frame for base scenario prevention
#'   (recurrent depression).
#' @param df_prev_sub_alt Data frame for alternative scenario prevention
#'   (sub-clinical depression).
#' @param df_tr_mild_alt Data frame for alternative scenario treatment
#'   (mild depression).
#' @param df_tr_mod_alt Data frame for alternative scenario treatment
#'   (moderate depression).
#' @param df_tr_sev_alt Data frame for alternative scenario treatment
#'   (severe depression).
#' @param df_prev_rec_alt Data frame for alternative scenario prevention
#'   (recurrent depression).
#'
#' @details
#' The \code{parameters} list must contain numeric values controlling disease
#' progression, recovery, relapse, disability weights, discounting, and cost
#' accumulation. Required elements include:
#'
#' \strong{General simulation parameters}
#' \describe{
#'   \item{dw_conversion_fact}{Disability-weight conversion factor.}
#'   \item{discount_rate_daly}{Discount rate for DALYs.}
#'   \item{scale_shape_gamma_cost}{Gamma distribution scale/shape cost factor.}
#'   \item{disc_rate_cost}{Discount rate for economic costs.}
#'   \item{leavemodel}{Probability of leaving the model.}
#'   \item{mean_dur_chron}{Mean duration of chronic phase.}
#' }
#'
#' \strong{Population incidence inputs}
#' \describe{
#'   \item{incidence_no_history}{Incidence among individuals without prior disease.}
#'   \item{pmild}{Proportion of incident mild cases.}
#'   \item{pmoderate}{Proportion of incident moderate cases.}
#'   \item{psevere}{Proportion of incident severe cases.}
#' }
#'
#' \strong{Stage-progression probabilities}
#' \describe{
#'   \item{mildrecovery}{Recovery probability from mild depression.}
#'   \item{mildpartial}{Partial remission probability (mild).}
#'   \item{mildchronic}{Chronic transition probability (mild).}
#'
#'   \item{moderaterecovery}{Recovery probability (moderate).}
#'   \item{moderatepartial}{Partial remission probability (moderate).}
#'   \item{moderatechronic}{Chronic transition probability (moderate).}
#'
#'   \item{severerecovery}{Recovery probability (severe).}
#'   \item{severepartial}{Partial remission probability (severe).}
#'   \item{severechronic}{Chronic transition probability (severe).}
#' }
#'
#' \strong{Recovery-state outcomes}
#' \describe{
#'   \item{mildrecoverycured}{Cure probability from mild--recovery.}
#'   \item{mildrecoveryrelapse}{Relapse probability from mild--recovery.}
#'   \item{mildpartialcured}{Cure probability from mild--partial.}
#'   \item{mildpartialrelapse}{Relapse probability from mild--partial.}
#'
#'   \item{moderaterecoverycured}{Cure probability from moderate--recovery.}
#'   \item{moderaterecoveryrelapse}{Relapse probability from moderate--recovery.}
#'   \item{moderatepartialcured}{Cure probability from moderate--partial.}
#'   \item{moderatepartialrelapse}{Relapse probability from moderate--partial.}
#'
#'   \item{severerecoverycured}{Cure probability from severe--recovery.}
#'   \item{severerecoveryrelapse}{Relapse probability from severe--recovery.}
#'   \item{severepartialcured}{Cure probability from severe--partial.}
#'   \item{severepartialrelapse}{Relapse probability from severe--partial.}
#' }
#'
#' \strong{Relapse multipliers}
#' \describe{
#'   \item{increased_relapse_1}{Relapse multiplier (category 1).}
#'   \item{increased_relapse_2}{Relapse multiplier (category 2).}
#'   \item{increased_relapse_3}{Relapse multiplier (category 3).}
#'   \item{increased_relapse_4}{Relapse multiplier (category 4).}
#'   \item{increased_relapse_5}{Relapse multiplier (category 5).}
#' }
#'
#' @return A list with two elements:
#' \describe{
#'   \item{base}{Model output using the base scenario.}
#'   \item{alt}{Model output using the alternative scenario.}
#' }
#' 
#' #' @examples
#' res <- run_model()
#'
#' @export

run_model <- function(
    parameters = parameter_list,
    sim_runs = 1000,
    total_population = 10518000,
    
    # Base data frames
    df_prev_sub_base = data_prev_sub_base,
    df_tr_mild_base = data_tr_mild_base,
    df_tr_mod_base = data_tr_mod_base,
    df_tr_sev_base = data_tr_sev_base,
    df_prev_rec_base = data_prev_rec_base,

    
    # Alternative data frames
    df_prev_sub_alt = data_prev_sub_alt,
    df_tr_mild_alt = data_tr_mild_alt,
    df_tr_mod_alt = data_tr_mod_alt,
    df_tr_sev_alt = data_tr_sev_alt,
    df_prev_rec_alt = data_prev_rec_alt
    
    # Numeric parameters
) {
  

  # --- 0. Rename names in list for model runs ----------------------------------
  rename_map <- c(
    "excess mortality" = "excess_mortality",
    "retirement rate" = "retirement_rate",
    "death rate" = "death_rate",
    "mean duration of chronicity (year)" = "mean_dur_chron",
    "increased relapse 1" = "increased_relapse_1",
    "increased relapse 2" = "increased_relapse_2",
    "increased relapse 3" = "increased_relapse_3",
    "increased relapse 4" = "increased_relapse_4",
    "increased relapse 5" = "increased_relapse_5",
    "discount rate daly averted" = "discount_rate_daly",
    "discount rate costs" = "disc_rate_cost", 
    "dw conversion factor" = "dw_conversion_fact",
    "Scale/shape Gamma cost distribution" = "scale_shape_gamma_cost", 
    "Incidence no history"  = "incidence_no_history"
  )
  
  idx <- names(parameters) %in% names(rename_map)
  
  names(parameters)[idx] <- rename_map[names(parameters)[idx]]
  
  
  
  # --- 1. Compute prophylactic -------------------------------------------------
  tr_mild_profilactic_base <- sum(df_tr_mild_base$cov * df_tr_mild_base$adh, na.rm = TRUE) * 0.25
  tr_mod_profilactic_base <- sum(df_tr_mod_base$cov * df_tr_mod_base$adh, na.rm = TRUE) * 0.25
  tr_sev_profilactic_base <- sum(df_tr_sev_base$cov * df_tr_sev_base$adh, na.rm = TRUE) * 0.25
  
  tr_mild_profilactic_alt <- sum(df_tr_mild_alt$cov * df_tr_mild_alt$adh, na.rm = TRUE) * 0.25
  tr_mod_profilactic_alt <- sum(df_tr_mod_alt$cov * df_tr_mod_alt$adh, na.rm = TRUE) * 0.25
  tr_sev_profilactic_alt <- sum(df_tr_sev_alt$cov * df_tr_sev_alt$adh, na.rm = TRUE) * 0.25
  
  
  # --- 2. Build transition matrix ---------------------------------------------
  leavemodel <-  (1 - parameters[["death_rate"]] * parameters[["excess_mortality"]]) * (1 - parameters[["retirement_rate"]])
 
  parameters$leavemodel <- leavemodel
  
  first_part_names <- c(
    "death_rate",
    "retirement_rate",
    "excess_mortality",
    "increased_relapse_1",
    "increased_relapse_2",
    "increased_relapse_3",
    "increased_relapse_4",
    "increased_relapse_5",
    "mean_dur_chron",
    "leavemodel",
    "incidence_no_history",
    "pmild",
    "pmoderate",
    "psevere",
    "mildrecovery",
    "mildpartial",
    "mildchronic",
    "moderaterecovery",
    "moderatepartial",
    "moderatechronic",
    "severerecovery",
    "severepartial",
    "severechronic",
    "mildrecoverycured",
    "mildrecoveryrelapse",
    "mildpartialcured",
    "mildpartialrelapse",
    "moderaterecoverycured",
    "moderaterecoveryrelapse",
    "moderatepartialcured",
    "moderatepartialrelapse",
    "severerecoverycured",
    "severerecoveryrelapse",
    "severepartialcured",
    "severepartialrelapse"
  )
  
  missing <- setdiff(first_part_names, names(parameters))
  if (length(missing) > 0) {
    stop("`parameters` is missing required first-part inputs: ",
         paste(missing, collapse = ", "),
         call. = FALSE)
  }
  
  parameters_first <- parameters[first_part_names]
  
  tm <- do.call(func_first_part_model, parameters_first)
   
  
  
  
  
  
  # --- 3. Run base simulation -------------------------------------------------
  res_base <- fun_sim_model(
    transition_matrix        = tm,
    sim_runs                 = as.integer(sim_runs),
    total_population         = as.integer(total_population),
    
    df_prev_sub              = df_prev_sub_base,
    df_tr_mild               = df_tr_mild_base,
    df_tr_mod                = df_tr_mod_base,
    df_tr_sev                = df_tr_sev_base,
    df_prev_rec              = df_prev_rec_base,
    
    dw_conversion_fact       = parameters[["dw_conversion_fact"]],
    tr_mild_profilactic      = tr_mild_profilactic_base,
    tr_mod_profilactic       = tr_mod_profilactic_base,
    tr_sev_profilactic       = tr_sev_profilactic_base,
    
    discount_rate_daly       = parameters[["discount_rate_daly"]],
    scale_shape_gamma_cost   = parameters[["scale_shape_gamma_cost"]],
    disc_rate_cost           = parameters[["disc_rate_cost"]],
    
    increased_relapse_1      = parameters[["increased_relapse_1"]],
    increased_relapse_2      = parameters[["increased_relapse_2"]],
    increased_relapse_3      = parameters[["increased_relapse_3"]],
    increased_relapse_4      = parameters[["increased_relapse_4"]],
    increased_relapse_5      = parameters[["increased_relapse_5"]],
    
    leavemodel               = parameters[["leavemodel"]],
    mean_dur_chron           = parameters[["mean_dur_chron"]],
    
    incidence_no_history     = parameters[["incidence_no_history"]],
    pmild                    = parameters[["pmild"]],
    pmoderate                = parameters[["pmoderate"]],
    psevere                  = parameters[["psevere"]],
    
    mildrecovery             = parameters[["mildrecovery"]],
    mildpartial              = parameters[["mildpartial"]],
    mildchronic              = parameters[["mildchronic"]],
    moderaterecovery         = parameters[["moderaterecovery"]],
    moderatepartial          = parameters[["moderatepartial"]],
    moderatechronic          = parameters[["moderatechronic"]],
    severerecovery           = parameters[["severerecovery"]],
    severepartial            = parameters[["severepartial"]],
    severechronic            = parameters[["severechronic"]],
    
    mildrecoverycured        = parameters[["mildrecoverycured"]],
    mildrecoveryrelapse      = parameters[["mildrecoveryrelapse"]],
    mildpartialcured         = parameters[["mildpartialcured"]],
    mildpartialrelapse       = parameters[["mildpartialrelapse"]],
    
    moderaterecoverycured    = parameters[["moderaterecoverycured"]],
    moderaterecoveryrelapse  = parameters[["moderaterecoveryrelapse"]],
    moderatepartialcured     = parameters[["moderatepartialcured"]],
    moderatepartialrelapse   = parameters[["moderatepartialrelapse"]],
    
    severerecoverycured      = parameters[["severerecoverycured"]],
    severerecoveryrelapse    = parameters[["severerecoveryrelapse"]],
    severepartialcured       = parameters[["severepartialcured"]],
    severepartialrelapse     = parameters[["severepartialrelapse"]]
  )
  
  # --- 4. Run alternative simulation -----------------------------------------
  res_alt <- fun_sim_model(
    transition_matrix        = tm,
    sim_runs                 = as.integer(sim_runs),
    total_population         = as.integer(total_population),
    
    df_prev_sub              = df_prev_sub_alt,
    df_tr_mild               = df_tr_mild_alt,
    df_tr_mod                = df_tr_mod_alt,
    df_tr_sev                = df_tr_sev_alt,
    df_prev_rec              = df_prev_rec_alt,
    
    dw_conversion_fact       = parameters[["dw_conversion_fact"]],
    tr_mild_profilactic      = tr_mild_profilactic_alt,
    tr_mod_profilactic       = tr_mod_profilactic_alt,
    tr_sev_profilactic       = tr_sev_profilactic_alt,
    
    discount_rate_daly       = parameters[["discount_rate_daly"]],
    scale_shape_gamma_cost   = parameters[["scale_shape_gamma_cost"]],
    disc_rate_cost           = parameters[["disc_rate_cost"]],
    
    increased_relapse_1      = parameters[["increased_relapse_1"]],
    increased_relapse_2      = parameters[["increased_relapse_2"]],
    increased_relapse_3      = parameters[["increased_relapse_3"]],
    increased_relapse_4      = parameters[["increased_relapse_4"]],
    increased_relapse_5      = parameters[["increased_relapse_5"]],
    
    leavemodel               = parameters[["leavemodel"]],
    mean_dur_chron           = parameters[["mean_dur_chron"]],
    
    incidence_no_history     = parameters[["incidence_no_history"]],
    pmild                    = parameters[["pmild"]],
    pmoderate                = parameters[["pmoderate"]],
    psevere                  = parameters[["psevere"]],
    
    mildrecovery             = parameters[["mildrecovery"]],
    mildpartial              = parameters[["mildpartial"]],
    mildchronic              = parameters[["mildchronic"]],
    moderaterecovery         = parameters[["moderaterecovery"]],
    moderatepartial          = parameters[["moderatepartial"]],
    moderatechronic          = parameters[["moderatechronic"]],
    severerecovery           = parameters[["severerecovery"]],
    severepartial            = parameters[["severepartial"]],
    severechronic            = parameters[["severechronic"]],
    
    mildrecoverycured        = parameters[["mildrecoverycured"]],
    mildrecoveryrelapse      = parameters[["mildrecoveryrelapse"]],
    mildpartialcured         = parameters[["mildpartialcured"]],
    mildpartialrelapse       = parameters[["mildpartialrelapse"]],
    
    moderaterecoverycured    = parameters[["moderaterecoverycured"]],
    moderaterecoveryrelapse  = parameters[["moderaterecoveryrelapse"]],
    moderatepartialcured     = parameters[["moderatepartialcured"]],
    moderatepartialrelapse   = parameters[["moderatepartialrelapse"]],
    
    severerecoverycured      = parameters[["severerecoverycured"]],
    severerecoveryrelapse    = parameters[["severerecoveryrelapse"]],
    severepartialcured       = parameters[["severepartialcured"]],
    severepartialrelapse     = parameters[["severepartialrelapse"]]
  )
  
  print(res_base)
  print(res_alt)
  
  # --- 5. Output both ---------------------------------------------------------
  list(
    base = res_base,
    alt  = res_alt
  )
}
