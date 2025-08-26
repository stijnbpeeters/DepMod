# Function for applying transition matrix to population -------------------


mild_incidence <- Incidence * pmild
moderate_incidence <- Incidence * pmoderate
severe_incidence <- Incidence * psevere


Simulate_mm_population <- function(total_years){
  initial_incidences <- rep(0, length(states))
  names(initial_incidences) <- states
  initial_incidences["mild_incidence"] <- mild_incidence
  initial_incidences["moderate_incidence"] <- moderate_incidence
  initial_incidences["severe_incidence"] <- severe_incidence
  
  
  total_years <- total_years  + 1 
  
  results_population <- array(0, 
                              dim = c(total_years, length(states), total_years), 
                              dimnames = list(Year = 0:(total_years -1 ), state = states, StartYear = 0:(total_years-1)))
  
  
  population <- array(0,
                      dim = c(total_years, length(states)),
                      dimnames = list(Year = 0:47, state = states))
  
  
  population["0",] <- initial_incidences
  
  for(i in 2:total_years){
    population[i, ] <- population[i-1, ] %*% transition_matrix[, , i-1]
  } 
  
  for (StartYear in 0:(total_years)) {
    
    if ((StartYear + 1) <= total_years) {
      results_population[(StartYear + 1):total_years, , StartYear + 1] <- population[1:(total_years - StartYear), , drop = FALSE]
    }
  }
  
  results_population[is.na(results_population)] <- 0
  
  results_population <- apply(results_population, c(1,2), sum)
  
  
  
  return(results_population)
}