#' @title run_abm.R
#'
#' @description Run the different functions to get the full simulation of a cohort
#' of patients, in the baseline scenario (standard of care) and with the different intervention scenarios
#'
#' @param init A list including the initial conditions of the simulation:
#' prevalence of the infection, maximum span of infection considered, initial size of the cohort simulated
#' @param param A list including the biological and intervention parameters, along with the age-sex-specific death rates
#' @param scenario_simulation Character vector indicating which scenario is being considered for the simulation
#' @param intervention_start A numeric value indicating at which time step the intervention starts
#' (i.e. when the simulation starts using the intervention parameters over the baseline parameters)
#' [default = 0]
#' @param prop_male A numeric value indicating what share of the population is male
#' This value is currently scraped from the UNAIDS indicators
#' @param n_steps A numeric value indicating the duration of the intervention
#' [default = 52*5], i.e. 5 years
#'
#' @return This function returns the \code{dat} object, recording the evolution of the attributes and epidemiological modules,
#'         for the duration of the simulation
#' @seealso This function is used by:
#'   \code{\link{run_simulations}}         
run_abm <- function(init, param, scenario_simulation, intervention_start = 0, prop_male, nsteps = 52 * 5){
  
  if(intervention_start == 0){
    scenario <- scenario_simulation
    dat <- initialize(init, 
                      param, 
                      scenario,
                      prop_male,
                      nsteps)
    
    dat$param$prop_male <- prop_male
    at <- 1
    dat <- prevalence(dat, at)
    dat <- aging(dat, at)
    dat <- cd4(dat, at)
    dat <- deaths(dat, at)
    dat <- vl(dat, at)
    dat <- transmission(dat, at)
    dat$epi$diag[at] <- 0
    dat$epi$onTrt[at] <- 0
    
    for (at in 2:nsteps) {
      
      dat <- prevalence(dat, at)
      dat <- aging(dat, at)
      dat <- cd4(dat, at)
      dat <- deaths(dat, at)
      dat <- arrival(dat,at, scenario)
      dat <- test(dat, at, scenario)
      dat <- trt(dat, at, scenario)
      dat <- vl(dat, at)
      dat <- transmission(dat, at)
      
    }
  } else {
    dat <- initialize(init, 
                      param, 
                      scenario = "parameters_baseline",
                      prop_male,
                      nsteps)
  
    dat$param$prop_male <- prop_male
    
    at <- 1
    dat <- prevalence(dat, at)
    dat <- aging(dat, at)
    dat <- cd4(dat, at)
    dat <- deaths(dat, at)
    dat <- vl(dat, at)
    dat <- transmission(dat, at)
    dat$epi$diag[at] <- 0
    dat$epi$onTrt[at] <- 0
    
    for (at in 2:nsteps) {
      
      scenario <- ifelse(intervention_start > at, "parameters_baseline", scenario_simulation)
      
      dat <- prevalence(dat, at)
      dat <- aging(dat)
      dat <- cd4(dat, at)
      dat <- deaths(dat, at)
      dat <- arrival(dat,at, scenario)
      dat <- test(dat, at, scenario)
      dat <- trt(dat, at, scenario)
      dat <- vl(dat, at)
      dat <- transmission(dat, at)
      
    }
  }

  
  return(dat)
}
