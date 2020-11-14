#' @title run_simulation.R
#'
#' @description Run agent-based models `nsim` times
#' under the different scenarios provided by the user
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
#' @param nsim
#' [default = 1000]
#' 
#' @return This function returns the \code{out} collecting the summary outcomes of interest
#' @export run_simulations
run_simulations <- function(init, param, scenario_simulation, intervention_start = 0, prop_male, nsteps = 52 * 5, nsim = 100){
  
 res <- lapply(1:nsim, function(sim){
    res <- run_abm(init, param, scenario_simulation, intervention_start = 0, prop_male, nsteps = nsteps)

    if(sim %% 2 == 0){
      cat("simulation = ", sim,"\n" )
    }
    return(res)
  })
  
  out <- res
  
  return(out)
  
}