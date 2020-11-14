#' summarize the main health outcomes of the simulation
#' 
#' @param simul_results the object storing the population metrics and simulation outcomes across every time step of the simulation
#'
#' @return A list of outputs - the main health outcomes of interest for the simulation
#' @importFrom data.table rbindlist
#' @export summary_outcomes
summary_outcomes <- function(simul_results){
  out = list()
  out[["death"]] = rbindlist(mapply(function(results, index){data.frame(simul = index, deaths=results$epi$ds.flow + results$epi$di.flow, time=seq(length(results$epi$ds.flow)))}, simul_results, seq(length(simul_results)), SIMPLIFY =  F))
  out[["newInf"]] = rbindlist(mapply(function(results, index){data.frame(simul = index, newInf=results$epi$si.flow, time=seq(length(results$epi$si)))}, simul_results, seq(length(simul_results)), SIMPLIFY =  F))
  out[["LTFU"]] = rbindlist(mapply(function(results, index){data.frame(simul = index, LTFU=results$epi$Lfu, time=seq(length(results$epi$Lfu)))}, simul_results, seq(length(simul_results)), SIMPLIFY =  F))
  out[["onTrt"]] = rbindlist(mapply(function(results, index){data.frame(simul = index, onTrt=results$epi$onTrt, time=seq(length(results$epi$onTrt)))}, simul_results, seq(length(simul_results)), SIMPLIFY =  F))
  out[["Diagnosed"]] = rbindlist(mapply(function(results, index){data.frame(simul = index, diag=results$epi$diag, time=seq(length(results$epi$diag)))}, simul_results, seq(length(simul_results)), SIMPLIFY =  F))
  out[["VlSupp"]] = rbindlist(mapply(function(results, index){data.frame(simul = index, VlSupp=results$epi$VlSupp, time=seq(length(results$epi$VlSupp)))}, simul_results, seq(length(simul_results)), SIMPLIFY =  F))
  out[["meanAge"]] = rbindlist(mapply(function(results, index){data.frame(simul = index, meanAge=results$epi$meanAge, time=seq(length(results$epi$meanAge)))}, simul_results, seq(length(simul_results)), SIMPLIFY =  F))
  out[["AidsonART"]] = rbindlist(mapply(function(results, index){data.frame(simul = index, AidsonART=results$epi$AidsonART, time=seq(length(results$epi$AidsonART)))}, simul_results, seq(length(simul_results)), SIMPLIFY =  F))
  out[["AidsoffART"]] = rbindlist(mapply(function(results, index){data.frame(simul = index, AidsoffART=results$epi$AidsoffART, time=seq(length(results$epi$AidsoffART)))}, simul_results, seq(length(simul_results)), SIMPLIFY =  F))
  out[["n"]] = rbindlist(mapply(function(results, index){data.frame(simul = index, num=results$epi$num, time=seq(length(results$epi$num)))}, simul_results, seq(length(simul_results)), SIMPLIFY =  F))
  out[["pop"]] = rbindlist(mapply(function(results, index){data.frame(simul = index, age=results$attr$age, male=results$attr$male)}, simul_results, seq(length(simul_results)), SIMPLIFY =  F))
  return(out)
  
}

## calculate_DALYs() --------------------------------------------------------------->
#' @title Calculate Disability-Adjusted-Life-Years for each scenario and each simulation.
#' One DALYs can be thought of as one healthy year of life lost.
#'
#' @description
#' \code{calculate_DALYs} transforms the number of deaths, new infections,
#' patients on and off treatment into a summary metric.
#'
#' @details
#' This function uses disability weights from the Global Burden of Disease Study to summarize
#' the number of DALYs in each of the scenario and therefore deduced the DALYs averted thanks
#' to the intervention.
#'
#' @param res_sim the main health outcomes of interest for different simulation and a given scenario
#'
#' @return `DALYs` a list object with the number of DALYs for each simulation
#'
#' @importFrom  data.table data.table
#' @importFrom data.table :=
#' @export calculate_DALYs
calculate_DALYs <- function(res_sim, parameters_bio, param){
  
  tmp <- data.table(cbind(res_sim$n,AidsonART=res_sim$AidsonART$AidsonART, AidsoffART=res_sim$AidsoffART$AidsoffART, death = res_sim$death$deaths))
  tmp[, YLD := ((num-AidsonART-AidsoffART)*parameters_bio$Disability_weights$hiv$value +
                  AidsonART*parameters_bio$Disability_weights$aids_art$value +
                  AidsoffART*parameters_bio$Disability_weights$aids_no_art$value)/(365.25/param$time.unit)]
  tmp[, YLL := death*(max(time)-time)/(365.25/param$time.unit)]
  tmp[, DALY := YLD + YLL]
  
  DALYs <- tmp[, list(DALYs = sum(DALY)), by = 'simul']
  
  return(DALYs)
}
