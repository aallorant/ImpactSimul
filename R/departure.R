#' @title: departure module
#' 
#' @description: Stochastically determined deaths at each time period of the simulation
##              (1) Susceptible deaths: HIV-free deaths based on age and sex (age and sex specific death rates pulled from a life table) 
##              (2) Infected deaths: HIV deaths based on CD4 count, either deterministically if CD4 count < 0, or stochastically
##                  if individuals in their late AIDS-stage (CD4 count below the AIDS-stage CD4 count threshold)
##              (3) Update attributes: record death status (active variable turns to 0), record separetely deaths for indivuals on treatment or lost to follow-up
##              (4) Update population: delete individuals that are not active (i.e. that are dead)
##              (5) Summary statistics: count the number of deaths, and stratify by treatment status, for each time step
#' @param dat 
#' @param at 
#' @return
#' This function returns the \code{dat} object recording for each time step \code{at}, the number of susceptible deaths 
#' \code{ds.flow}, and infected deaths \code{di.flow}
#' broken down into on treatment and lost to follow-up deaths \code{dtrt.flow} and \code{dlfu.flow} (therefore, `di.flow = dtrt.flow + dlfu.flow`)
#' @seealso This function is used by:
#'   \code{\link{run_abm}}
deaths <- function(dat, at) {
  
  ### 1. Susceptible Deaths ###
  
  ## Variables
  male <- dat$attr$male
  age <- dat$attr$age
  cd4Count <- dat$attr$cd4Count
  
  di.cd4.aids <- dat$param$di.cd4.aids
  ds.exit.age <- dat$param$ds.exit.age
  
  ## Eligible individuals for susceptible deaths are uninfected, or pre-death infected but unhealthy old
  idsEligSus <- which((is.na(cd4Count) |
                         cd4Count > di.cd4.aids))
  nEligSus <- length(idsEligSus)

  # Set age-sex specific rates
  ds.rates <- dat$param$ds.rates
  if (nEligSus > 0) {
    rates <- ds.rates$mrate[round(100*male[idsEligSus] + age[idsEligSus])]
  }


  ## Process
  nDeathsSus <- 0; idsDeathsSus <- NULL
  if (nEligSus > 0) {
    vecDeathsSus <- which(rbinom(nEligSus, 1, rates) == 1)
    nDeathsSus <- length(vecDeathsSus)
  }


  ## Update Attributes
  if (nDeathsSus > 0) {
    idsDeathsSus <- idsEligSus[vecDeathsSus]
    dat$attr$active[idsDeathsSus] <- 0
  }

  
  ### 2. Infected Deaths ###
  
  ## Variables
  active <- dat$attr$active
  di.cd4.rate <- dat$param$di.cd4.rate
  
  ## Process
  nDeathsInf <- 0; idsDeathsInf <- NULL
  nDeathsLfu <- 0; idsDeathsLfu <- NULL
  nDeathsonTrt <- 0; idsDeathsonTrt <- NULL
  
  cd4Count <- dat$attr$cd4Count
  stopifnot(length(active) == length(cd4Count))
  
  idsEligInf <- which(active == 1 & cd4Count <= di.cd4.aids)
  nEligInf <- length(idsEligInf)
  
  if (nEligInf > 0) {
    vecDeathsInf <- which(rbinom(nEligInf, 1, di.cd4.rate) == 1)
    if (length(vecDeathsInf) > 0) {
      idsDeathsInf <- idsEligInf[vecDeathsInf]
      nDeathsInf <- length(idsDeathsInf)
    }
  }
  
  idsDeathsDet <- which(cd4Count <= 0)
  if (length(idsDeathsDet) > 0) {
    idsDeathsInf <- c(idsDeathsInf, idsDeathsDet)
    nDeathsInf <- nDeathsInf + length(idsDeathsDet)
  }
  
  ### 3. Update Attributes ###
  if (nDeathsInf > 0) {
    dat$attr$active[idsDeathsInf] <- 0
    # Individuals that died while being on treatment
    idsDeathsonTrt <- which(dat$attr$txStat[idsDeathsInf] == 1)
    nDeathsonTrt <- length(idsDeathsonTrt)
    
    # Individuals that died while being lost to follow-up
    idsDeathsLfu <- which(dat$attr$Lfu[idsDeathsInf] == 1)
    nDeathsLfu <- length(idsDeathsLfu)
  }
  
  ## 4. Update Population Structure ##
  dead <- which(dat$attr$active == 0)
  dat$attr <- deleteAttr(dat$attr, dead)
  # 
  ### 5. Summary Statistics ###
  dat$epi$ds.flow[at] <- nDeathsSus
  dat$epi$di.flow[at] <- nDeathsInf
  dat$epi$dtrt.flow[at] <- nDeathsonTrt
  dat$epi$dlfu.flow[at] <- nDeathsLfu
  
  return(dat)
}