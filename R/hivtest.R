#' @title: test module
#' 
#' @description: Stochastically determined which individuals are getting tested at every time step `at'
##              Individuals are either: (i) selected at random for testing, with the probability of being tested based on the national annual HIV testing rate 
##                                      (converted to a daily testing rate)
##                                      (ii) testing as soon as their CD4 count reaches a value that is below their CD4 count at treatment initiation
##                                      as determined in the initialization of the simulation
#' @param dat 
#' @param at 
#' @param scenario scenario's name
#'@return: This function returns the \code{dat} object with updated \code{dxStat}, \code{txStat}, and \code{dxTime} attributes
#' @seealso This function is used by:
#'   \code{\link{run_abm}}
test <- function(dat, at, scenario) {
  
  # param
  hiv.test.rate <- dat$param[[scenario]]$hiv.test.rate # the HIV testing rate depends on the scenario
  
  # Variables
  status <- dat$attr$status
  txCD4min <- dat$attr$txCD4min
  cd4Count <- dat$attr$cd4Count
  dxStat <- dat$attr$dxStat
  
  # Process
  # individuals diagnosed are individuals selected at random for testing
  tested <- which((rbinom(n = length(status),
                          size = 1, prob = hiv.test.rate) == 1 & status == 1 & dxStat == 0))
  
  # Results
  if (length(tested) > 0) {
    dat$attr$dxStat[tested] <- 1
    dat$attr$txStat[tested] <- 0
    dat$attr$dxTime[tested] <- at
  }
  dat$epi$diag[at] <- sum(dat$attr$dxStat, na.rm = T)
  return(dat)
}