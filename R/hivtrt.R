#' @title treatment module
#' 
#'
#' @description Module function for anti-retroviral treatment initiation and
#'              adherence over time.
#'
#' @param dat 
#' @param at 
#' @param scenario scenario's name
#' @return
#' This function returns the \code{dat} object with updated treatment status for each individual, and the cohort summary
#' in \code{dat$epi} with the number of lost to follow-up
#' \code{lfu}, and the number of patients treated \code{onTrt} at each time unit.
#' @seealso This function is used by:
#'   \code{\link{run_abm}}

trt <- function(dat, at, scenario) {

# Variables ---------------------------------------------------------------
dxStat <- dat$attr$dxStat
txStat <- dat$attr$txStat
txStartTime <- dat$attr$txStartTime
txStops <- dat$attr$txStops
Lfu <- dat$attr$Lfu
txTimeOn <- dat$attr$txTimeOn
txTimeOff <- dat$attr$txTimeOff
txCD4start <- dat$attr$txCD4start
cd4Count <- dat$attr$cd4Count
facility <- dat$attr$facility
txType <- dat$attr$txType

tx.coverage <- dat$param[[scenario]]$tx.coverage
tx.adhere.full <- dat$param[[scenario]]$tx.adhere.full
tx.adhere.part <- dat$param[[scenario]]$tx.adhere.part
tx.lfu <- dat$param[[scenario]]$tx.lfu

# ensuring facility parameters do not change from one period to the next

# if(at == 1){
  
  tx.adhere.full.fac <- dat$param$tx.adhere.full.fac <- pmax(0,pmin(1,rnorm(n = length(unique(facility)), mean = tx.adhere.full, sd = tx.adhere.full/5)))
  tx.lfu.fac <- dat$param$tx.lfu.fac <- pmax(0,pmin(1,rnorm(n = length(unique(facility)), mean = tx.lfu, sd = tx.lfu/5)))
  tx.adhere.part.fac <- dat$param$tx.adhere.part.fac <- pmax(0,pmin(1,rnorm(n = length(unique(facility)), mean = tx.adhere.part, sd = tx.adhere.part/5)))
  
# } else
#   tx.adhere.full.fac <- dat$param$tx.adhere.full.fac
#   tx.lfu.fac <- dat$param$tx.lfu.fac
#   tx.adhere.part.fac <- dat$param$tx.adhere.part.fac 

# Start tx for tx naive ---------------------------------------------------

## Patients eligible for treatment initiation are infected, diagnosed, treatment naive and not lfu
idsElig <- which(dxStat == 1 & txStat == 0 &
                   is.na(txStartTime) & !is.na(cd4Count) & is.na(Lfu))
nElig <- length(idsElig)
idsTx <- NULL


## Treatment coverage
nStart <-round(tx.coverage * nElig)
if (nStart > 0) {
  idsTx <- sample(idsElig, nStart)
}

## Treatment type assignment
if (length(idsTx) > 0) {
  needtxType <- which(is.na(txType[idsTx]))
  if (length(needtxType) > 0) {
    txType[idsTx[needtxType]] <- vapply(tx.adhere.full.fac[facility[idsTx[needtxType]]], function(x) rbinom(1, 1, x), as.integer(1L))
  }
  # if (tx.adhere.part == 0) {
  #   idsTx <- intersect(idsTx, which(txType == 1))
  # }
}

if (length(idsTx) > 0) {
  txStat[idsTx] <- 1
  txStartTime[idsTx] <- at
  txStops[idsTx] <- 0
  txTimeOn[idsTx] <- 0
  txTimeOff[idsTx] <- 0
  txCD4start[idsTx] <- cd4Count[idsTx]
}

# Lost to follow-up ------------------------------------------------------
idsLfu <- NULL
idsEligLfu <- which(!is.na(txStartTime) & is.na(Lfu))
nEligLfu <- length(idsEligLfu)

if (nEligLfu > 0) {
  vecLfu <- which(vapply(tx.lfu.fac[facility[idsEligLfu]], function(x) rbinom(1, 1, x), as.integer(1L)) == 1)
  if (length(vecLfu) > 0) {
    idsLfu <- idsEligLfu[vecLfu]
    Lfu[idsLfu] <- 1
  }
}

# Stop tx -----------------------------------------------------------------
idsStop <- NULL
idsEligStop <- which(dat$attr$txStat == 1 & txType == 0)
nEligStop <- length(idsEligStop)
if (nEligStop > 0) {
  vecStop <- which(vapply(1-tx.adhere.part.fac[facility[idsEligStop]], function(x) rbinom(1, 1, x), as.integer(1L)) == 1)
  if (length(vecStop) > 0) {
    idsStop <- idsEligStop[vecStop]
    txStat[idsStop] <- 0
    txStops[idsStop] <- txStops[idsStop] + 1
  }
}


# Restart tx --------------------------------------------------------------
idsRest <- NULL
idsEligRest <- which(dat$attr$txStat == 0 & txStops > 0 & is.na(Lfu) )
nEligRest <- length(idsEligRest)
if (nEligRest > 0) {
  vecRes <- which(vapply(1-tx.adhere.part.fac[facility[idsEligRest]], function(x) rbinom(1, 1, x), as.integer(1L)) == 1)
  if (length(vecRes) > 0) {
    idsRest <- idsEligRest[vecRes]
    txStat[idsRest] <- 1
    dat$attr$vlSlope[idsRest] <- NA
  }
}


# Output ------------------------------------------------------------------
idsOnTx <- which(txStat == 1)
idsOffTx <- which(txStat == 0 & !is.na(txStartTime))
txTimeOn[idsOnTx] <- txTimeOn[idsOnTx] + 1
txTimeOff[idsOffTx] <- txTimeOff[idsOffTx] + 1

dat$attr$txStat <- txStat
dat$attr$txStartTime <- txStartTime
dat$attr$txStops <- txStops
dat$attr$Lfu <- Lfu
dat$attr$txTimeOn <- txTimeOn
dat$attr$txTimeOff <- txTimeOff
dat$attr$txType <- txType
dat$attr$txCD4start <- txCD4start

dat$epi$onTrt[at] <- sum(txStat, na.rm = T)
dat$epi$Lfu[at] <- sum(Lfu, na.rm = T)

return(dat)
}
