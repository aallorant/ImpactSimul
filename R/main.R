# install.packages("devtools")
devtools::install_github("grlurton/ImpactSimul")

library(ImpactSimul)
library("readxl")
library(dplyr)
library(yaml)
library(reticulate)
library(stringr)

download_data <- F
run_life_table <- TRUE
n_sim <- 100
nsteps <- 52 * 5
parameters_folder <-
  time.unit <- 7

###############################
##### SET UP THE LIFE TABLE ###
###############################
setwd("C:/Users/allorant/Desktop/Solthis/Empower/analysis/Empower-II/")
if(run_life_table){
  lt_male <- read_excel("data/lifeTables_SierraLeone.xlsx", sheet = "Male")
  lt_female <- read_excel("data/lifeTables_SierraLeone.xlsx", sheet = "Male")
  lt <- prep_life_tables(lt_male, lt_female, "data/ltSierraLeone.rds")
}

if(download_data){
  extract_unaids("ImpactSimul", "src/data_preparation/unaids_scrap.py", "Sierra Leone", "utils/chromedriver", "data/unaids_estimates/")
}

parameters_bio <- yaml.load_file("params/parameters_bio.yaml")

list_scenarios <- create_scenario_list()


#############################
### Loading external data ###
#############################

# estimates UNAIDS
data <- readRDS("data/unaids_estimates/SierraLeone/UNAIDS_estimates_SierraLeone.rds")
prop_male <- data$MeanNum[grepl(pattern = "Men aged 15 and over newly infected with HIV ", 
                                x = data$indic)]/data$MeanNum[grepl(pattern = "Adults aged 15 and over newly infected with HIV", 
                                                                    x = data$indic)]

#########################
### Loading functions ###
#########################
param <- params(time.unit = time.unit)

init <- init(i.prev.male = 1,
             i.prev.feml = 1,
             max.inf.time = 15 * 365,
             n=param$N)

for(scenario_simulation in names(list_scenarios)){
  print(scenario_simulation)
  assign(paste0("result_", scenario_simulation), run_simulations(init, param, scenario_simulation, intervention_start = 0, prop_male, nsteps = nsteps, nsim = 10))
}

## Summary statistics for the simulation results of the different scenarios
for(m in names(list_scenarios)){
  assign(paste0("summary_result_",m), summary_outcomes(get(paste0("result_",m))))
  assign(paste0("DALYs_",m),calculate_DALYs(get(paste0("summary_result_",m)), parameters_bio, param))
}

## Storing all the different scenarios' summary outcomes in one objects
res <- bind_rows(lapply(names(list_scenarios), function(m){
  tibble(
    Death = get(paste0("summary_result_",m))$death$deaths,
    `% Diagnosed` = get(paste0("summary_result_",m))$Diagnosed$diag/get(paste0("summary_result_",m))$n$num,
    `New infection` = get(paste0("summary_result_",m))$newInf$newInf,
    `Lost to follow-up` = get(paste0("summary_result_",m))$LTFU$LTFU,
    `On treatment` = get(paste0("summary_result_",m))$onTrt$onTrt,
    `Virally suppressed` = get(paste0("summary_result_",m))$VlSupp$VlSupp,
    `AIDS on treatment` = get(paste0("summary_result_",m))$AidsonART$AidsonART,
    `AIDS off treatment` = get(paste0("summary_result_",m))$AidsoffART$AidsoffART,
    time = get(paste0("summary_result_",m))$death$time,
    simul = rep(1:n_sim, each = nsteps),
    scenario = str_replace_all(str_remove(m,"parameters_"), "_", " ")
  )}))


## Comparing the key metrics of the epidemics simulated under the different scenarios

result_comparison_plot(res, h = 5)

## DALYs comparison for the different scenarios

daly <- bind_rows(lapply(names(list_scenarios), function(m){
  tibble(
    dalys = get(paste0("DALYs_",m))$DALYs,
    scenario = str_replace_all(str_remove(m,"parameters_"), "_", " ")
  )}))


DALY_comparison(daly)
## mean effect and 95% empirical confidence interval
# 
# c(mean(DALYs_0$DALYs - DALYs_1$DALYs),
#   quantile(DALYs_0$DALYs - DALYs_1$DALYs, .025),
#   quantile(DALYs_0$DALYs - DALYs_1$DALYs, .975)
# )
