# install.packages("devtools")
devtools::install_github("grlurton/ImpactSimul")

library(ImpactSimul)
library("readxl")
library(dplyr)
library(yaml)
library(reticulate)

download_data <- TRUE
run_life_table <- TRUE
n_sim <- 100
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
  assign(paste0("result_", scenario_simulation), run_simulations(init, param, scenario_simulation, intervention_start = 0, prop_male, nsteps = 52 * 5, nsim = 10))
}

## Simulation results for the different scenarios

res_0 <- summary_outcomes(result_parameters_baseline)
res_1 <- summary_outcomes(result_parameters_intervention)

resSave <- res
res <- bind_rows(
  tibble(
    Death = res_0$death$deaths,
    `% Diagnosed` = res_0$Diagnosed$diag/res_0$n$num,
    `New infection` = res_0$newInf$newInf,
    `Lost to follow-up` = res_0$LTFU$LTFU,
    `On treatment` = res_0$onTrt$onTrt,
    `Virally suppressed` = res_0$VlSupp$VlSupp,
    `AIDS on treatment` = res_0$AidsonART$AidsonART,
    `AIDS off treatment` = res_0$AidsoffART$AidsoffART,
    time = res_0$death$time,
    simul = rep(1:n_sim, each = nsteps),
    scenario = "baseline"
  ),
  tibble(
    Death = res_1$death$deaths,
    `% Diagnosed` = res_1$Diagnosed$diag/res_1$n$num,
    `New infection` = res_1$newInf$newInf,
    `Lost to follow-up` = res_1$LTFU$LTFU,
    `On treatment` = res_1$onTrt$onTrt,
    `Virally suppressed` = res_1$VlSupp$VlSupp,
    `AIDS on treatment` = res_1$AidsonART$AidsonART,
    `AIDS off treatment` = res_1$AidsoffART$AidsoffART,
    time = res_1$death$time,
    simul = rep(1:n_sim, each = nsteps),
    scenario = "intervention"
  )
)

result_comparison_plot(res, h = 5)
## DALYs comparison for the different scenarios

DALYs_0 <- calculate_DALYs(res_0, parameters_bio, param)
DALYs_1 <- calculate_DALYs(res_1, parameters_bio, param)

daly <- bind_rows(
  tibble(
  dalys = DALYs_0$DALYs,
  scenario = "baseline"
  ),
  tibble(
    dalys = DALYs_1$DALYs,
    scenario = "intervention"
  )
)

DALY_comparison(daly)
## mean effect and 95% empirical confidence interval

c(mean(DALYs_0$DALYs - DALYs_1$DALYs),
  quantile(DALYs_0$DALYs - DALYs_1$DALYs, .025),
  quantile(DALYs_0$DALYs - DALYs_1$DALYs, .975)
)
