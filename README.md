
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ImpactSimul

<!-- badges: start -->

<!-- badges: end -->

ImpactSimul is an Agent Based Model aimed at helping estimate the impact
of an intervention in an HIV program, using a simple and tractable
approach. It was first developed to estimate the impact to be expected
from Solthis’ Empower II program. It is written in R.

## Installation

### Install the R package

You can install ImpactSimul from [GitHub](https://github.com/) just by
running:

``` r
# install.packages("devtools")
devtools::install_github("grlurton/ImpactSimul")
```

### Install Python

To obtain epidemiological data on a country, ImpactSim uses a Python
script that queries the UNAIDS database and gets standardized data. To
get this running, we need to set-up a python environment and download
*chromedriver*. You shouldn’t have to write any line of Python and will
use the script `unaids_scrap.py` which you can find under `/utils` in in
this repo which you can put into the `utils` directory in your project’s
folder..

We suggest installing Python using Anaconda. 1. Go to
<https://www.anaconda.com/products/individual> 2. Download the
appropriate installer for your operating system. 3.Just double-click the
downloaded file. In most cases, you can just keep all default options in
the installer.

### Load the conda environment

To install the Python libraries you’ll need for the extraction, you just
need to install the conda environment `ImpactSimulEnv.yml` which you can
find under `/utils` in this repo.

Once you have downloaded it, just open a terminal in the directory you
have downloaded it in, and run the following code.

``` bash
conda env create -f ImpactSimulEnv.yml -n ImpactSimul
```

### Install ChromeDriver

1.  Install the Chrome web browser, available on
    <https://www.google.com/chrome/>
2.  Download ChromeDriver from
    <https://chromedriver.chromium.org/downloads> . Make sure you pick
    the driver corresponding to your version of the Chrome browser.
3.  Unzip ChromeDriver, and move the file into the `utils` directory in
    your project’s folder

## Example

``` r
library(ImpactSimul)
library("readxl")
library(dplyr)
library(yaml)
library(reticulate)

download_data <- TRUE
run_life_table <- TRUE
n_sim <- 1000
parameters_folder <-
time.unit <- 7

###############################
##### SET UP THE LIFE TABLE ###
###############################

if(run_life_table){
  lt_male <- read_excel("data/lifeTables_SierraLeone.xlsx", sheet = "Male")
  lt_female <- read_excel("data/lifeTables_SierraLeone.xlsx", sheet = "Male")
  lt <- prep_life_tables(lt_male, lt_female, "data/ltSierraLeone.rds")
}

if(download_data){
  extract_unaids("ImpactSimul", "utils/unaids_scrap.py", "Sierra Leone", "utils/chromedriver", "data/unaids_estimates/")
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
  assign(paste0("result_", scenario_simulation), run_simulations(init, param, scenario_simulation, intervention_start = 0, prop_male, nsteps = 52 * 5, nsim = 100))
}

## Simulation results for the different scenarios

res_0 <- summary_outcomes(result_parameters_baseline)
res_1 <- summary_outcomes(result_parameters_intervention)

## DALYs comparison for the different scenarios

DALYs_0 <- calculate_DALYs(res_0, parameters_bio, param)
DALYs_1 <- calculate_DALYs(res_1, parameters_bio, param)



hist(sample(DALYs_0$DALYs, 10000, replace=T) - sample(DALYs_1$DALYs, 10000, replace=T), 
     main = "DALYS averted")

## mean effect and 95% empirical confidence interval

c(mean(DALYs_0$DALYs - DALYs_1$DALYs),
  quantile(DALYs_0$DALYs - DALYs_1$DALYs, .025),
  quantile(DALYs_0$DALYs - DALYs_1$DALYs, .975)
  )
```
