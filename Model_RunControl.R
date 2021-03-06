## Run control file for pension simulation model
## 8/2017


#*********************************************************************************************************
#                                               Preamble ####
#*********************************************************************************************************

#rm(list = ls()) #MattC - moved to overlay script so custom data not deleted
gc()

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
library(doParallel)
library(microbenchmark)
library(readxl)
library(stringr)
library(xlsx)
# library(XLConnect) # slow but convenient because it reads ranges
# devtools::install_github("donboyd5/decrements")
# devtools::install_github("donboyd5/pp.prototypes")

#library(pp.prototypes) #MattC omit because using VT data
#library(decrements)               # mortality and termination for now


# Initial actives and retirees 
# Load data for new prototypes before they are in the pp.prototypes package
#load("Data/2015-10-07/actives.rda") #MattC omit when get VT data
#load("Data/2015-10-07/retirees.rda")  #MattC omit when get VT data


# Decrements
load("Data/2015-10-07/retrates.rda");  retrates %<>% dplyr::rename(qxr = retrate)  #MattC omit when get VT data
#load("Data/retrates_AZ.RData"); retrates <- retrate_AZ #MattC omit when get VT data
load("Data/2015-10-07/termrates.rda"); termrates %<>% dplyr::rename(qxt = termrate) # %>% mutate(qxt = 0.5*qxt) #MattC omit when get VT data
load("Data/2015-10-07/mortality.rda") #MattC omit when get VT data
load("Data/winklevossdata.rdata") # disability, disability mortaity and early retirement #MattC omit when get VT data

# Salary scale
#load("Data/2015-10-07/salgrowth.rda"); salgrowth %<>% mutate(age = NULL) #MattC omit because using VT data

source("Functions.R")
source("MattC_graphics.R") #MattC overwrite draw quantile routine

devMode <- FALSE # Enter development mode if true. Parameters and initial population will be imported from Dev_Params.R instead of the RunControl file. 

#retirees %<>% mutate(nretirees = 0) #MattC this will end up with 0 retirees???
#actives %<>% mutate(nactives = 0) 
#actives
#retrates %<>% mutate(qxr = ifelse(age == 65, 1, 0)) 

#*********************************************************************************************************
#                      ## Calibration of decrements  ####
#*********************************************************************************************************

# Calibrate term rates, mortality rates and retirement rates to approximately match workforce flows of AZ-PERS
# in 2013.  

#MattC Should adjust 3 lines below for VT????
termrates %<>% mutate(qxt = 1.2 * qxt)

mortality %<>% mutate(qxm = 0.6 * qxm) %>% 
               mutate(qxm.r = qxm)

retrates %<>% mutate(qxr = qxr * 0.7) 



#*********************************************************************************************************
# Read in Run Control files ####
#*********************************************************************************************************

#Uncomment these lines to use with MattC_charts.R
folder_run <- "IO_M2.1_new" 
# folder_run <- "IO_M1_new"
# folder_run <- "IO_M2.1history_new" 
 
filename_RunControl <- dir(folder_run, pattern = "^MattC_RunControl") #MattC

#Uncomment these 2 lines to use with Report_M1.R
#folder_run <- "IO_M1_new" 
#filename_RunControl <-"Reprod_RunControl_M1_new.xlsx"

#Uncomment these 2 lines to use with Report_M2.R
#folder_run <- "IO_M2.1_new"
#filename_RunControl <- "Reprod_RunControl_M2.1_new.xlsx"


path_RunControl <- paste0(folder_run, "/" ,filename_RunControl)


# Import global parameters
Global_params <- read_excel(path_RunControl, sheet="GlobalParams", skip = 1) 
termrates$planname <- rep("VSERS",Global_params$nyear+1) #MattC kludge/temp fix to get data in for VT

# Import parameters for all plans
plan_params        <- read_excel(path_RunControl, sheet="RunControl",    skip=4) %>% filter(!is.na(runname))
plan_returns       <- read_excel(path_RunControl, sheet="Returns",       skip=0) %>% filter(!is.na(runname))
plan_contributions <- read_excel(path_RunControl, sheet="Contributions", skip=0) %>% filter(!is.na(runname))


#*********************************************************************************************************
# Read in Run Control files ####
#*********************************************************************************************************

## select plans
runlist <- plan_params %>% filter(include == TRUE) %>% select(runname) %>% unlist
# runlist <- runlist[runlist == "R4F3"]
# runlist <- runlist[runlist == "average1"|runlist == "average3"]
# runlist <- runlist[runlist == "average3"]
runlist


## Run selected plans 
runName = runlist #MattC temp kludge for 1 run test and stepping through manually
for (runName in runlist){

suppressWarnings(rm(paramlist, Global_paramlist))
  
## Extract plan parameters 
paramlist    <- get_parmsList(plan_params, runName)
paramlist$plan_returns <- plan_returns %>% filter(runname == runName)
if(paramlist$exCon) paramlist$plan_contributions <- trans_cont(plan_contributions, runName) else 
                    paramlist$plan_contributions <- list(0) 


## Extract global parameters and coerce the number of simulation to 1 when using deterministic investment reuturns.
Global_paramlist <- Global_params %>% as.list
if ((paramlist$return_type == "simple" & paramlist$ir.sd == 0) |
    (paramlist$return_type == "internal" &  all(paramlist$plan_returns$ir.sd == 0))|
    (paramlist$return_type == "external")){

  Global_paramlist$nsim <- 1
}

## Run the model
 source("Model_Master.R", echo = TRUE)
}



