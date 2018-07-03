#source("Inputs_Salary_Benefit.R") #used to create /Data/example_Salary_benefit.RData
# -> this has a conflict between XLConnect and xlsx packages
source("Inputs_Import_Winklevoss.R") #used to create /Data/winklevossdata.RData
##	-> used by template RunControl.R
##	-> used by RunControl_reprod_M1
##	-> used by RunControl_reprod_M2
##	-> referenced in Model_Master
#source("Import_AZ_retrate.R") #used to create /Data/retrates_AZ_RData
# -> uses f2n function of unknown source?
#source("Data/2015-10-07/make_prototypes(14).r") #create actives,retirees,retrates,salgrowth,termrates.rda files
#mortality.rda appears to come from "decrements-master" project
#-> need to find "cton" function and fix ./data_raw folder name

#**************************************************************************************
#                      Running model                                               ####
#**************************************************************************************

## Run model for report M1
source("Model_RunControl_reprod_M1.R")

## Run model for report M2.1a and M2.1b
source("Model_RunControl_reprod_M2.1.R")

## Where are the outputs saved
# Report M1:      "IO_M1_new/"
# Report M2.1a/b: "IO_M2.1_new/"

#**************************************************************************************
#                      Producing graphs and tables                                 ####
#**************************************************************************************

## Producing graphs and tables
source("IO_M1_new/Report_M1.R")  
source("IO_M2.1_new/Report_M2.1a.R")
source("IO_M2.1_new/Report_M2.1b.R")

## Where are the outputs saved
 # Report M1:      "IO_M1_new/"
 # Report M2.1a:   "IO_M2.1_new/M2.1a_outputs"
 # Report M2.1b:   "IO_M2.1_new/M2.1b_outputs"

source("IO_M1_new/trade-off.R") #contribution volatility and funded status analysis
source("IO_M1_new/Report_M1_MFC2017.R")
source("IO_M1_new/NTA2016/Report_M1_NTA2016.R")

source("IO_M2.1_new/Analysis_Demo_Loading.R") #uses benefits scenarios
source("IO_M2.1_new/Analysis_Demo_all.R")
source("IO_M2.1_new/Analysis_Demo_risks.R")
source("IO_M2.1_new/Analysis_Demo_report.R")
source("IO_M2.1_new/Analysis_Demo_SingleRun.R")

#---------- experiment here to get a projected benefits curve
label <- runs_single_labels
runs_single <- c("D1F075-average", "D1F075_mature1-gn1", "D1F075_mature2-gn1", "D1F075-immature_g1")
simnum <- 56
det.runs = "D1F075_average"

df_single <- results_all %>%  filter(runname %in% runs_single, year <= 110) %>% 
  filter(sim == simnum) %>%
  left_join(label) %>% 
  mutate(run.label = ifelse(is.na(run.label), runname, run.label),
         runname   = ifelse(sim == 0, paste0(runname,   "-Determ"), runname),
         run.label = ifelse(sim == 0, paste0(run.label, "-Determ"), run.label)) %>%  
  group_by(runname, sim)
 
ls <- 1.25 # linesize
ps <- 2.25 # pointsize

p5 <- ggplot(data=filter(df_single, sim == simnum, runname == runs_single[1]) %>% # any run will do
               select(year, B, B.v) %>% 
               gather(variable, value, -year, -runname, -sim),
             aes(x = year, y = value*100, group = variable)) +
  geom_point(aes(colour=variable, shape=variable), size=ps) +
  geom_line(aes(colour=variable), size=ls) +
  scale_color_discrete(name = "",    labels = c("B", "B.v")) +
  scale_shape_discrete(name = "",    labels = c("B", "B.v")) +
  scale_linetype_discrete(name = "", labels = c("B", "B.v")) +
  scale_y_continuous(breaks = c(seq(-50, 5, 5), 7.5, seq(10, 50, 5)), name = "$") + 
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  geom_hline(yintercept = 7.5) +
  geom_hline(yintercept = 0, linetype="dotdash", size=.5) +
  labs(title=paste0("Benefits sim # ", simnum ), x = "Year") +
  theme(plot.title=element_text(size=14), legend.text = element_text(size = 11))  + theme_bw()

p5

#-----------------------
load(paste0(IO_folder, "/Outputs_I8F075-4.RData"))
test <- outputs_list$results
test2 <- (test[which(test[,"sim"]==56),c("B.v")])
plot(test2/((1+0.75)^(seq(1,60,1))))

#--------------------------------------------------------------------------
source("IO_M2.1_new/Results_Measures.R") #uses D1F050 scenarios

source("IO_M2.1_new/Analysis_Ben_Loading.R") #uses benefits scenarios 
source("IO_M2.1_new/Analysis_Ben_Risk.R")

source("IO_M2.1_new/Analysis_Investment_Loading.R")
source("IO_M2.1_new/Analysis_Investment_Risk.R")

source("IO_M2.1_new/Analysis_M2.1_checkIssues.R") 

source("IO_M2.1_new/Boyd_OrderOfReturns(1).r")
source("IO_M2.1_new/Report_M2.1b_MFC2017.R") #has problem with one version of df_inv3

#source("IO_M2.1history_new/getReturn.R")

#source("IO_M2.1_new/Analysis_Outputs_M2.1_new.Rmd")
#source("IO_M2.1_new/Analysis_Outputs_M2.1a_report.Rmd")
#source("IO_M2.1_new/Analysis_Outputs_M2.1_June2016.Rmd")
#source("IO_M2.1history_new/Analysis_Outputs_M2.1history_new.Rmd")

