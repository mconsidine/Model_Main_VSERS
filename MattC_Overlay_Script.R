rm(list = ls())
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
#library(readxl)
library(stringr)
#library(xlsx)
library(XLConnect) # slow but convenient because it reads ranges

#if(!require("devtools"))
#  install.packages("devtools")

#if(!require("decrements"))
#install_github("donboyd5/decrements")
#if(!require("pp.prototypes"))
#install_github("donboyd5/pp.prototypes")

#if(require("devtools") && require("microbenchmark"))
#  {
#    if (require("decrements") && require("pp.prototypes"))
#    {
      print("Sourcing Model_RunControl.R")
      #rm(list = ls())
      Model_Control_Script <- "Model_RunControl.R"
      set.seed(123)
      source("Functions.R")
      file_path <- paste0("Data/")
      #using PA-PSERS input
      #MATTC_plandataworkbook = "PA-PSERS.xlsx"
      #MATTC_plansalarygrowthsheet = "SalaryGrowth"
      #MATTC_plansalarystart = 20
      #MATTC_plansalaryend = 70
      #MATTC_plansalaryincrement = 10
      #MATTC_activestart = 25
      #MATTC_activeblocksize = 5
      #MATTC_activeincrement = round(MATTC_activeblocksize/2) #get midpoint of range as integer
      #MATTC_activeend = 64
      #MATTC_yosblocksize = 5
      #MATTC_yosincrement = round(MATTC_yosblocksize/2)
      #MATTC_yosend = 40
      #MATTC_plansheet1 = "PA-PSERS"
      #MATTC_plansheetregion1 = "A5:L24"
      
      #using VSERS input
      MATTC_plandataworkbook = "VT-plans.xlsx"
      MATTC_plansalarygrowthsheet = "VSERS_SalaryGrowth"
      
      MATTC_plansalarystart = 20 #25
      MATTC_plansalaryend = 70 #64
      MATTC_plansalaryincrement = 5
      
      MATTC_activestart = 20
      MATTC_activeblocksize = 5
      MATTC_activeincrement = round(MATTC_activeblocksize/2) #get midpoint of range as integer
      MATTC_activeend = 69

      MATTC_retstart = 30
      MATTC_retblocksize = 5
      MATTC_retincrement = round(MATTC_retblocksize/2) #get midpoint of range as integer
      MATTC_retend = 110      
      
      MATTC_yosblocksize = 5
      MATTC_yosincrement = round(MATTC_yosblocksize/2)
      MATTC_yosend = 40
      
      MATTC_plansheet1 = "VSERS" 
      MATTC_plansheet2 = "VSERSben" 
      MATTC_plansheetregion1 = "A5:L28" 
      MATTC_plansheetregion2 = "A1:G69" 

      #library(gtools) #MattC has a version of na.replace??
      library(zoo) #MattC include for na.locf function
      na.replace <- function(x, k, fromLast=FALSE) { #source stackoverflow Eldar Agalarov; modified by MattC
        isNA <- is.na(x[, k])
        x[isNA, k ] <- na.locf(x[, k], na.rm = F, fromLast=fromLast)[isNA]
        x
      }
      #MATTC_wb <- loadWorkbook(paste0(file_path, MATTC_plandataworkbook))
      #SS <- readWorksheet(MATTC_wb, sheet=MATTC_plansalarygrowthsheet, header=TRUE, region="A3:B15")
      SS <- readWorksheetFromFile(paste0(file_path, MATTC_plandataworkbook), 
                                  sheet=MATTC_plansalarygrowthsheet, 
                                  header=TRUE, 
                                  region="A3:B15")
      SS %<>% right_join(data.frame(age = MATTC_plansalarystart:MATTC_plansalaryend))
      SS %<>% mutate(age.match = floor(age/MATTC_plansalaryincrement)*MATTC_plansalaryincrement)
      SS %<>% mutate(sscale.hist.rate = cton(growth)/100)
      SS %<>% na.replace(4) #fill NAs with last non-NA observation carried forward
      SS %<>% na.replace(4,fromLast=TRUE) #fill leading NAs if any with first non-NA value
      SS %<>% select(-c(growth,age.match))
      
      age.mid <- c(MATTC_activestart, 
                   seq(MATTC_activestart+MATTC_activeincrement, MATTC_activeend-MATTC_activeincrement, MATTC_activeblocksize), 
                   MATTC_activeend+MATTC_activeincrement)
      
      yos.mid <- c(seq(MATTC_yosincrement, MATTC_yosend-MATTC_yosincrement, MATTC_yosblocksize),  
                   MATTC_yosend+MATTC_yosincrement)
      
      # convert to the ea x age format 
      df <- readWorksheetFromFile(paste0(file_path, MATTC_plandataworkbook), 
                                  sheet=MATTC_plansheet1, header=FALSE, # forceConversion=TRUE,
                                  region=MATTC_plansheetregion1)
      names(df) <- c("order", "tabletype", "agegrp", yos.mid)
      
      avgpay <- df %>% filter(tabletype=="avgpay")  %>% 
        arrange(order) %>%
        mutate(age=age.mid) %>%
        select(-order, -tabletype, -agegrp) %>%
        gather(yos, salary, -age) %>%
        filter(!is.na(salary)) %>%
        mutate(yos = as.numeric(yos)) #%>% #MattC changed from f2n (ok??)
      
      avgpay %<>% #select(-age.match, -yos.match) %>% 
        filter(age - yos >= 20) %>% 
        mutate(ea = age - yos)
      
      workforce <- df %>% filter(tabletype=="workforce")  %>% 
        arrange(order) %>%
        mutate(age=age.mid) %>%
        select(-order, -tabletype, -agegrp) %>%
        gather(yos, nactives, -age) %>%
        filter(!is.na(nactives)) %>%
        mutate(yos = as.numeric(yos)) #MattC changed from f2n (ok??)
        
      workforce %<>% #select(-age.match, -yos.match) %>% 
        filter(age - yos >= 20) %>% 
        mutate(ea = age - yos)
        
      #read block of Vermont retiree data
      df2 <- readWorksheetFromFile(paste0(file_path, MATTC_plandataworkbook), sheet=MATTC_plansheet2, header=FALSE, region=MATTC_plansheetregion2)
      names(df2) <- c("age", "nservice","benservice","ndisab","bendisab","nbenef","benbenef")
        
      avgben <- df2 %>% arrange(age) %>%
        mutate(avgbenservice=benservice/nservice) %>%
        mutate(avgbendisab=bendisab/ndisab) %>%
        mutate(avgbenbenef=benbenef/nbenef) %>%
        mutate(avgbenefit=(benservice+bendisab+benbenef)/(nservice+ndisab+nbenef)) %>%
        mutate(ntotal=nservice+ndisab+nbenef) %>%
        mutate(bentotal=benservice+bendisab+benbenef) %>%
        mutate(age.match=floor(age/MATTC_retblocksize)*MATTC_retblocksize) 
        
      byage <- avgben %>% group_by(age.match)
      byage2 <- byage %>% summarise(a=sum(bentotal)/sum(ntotal), b=sum(ntotal))
      byage2 %<>% mutate(age=age.match+MATTC_retincrement)
      byage2 <- arrange(rbind(byage2,c(105,0,0,107)),age.match)

      #-------------------------------------------------
      #What follows replicates the data structure provided by pp.prototypes and should be customized for Vermont

#Salary scales     
        MATTCsalgrowth.assume.age.start = 20
        MATTCsalgrowth.assume.age.end = 70
        MATTCsalgrowth.assume.age.rangesize = length(seq(from = MATTCsalgrowth.assume.age.start, 
                                                         to = MATTCsalgrowth.assume.age.end, by = 1))
        ############################################################
        #plan names
        MATTCplannames = c( rep("VMERS.yos", MATTCsalgrowth.assume.age.rangesize), 
                            rep("VSERS.yos", MATTCsalgrowth.assume.age.rangesize), 
                            rep("VSTRS.yos", MATTCsalgrowth.assume.age.rangesize))
        ############################################################
        #active age ranges for assumed salary growth data
        MATTCageranges = c( seq(from = MATTCsalgrowth.assume.age.start, to = MATTCsalgrowth.assume.age.end, by = 1),
                            seq(from = MATTCsalgrowth.assume.age.start, to = MATTCsalgrowth.assume.age.end, by = 1), 
                            seq(from = MATTCsalgrowth.assume.age.start, to = MATTCsalgrowth.assume.age.end, by = 1))
        ############################################################
        #assumed salary growth data
        MATTCsalscale.VMERS.assume = as.numeric(unlist(select(SS,sscale.hist.rate)*.5))
                                            
        MATTCsalscale.VSERS.assume = as.numeric(unlist(select(SS,sscale.hist.rate)))
        
        MATTCsalscale.VSTRS.assume = as.numeric(unlist(select(SS,sscale.hist.rate)*1.5))
        
        MATTCsscale.rates.assume = c( MATTCsalscale.VMERS.assume, MATTCsalscale.VSERS.assume, MATTCsalscale.VSTRS.assume)
        
        #set up assumed salary growth by age (why do these differ by funding level?)
        MATTCsalgrowth.assume <- structure(
          list(
            planname = MATTCplannames, 
            age = MATTCageranges, 
            sscale.assume.rate = MATTCsscale.rates.assume
          ), 
          .Names = c("planname", "age", "sscale.assume.rate"), 
          class = "data.frame",
          row.names = c(NA, -length(MATTCplannames))
        )
        
        MATTCsalgrowth <- structure( #MattC new structure added
          list(
            planname = MATTCplannames, 
          #  age = MATTCageranges,
            yos = as.numeric(rep(NA,length(MATTCageranges))), 
            salgrowth = MATTCsscale.rates.assume
          ), 
          .Names = c("planname", #"age",
                     "yos", "salgrowth"), 
          class = "data.frame",
          row.names = c(NA, -length(MATTCplannames))
        )
        
        ############################################################
        #historical salary growth scales for each plan
        MATTCsalscale.VMERS.hist = as.numeric(unlist(select(SS,sscale.hist.rate)))
        
        MATTCsalscale.VSERS.hist = as.numeric(unlist(select(SS,sscale.hist.rate)))
        
        MATTCsalscale.VSTRS.hist = as.numeric(unlist(select(SS,sscale.hist.rate)))
        
        MATTCsscale.rates.hist = c( MATTCsalscale.VMERS.hist, MATTCsalscale.VSERS.hist, MATTCsalscale.VSTRS.hist)
        
        #set up historical salary growth by age
        MATTCsalgrowth.hist <- structure(
          list(
            planname = MATTCplannames, 
            age = MATTCageranges, 
            sscale.hist.rate = MATTCsscale.rates.hist
          ), 
          .Names = c("planname", "age", "sscale.hist.rate"), 
          class = "data.frame",
          row.names = c(NA, -length(MATTCplannames))
        )

#RETIREES 
      retage.mid <- c(#MATTC_retstart, 
                seq(MATTC_retstart+MATTC_retincrement, MATTC_retend-MATTC_retincrement, MATTC_retblocksize), 
                MATTC_retend+MATTC_retincrement)

        ############################################################
        #names of each plan system for retiree data
        MATTCretirees.plannames = c( rep("VMERS.fillin", length(retage.mid)), 
                                     rep("VSERS.fillin", length(retage.mid)), 
                                     rep("VSTRS.fillin", length(retage.mid)))
        
        ############################################################
        #age ranges for each system
        MATTCretirees.ageranges = c( retage.mid,
                                     retage.mid,
                                     retage.mid)
        
        ############################################################
        #number of retirees in each system, by age ranges
        MATTCretirees.nretirees.VMERS = c(
          as.matrix(byage2[,"b"])
        )
        
        MATTCretirees.nretirees.VSERS = c(
          as.matrix(byage2[,"b"])
        )
        
        MATTCretirees.nretirees.VSTRS = c(
          as.matrix(byage2[,"b"])
        )
        
        MATTCretirees.nretirees = c( MATTCretirees.nretirees.VMERS, MATTCretirees.nretirees.VSERS, MATTCretirees.nretirees.VSTRS)
        
        ############################################################
        #average benefits for each system, by age ranges
        MATTCretirees.benefit.VMERS = c( as.matrix(byage2[,"a"]))
        
        MATTCretirees.benefit.VSERS = c( as.matrix(byage2[,"a"]))
        
        MATTCretirees.benefit.VSTRS = c( as.matrix(byage2[,"a"]))
        
        MATTCretirees.benefit = c(MATTCretirees.benefit.VMERS, MATTCretirees.benefit.VSERS, MATTCretirees.benefit.VSTRS)
        
        #set up retirees data
        MATTCretirees <- structure(
          list(
            planname = MATTCretirees.plannames, 
            age = MATTCretirees.ageranges, 
            age.cell = MATTCretirees.ageranges,
            nretirees = MATTCretirees.nretirees,
            benefit = MATTCretirees.benefit
          ), 
          .Names = c("planname", "age", "age.cell", "nretirees", "benefit"), 
          class = "data.frame",
          row.names = c(NA, -length(MATTCretirees.plannames))
        )
        
#REFORMAT ACTIVES DATA
        ############################################################
        MATTCactives.plannames = c( rep("VMERS.fillin.yos", nrow(workforce)), 
                                    rep("VSERS.fillin.yos", nrow(workforce)), 
                                    rep("VSTRS.fillin.yos", nrow(workforce)))
        ############################################################        
        MATTCactives.ageranges = c(workforce[,"age"], 
                                    
                                   workforce[,"age"],
                                    
                                   workforce[,"age"])
        ############################################################        
        MATTCactives.ea = c(workforce[,"ea"], 
                            
                            workforce[,"ea"], 
                            
                            workforce[,"ea"])
        ############################################################        
        MATTCactives.nactives = c(workforce[,"nactives"], 
                                  
                                  workforce[,"nactives"], 
                                  
                                  workforce[,"nactives"])
        ############################################################        
        MATTCactives.salary = c(avgpay[,"salary"], 
                                
                                avgpay[,"salary"], 
                                
                                avgpay[,"salary"])
        
        #set up actives data
        MATTCactives <- structure(
          list(
            planname = MATTCactives.plannames, 
            age = MATTCactives.ageranges, 
            ea = MATTCactives.ea,
            age.cell = MATTCactives.ageranges, 
            ea.cell = MATTCactives.ea,
            nactives = MATTCactives.nactives,
            #yos.cell = rep(NA,length(MATTCactives.nactives)),
            yos.cell = MATTCactives.ageranges - MATTCactives.ea,
            salary = MATTCactives.salary
          ), 
          .Names = c("planname", "age", "ea", "age.cell", "ea.cell", "nactives", "yos.cell", "salary"), 
          class = "data.frame",
          row.names = c(NA, -length(MATTCactives.plannames))
        )
        ############################################################        
        
        actives = MATTCactives
        retirees = MATTCretirees
        salgrowth.hist = MATTCsalgrowth.hist
        salgrowth.assume = MATTCsalgrowth.assume
        salgrowth = MATTCsalgrowth
        
      #--------------------------------------------------------------------------------------

      source(Model_Control_Script)
      
#    } else {
#      print("Something wrong with decrements or pp.prototypes")
#    }
#  } else {
#  print("Something wrong with devtools or microbenchmark")
#}
source("MattC_graphics.R")
source("MattC_charts.R")

 