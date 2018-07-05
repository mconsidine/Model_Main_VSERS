#MattC This is the code used in the markdown routine, editted to run interactively in a console window

#date: '`r format(Sys.time(), "%B %d, %Y")`'
#{r mainSet options, echo=FALSE, cache=FALSE}

#```{r Preamble, echo = FALSE, include  = FALSE}
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(magrittr) # to use %<>%

#rm(list = ls())
source("MattC_graphics.R")
source("Functions.R")



#{r Read, echo=FALSE, include = FALSE}

folder_run <- "IO_M2.1_new"
file_select <- dir(folder_run, pattern = "Outputs_VSERS_") #20180705190559.RData

fn <- function(x) {
  load(paste0(folder_run, "/", x))
  outputs_list
}

lists_all        <- alply(file_select, 1, fn)
names(lists_all) <- laply(lists_all, function(x) x$paramlist$runname)

## Combine the results into a single data frame.
results_all <- ldply(lists_all, function(x) x$results) %>% select(-X1)

#{r, echo = FALSE}

# select variables to be displayed in the kable function. See below for a list of all avaiable variables and explanations.

var.display <- c("runname", "year",  "AL",    "AA",   "FR", "NC",    "SC", "UAAL",
                 "AL_PR", "NC_PR", "C_PR", "ERC_PR", "PR")

r1 <- results_all %>% filter(sim %in% 1, year == 1) %>% select(one_of(var.display))
kable(r1, digits = 2)

r1$PR_growth


# Variables to Examine
#- **FR:** Funded ratio
#- **ERC_PR:** Employer contribution rates
#- **C_PR:** Total contribution rates
#- **B_PR:** Benefit payment rates 


# Funding Policies:
#**F1:** Highly smoothed

#- asset smoothing: 10 years;
#- amortization: constant percent, 30 years, open


#**F2:** highly unsmoothed

#- market assets;
#- amortization: constant dollar, 10 years, closed


#**F3:** typical

#- asset smoothing: 5 years;
#- amortization constant percent, 20 years, open



# Notes

#Using rp2000.hybrid

# Scenario 4 
#Fixed discount rate 7.5%, stochastic investment return arithmetic mean 7.5%, sd 12%


#{r S4, echo = FALSE, cache = F}

prefix <- "R4F"

draw_quantiles(paste0(prefix, 1:3), "FR", ylim = c(-5, 250))
MATTC_draw_quantiles(paste0(prefix, 1:3), "FR", ylim = c(-5, 250), yscaleincr=50) %>% print #MattC

draw_quantiles(paste0(prefix, 1:3), "ERC_PR", ylim = c(-2, 30)) %>% print #MattC

draw_quantiles(paste0(prefix, 1:3), "C_PR", ylim = c(-2, 30)) %>% print #MattC

draw_quantiles(paste0(prefix, 1:3), "B_PR") %>% print #MattC


# Scenario 6 
#Fixed Return Fixed discount rate and investment return, 7.5% each, with a 5-year period of low employer contributions


#{r S6, echo = FALSE, cache = F, eval = T}

prefix <- "R6F"

draw_quantiles(paste0(prefix, 1:3), "FR", ylim = c(40, 120))

draw_quantiles(paste0(prefix, 1:3), "ERC_PR")

draw_quantiles(paste0(prefix, 1:3), "C_PR")

draw_quantiles(paste0(prefix, 1:3), "B_PR")


# Look at the Geometric returns in Scenario 4
#{r geoReturn, echo = FALSE}

get_geoReturn <- function(x) prod(1 + x)^(1/length(x)) - 1

df <- results_all %>% filter(runname == "R4F3") %>%  group_by(sim) %>% 
  summarise(geoReturn = get_geoReturn(i.r))

df$geoReturn %>% summary
df$geoReturn %>% hist(40, FALSE,  main = "Histogram of Geometric Returns")
abline(v = 0.075, col = "red")

#{r move_file, eval= FALSE}
##################################################################MattC code starts here
dev.off()
prefix <- "R4F"

draw_quantiles(paste0(prefix, 1:3), "FR", ylim = c(-5, 250)) %>% print

library(plotrix)
matttest1 <- MATTC_get_quantiles(runName = paste0(prefix, 1:3), 
                            varName = "FR",
                            data    = results_all,
                            year.max = 80,
                            qts = c(0.1, 0.25, 0.5, 0.75, 0.9)) #  %>% 
  #gather(Quantile, Value, -runname, -year)
#matttest1<-get_quantiles(paste0(prefix, 1:3), "FR") #%>% print
vals <- matttest1[,-c(1,2)]
qbreaks <- dim(vals)[2]
qhalf <- (qbreaks-1)/2
qrows <- nrow(vals)
op <- par(mar = c(5,5,4,2) + 0.1)
cols <- color.scale(1:qbreaks,extremes=c("darkgreen","grey"))
matplot(vals, type="n", xlab = 'Years', ylab = 'Ratio',
        ylim = c(0,250),
        main = 'Projected ratios for a hypothetical plan', yaxt="n")

for (i in 1:qhalf){
  lines(vals[,i],col=cols[qbreaks-i+1]) 
  lines(vals[,qbreaks-i+1],col=cols[qbreaks-i+1])
  polygon(c(seq(1:qrows),rev(seq(1:qrows))),c(vals[,qbreaks-i+1],rev(vals[,i])),col=cols[qbreaks-i+1],border="NA")
}

abline(h=c(0.8*100),lty=2,col="red")
grid(NULL,NULL,lty=3, col="cornsilk2")
#pts <- c(pretty(as.matrix(vals), n=7),0.8*100)
pts <- c(seq(0,round(max(vals)+50,-2),50),80)
axis(2, at = pts, labels = paste(pts, "%", sep=""), las=2)
lines(vals[,qhalf+1],col="black")
box()
par(op)
