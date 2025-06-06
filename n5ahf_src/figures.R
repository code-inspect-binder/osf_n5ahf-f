## Script Information -------------
##
## Script name: figures.R
##
## Purpose of script: Reproduce figure from "Double Hit" manuscript
##
## Author: Dr. Aaron R. Caldwell
##
## Date Created: 2020-02-24
##
## 
## Email: aaron.r.caldwell2.ctr@mail.mil
##
## Notes: All figures should be reproduced. 
##   All data did not make it in the manuscript.
##    Therefore, feel free to explore!
##


# Import Packages -------
library(readr)
library(tidyverse)
library(Hmisc)
library(labelled)
library(ggpubr)
theme_set(theme_bw())

# Import Data ---------------
df_HIT <- read_csv("df_HIT.csv", 
                         na = ".") %>%
  mutate(Type = as.factor(Type),
         ID = as.factor(ID),
         Recovery = as.factor(Recovery),
         SacTime = as.factor(SacTime),
         log_CK = 100*log(CK),
         log_AST = 100*log(AST)) %>%
  mutate(SacTime = fct_relevel(SacTime, "min30","hr3"))
# levels(df_HIT$SacTime)

## Export transformed data
write.csv(df_HIT, "df_hit2.csv")
## ---------------------------

# Functions and Settings for plots --------
stat_sum_df = function(fun,colour = colour, geom="pointrange", ...) {
  stat_summary(fun.data = fun,geom=geom, ...)
}

pos_1 <- position_jitterdodge(
  jitter.width  = 0.25,
  jitter.height = 0,
  dodge.width   = .75
)

# Create new dataset -------
# This dataset contains many analytes NOT presented in the manuscript.
df_HITvis <- df_HIT %>%
  filter(Type == "EHS") %>%
  mutate(Recovery2 = factor(Recovery,
                           labels = c("EHI0",
                                      "EHI1",
                                      "EHI3",
                                      "EHI7")),
         SacTime2 = factor(SacTime,
                           labels = c("min-30",
                                      "hr-3",
                                      "day-1",
                                      "day-7")))
# Get level ordering correct

levels(df_HITvis) = c("EHI0","EHI1","EHI3","EHI7")

# Create control dataset for inflammation
df_inflam_CON = read_csv("df_dat2.csv", 
                         na = ".") %>%
  mutate(Recovery = as.factor(Recovery),
         SacTime = as.factor(SacTime)) %>%
  mutate(SacTime = fct_relevel(SacTime, "min30","hr3")) %>% 
  mutate(
    logFABP2 = 100*log(FABP2),
    logHSP70PL = 100*log(HSP70PL),
    logCORT = 100*log(Cort),
    log_HSP70LIV = 100*log(HSP70LIV)
  ) %>% filter(Type == "EXC")

# Create active treatment datasets
df_inflam <- read_csv("df_dat2.csv", 
                      na = ".") %>%
  mutate(Type = as.factor(Type),
         ID = as.factor(Animal),
         Recovery = as.factor(Recovery),
         SacTime = as.factor(SacTime)) %>%
  mutate(SacTime = fct_relevel(SacTime, 
                               "min30","hr3")) %>%
  mutate(Recovery2 = factor(Recovery,
                            labels = c("EHI0",
                                       "EHI1",
                                       "EHI3",
                                       "EHI7")),
         SacTime2 = factor(SacTime,
                           labels = c("min-30",
                                      "hr-3",
                                      "day-1",
                                      "day-7"))) %>% 
  filter(Type == "EHS")


# Figure 2 -----------

source("fig2.R")

# Figure 3 -----------

source("fig3.R")

# Figure 4 -----------

source("fig4.R")

# Figure 5 -----------

source("fig5.R")

# Figure 6 ---------

source("fig6.R")
