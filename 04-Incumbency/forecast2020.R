library(tidyverse)

#####------------------------------------------------------#
##### Read and merge data ####
#####------------------------------------------------------#

setwd("/Users/samlowry/iCloud Drive (Archive) - 1/Documents/rstudio/Enos/1347_labs/04-Incumbency")


popvote_df    <- read_csv("data/popvote_1948-2016.csv")
pvstate_df    <- read_csv("data/popvote_bystate_1948-2016.csv")
economy_df    <- read_csv("data/econ.csv")
approval_df   <- read_csv("data/approval_gallup_1941-2020.csv")
pollstate_df  <- read_csv("data/pollavg_bystate_1968-2016.csv")
fedgrants_df  <- read_csv("data/fedgrants_bystate_1988-2008.csv")

summary(lm(popvote_df, formula = pv2p ~ incumbent))

#####------------------------------------------------------#
#####  Time-for-change model ####
#####------------------------------------------------------#

tfc_df <- popvote_df %>%
  filter(incumbent_party) %>%
  select(year, candidate, party, pv, pv2p, incumbent) %>%
  inner_join(
    approval_df %>% 
      group_by(year, president) %>% 
      slice(1) %>% 
      mutate(net_approve=approve-disapprove) %>%
      select(year, incumbent_pres=president, net_approve, poll_enddate),
    by="year"
  ) %>%
  inner_join(
    economy_df %>%
      filter(quarter == 2) %>%
      select(GDP_growth_qt, year),
    by="year"
  )

## TODO:

## - fit 

## - evaluate

## - compare to previous models

