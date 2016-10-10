##Read Data
library(shiny)
library(ggplot2)
library(ggrepel)
projDta <- readRDS("popnDta.rds")
projDta$variable <- as.numeric(as.character(projDta$variable))
#Add "Over" before age cohorts
projDta[projDta$Age =="65"|projDta$Age =="75"|projDta$Age =="85", 1] <- paste("Over",projDta[projDta$Age =="65"|projDta$Age =="75"|projDta$Age =="85", 1])