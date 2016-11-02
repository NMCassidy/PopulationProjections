##Read Data
library(shiny)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(DT)
library(reshape2)
library(shinythemes)
library(readr)
#library(quantmod)  #For year on year change
projDta <- readRDS("popnDta.rds")
projDta$variable <- as.numeric(as.character(projDta$variable))
#Add "Over" before age cohorts
projDta[projDta$Age =="65"|projDta$Age =="75"|projDta$Age =="85", 1] <- paste("Aged",projDta[projDta$Age =="65"|projDta$Age =="75"|projDta$Age =="85", 1], "and Over")
projDta[projDta$Age == "15", 1] <-"Aged 15 and Under"
projDta$value <- round(projDta$value, 2)
dlData <- dcast(projDta, LA ~ Age + variable)
#Read Adjusted data and make same alterations
projDtaAdj <- readRDS("popnDtaAdjusted.rds")
projDtaAdj$variable <- as.numeric(as.character(projDtaAdj$variable))
projDtaAdj[projDtaAdj$Age =="65"|projDtaAdj$Age =="75"|projDtaAdj$Age =="85", 1] <- paste("Aged",projDtaAdj[projDtaAdj$Age =="65"|projDtaAdj$Age == "75"|projDtaAdj$Age =="85", 1], "and Over")
projDtaAdj[projDtaAdj$Age =="15",1] <- "Aged 15 and Under"
projDtaAdj$value <- round(projDtaAdj$value, 2)

#Read healthy life expectancy data
HLEdta<- read_csv("HealthyLE.csv")
