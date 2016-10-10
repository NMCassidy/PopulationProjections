##Read Data
library(shiny)
library(ggplot2)
library(ggrepel)
projDta <- readRDS("popnDta.rds")
projDta$variable <- as.numeric(as.character(projDta$variable))