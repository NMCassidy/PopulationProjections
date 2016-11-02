##Data source and preparation
library(readxl)
library(reshape2)
dtaLA <- data.frame()
for(i in 6:37){
 # dta <- read_excel("Q:/PopulationProjections/detailed LA 2012-pop proj.xlsx", sheet = i, skip = 1, col_name = FALSE)
  dta <- read_excel("Q:/PopulationProjections/pop-proj-scot-areas-14-det-tab-ca-area.xlsx", sheet = i, skip = 1, col_name = FALSE)
  dta[2,1] <- dta[1,1]
  dta <- dta[c(2, 5:96),1:27]
  #dta[1] <- apply(dta[1], MARGIN = 1, function(x) gsub("\\.000000", "", x))
  #dta[2] <- apply(dta[2], MARGIN = 1, function(x) gsub("\\.00", "", x))
  colnames(dta) <- dta[1,]
  dta <- dta[-1,]
  row.names(dta) <- 1:nrow(dta)
  dta[2:27] <-apply(dta[2:27], MARGIN = 2, function(x) as.numeric(x))
  dta[93,] <- c(65, apply(dta[67:92,2:27], MARGIN = 2, sum))
  dta[94,] <- c(75, apply(dta[77:92, 2:27], MARGIN = 2, sum))
  dta[95,] <- c(85, apply(dta[87:92, 2:27], MARGIN =2, sum))
  dta[96,] <- c(999, apply(dta[c(2:17, 67:92), 2:27], MARGIN = 2, sum))
  dta[97,] <- c(99, apply(dta[18:66, 2:27], MARGIN = 2, sum))
  dta[98,] <- c(00, dta[96, 2:27]/dta[97, 2:27]*100)
  dta[98,1] <- "Dependency Ratio"
  dta[99,] <- c(15, apply(dta[2:17, 2:27], MARGIN = 2, sum))
  dta <- dta[c(1,99,93:95,98),] 
  cnc <- colnames(dta)[1]
  colnames(dta)[1] <- "Age"
  dta <- melt(dta)
  dta$LA <- cnc
  dtaLA <- rbind(dtaLA, dta)
}

#save 
saveRDS(dtaLA, "Q:/PopulationProjections/popnDta.rds")

#Adjusted figures 75-84 = *2; 85+ = *3
dtaLAA <- dtaLA
#adj75 <- tapply(dtaLA$value, list(dtaLA$LAA, dtaLAA$variable), function(x) {x[4]-x[5]}) 
#adj75 <- adj75*2
#adj75 <- as.data.frame(adj75)
dtaLAA[dtaLAA$Age == "All ages", 3] <- dtaLAA[dtaLAA$Age == "All ages",3] - dtaLAA[dtaLAA$Age == "65", 3]
dtaLAA[dtaLAA$Age == "65", 3] <- dtaLAA[dtaLAA$Age == "65", 3] - dtaLAA[dtaLAA$Age == "75", 3]
dtaLAA[dtaLAA$Age == "75", 3] <- (dtaLAA[dtaLAA$Age== "75", 3] - dtaLAA[dtaLAA$Age =="85", 3]) *2
dtaLAA[dtaLAA$Age == "85", 3] <- dtaLAA[dtaLAA$Age == "85", 3] * 3
dtaLAA[dtaLAA$Age == "75", 3] <- dtaLAA[dtaLAA$Age == "75", 3] + dtaLAA[dtaLAA$Age == "85", 3]
dtaLAA[dtaLAA$Age == "65", 3] <- dtaLAA[dtaLAA$Age == "65", 3] + dtaLAA[dtaLAA$Age == "75", 3]
dtaLAA[dtaLAA$Age == "All ages", 3] <- dtaLAA[dtaLAA$Age == "All ages", 3] + dtaLAA[dtaLAA$Age == "65", 3]
dtaLAA[dtaLAA$Age == "Dependency Ratio", 3] <- (dtaLAA[dtaLAA$Age == "15", 3] + dtaLAA[dtaLAA$Age == "65", 3]) / (dtaLAA[dtaLAA$Age == "All ages", 3] - dtaLAA[dtaLAA$Age == "15",3] - 
                                               dtaLAA[dtaLAA$Age == "65",3]) *100
#save
saveRDS(dtaLAA, "Q:/PopulationProjections/popnDtaAdjusted.rds")
