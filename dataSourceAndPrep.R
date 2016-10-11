##Data source and preparation
library(readxl)
library(reshape2)
dtaLA <- data.frame()
for(i in 4:35){
  dta <- read_excel("Q:/PopulationProjections/detailed LA 2012-pop proj.xlsx", sheet = i, skip = 1, col_name = FALSE)
  dta[2,1] <- dta[1,1]
  dta <- dta[c(2, 5:96),c(1:14,17:29)]
  dta[1,2] <- "2012"
  dta[1] <- apply(dta[1], MARGIN = 1, function(x) gsub("\\.000000", "", x))
  dta[2] <- apply(dta[2], MARGIN = 1, function(x) gsub("\\.00", "", x))
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
  dta <- dta[c(1,93:95,98),] 
  cnc <- colnames(dta)[1]
  colnames(dta)[1] <- "Age"
  dta <- melt(dta)
  dta$LA <- cnc
  dtaLA <- rbind(dtaLA, dta)
}

#save 
saveRDS(dtaLA, "Q:/PopulationProjections/popnDta.rds")
