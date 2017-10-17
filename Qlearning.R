
rm(list=ls())
gc()
##source("R_Code/Functions.R")

# Libraries to extract
library(forecast)
library(ggplot2)
library(fpp)
library(sqldf)
library(data.table)
library(plyr)

###################################
# Combine Data + Save RDS
###################################
setwd("C:\\Users\\314354\\Documents\\QLearning")


## Start from Here
Data <- read.csv("Dynamic_Learning\\F01C_RC_Data.csv", header=TRUE, sep=",")

C_Data <- Data[!is.na(Data$RC_DEMAND),]
All_Products <- unique(C_Data$PRODUCT_ID) 

for(x in 1:length(All_Products)) {
  
  P_ID <- paste(All_Products[x], '', sep='')
  Product_Data <- C_Data[C_Data$PRODUCT_ID==P_ID,]
  All_Locations <- unique(Product_Data$LOCATION_ID)
  
  All_Locations_Data <- NULL
  
  for(i in 1:length(All_Locations)) {
    
    L_ID <- paste(All_Locations[i], '', sep='')
    
    Location_Data <- Product_Data[Product_Data$LOCATION_ID==L_ID,]
    Location_Data = Location_Data[with(Location_Data, order(PRODUCT_ID, LOCATION_ID, as.Date(LAST_REVIEW_CYCLE, format="%d-%b-%y"))),]
    
    Learning <- NULL
    Learning[1] <- NULL
    
    if(nrow(Location_Data)>=52) {
      
      for(z in 2:nrow(Location_Data)) {
        
        New_Cal_Dem <- (0.2 * Location_Data$RC_DEMAND[z]) + (0.8 * Location_Data$RC_OLD_AVG_DEMAND[z])
        New_Cal_Dem <- round(New_Cal_Dem, 2)
        Ex_Dem <- round(Location_Data$RC_AVG_DEMAND[z], 2)
        
        if( (New_Cal_Dem == Ex_Dem) && (Location_Data$RC_OLD_AVG_DEMAND[z] != Location_Data$RC_OLD_AVG_DEMAND[z-1]) ) {
          
          Learning[z] <- 1
          
        } else {
          
          Learning[z] <- 0
          
        }
        
      }##End of for(z in 1:nrow(Location_Data))
      
      Location_Data$Learning <- Learning
      
      
      if(is.null(All_Locations_Data)) {
        
        All_Locations_Data <- Location_Data
        
      } else {
        All_Locations_Data <- rbind(All_Locations_Data, Location_Data)
      }
      
      
    }##End of if(nrow(Location_Data)>=52)
    
    
  }##End of for(i in 1:
  
  FName <- paste('Dynamic_Learning\\File_', P_ID, '.csv', sep='')
  write.csv(All_Locations_Data, file = FName,row.names=FALSE)
  
  All_Locations_Data <- NULL
  
}##End of for(x in 1:



## Combine All Files into a single CSV file
Files = list.files(path="Dynamic_Learning\\" , pattern="*.csv")

length(Files)
##library(R.utils)

##lapply(Filter(function(x) countLines(x)==0, list.files(pattern='.csv')), unlink)

Final_Data <- do.call("rbind",lapply(Files, FUN=function(files){read.csv(paste("Dynamic_Learning\\", files, sep=""))}))
Final_Data$X <- NULL


## Produce Rounded Demand Figures
Final_Data$Calc_Demand <- round((0.2 * Final_Data$RC_DEMAND) + (0.8 * Final_Data$RC_OLD_AVG_DEMAND), 2)
Final_Data$RC_AVG_DEMAND <- round(Final_Data$RC_AVG_DEMAND, 2)
Final_Data[is.na(Final_Data$Learning),]$Learning <- 0

write.csv(Final_Data, "Dynamic_Learning/F01C_Final_Data.csv",row.names = FALSE)


## Calculate Average RC_Demand for each product and product-store combination
## Compare product averages with product-store averages 


## Start from Here
Final_Data <- read.csv("Dynamic_Learning/F01C_Final_Data.csv", header=TRUE, sep=",")

All_Products <- unique(Final_Data$PRODUCT_ID)

Product_ID <- NULL
Avg <- NULL


Agg <- aggregate(Final_Data$RC_DEMAND, list(Final_Data$PRODUCT_ID, Final_Data$LOCATION_ID), mean)
setnames(Agg, c('Group.1', 'Group.2', 'x'), c('PRODUCT_ID', 'LOCATION_ID', 'Avg_Demand'))

Final_Data_Comb = sqldf("select t1.*, t2.Avg_Demand from Final_Data t1, Agg t2 where t1.PRODUCT_ID=t2.PRODUCT_ID and t1.LOCATION_ID=t2.LOCATION_ID")
Final_Data_Comb <- Final_Data_Comb[Final_Data_Comb$Avg_Demand>12.0,]

Final_Data <- Final_Data_Comb

## Produce Learning Parameter Statistics
P_L_Combinations = sqldf("select PRODUCT_ID, LOCATION_ID, count(*) cnt from Final_Data where Learning=1 group by PRODUCT_ID, LOCATION_ID
                         order by cnt desc")
P_L_Combinations <- P_L_Combinations[P_L_Combinations$cnt>9,]


# Top 5 Rows
#837996       S1242  45
#837996       S1132  42
#982764       S1242  40
#252812       S1132  37
#252812       S1242  37


Product <- NULL
Location <- NULL
Weeks <- NULL
Parameter <- NULL

for(z in 1:nrow(P_L_Combinations)) {
  
  Prod <- paste(P_L_Combinations$PRODUCT_ID[z], '', sep='')
  Loc <- paste(P_L_Combinations$LOCATION_ID[z], '', sep='')
  Cnt <- P_L_Combinations$cnt[z]
  
  Sel_Data <- Final_Data[Final_Data$PRODUCT_ID==Prod & Final_Data$LOCATION_ID==Loc,]
  
  Error_Weight <- NULL
  Error_Value <- NULL
  Index <- 1
  
  for(x in 20:50) {
    
    Lear_Weight <- x/100
    New_Fcst <- (Lear_Weight * Sel_Data$RC_DEMAND) + ((1-Lear_Weight) * Sel_Data$RC_OLD_AVG_DEMAND)
    
    Error <- NULL
    
    for(i in 1:(nrow(Sel_Data)-1)) {
      
      if(Sel_Data$Learning[i]==1 && Sel_Data$Learning[i+1]==1) {
        
        Demand <- Sel_Data$RC_DEMAND[i+1]
        Fcst <- New_Fcst[i]
        
        Error[i] <- abs(Demand - Fcst)/Demand
        
      } else {
        
        Error[i] <- 0
      }
      
      
    } ## End of for(i in 1:nrow(Sel_Data))
    
    Error <- as.data.frame(Error)
    Error_NoZero <- Error[Error!=0,]
    
    Error_Weight[Index] <- Lear_Weight
    Error_Value[Index] <- sum(Error_NoZero)/length(Error_NoZero)
    
    Index <- Index + 1
    
  }## End of for(x in 1:nrow(Sel_Data))
  
  New_Lear_Para <- 19 + match(min(Error_Value), Error_Value)
  
  
  Product[z] <- Prod
  Location[z] <- Loc
  Weeks[z] <- Cnt
  Parameter[z] <- New_Lear_Para
  
  
}## End of for(z in 1:nrow(P_L_Combinations))


Final_Lear_Data <- c(as.data.frame(Product), as.data.frame(Location), as.data.frame(Weeks), as.data.frame(Parameter))
Final_Lear_Data <- as.data.frame(Final_Lear_Data)

write.csv(Final_Lear_Data, "Dynamic_Learning/Final_Learning_Data.csv",row.names = FALSE)

Lear_D_Stats = sqldf("select Parameter, count(*) cnt from Final_Lear_Data group by Parameter
                     order by Parameter")
write.csv(Lear_D_Stats, "Dynamic_Learning/Learning_D_Stats.csv",row.names = FALSE)


Stat_1 = sqldf("select Product, count(*) cnt from Final_Lear_Data where Parameter<>20 group by Product order by cnt desc")
write.csv(Stat_1, "Dynamic_Learning/Product_Counts.csv",row.names = FALSE)

Stat_1 = sqldf("select Product, count(*) cnt from Final_Lear_Data where Parameter<>20 order by cnt desc")
write.csv(Stat_1, "Dynamic_Learning/Product_Counts1.csv")


## Start from Here
Data <- read.csv("Dynamic_Learning/Final_Learning_Data.csv", header=TRUE, sep=",")

Data <- Data[Data$Parameter!=20,]


FileText <- ""

for(i in 1:nrow(Data)) {
  
  P_ID <- paste(Data$Product[i], '', sep='')
  
  if(nchar(P_ID)==5) {
    
    P_ID <- paste('000', P_ID, '', sep='')
    
  } else if(nchar(P_ID)==6) {
    
    P_ID <- paste('00', P_ID, '', sep='')
    
  }
  
  ##Update qr_product_location set weight_1=0.4 where product_id = â??00485326'
  
  newLineText <-  paste('Update qr_product_location set weight_1=', Data$Parameter[i]/100 ,' where product_id=\'',
                        P_ID, '\' and location_id=\'', Data$Location[i], '\'', '\n', sep='')
  FileText <- paste(FileText, newLineText, sep='')
  
}

write(FileText, "Dynamic_Learning/F01C_Data_Fix.txt")