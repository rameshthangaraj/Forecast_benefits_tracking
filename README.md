# Forecast_benefits_tracking

Product_Data <- read.csv("PL_DOW/Input/747004_EP_Data.csv", header=TRUE, sep=",")
Test_Data <- Product_Data[(as.Date(Product_Data$DAY_DT,'%d-%b-%y') >= as.Date('16-May-16','%d-%b-%y')),]
Old_Dows <- read.csv("PL_DOW/Input/747004_Old_Dows.csv", header=TRUE, sep=",")

Stores_Hier <- read.csv("Data/Stores_Hier.csv", header=TRUE, sep=",")

All_Locations <- unique(Test_Data$LOCATION_ID)

Location <- NULL
E_1 <- NULL
E_2 <- NULL
E_3 <- NULL
E_4 <- NULL
Index = 1

for(x in 1:length(All_Locations)) {
  
  L_ID <- paste(All_Locations[x], '', sep='')
  STG_Value <- paste(Stores_Hier[Stores_Hier$LOCATION_ID==L_ID,]$HIER1_LEVEL1_ID, '', sep='')
  
  Old_Dows_Values <- Old_Dows[Old_Dows$LOCATION_ID==STG_Value,]
  New_Dows_Values <- Final_Dow_Data[Final_Dow_Data$LOCATION_ID==L_ID,]
  
  if(nrow(Old_Dows_Values)==7 & nrow(New_Dows_Values)==7) {
  
   Location_Data <- Test_Data[Test_Data$LOCATION_ID==L_ID,]
   Location_Data$DOW <- weekdays(as.Date(Location_Data$DAY_DT,'%d-%b-%y'))
  
   Location_Data_Sunday <- Location_Data[Location_Data$DOW=='Sunday',]
   Location_Data_Sunday$C_Forecast <- (Location_Data_Sunday$FORECAST/Old_Dows_Values[Old_Dows_Values$DOW==1,]$DOW_Index) * New_Dows_Values[New_Dows_Values$DOW==1,]$DOW_Index
   
   Location_Data_Monday <- Location_Data[Location_Data$DOW=='Monday',]
   Location_Data_Monday$C_Forecast <- (Location_Data_Monday$FORECAST/Old_Dows_Values[Old_Dows_Values$DOW==2,]$DOW_Index) * New_Dows_Values[New_Dows_Values$DOW==2,]$DOW_Index
   
   Location_Data_Tuesday <- Location_Data[Location_Data$DOW=='Tuesday',]
   Location_Data_Tuesday$C_Forecast <- (Location_Data_Tuesday$FORECAST/Old_Dows_Values[Old_Dows_Values$DOW==3,]$DOW_Index) * New_Dows_Values[New_Dows_Values$DOW==3,]$DOW_Index
   
   Location_Data_Wednesday <- Location_Data[Location_Data$DOW=='Wednesday',]
   Location_Data_Wednesday$C_Forecast <- (Location_Data_Wednesday$FORECAST/Old_Dows_Values[Old_Dows_Values$DOW==4,]$DOW_Index) * New_Dows_Values[New_Dows_Values$DOW==4,]$DOW_Index
   
   Location_Data_Thursday <- Location_Data[Location_Data$DOW=='Thursday',]
   Location_Data_Thursday$C_Forecast <- (Location_Data_Thursday$FORECAST/Old_Dows_Values[Old_Dows_Values$DOW==5,]$DOW_Index) * New_Dows_Values[New_Dows_Values$DOW==5,]$DOW_Index
   
   Location_Data_Friday <- Location_Data[Location_Data$DOW=='Friday',]
   Location_Data_Friday$C_Forecast <- (Location_Data_Friday$FORECAST/Old_Dows_Values[Old_Dows_Values$DOW==6,]$DOW_Index) * New_Dows_Values[New_Dows_Values$DOW==6,]$DOW_Index
   
   Location_Data_Saturday <- Location_Data[Location_Data$DOW=='Saturday',]
   Location_Data_Saturday$C_Forecast <- (Location_Data_Saturday$FORECAST/Old_Dows_Values[Old_Dows_Values$DOW==7,]$DOW_Index) * New_Dows_Values[New_Dows_Values$DOW==7,]$DOW_Index
   
   Location_CDATA = rbind(Location_Data_Sunday, Location_Data_Monday, Location_Data_Tuesday, Location_Data_Wednesday, Location_Data_Thursday, Location_Data_Friday, Location_Data_Saturday)
   
   Location_CDATA = Location_CDATA[with(Location_CDATA, order(PRODUCT_ID, LOCATION_ID, as.Date(DAY_DT, format="%d-%b-%y"))),]
   
   
   Location_CDATA$Error_1 = sum(abs(Location_CDATA$SALES_QTY - Location_CDATA$FORECAST))/sum(Location_CDATA$SALES_QTY)
   Location_CDATA$Error_2 = sum(abs(Location_CDATA$SALES_QTY - Location_CDATA$C_Forecast))/sum(Location_CDATA$SALES_QTY)
   
   
   Location_CDATA$E_3 = abs(Location_CDATA$SALES_QTY - Location_CDATA$FORECAST)/Location_CDATA$SALES_QTY
   Location_CDATA$E_3[ is.nan(Location_CDATA$E_3) ] <- 0
   Location_CDATA$E_3[ is.infinite(Location_CDATA$E_3) ] <- 0
   Location_CDATA$E_3 <- sum(Location_CDATA$E_3)/nrow(Location_CDATA)
   
   Location_CDATA$E_4 = abs(Location_CDATA$SALES_QTY - Location_CDATA$C_Forecast)/Location_CDATA$SALES_QTY
   Location_CDATA$E_4[ is.nan(Location_CDATA$E_4) ] <- 0
   Location_CDATA$E_4[ is.infinite(Location_CDATA$E_4) ] <- 0
   Location_CDATA$E_4 <- sum(Location_CDATA$E_4)/nrow(Location_CDATA)
   
   
   Location[Index] <- L_ID
   E_1[Index] <- unique(Location_CDATA$Error_1)
   E_2[Index] <- unique(Location_CDATA$Error_2)
   E_3[Index] <- unique(Location_CDATA$E_3)
   E_4[Index] <- unique(Location_CDATA$E_4)
   
   Index = Index + 1
  
 
  }
  
  
}

Final_Errors <- c(as.data.frame(Location), as.data.frame(E_1), as.data.frame(E_2), as.data.frame(E_3), as.data.frame(E_4))
Final_Errors <- as.data.frame(Final_Errors)
setnames(Final_Errors, c('Location', 'E_1', 'E_2', 'E_3', 'E_4'), c('LOCATION_ID', 'E_1', 'E_2', 'E_3', 'E_4'))


Final_Errors <- merge(x = Final_Errors, y = Stats_Data, by = "LOCATION_ID")
Final_Errors$Imp_1 = (Final_Errors$E_1*100) - (Final_Errors$E_2*100)
Final_Errors$Imp_2 = (Final_Errors$E_3*100) - (Final_Errors$E_4*100)

write.csv(Final_Errors, "PL_DOW/Final_Errors.csv")