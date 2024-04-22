################################################################################
# PACKAGES NEEDED 
################################################################################

if (!requireNamespace("haven", quietly = TRUE)) {install.packages("haven")}
if (!requireNamespace("dplyr", quietly = TRUE)) {install.packages("dplyr")}
if (!requireNamespace("lubridate", quietly = TRUE)) {install.packages("lubridate")}
if (!requireNamespace("readxl", quietly = TRUE)) {install.packages("readxl")}
if (!requireNamespace("tidyr", quietly = TRUE)) {install.packages("tidyr")}

# load packages 
library(haven)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyr)

################################################################################
# DATA CLEANING & Basic Variables Creation                                                                
################################################################################

##############                 RAW MATERIAL I                   ################                          

# Import data
Demographics <- read_sas('RawDataIDemographics.sas7bdat')

### Data Transformation 
#need to set specific format for remainder of columns because they don't contain '-' between segments like RegDate
Demographics <- Demographics %>%
  mutate(RegDate = as.Date(RegDate),
         FirstAct = as.Date(FirstAct, format = "%Y%m%d"), 
         FirstPay = as.Date(FirstPay, format = "%Y%m%d"),
         FirstSp = as.Date(FirstSp, format = "%Y%m%d"),
         FirstCa = as.Date(FirstCa, format = "%Y%m%d"),
         FirstGa = as.Date(FirstGa, format = "%Y%m%d"),
         FirstPo = as.Date(FirstPo, format = "%Y%m%d"),
         Gender = as.factor(Gender)) #convert gender to factor type

sum(duplicated(Demographics$UserID))
# no duplicates found!

# remove rows where FirstAct is null
Demographics <- Demographics[!is.na(Demographics$FirstAct),]
# removed 2 rows

# Merge with Description 
Appendix2 <- read_excel("Appendices Group Assignment.xlsx", sheet = "Appendix 2")
Demographics <- merge(Appendix2, Demographics, by= "Country", all= TRUE)

Appendix3 <- read_excel("Appendices Group Assignment.xlsx", sheet = "Appendix 3")
Demographics <- merge(Appendix3, Demographics, by= "Language", all = TRUE)

Appendix4 <- read_excel("Appendices Group Assignment.xlsx", sheet = "Appendix 4")
Demographics <- merge(Appendix4, Demographics, by = "ApplicationID", all = TRUE)

### FINAL ADJUSTMENT FOR THIS DATASET 

# Remove repeated variables 
# "ApplicationID", "Language", "Country"                                                       
Demographics <- subset(Demographics, select = -c(ApplicationID, Language, Country))

# Reorder columns with "UserID" as the first column                                                                    
Demographics <- Demographics[, c("UserID", setdiff(names(Demographics), "UserID"))]

# Check 
glimpse(Demographics)
summary(Demographics)
head(Demographics)

##############                  RAW MATERIAL II                  ###############                                       

# Read data
DailyAgg <- read_sas("RawDataIIUserDailyAggregation.sas7bdat")

### Data Transformation
# Check if there are missing values
colSums(is.na(DailyAgg))

# Format the Date
DailyAgg <- DailyAgg %>%
  mutate(Date = as.Date(Date, format = "%Y%m%d"))

# Round the number into 2 decimal 
DailyAgg$Stakes <- round(DailyAgg$Stakes,2)
DailyAgg$Winnings <- round(DailyAgg$Winnings,2)

### Merge with Description 
Appendix1 <- read_excel("Appendices Group Assignment.xlsx", sheet = "Appendix 1")
DailyAgg <- merge(Appendix1, DailyAgg, by = "ProductID", all = TRUE)

####                    AGGREGATION SUMMARY PER USERID                      ####                                       

DailyAgg <- DailyAgg %>%
  group_by(UserID) %>%
  mutate(
    FirstAggDate = min(Date),
    LastAggDate = max(Date),
    TotalAggDays = as.numeric(difftime(max(Date) + 1, min(Date), units = "days"))                                       
  ) %>%
  ungroup()


####             SUMMARY RELATED TO WINNING, STAKES, AND BETS               ####                      

# Calculate summary for each user
DailyAgg <- DailyAgg %>%
  group_by(UserID) %>%
  arrange(UserID) %>% 
  mutate(TotalWinnings = sum(Winnings),
         AvgWinnings = mean(Winnings),
         TotalStakes = sum(Stakes),
         AvgStakes = mean(Stakes),
         TotalBets = sum(Bets),
         Earning = sum(Winnings - Stakes))


####                     TOTAL PRODUCTION DESCRIPTION                       ####     
# Create the Total product description for each UserID & Earning Per Customer                              
DailyAgg <- DailyAgg %>%
  group_by(UserID) %>%
  arrange(ProductID) %>%                                                                     
  mutate(AllProductDescriptions = paste(unique(`Product Description`), collapse = ", ")) 

####               COUNTS OF PRODUCTION DESCRIPTION OCCURENCES                ####     

DailyAgg <- DailyAgg %>%
  group_by(UserID, AllProductDescriptions) %>%
  mutate(ProductComboCount = n()) %>%
  ungroup()

####     DUMMY VARIABLES AND CALCULATION BASE ON PRODUCT DESCRIPTION        ####

# Create Dummy Variables without the prefix and with actual values as column names
Dummy_ProductDescription <- model.matrix(~ `Product Description` - 1, data = DailyAgg)

# Merge with DailyAgg 
DailyAgg <- cbind(DailyAgg, Dummy_ProductDescription)

colnames(DailyAgg) <-gsub("`Product Description`","Pro ",colnames(DailyAgg))

# Create summary for each Product description 
PRODUCT <- DailyAgg %>% 
  group_by(UserID,ProductID)%>%
  summarise(Cas_BM_TWin = ifelse( `Pro Casino BossMedia`  == 1, sum(Winnings),0),
            Cas_BM_AWin = ifelse(`Pro Casino BossMedia` == 1, mean(Winnings),0),
            Cas_BM_TStakes = ifelse(`Pro Casino BossMedia` == 1, sum(Stakes),0),
            Cas_BM_AStakes = ifelse(`Pro Casino BossMedia`== 1, mean(Stakes),0),
            Cas_BM_TBets = ifelse(`Pro Casino BossMedia` == 1, sum(Bets),0),
            Cas_BM_Earn = ifelse(`Pro Casino BossMedia` == 1, sum(Winnings-Stakes),0),
            
            Cas_CW_TWin = ifelse(`Pro Casino Chartwell` == 1, sum(Winnings),0),
            Cas_CW_AWin = ifelse(`Pro Casino Chartwell` == 1, mean(Winnings),0),
            Cas_CW_TStakes = ifelse(`Pro Casino Chartwell` == 1, sum(Stakes),0),
            Cas_CW_AStakes = ifelse(`Pro Casino Chartwell` == 1, mean(Stakes),0),
            Cas_CW_TBets = ifelse(`Pro Casino Chartwell` == 1, sum(Bets),0),
            Cas_CW_Earn = ifelse(`Pro Casino Chartwell` == 1, sum(Winnings-Stakes),0),
          
            G_Bwin_TWin = ifelse(`Pro Games bwin`== 1, sum(Winnings),0),
            G_Bwin_AWin = ifelse(`Pro Games bwin`== 1, mean(Winnings),0),
            G_Bwin_TStakes = ifelse(`Pro Games bwin`== 1, sum(Stakes),0),
            G_Bwin_AStakes = ifelse(`Pro Games bwin`== 1, mean(Stakes),0),
            G_Bwin_TBets = ifelse(`Pro Games bwin`== 1, sum(Bets),0),
            G_Bwin_Earn = ifelse(`Pro Games bwin`== 1, sum(Winnings-Stakes),0),
            
            G_VS_TWin = ifelse(`Pro Games VS`== 1, sum(Winnings),0),
            G_VS_AWin = ifelse(`Pro Games VS`== 1, mean(Winnings),0),
            G_VS_TStakes = ifelse(`Pro Games VS`== 1, sum(Stakes),0),
            G_VS_AStakes = ifelse(`Pro Games VS` == 1, mean(Stakes),0),
            G_VS_TBets = ifelse(`Pro Games VS` == 1, sum(Bets),0),
            G_VS_Earn = ifelse(`Pro Games VS` == 1, sum(Winnings-Stakes),0),
            
            Pok_BM_TWin = ifelse(`Pro Poker BossMedia`== 1, sum(Winnings),0),
            Pok_BM_AWin = ifelse(`Pro Poker BossMedia`  == 1, mean(Winnings),0),
            Pok_BM_TStakes = ifelse(`Pro Poker BossMedia` == 1, sum(Stakes),0),
            Pok_BM_AStakes = ifelse(`Pro Poker BossMedia` == 1, mean(Stakes),0),
            Pok_BM_TBets = ifelse(`Pro Poker BossMedia` == 1, sum(Bets),0),
            Pok_BM_Earn = ifelse(`Pro Poker BossMedia` == 1, sum(Winnings-Stakes),0),
            
            SB_FO_TWin = ifelse(`Pro Sports book fixed-odd` == 1, sum(Winnings),0),
            SB_FO_AWin = ifelse(`Pro Sports book fixed-odd`== 1, mean(Winnings),0),
            SB_FO_TStakes = ifelse(`Pro Sports book fixed-odd`== 1, sum(Stakes),0),
            SB_FO_AStakes = ifelse(`Pro Sports book fixed-odd`== 1, mean(Stakes),0),
            SB_FO_TBets = ifelse(`Pro Sports book fixed-odd`== 1, sum(Bets),0),
            SB_FO_Earn = ifelse(`Pro Sports book fixed-odd`== 1, sum(Winnings-Stakes),0),
            
            SB_LA_TWin = ifelse(`Pro Sports book live-action`== 1, sum(Winnings),0),
            SB_LA_AWin = ifelse(`Pro Sports book live-action` == 1, mean(Winnings),0),
            SB_LA_TStakes = ifelse(`Pro Sports book live-action` == 1, sum(Stakes),0),
            SB_LA_AStakes = ifelse(`Pro Sports book live-action` == 1, mean(Stakes),0),
            SB_LA_TBets = ifelse(`Pro Sports book live-action` == 1, sum(Bets),0),
            SB_LA_Earn = ifelse(`Pro Sports book live-action` == 1, sum(Winnings-Stakes),0),
            
            TOTO_TWin = ifelse(`Pro Supertoto` == 1, sum(Winnings),0),
            TOTO_AWin = ifelse(`Pro Supertoto` == 1, mean(Winnings),0),
            TOTO_TStakes = ifelse(`Pro Supertoto` == 1, sum(Stakes),0),
            TOTO_AStakes = ifelse(`Pro Supertoto`== 1, mean(Stakes),0),
            TOTO_TBets = ifelse(`Pro Supertoto` == 1, sum(Bets),0),
            TOTO_Earn = ifelse(`Pro Supertoto` == 1, sum(Winnings-Stakes),0)
  )

# remove duplicate, keep only the rows have values 
PRODUCT <- PRODUCT[!duplicated(PRODUCT[c("UserID", "ProductID")]), ]

# Create a new data frame with summed values grouped by "UserID"
product <- aggregate(. ~ UserID, PRODUCT, sum)
product <- subset(product, select = -c(ProductID))

DailyAgg <-subset(DailyAgg, select = -c(`Product Description`,ProductID, Date, Winnings, Stakes, Bets))
DailyAgg <- merge(DailyAgg,product, by = "UserID")
DailyAgg <- DailyAgg[!duplicated(DailyAgg[c("UserID")]),]

# Check                                                                                                                  
glimpse(DailyAgg)
summary(DailyAgg)
head(DailyAgg)

##############                  RAW MATERIAL III                  ##############

# Read data
PokerChip <- read_sas("RawDataIIIPokerChipConversions.sas7bdat")

### Data Transformation
# Check if there are missing values
colSums(is.na(PokerChip))

# TransDateTime: Format the Date 
PokerChip$TransDateTime <- as.Date(PokerChip$TransDateTime, format = "%Y-%m-%d")

# TransType: Change 124 into Buy, 24 into Sell
PokerChip$TransType<-ifelse( PokerChip$TransType == "124","Buy", ifelse( PokerChip$TransType == "24", "Sell",  PokerChip$TransType))

# TransAmount: Round the number into 2 decimal 
PokerChip$TransAmount <-  round(PokerChip$TransAmount,2)

####                         TOTAL DATE OF PLAYING                          ####                                        
# Create the Total Date of Playing variable
PokerChip <- PokerChip %>% 
  group_by(UserID) %>% 
  mutate(TransactionDays = as.numeric(difftime(max(TransDateTime) + 1, min(TransDateTime), units = "days")))

####                             TIMES OF PLAYING                           ####   
# Create the Times of Playing variable
PokerChip <- PokerChip %>% 
  filter(tolower(TransType) == "sell") %>% 
  group_by(UserID) %>%
  mutate(TimesOfPlaying = n())

####                           TRANSACTION SUMMARY                          ####   
# Summarize buy and sell amount for each customer
PokerChip <- PokerChip %>%
  group_by(UserID) %>%
  mutate(Buy_Amount = sum(TransAmount[TransType == "Buy"]),
         Sell_Amount = sum(TransAmount[TransType == "Sell"]))

### FINAL ADJUSTMENT FOR THIS DATASET 

# Remove unnecessary columns 
PokerChip <- subset(PokerChip, select = -c(TransType, TransAmount,TransDateTime))

# Keep one row per unique UserID
PokerChip <- distinct(PokerChip, UserID, .keep_all = TRUE)

# Check
glimpse(PokerChip)
summary(PokerChip)
head(PokerChip)

################################################################################
# MERGE THREE DATASETS BY USERID 
################################################################################

DataMart <- merge(Demographics,DailyAgg, by= "UserID", all= TRUE)
DataMart <- merge(DataMart,PokerChip, by= "UserID", all= TRUE)

#this step is to replace NA values with 0                                                                                
DataMart <- DataMart %>% 
  mutate(Earning = ifelse(is.na(Earning), 0, Earning),
         TotalWinnings = ifelse(is.na(TotalWinnings), 0, TotalWinnings),
         AvgWinnings = ifelse(is.na(AvgWinnings), 0, AvgWinnings),
         TotalStakes = ifelse(is.na(TotalStakes), 0, TotalStakes),
         AvgStakes = ifelse(is.na(AvgStakes), 0, AvgStakes),
         TotalBets = ifelse(is.na(TotalBets), 0, TotalBets),
         TotalAggDays =ifelse(is.na(TotalAggDays), 0, TotalAggDays),
         TransactionDays =ifelse(is.na(TransactionDays), 0, TransactionDays),
         TimesOfPlaying =ifelse(is.na(TimesOfPlaying), 0, TimesOfPlaying),
         Buy_Amount =ifelse(is.na(Buy_Amount), 0, Buy_Amount),
         Sell_Amount =ifelse(is.na(Sell_Amount), 0, Sell_Amount))

################################################################################
# CALCULATION OF ADVANCE MARKETING DESCRIPTIVES 
################################################################################

##############   COMPLETE NEW VARIABLE -- LENGTH OF RELATIONSHIP  ############## 

# length of relationship (last betting activity date - regdate)
DataMart <- DataMart%>%
  mutate(LoR = LastAggDate - RegDate)
#convert Length of Relationship to numeric for future calculations
DataMart$LoR_days <- as.numeric(DataMart$LoR, units='days')
DataMart <- subset(DataMart, select = -c(LoR))

##############     COMPLETE NEW VARIABLE -- TIME TO FIRST BET     ##############                              

# length of relationship (First betting activity date - regdate)
DataMart <- DataMart%>%
  mutate(TimeToFirstBet = FirstAggDate - RegDate)
#convert Length of Relationship to numeric for future calculations
DataMart$TimeToFirstBet_days <- as.numeric(DataMart$TimeToFirstBet, units='days')
DataMart <- subset(DataMart, select = -c(TimeToFirstBet))

##############           COMPLETE NEW VARIABLE -- LOYALTY         ############## 

# loyalty -  number of bets/length of relationship                                                                
DataMart <- DataMart%>%
  mutate(Loyalty = TotalBets/LoR_days)

##############      COMPLETE NEW VARIABLE -- PROFIT LEVEL         ##############

# Profit level (look at the total spend - total won amount for each customer and segment into low/med/high)
DataMart <- DataMart%>%
  mutate(TotalSpend = Buy_Amount - Sell_Amount - Earning )

#get summary statistics on total spend to determine spend categories
summary(DataMart$TotalSpend)
#find min is -37037.76 RISK TO COMPANY PROFITS, Median is 49.60 AVG is 201.75 Max is 76165.57

#create function to cateogize the TotalSpend column for use in an lapply function
category <- function(x){
  if (is.na(x)){
    return('N/A')
  }else if (x<0){
    return('PROFIT RISK')
  }else if ((x>=0)&(x<=50)){
    return('low')
  }else if ((x>50)&(x<=1000)){
    return('medium')
  }else {
    return('high')
  }
}
#use lapply to create the categorization
DataMart$SpendCategory <- unlist(lapply(DataMart$TotalSpend, category))

glimpse(DataMart)

################################################################################
# ADDITIONAL VARIABLES FOR VISUALIZATION
################################################################################

####       AGGREGATED AVGWINNS BY PRODUCT DESCRIPTION COMBO FOR SHINY       ####     

Avgwin <- DataMart %>%
  group_by(AllProductDescriptions) %>%
  summarise(TotalAvgWins = mean(AvgWinnings))

DataMart <- DataMart %>%
  left_join(Avgwin, by = "AllProductDescriptions")

####       AGGREGATED TOTALBETS BY PRODUCT DESCRIPTION COMBO FOR SHINY      ####     

AvgTotBet <- DataMart %>%
  group_by(AllProductDescriptions) %>%
  summarise(AvgTotalBets = mean(TotalBets))

DataMart <- DataMart %>%
  left_join(AvgTotBet, by = "AllProductDescriptions")

####AGGREGATED TIMETOFIRSTBET_DAYS BY PRODUCTION DESCRIPTION COMBO FOR SHINY####     

AvgDaysToFirstBet <- DataMart %>%
  group_by(AllProductDescriptions) %>%
  summarise(AvgDaysToFirstBet = mean(TimeToFirstBet_days, na.rm = TRUE))

DataMart <- DataMart %>%
  left_join(AvgDaysToFirstBet, by = "AllProductDescriptions")

################################################################################
# MARKETING DATA MART                                                                                    
################################################################################
# Remove rows in DataMart where UserID  is NAN
DataMart <- DataMart[!is.na(DataMart$UserID), ]

# Write CSV 
write.csv(DataMart,file = "DataMart.csv")

