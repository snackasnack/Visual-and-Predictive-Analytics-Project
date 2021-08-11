setwd("C:/Users/yodar/Desktop/NTU Year 2.1/Business/BC2406 - Analytics 1/Project")
library(data.table)
library(ggplot2)
library(stringr)
library(quanteda) 
library(readtext)
library(reshape2)
library(dplyr)
library(matrixStats)
library (tidyverse)
library(caret)
library(splitstackshape)
library(car)
library(rpart)
library(rpart.plot)

data <- fread("Mutual Funds.csv")

#=================================== Columns in dataset ===========================================
colnames(data)
# categorising the variables
# 1) 1:2 fund identification
# 2) 3:19 fund statistics 
# 3) 20:30 holding positions by industry
# 4) 31:39 fund ratings
# 5) 40:62 fund returns
# 6) 63:74 alphas and betas (alpha: amount that the investment has returned in comparison to the 
#                                   market index or other broad benchmark that it is compared against.
#                           beta: measures volatility of an investment, indication of relative risks.)
# 7) 75:80 fund mean annual returns
# 8) 81:86 fund r-squared (measure of the % of a fund's performance as a result of the benchmark.)
# 9) 87:92 fund SD (consistency of return over time)
# 10) 93:98 fund sharpe (average return earned in excess of the risk-free rate per
#                         unit of volatility or total risk, adjusted by SD)
# 11) 99:104 fund treynor ratio (indicates how much return an investment earned
#                                for the amount of risk the investment assumed.)

#=================================== Cleaning data ===========================================

# data specifically for Large Growth and the sum of industry sector is close to 100
data1 <- data[which(category == 'Large Growth'& ((basic_materials +consumer_cyclical + financial_services+real_estate+ consumer_defensive+ healthcare+ utilities+ communication_services+ energy+ industrials+ technology) >99.9)& ((basic_materials +consumer_cyclical + financial_services+real_estate+ consumer_defensive+ healthcare+ utilities+ communication_services+ energy+ industrials+ technology) < 100.1))]

#sum of number of sectors for each fund
data2 <- data1[, .(basic_materials, consumer_cyclical, financial_services,real_estate, consumer_defensive, healthcare, utilities, communication_services, energy, industrials, technology)]
data2$NumOfSector <- rowSums(data2>0) 

#find which sector is the max for each fund
data2 <- data2[, col_max := colnames(.SD)[max.col(.SD, ties.method = "first")]]

#merge the 2 columns with data1
data1$NumOfSector <- data2$NumOfSector
data1$highest <- data2$col_max

#remove all those with funds returns = 0 for 3, 5 and 10 years or if it is empty
data1 <- data1[which(fund_return_1year != '' & fund_return_3years != 0 & fund_return_5years != 0 & fund_return_10years != 0)]

#================================== Data Exploration ==============================================================================

#visualize 4 graphs in the same plot
par(mfrow= c(2,2))
#sketch number of sectors against returns over 1,3,5,10 years
scatter.smooth(data1$NumOfSector,data1$fund_return_1year)
scatter.smooth(data1$NumOfSector,data1$fund_return_3years)
scatter.smooth(data1$NumOfSector,data1$fund_return_5years)
scatter.smooth(data1$NumOfSector,data1$fund_return_10years)

#for 1,3,5 years, can see a clear downward trend as the num of sector increase
#For 10 years, after 6 sectors, the returns stabilize even as sector increase
#The least amount of sectors  is 3

# 10 years
#sketch number of sectors against returns over 10 years with the highest % in the sector
ggplot(data=data1,aes(NumOfSector, fund_return_10years, linetype = highest, color=highest))+
  geom_point()+
  geom_smooth(method = 'lm')

#technology has the least negative gradient
#consumer cyclical has the highest negative gradient

#individually plot and see each fund with the highest sector against number of sectors
data_cyc <- data1[which(highest == 'consumer_cyclical')]
data_fin <- data1[which(highest == 'financial_services')]
data_health <- data1[which(highest == 'healthcare')]
data_ind <- data1[which(highest == 'industrials')]
data_tech <- data1[which(highest == 'technology')]


ggplot(data=data_cyc,aes(NumOfSector, fund_return_10years, linetype = highest))+
  geom_point()+
  geom_smooth(method = 'lm')


ggplot(data=data_fin,aes(NumOfSector, fund_return_10years, linetype = highest))+
  geom_point()+
  geom_smooth(method = 'lm')

ggplot(data=data_health,aes(NumOfSector, fund_return_10years, linetype = highest))+
  geom_point()+
  geom_smooth(method = 'lm')

ggplot(data=data_ind,aes(NumOfSector, fund_return_10years, linetype = highest))+
  geom_point()+
  geom_smooth(method = 'lm')

ggplot(data=data_tech,aes(NumOfSector, fund_return_10years, linetype = highest))+
  geom_point()+
  geom_smooth(method = 'lm')

#visualize how the returns change for each sector as the percentage of each sector increase
scatter.smooth(data1$consumer_cyclical,data1$fund_return_10years)
scatter.smooth(data1$basic_materials,data1$fund_return_10years)
scatter.smooth(data1$financial_services,data1$fund_return_10years)
scatter.smooth(data1$real_estate,data1$fund_return_10years)
scatter.smooth(data1$consumer_defensive,data1$fund_return_10years)
scatter.smooth(data1$healthcare,data1$fund_return_10years)
scatter.smooth(data1$utilities,data1$fund_return_10years)
scatter.smooth(data1$communication_services,data1$fund_return_10years)
scatter.smooth(data1$energy,data1$fund_return_10years)
scatter.smooth(data1$industrials,data1$fund_return_10years)
scatter.smooth(data1$technology,data1$fund_return_10years)
#technology sees an increase in return as propotion of technology increase while the rest of the factors remains largely constant

#-------------------------1,3,5 years---------------------------------------------------------------
# 5 years
#sketch number of sectors against returns over 5 years with the highest % in the sector
ggplot(data=data1,aes(NumOfSector, fund_return_5years, linetype = highest, color=highest))+
  geom_point()+
  geom_smooth(method = 'lm')

# 3 years
#sketch number of sectors against returns over 3 years with the highest % in the sector
ggplot(data=data1,aes(NumOfSector, fund_return_3years, linetype = highest, color=highest))+
  geom_point()+
  geom_smooth(method = 'lm')

# 1 year
#sketch number of sectors against returns over 1 year with the highest % in the sector
ggplot(data=data1,aes(NumOfSector, fund_return_1year, linetype = highest, color=highest))+
  geom_point()+
  geom_smooth(method = 'lm')
#--------------------------------Num of sectors and funds-----------------------------------------------------

#number of sectors against fund r squared 10 years
ggplot(data=data1, aes(NumOfSector,fund_r_squared_10years)) + geom_point() + geom_smooth(method = 'loess')
#we can see that adjusted R^2 increase as number of sectors increase. This means that % of a fund's performance as a result of 
#the benchmark increase as number of sectors increase.

#number of sectors against fund beta 10 years
ggplot(data=data1, aes(NumOfSector,fund_beta_10years)) + geom_point() + geom_smooth(method = 'loess')
#we can see beta decrease as the number of sectors increase. This means that the volatility will decrease and stabalise as the 
#number of sectors increase.

#number of sectors against fund sharpe_ratio 10 years
ggplot(data=data1, aes(NumOfSector,fund_sharpe_ratio_10years)) + geom_point() + geom_smooth(method = 'loess')
#we can see sharpe ratio decrease as the number of sectors increase. This means that the average return per unit volatility 
#will decrease and stabalise as the number of sectors increase.

#number of sectors against fund sd 10 years
ggplot(data=data1, aes(NumOfSector,fund_standard_deviation_10years)) + geom_point() + geom_smooth(method = 'loess')
#we can see standard deviation decrease as the number of sectors increase. This means the fund is more stable and would not
#fluctuate as much.

#================================== Text Mining ==============================================================================

#sentiment analysis all years
#read in file
sentiment_analysis <- readtext("C:/Users/yodar/Desktop/NTU Year 2.1/Business/BC2406 - Analytics 1/Project/*.txt",
                               docvarsfrom = "filenames",
                               docvarnames = c("Year","type"),
                               dvsep = "_",
                               encoding = "UTF-8")
#create corpus
sa <- corpus(sentiment_analysis)
#tokenise and remove punctuation and numbers
sa.tokens <- tokens(sa, remove_punct = T, remove_numbers = T)

#construct document feature matrix and convert to data frame
sa_all <- dfm(sa.tokens, dictionary = data_dictionary_LSD2015)
sa_all.df <- convert(sa_all, to = "data.frame") 

#adjusting negatives due to negated words
sa_all.df$adj.negative <- sa_all.df$negative + sa_all.df$neg_positive - sa_all.df$neg_negative
#adjusting positives due to negated words
sa_all.df$adj.positive <- sa_all.df$positive + sa_all.df$neg_negative - sa_all.df$neg_positive
#calculate overall sentiment
sa_all.df$sentiment <- sa_all.df$adj.positive - sa_all.df$adj.negative

#return the data frame
sa_all.df

#visualize the data frame, with sentiment being bolded
sentiment.df <- data.frame(Year = substr(sa_all.df$doc_id, start = 1, stop =4),
                           Negative = sa_all.df$adj.negative, Positive = sa_all.df$adj.positive, 
                           Overall = sa_all.df$sentiment,
                           stringsAsFactors = F)
sentiment.df$Year <- as.integer(sentiment.df$Year)
sentiment.long <- melt(sentiment.df, id = "Year")
colnames(sentiment.long)[2] <- "Sentiment"
colnames(sentiment.long)[3] <- "Score"

ggplot(data = sentiment.long, aes(x = Year, y = Score, colour = Sentiment)) +
  scale_x_continuous(breaks = seq(2010, 2018, by = 1)) +
  geom_line(aes(size = Sentiment)) +
  scale_size_manual(values = c(0.1, 0.1, 2))

#================================== Bar plot to confirm =========================================================================

par(mar=c(1,1,1,1))

delete2018 <- data[,c("fund_return_2018")]
data2018 <- data[complete.cases(delete2018),]
range2018a<- quantile(data2018$fund_return_2018,0.25)-1.5*IQR(data2018$fund_return_2018)
range2018b<- quantile(data2018$fund_return_2018,0.75)+1.5*IQR(data2018$fund_return_2018)
data2018 <- data2018[fund_return_2018 > range2018a & fund_return_2018 < range2018b]

delete2017 <- data[,c("fund_return_2017")]
data2017 <- data[complete.cases(delete2017),]
range2017a<- quantile(data2017$fund_return_2017,0.25)-1.5*IQR(data2017$fund_return_2017)
range2017b<- quantile(data2017$fund_return_2017,0.75)+1.5*IQR(data2017$fund_return_2017)
data2017 <- data2017[fund_return_2017 > range2017a & fund_return_2017 < range2017b]

delete2016 <- data[,c("fund_return_2016")]
data2016 <- data[complete.cases(delete2016),]
range2016a<- quantile(data2016$fund_return_2016,0.25)-1.5*IQR(data2016$fund_return_2016)
range2016b<- quantile(data2016$fund_return_2016,0.75)+1.5*IQR(data2016$fund_return_2016)
data2016 <- data2016[fund_return_2016 > range2016a & fund_return_2016 < range2016b]

delete2015 <- data[,c("fund_return_2015")]
data2015 <- data[complete.cases(delete2015),]
range2015a<- quantile(data2015$fund_return_2015,0.25)-1.5*IQR(data2015$fund_return_2015)
range2015b<- quantile(data2015$fund_return_2015,0.75)+1.5*IQR(data2015$fund_return_2015)
data2015 <- data2015[fund_return_2015 > range2015a & fund_return_2015 < range2015b]


delete2014 <- data[,c("fund_return_2014")]
data2014 <- data[complete.cases(delete2014),]
range2014a<- quantile(data2014$fund_return_2014,0.25)-1.5*IQR(data2014$fund_return_2014)
range2014b<- quantile(data2014$fund_return_2014,0.75)+1.5*IQR(data2014$fund_return_2014)
data2014 <- data2014[fund_return_2014 > range2014a & fund_return_2014 < range2014b]

delete2013 <- data[,c("fund_return_2013")]
data2013 <- data[complete.cases(delete2013),]
range2013a<- quantile(data2013$fund_return_2013,0.25)-1.5*IQR(data2013$fund_return_2013)
range2013b<- quantile(data2013$fund_return_2013,0.75)+1.5*IQR(data2013$fund_return_2013)
data2013 <- data2013[fund_return_2013 > range2013a & fund_return_2013 < range2013b]

delete2012 <- data[,c("fund_return_2012")]
data2012 <- data[complete.cases(delete2012),]
range2012a<- quantile(data2012$fund_return_2012,0.25)-1.5*IQR(data2012$fund_return_2012)
range2012b<- quantile(data2012$fund_return_2012,0.75)+1.5*IQR(data2012$fund_return_2012)
data2012 <- data2012[fund_return_2012 > range2012a & fund_return_2012 < range2012b]

delete2011 <- data[,c("fund_return_2011")]
data2011 <- data[complete.cases(delete2011),]
range2011a<- quantile(data2011$fund_return_2011,0.25)-1.5*IQR(data2011$fund_return_2011)
range2011b<- quantile(data2011$fund_return_2011,0.75)+1.5*IQR(data2011$fund_return_2011)
data2011 <- data2011[fund_return_2011 > range2011a & fund_return_2011 < range2011b]

delete2010 <- data[,c("fund_return_2010")]
data2010 <- data[complete.cases(delete2010),]
range2010a<- quantile(data2010$fund_return_2010,0.25)-1.5*IQR(data2010$fund_return_2010)
range2010b<- quantile(data2010$fund_return_2010,0.75)+1.5*IQR(data2010$fund_return_2010)
data2010 <- data2010[fund_return_2010 > range2010a & fund_return_2010 < range2010b]

ggplot()+
  geom_boxplot(data=data2018,aes(factor(2018), y=fund_return_2018, fill = "2018"))+
  geom_boxplot(data=data2017,aes(factor(2017), y=fund_return_2017, fill = "2017"))+
  geom_boxplot(data=data2016,aes(factor(2016), y=fund_return_2016, fill = "2016"))+
  geom_boxplot(data=data2015,aes(factor(2015), y=fund_return_2015, fill = "2015"))+
  geom_boxplot(data=data2014,aes(factor(2014), y=fund_return_2014, fill = "2014"))+
  geom_boxplot(data=data2013,aes(factor(2013), y=fund_return_2013, fill = "2013"))+
  geom_boxplot(data=data2012,aes(factor(2012), y=fund_return_2012, fill = "2012"))+
  geom_boxplot(data=data2011,aes(factor(2011), y=fund_return_2011, fill = "2011"))+
  geom_boxplot(data=data2010,aes(factor(2010), y=fund_return_2010, fill = "2010"))+
  labs(x="Year",y="Funds Returns During The Year",
       titles="Comparison Of Funds Returns Over The Years",fill="Year")+
  theme(plot.title = element_text(hjust = 0.5))
#barplot confirms that our text mining is accurate as our good and bad is for text mining is reflected in the bar plot.


#================================== Linear Regression =========================================================================

#---cleaning for liner regression and CART as we can split them based on the good and bad years. (from text-mining)----------
data3$basic_materials_level <- ifelse(data3$basic_materials<quantile (data3$basic_materials,0.1,na.rm=TRUE),1,
                                      ifelse(data3$basic_materials<quantile (data3$basic_materials,0.2,na.rm=TRUE),2,
                                             ifelse(data3$basic_materials<quantile (data3$basic_materials,0.3,na.rm=TRUE),3,
                                                    ifelse(data3$basic_materials<quantile (data3$basic_materials,0.4,na.rm=TRUE),4,
                                                           ifelse(data3$basic_materials<quantile (data3$basic_materials,0.5,na.rm=TRUE),5,
                                                                  ifelse(data3$basic_materials<quantile (data3$basic_materials,0.6,na.rm=TRUE),6,
                                                                         ifelse(data3$basic_materials<quantile (data3$basic_materials,0.7,na.rm=TRUE),7,
                                                                                ifelse(data3$basic_materials<quantile (data3$basic_materials,0.8,na.rm=TRUE),8,
                                                                                       ifelse(data3$basic_materials<quantile (data3$basic_materials,0.9,na.rm=TRUE),9,10)))))))))

data3$consumer_cyclical_level <- ifelse(data3$consumer_cyclical<quantile (data3$consumer_cyclical,0.1,na.rm=TRUE),1,
                                        ifelse(data3$consumer_cyclical<quantile (data3$consumer_cyclical,0.2,na.rm=TRUE),2,
                                               ifelse(data3$consumer_cyclical<quantile (data3$consumer_cyclical,0.3,na.rm=TRUE),3,
                                                      ifelse(data3$consumer_cyclical<quantile (data3$consumer_cyclical,0.4,na.rm=TRUE),4,
                                                             ifelse(data3$consumer_cyclical<quantile (data3$consumer_cyclical,0.5,na.rm=TRUE),5,
                                                                    ifelse(data3$consumer_cyclical<quantile (data3$consumer_cyclical,0.6,na.rm=TRUE),6,
                                                                           ifelse(data3$consumer_cyclical<quantile (data3$consumer_cyclical,0.7,na.rm=TRUE),7,
                                                                                  ifelse(data3$consumer_cyclical<quantile (data3$consumer_cyclical,0.8,na.rm=TRUE),8,
                                                                                         ifelse(data3$consumer_cyclical<quantile (data3$consumer_cyclical,0.9,na.rm=TRUE),9,10)))))))))

data3$financial_services_level <- ifelse(data3$financial_services<quantile (data3$financial_services,0.1,na.rm=TRUE),1,
                                         ifelse(data3$financial_services<quantile (data3$financial_services,0.2,na.rm=TRUE),2,
                                                ifelse(data3$financial_services<quantile (data3$financial_services,0.3,na.rm=TRUE),3,
                                                       ifelse(data3$financial_services<quantile (data3$financial_services,0.4,na.rm=TRUE),4,
                                                              ifelse(data3$financial_services<quantile (data3$financial_services,0.5,na.rm=TRUE),5,
                                                                     ifelse(data3$financial_services<quantile (data3$financial_services,0.6,na.rm=TRUE),6,
                                                                            ifelse(data3$financial_services<quantile (data3$financial_services,0.7,na.rm=TRUE),7,
                                                                                   ifelse(data3$financial_services<quantile (data3$financial_services,0.8,na.rm=TRUE),8,
                                                                                          ifelse(data3$financial_services<quantile (data3$financial_services,0.9,na.rm=TRUE),9,10)))))))))

data3$real_estate_level <- ifelse(data3$real_estate<quantile (data3$real_estate,0.1,na.rm=TRUE),1,
                                  ifelse(data3$real_estate<quantile (data3$real_estate,0.2,na.rm=TRUE),2,
                                         ifelse(data3$real_estate<quantile (data3$real_estate,0.3,na.rm=TRUE),3,
                                                ifelse(data3$real_estate<quantile (data3$real_estate,0.4,na.rm=TRUE),4,
                                                       ifelse(data3$real_estate<quantile (data3$real_estate,0.5,na.rm=TRUE),5,
                                                              ifelse(data3$real_estate<quantile (data3$real_estate,0.6,na.rm=TRUE),6,
                                                                     ifelse(data3$real_estate<quantile (data3$real_estate,0.7,na.rm=TRUE),7,
                                                                            ifelse(data3$real_estate<quantile (data3$real_estate,0.8,na.rm=TRUE),8,
                                                                                   ifelse(data3$real_estate<quantile (data3$real_estate,0.9,na.rm=TRUE),9,10)))))))))
data3$consumer_defensive_level <- ifelse(data3$consumer_defensive<quantile (data3$consumer_defensive,0.1,na.rm=TRUE),1,
                                         ifelse(data3$consumer_defensive<quantile (data3$consumer_defensive,0.2,na.rm=TRUE),2,
                                                ifelse(data3$consumer_defensive<quantile (data3$consumer_defensive,0.3,na.rm=TRUE),3,
                                                       ifelse(data3$consumer_defensive<quantile (data3$consumer_defensive,0.4,na.rm=TRUE),4,
                                                              ifelse(data3$consumer_defensive<quantile (data3$consumer_defensive,0.5,na.rm=TRUE),5,
                                                                     ifelse(data3$consumer_defensive<quantile (data3$consumer_defensive,0.6,na.rm=TRUE),6,
                                                                            ifelse(data3$consumer_defensive<quantile (data3$consumer_defensive,0.7,na.rm=TRUE),7,
                                                                                   ifelse(data3$consumer_defensive<quantile (data3$consumer_defensive,0.8,na.rm=TRUE),8,
                                                                                          ifelse(data3$consumer_defensive<quantile (data3$consumer_defensive,0.9,na.rm=TRUE),9,10)))))))))

data3$healthcare_level <- ifelse(data3$healthcare<quantile (data3$healthcare,0.1,na.rm=TRUE),1,
                                 ifelse(data3$healthcare<quantile (data3$healthcare,0.2,na.rm=TRUE),2,
                                        ifelse(data3$healthcare<quantile (data3$healthcare,0.3,na.rm=TRUE),3,
                                               ifelse(data3$healthcare<quantile (data3$healthcare,0.4,na.rm=TRUE),4,
                                                      ifelse(data3$healthcare<quantile (data3$healthcare,0.5,na.rm=TRUE),5,
                                                             ifelse(data3$healthcare<quantile (data3$healthcare,0.6,na.rm=TRUE),6,
                                                                    ifelse(data3$healthcare<quantile (data3$healthcare,0.7,na.rm=TRUE),7,
                                                                           ifelse(data3$healthcare<quantile (data3$healthcare,0.8,na.rm=TRUE),8,
                                                                                  ifelse(data3$healthcare<quantile (data3$healthcare,0.9,na.rm=TRUE),9,10)))))))))

data3$utilities_level <- ifelse(data3$utilities<quantile (data3$utilities,0.1,na.rm=TRUE),1,
                                ifelse(data3$utilities<quantile (data3$utilities,0.2,na.rm=TRUE),2,
                                       ifelse(data3$utilities<quantile (data3$utilities,0.3,na.rm=TRUE),3,
                                              ifelse(data3$utilities<quantile (data3$utilities,0.4,na.rm=TRUE),4,
                                                     ifelse(data3$utilities<quantile (data3$utilities,0.5,na.rm=TRUE),5,
                                                            ifelse(data3$utilities<quantile (data3$utilities,0.6,na.rm=TRUE),6,
                                                                   ifelse(data3$utilities<quantile (data3$utilities,0.7,na.rm=TRUE),7,
                                                                          ifelse(data3$utilities<quantile (data3$utilities,0.8,na.rm=TRUE),8,
                                                                                 ifelse(data3$utilities<quantile (data3$utilities,0.9,na.rm=TRUE),9,10)))))))))

data3$communication_services_level <- ifelse(data3$communication_services<quantile (data3$communication_services,0.1,na.rm=TRUE),1,
                                             ifelse(data3$communication_services<quantile (data3$communication_services,0.2,na.rm=TRUE),2,
                                                    ifelse(data3$communication_services<quantile (data3$communication_services,0.3,na.rm=TRUE),3,
                                                           ifelse(data3$communication_services<quantile (data3$communication_services,0.4,na.rm=TRUE),4,
                                                                  ifelse(data3$communication_services<quantile (data3$communication_services,0.5,na.rm=TRUE),5,
                                                                         ifelse(data3$communication_services<quantile (data3$communication_services,0.6,na.rm=TRUE),6,
                                                                                ifelse(data3$communication_services<quantile (data3$communication_services,0.7,na.rm=TRUE),7,
                                                                                       ifelse(data3$communication_services<quantile (data3$communication_services,0.8,na.rm=TRUE),8,
                                                                                              ifelse(data3$communication_services<quantile (data3$communication_services,0.9,na.rm=TRUE),9,10)))))))))

data3$energy_level <- ifelse(data3$energy<quantile (data3$energy,0.1,na.rm=TRUE),1,
                             ifelse(data3$energy<quantile (data3$energy,0.2,na.rm=TRUE),2,
                                    ifelse(data3$energy<quantile (data3$energy,0.3,na.rm=TRUE),3,
                                           ifelse(data3$energy<quantile (data3$energy,0.4,na.rm=TRUE),4,
                                                  ifelse(data3$energy<quantile (data3$energy,0.5,na.rm=TRUE),5,
                                                         ifelse(data3$energy<quantile (data3$energy,0.6,na.rm=TRUE),6,
                                                                ifelse(data3$energy<quantile (data3$energy,0.7,na.rm=TRUE),7,
                                                                       ifelse(data3$energy<quantile (data3$energy,0.8,na.rm=TRUE),8,
                                                                              ifelse(data3$energy<quantile (data3$energy,0.9,na.rm=TRUE),9,10)))))))))

data3$industrials_level <- ifelse(data3$industrials<quantile (data3$industrials,0.1,na.rm=TRUE),1,
                                  ifelse(data3$industrials<quantile (data3$industrials,0.2,na.rm=TRUE),2,
                                         ifelse(data3$industrials<quantile (data3$industrials,0.3,na.rm=TRUE),3,
                                                ifelse(data3$industrials<quantile (data3$industrials,0.4,na.rm=TRUE),4,
                                                       ifelse(data3$industrials<quantile (data3$industrials,0.5,na.rm=TRUE),5,
                                                              ifelse(data3$industrials<quantile (data3$industrials,0.6,na.rm=TRUE),6,
                                                                     ifelse(data3$industrials<quantile (data3$industrials,0.7,na.rm=TRUE),7,
                                                                            ifelse(data3$industrials<quantile (data3$industrials,0.8,na.rm=TRUE),8,
                                                                                   ifelse(data3$industrials<quantile (data3$industrials,0.9,na.rm=TRUE),9,10)))))))))

data3$technology_level <- ifelse(data3$technology<quantile (data3$technology,0.1,na.rm=TRUE),1,
                                 ifelse(data3$technology<quantile (data3$technology,0.2,na.rm=TRUE),2,
                                        ifelse(data3$technology<quantile (data3$technology,0.3,na.rm=TRUE),3,
                                               ifelse(data3$technology<quantile (data3$technology,0.4,na.rm=TRUE),4,
                                                      ifelse(data3$technology<quantile (data3$technology,0.5,na.rm=TRUE),5,
                                                             ifelse(data3$technology<quantile (data3$technology,0.6,na.rm=TRUE),6,
                                                                    ifelse(data3$technology<quantile (data3$technology,0.7,na.rm=TRUE),7,
                                                                           ifelse(data3$technology<quantile (data3$technology,0.8,na.rm=TRUE),8,
                                                                                  ifelse(data3$technology<quantile (data3$technology,0.9,na.rm=TRUE),9,10)))))))))

data3$portfolio_cash_level <- ifelse(data3$portfolio_cash<quantile (data3$portfolio_cash,0.1,na.rm=TRUE),1,
                                     ifelse(data3$portfolio_cash<quantile (data3$portfolio_cash,0.2,na.rm=TRUE),2,
                                            ifelse(data3$portfolio_cash<quantile (data3$portfolio_cash,0.3,na.rm=TRUE),3,
                                                   ifelse(data3$portfolio_cash<quantile (data3$portfolio_cash,0.4,na.rm=TRUE),4,
                                                          ifelse(data3$portfolio_cash<quantile (data3$portfolio_cash,0.5,na.rm=TRUE),5,
                                                                 ifelse(data3$portfolio_cash<quantile (data3$portfolio_cash,0.6,na.rm=TRUE),6,
                                                                        ifelse(data3$portfolio_cash<quantile (data3$portfolio_cash,0.7,na.rm=TRUE),7,
                                                                               ifelse(data3$portfolio_cash<quantile (data3$portfolio_cash,0.8,na.rm=TRUE),8,
                                                                                      ifelse(data3$portfolio_cash<quantile (data3$portfolio_cash,0.9,na.rm=TRUE),9,10)))))))))

data3$portfolio_stocks_level <- ifelse(data3$portfolio_stocks<quantile (data3$portfolio_stocks,0.1,na.rm=TRUE),1,
                                       ifelse(data3$portfolio_stocks<quantile (data3$portfolio_stocks,0.2,na.rm=TRUE),2,
                                              ifelse(data3$portfolio_stocks<quantile (data3$portfolio_stocks,0.3,na.rm=TRUE),3,
                                                     ifelse(data3$portfolio_stocks<quantile (data3$portfolio_stocks,0.4,na.rm=TRUE),4,
                                                            ifelse(data3$portfolio_stocks<quantile (data3$portfolio_stocks,0.5,na.rm=TRUE),5,
                                                                   ifelse(data3$portfolio_stocks<quantile (data3$portfolio_stocks,0.6,na.rm=TRUE),6,
                                                                          ifelse(data3$portfolio_stocks<quantile (data3$portfolio_stocks,0.7,na.rm=TRUE),7,
                                                                                 ifelse(data3$portfolio_stocks<quantile (data3$portfolio_stocks,0.8,na.rm=TRUE),8,
                                                                                        ifelse(data3$portfolio_stocks<quantile (data3$portfolio_stocks,0.9,na.rm=TRUE),9,10)))))))))

data3$portfolio_bonds_level <- ifelse(data3$portfolio_bonds<quantile (data3$portfolio_bonds,0.1,na.rm=TRUE),1,
                                      ifelse(data3$portfolio_bonds<quantile (data3$portfolio_bonds,0.2,na.rm=TRUE),2,
                                             ifelse(data3$portfolio_bonds<quantile (data3$portfolio_bonds,0.3,na.rm=TRUE),3,
                                                    ifelse(data3$portfolio_bonds<quantile (data3$portfolio_bonds,0.4,na.rm=TRUE),4,
                                                           ifelse(data3$portfolio_bonds<quantile (data3$portfolio_bonds,0.5,na.rm=TRUE),5,
                                                                  ifelse(data3$portfolio_bonds<quantile (data3$portfolio_bonds,0.6,na.rm=TRUE),6,
                                                                         ifelse(data3$portfolio_bonds<quantile (data3$portfolio_bonds,0.7,na.rm=TRUE),7,
                                                                                ifelse(data3$portfolio_bonds<quantile (data3$portfolio_bonds,0.8,na.rm=TRUE),8,
                                                                                       ifelse(data3$portfolio_bonds<quantile (data3$portfolio_bonds,0.9,na.rm=TRUE),9,10)))))))))

data3$portfolio_others_level <- ifelse(data3$portfolio_others<quantile (data3$portfolio_others,0.1,na.rm=TRUE),1,
                                       ifelse(data3$portfolio_others<quantile (data3$portfolio_others,0.2,na.rm=TRUE),2,
                                              ifelse(data3$portfolio_others<quantile (data3$portfolio_others,0.3,na.rm=TRUE),3,
                                                     ifelse(data3$portfolio_others<quantile (data3$portfolio_others,0.4,na.rm=TRUE),4,
                                                            ifelse(data3$portfolio_others<quantile (data3$portfolio_others,0.5,na.rm=TRUE),5,
                                                                   ifelse(data3$portfolio_others<quantile (data3$portfolio_others,0.6,na.rm=TRUE),6,
                                                                          ifelse(data3$portfolio_others<quantile (data3$portfolio_others,0.7,na.rm=TRUE),7,
                                                                                 ifelse(data3$portfolio_others<quantile (data3$portfolio_others,0.8,na.rm=TRUE),8,
                                                                                        ifelse(data3$portfolio_others<quantile (data3$portfolio_others,0.9,na.rm=TRUE),9,10)))))))))
data3$portfolio_preferred_level <- ifelse(data3$portfolio_preferred<quantile (data3$portfolio_preferred,0.1,na.rm=TRUE),1,
                                          ifelse(data3$portfolio_preferred<quantile (data3$portfolio_preferred,0.2,na.rm=TRUE),2,
                                                 ifelse(data3$portfolio_preferred<quantile (data3$portfolio_preferred,0.3,na.rm=TRUE),3,
                                                        ifelse(data3$portfolio_preferred<quantile (data3$portfolio_preferred,0.4,na.rm=TRUE),4,
                                                               ifelse(data3$portfolio_preferred<quantile (data3$portfolio_preferred,0.5,na.rm=TRUE),5,
                                                                      ifelse(data3$portfolio_preferred<quantile (data3$portfolio_preferred,0.6,na.rm=TRUE),6,
                                                                             ifelse(data3$portfolio_preferred<quantile (data3$portfolio_preferred,0.7,na.rm=TRUE),7,
                                                                                    ifelse(data3$portfolio_preferred<quantile (data3$portfolio_preferred,0.8,na.rm=TRUE),8,
                                                                                           ifelse(data3$portfolio_preferred<quantile (data3$portfolio_preferred,0.9,na.rm=TRUE),9,10)))))))))
data3$portfolio_convertable_level <- ifelse(data3$portfolio_convertable<quantile (data3$portfolio_convertable,0.1,na.rm=TRUE),1,
                                            ifelse(data3$portfolio_convertable<quantile (data3$portfolio_convertable,0.2,na.rm=TRUE),2,
                                                   ifelse(data3$portfolio_convertable<quantile (data3$portfolio_convertable,0.3,na.rm=TRUE),3,
                                                          ifelse(data3$portfolio_convertable<quantile (data3$portfolio_convertable,0.4,na.rm=TRUE),4,
                                                                 ifelse(data3$portfolio_convertable<quantile (data3$portfolio_convertable,0.5,na.rm=TRUE),5,
                                                                        ifelse(data3$portfolio_convertable<quantile (data3$portfolio_convertable,0.6,na.rm=TRUE),6,
                                                                               ifelse(data3$portfolio_convertable<quantile (data3$portfolio_convertable,0.7,na.rm=TRUE),7,
                                                                                      ifelse(data3$portfolio_convertable<quantile (data3$portfolio_convertable,0.8,na.rm=TRUE),8,
                                                                                             ifelse(data3$portfolio_convertable<quantile (data3$portfolio_convertable,0.9,na.rm=TRUE),9,10)))))))))


#-------------assigning good/bad and risky/not risky----------
datagy <- data3[,c(65,73,75,79,26:36,90,92,94,15:20,84,86,88,126:142)]
datagy$avg_return <- rowMeans(datagy[,c(1:4)], na.rm = TRUE)
datagy$avg_beta <- rowMeans(datagy[,c(16:18)], na.rm = TRUE)
datagy$avg_alpha <- rowMeans(datagy[,c(25:27)], na.rm = TRUE)
datagy<- datagy[!is.na(datagy$avg_return), ]
datagy<- datagy[!is.na(datagy$avg_beta), ]
datagy<- datagy[!is.na(datagy$avg_alpha), ]
datagy <- datagy[rowSums(datagy[,c(5:15)])<100.1 & rowSums(datagy[,c(5:15)])>99.9]
datagy <- datagy[rowSums(datagy[,c(19:24)])<100.1 & rowSums(datagy[,c(19:24)])>99.9]
datagy$diversification_count <- rowSums(datagy[,5:15]>0)
range1 <- quantile(datagy$avg_beta,0.75)+1.5*IQR(datagy$avg_beta)
range2a <- quantile(datagy$avg_alpha,0.25)-1.5*IQR(datagy$avg_alpha)
range2b <- quantile(datagy$avg_alpha,0.75)+1.5*IQR(datagy$avg_alpha)
gyrisky <- datagy[avg_beta > 1 & avg_beta < range1]
gyrisky <- gyrisky[avg_alpha > range2a & avg_alpha < range2b]
gynotrisky <- datagy[avg_beta <= 1 & avg_beta > 0]
gynotrisky <- gynotrisky[avg_alpha > range2a & avg_alpha < range2b]

databy <- data3[,c(63,67,69,71,77,26:36,90,92,94,15:20,84,86,88,126:142)]
databy$avg_return <- rowMeans(databy[,c(1:5)], na.rm = TRUE)
databy$avg_beta <- rowMeans(databy[,c(17:19)], na.rm = TRUE)
databy$avg_alpha <- rowMeans(databy[,c(26:28)], na.rm = TRUE)
databy<- databy[!is.na(databy$avg_return), ]
databy<- databy[!is.na(databy$avg_beta), ]
databy<- databy[!is.na(databy$avg_alpha), ]
databy <- databy[rowSums(databy[,c(6:16)])<100.1 & rowSums(databy[,c(6:16)])>99.9]
databy <- databy[rowSums(databy[,c(20:25)])<100.1 & rowSums(databy[,c(20:25)])>99.9]
databy$diversification_count <- rowSums(databy[,6:16]>0)
range3 <- quantile(databy$avg_beta,0.75)+1.5*IQR(databy$avg_beta)
range4a <- quantile(databy$avg_alpha,0.25)-1.5*IQR(databy$avg_alpha)
range4b <- quantile(databy$avg_alpha,0.75)+1.5*IQR(databy$avg_alpha)
bynotrisky <- databy[avg_beta <= 1 & avg_beta > 0]
bynotrisky <- bynotrisky[avg_alpha < range4b & avg_alpha > range4a]
byrisky <- databy[avg_beta <= range3 & avg_beta > 1]
byrisky <- byrisky[avg_alpha < range4b & avg_alpha > range4a]

#goodyear_risky (gyrisky), goodyear_less_risky (gynotrisky)
#badyear_risky (byrisky), badyear_less_risky (bynotrisky)

#correlation
cor(gyrisky,gyrisky$avg_return)


#---------visualization and exploration for linear regression---------
scatter.smooth(gyrisky$diversification_count,gyrisky$avg_return)
scatter.smooth(byrisky$diversification_count,byrisky$avg_return)

scatter.smooth(gynotrisky$diversification_count,gynotrisky$avg_return)
scatter.smooth(bynotrisky$diversification_count,bynotrisky$avg_return)

hist(byrisky$basic_materials,breaks=10,col="blue")
hist(byrisky$consumer_cyclical,breaks=10,col="blue")
hist(byrisky$financial_services,breaks=10,col="blue")
hist(byrisky$real_estate,breaks=10,col="blue")
hist(byrisky$consumer_defensive,breaks=10,col="blue")
hist(byrisky$healthcare,breaks=10,col="blue")
hist(byrisky$utilities,breaks=10,col="blue")
hist(byrisky$communication_services,breaks=10,col="blue")
hist(byrisky$energy,breaks=10,col="blue")
hist(byrisky$industrials,breaks=10,col="blue")
hist(byrisky$technology,breaks=10,col="blue")

hist(bynotrisky$basic_materials,breaks=10,col="blue")
hist(bynotrisky$consumer_cyclical,breaks=10,col="blue")
hist(bynotrisky$financial_services,breaks=10,col="blue")
hist(bynotrisky$real_estate,breaks=10,col="blue")
hist(bynotrisky$consumer_defensive,breaks=10,col="blue")
hist(bynotrisky$healthcare,breaks=10,col="blue")
hist(bynotrisky$utilities,breaks=10,col="blue")
hist(bynotrisky$communication_services,breaks=10,col="blue")
hist(bynotrisky$energy,breaks=10,col="blue")
hist(bynotrisky$industrials,breaks=10,col="blue")
hist(bynotrisky$technology,breaks=10,col="blue")

#--------Train-Test for Linear Regression-----------
set.seed(2014)
train1.index <- createDataPartition(gynotrisky$diversification_count, p =.7, list=FALSE)
gynotrisky_train <- gynotrisky[train1.index,]
gynotrisky_test <- gynotrisky[-train1.index,]

set.seed(2014)
train2.index <- createDataPartition(gyrisky$diversification_count, p =.7, list=FALSE)
gyrisky_train <- gyrisky[train2.index,]
gyrisky_test <- gyrisky[-train2.index,]

set.seed(2014)
train3.index <- createDataPartition(bynotrisky$diversification_count, p =.7, list=FALSE)
bynotrisky_train <- bynotrisky[train3.index,]
bynotrisky_test <- bynotrisky[-train3.index,]

set.seed(2014)
train4.index <- createDataPartition(byrisky$diversification_count, p =.7, list=FALSE)
byrisky_train <- byrisky[train4.index,]
byrisky_test <- byrisky[-train4.index,]

#-------Model------------------

model1a <- lm(avg_return ~ diversification_count+basic_materials+consumer_cyclical+
                financial_services+real_estate+consumer_defensive+healthcare+utilities+
                communication_services+energy+industrials+technology+portfolio_cash+
                portfolio_stocks+portfolio_bonds+portfolio_others+portfolio_preferred+
                portfolio_convertable, data=gynotrisky_train)

model2a <- lm(avg_return ~ diversification_count+basic_materials+consumer_cyclical+
                financial_services+real_estate+consumer_defensive+healthcare+utilities+
                communication_services+energy+industrials+technology+portfolio_cash+
                portfolio_stocks+portfolio_bonds+portfolio_others+portfolio_preferred+
                portfolio_convertable, data=gyrisky_train)

model3a <- lm(avg_return ~ diversification_count+basic_materials+consumer_cyclical+
                financial_services+real_estate+consumer_defensive+healthcare+utilities+
                communication_services+energy+industrials+technology+portfolio_cash+
                portfolio_stocks+portfolio_bonds+portfolio_others+portfolio_preferred+
                portfolio_convertable, data=bynotrisky_train)

model4a <- lm(avg_return ~ diversification_count+basic_materials+consumer_cyclical+
                financial_services+real_estate+consumer_defensive+healthcare+utilities+
                communication_services+energy+industrials+technology+portfolio_cash+
                portfolio_stocks+portfolio_bonds+portfolio_others+portfolio_preferred+
                portfolio_convertable, data=byrisky_train)

summary(model1a)
summary(model2a)
summary(model3a)
summary(model4a)

#----------Model--------

model1b <- lm(avg_return ~ diversification_count+basic_materials_level+consumer_cyclical_level+
                financial_services_level+real_estate_level+consumer_defensive_level+healthcare_level+utilities_level+
                communication_services_level+energy_level+industrials_level+technology_level+portfolio_cash_level+
                portfolio_stocks_level+portfolio_bonds_level+portfolio_others_level+portfolio_preferred_level+
                portfolio_convertable_level, data=gynotrisky_train)

model2b <- lm(avg_return ~ diversification_count+basic_materials_level+consumer_cyclical_level+
                financial_services_level+real_estate_level+consumer_defensive_level+healthcare_level+utilities_level+
                communication_services_level+energy_level+industrials_level+technology_level+portfolio_cash_level+
                portfolio_stocks_level+portfolio_bonds_level+portfolio_others_level+portfolio_preferred_level+
                portfolio_convertable_level, data=gyrisky_train)

model3b <- lm(avg_return ~ diversification_count+basic_materials_level+consumer_cyclical_level+
                financial_services_level+real_estate_level+consumer_defensive_level+healthcare_level+utilities_level+
                communication_services_level+energy_level+industrials_level+technology_level+portfolio_cash_level+
                portfolio_stocks_level+portfolio_bonds_level+portfolio_others_level+portfolio_preferred_level+
                portfolio_convertable_level, data=bynotrisky_train)

model4b <- lm(avg_return ~ diversification_count+basic_materials_level+consumer_cyclical_level+
                financial_services_level+real_estate_level+consumer_defensive_level+healthcare_level+utilities_level+
                communication_services_level+energy_level+industrials_level+technology_level+portfolio_cash_level+
                portfolio_stocks_level+portfolio_bonds_level+portfolio_others_level+portfolio_preferred_level+
                portfolio_convertable_level, data=byrisky_train)

summary(model1b)
summary(model2b)
summary(model3b)
summary(model4b)

#---------Model-----------

model1c <- lm(avg_return ~ diversification_count+basic_materials_level+consumer_cyclical_level+
                financial_services_level+real_estate_level+consumer_defensive_level+healthcare_level+utilities_level+
                communication_services_level+energy_level+industrials_level+technology_level+portfolio_cash+
                portfolio_stocks+portfolio_bonds+portfolio_others+portfolio_preferred+
                portfolio_convertable, data=gynotrisky_train)

model2c <- lm(avg_return ~ diversification_count+basic_materials_level+consumer_cyclical_level+
                financial_services_level+real_estate_level+consumer_defensive_level+healthcare_level+utilities_level+
                communication_services_level+energy_level+industrials_level+technology_level+portfolio_cash+
                portfolio_stocks+portfolio_bonds+portfolio_others+portfolio_preferred+
                portfolio_convertable, data=gyrisky_train)

model3c <- lm(avg_return ~ diversification_count+basic_materials_level+consumer_cyclical_level+
                financial_services_level+real_estate_level+consumer_defensive_level+healthcare_level+utilities_level+
                communication_services_level+energy_level+industrials_level+technology_level+portfolio_cash+
                portfolio_stocks+portfolio_bonds+portfolio_others+portfolio_preferred+
                portfolio_convertable, data=bynotrisky_train)

model4c <- lm(avg_return ~ diversification_count+basic_materials_level+consumer_cyclical_level+
                financial_services_level+real_estate_level+consumer_defensive_level+healthcare_level+utilities_level+
                communication_services_level+energy_level+industrials_level+technology_level+portfolio_cash+
                portfolio_stocks+portfolio_bonds+portfolio_others+portfolio_preferred+
                portfolio_convertable, data=byrisky_train)

summary(model1c)
summary(model2c)
summary(model3c)
summary(model4c)

#------VIF for each model-----------

#gynotrisky
vif(model1a)
vif(model1b)
vif(model1c)
#gyrisky
vif(model2a)
vif(model2b)
vif(model2c)
#bynotrisky
vif(model3a)
vif(model3b)
vif(model3c)
#byrisky
vif(model4a)
vif(model4b)
vif(model4c)

#gynotrisky
vif(model1a)
vif(model1b)
vif(model1c)
#gyrisky
vif(model2a)
vif(model2b)
vif(model2c)
#bynotrisky
vif(model3a)
vif(model3b)
vif(model3c)
#byrisky
vif(model4a)
vif(model4b)
vif(model4c)

#-----------test for each model--------------

p1a_test <- predict(model1a,gynotrisky_test)
error1a <- (p1a_test - gynotrisky_test$avg_return)
rmse1a_test <- sqrt(mean(error1a^2))

p1a_test <- predict(model1b,gynotrisky_test)
error1b <- (p1a_test - gynotrisky_test$avg_return)
rmse1b_test <- sqrt(mean(error1b^2))

p1c_test <- predict(model1c,gynotrisky_test)
error1c <- (p1c_test - gynotrisky_test$avg_return)
rmse1c_test <- sqrt(mean(error1c^2))

p2a_test <- predict(model2a,gyrisky_test)
error2a <- (p2a_test - gyrisky_test$avg_return)
rmse2a_test <- sqrt(mean(error2a^2))

p2b_test <- predict(model2b,gyrisky_test)
error2b <- (p2b_test - gyrisky_test$avg_return)
rmse2b_test <- sqrt(mean(error2b^2))

p2c_test <- predict(model2c,gynotrisky_test)
error2c <- (p2c_test - gynotrisky_test$avg_return)
rmse2c_test <- sqrt(mean(error2c^2))

p3a_test <- predict(model3a,bynotrisky_test)
error3a <- (p3a_test - bynotrisky_test$avg_return)
rmse3a_test <- sqrt(mean(error3a^2))

p3b_test <- predict(model3b,bynotrisky_test)
error3b <- (p3b_test - bynotrisky_test$avg_return)
rmse3b_test <- sqrt(mean(error3b^2))

p3c_test <- predict(model3c,bynotrisky_test)
error3c <- (p3c_test - bynotrisky_test$avg_return)
rmse3c_test <- sqrt(mean(error3c^2))

p4a_test <- predict(model4a,byrisky_test)
error4a <- (p4a_test - byrisky_test$avg_return)
rmse4a_test <- sqrt(mean(error4a^2))

p4b_test <- predict(model4b,byrisky_test)
error4b <- (p4b_test - byrisky_test$avg_return)
rmse4b_test <- sqrt(mean(error4b^2))

p4c_test <- predict(model4c,byrisky_test)
error4c <- (p4c_test - byrisky_test$avg_return)
rmse4c_test <- sqrt(mean(error4c^2))


#================================== CART =========================================================================
#Using same cleaned data from Linear Regression

#-------1.1 GOOD year Risky (Industry Diversification)---------

set.seed(2014)
gyriskyi <- gyrisky[,c(45,48,5:15)]
gyricart1 <- rpart(avg_return ~., data = gyriskyi, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
CVerror.cap <- gyricart1$cptable[which.min(gyricart1$cptable[,"xerror"]), "xerror"] + gyricart1$cptable[which.min(gyricart1$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (gyricart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(gyricart1$cptable[i,1] * gyricart1$cptable[i-1,1]), 1)
cpgyri1 <- cp.opt
gyricart2 <- prune(gyricart1, cp = cpgyri1)
printcp(gyricart2, digits = 3)
rpart.plot(gyricart2, nn = T, main = "Optimal Tree for Industry Diversification (GY Risky)")
gyricart2$variable.importance #Decreasing order of importance: financial_services, consumer_defensive, technology

#Plot Higher Resolution (Will save image in working directory)
jpeg("gyricart2.jpeg", units="in", width=5, height=5, res=500)
rpart.plot(gyricart2, nn = T, main = "Optimal Tree for Industry Diversification (GY Risky)")
dev.off()

print(gyricart2)
#Optimal Tree path (terminal node 7): Outlier (68% return likely anomaly, insufficient data points within (only 2) )
#2nd optimal tree path (terminal node 13): (38% return, 7 data points within) Financial services <1.7, Utilities < 0.37, technology >= 57
#Choose 2nd optimal tree (Financial services <1.7, Utilities < 0.37, technology >= 57)
printcp(gyricart2, digits = 3)
#Root Node Error: 20
#TrainSet MSE: 20*0.506 = 10.12
#CV MSE: 20*0.9 = 18
median(gyriskyi$avg_return)
#Median avg_return of 24.5425


#-------1.2 GOOD year NOT Risky (Industry Diversification)--------

set.seed(2014)
gynotriskyi <- gynotrisky[,c(28,31,5:15)]
gynotricart1 <- rpart(avg_return ~., data = gynotriskyi, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
CVerror.cap <- gynotricart1$cptable[which.min(gynotricart1$cptable[,"xerror"]), "xerror"] + gynotricart1$cptable[which.min(gynotricart1$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (gynotricart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(gynotricart1$cptable[i,1] * gynotricart1$cptable[i-1,1]), 1)
cpgynotri1 <- cp.opt
gynotricart2 <- prune(gynotricart1, cp = cpgynotri1)
printcp(gynotricart2, digits = 3)
rpart.plot(gynotricart2, nn = T, main = "Optimal Tree for Industry Diversification (GY Not Risky)")
gynotricart2$variable.importance #Decreasing order of importance: Technology, Financial_services, Healthcare

#Plot Higher Resolution (Will save image in working directory)
jpeg("gynotricart2.jpeg", units="in", width=5, height=5, res=500)
rpart.plot(gynotricart2, nn = T, main = "Optimal Tree for Industry Diversification (GY Not Risky)")
dev.off()

print(gynotricart2)
#Optimal Tree path (Terminal node 3): (36% return likely anomaly, 3 data point) Real_estate >= 13
#2nd optimal tree path (Terminal node 5): (23% return, 182 data points) Real estate <13, Technology >= 18
printcp(gynotricart2, digits = 3)
#Root Node Error: 20.3
#TrainSet MSE: 20.3*0.566 = 11.49
#CV MSE: 20.3*0.783 = 15.89

#Choose 2nd optimal tree (Real estate <13, Technology >= 18)
#More reasonable since these funds are less risky and should have lower returns compared to #1.1

#---------1.3 BAD year Risky (Industry Diversification)-------------

set.seed(2014)
byriskyi <- byrisky[,c(29,32,6:16)]
byricart1 <- rpart(avg_return ~., data = byriskyi, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
CVerror.cap <- byricart1$cptable[which.min(byricart1$cptable[,"xerror"]), "xerror"] + byricart1$cptable[which.min(byricart1$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (byricart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(byricart1$cptable[i,1] * byricart1$cptable[i-1,1]), 1)
cpbyri1 <- cp.opt
byricart2 <- prune(byricart1, cp = cpbyri1)
printcp(byricart2, digits = 3)
rpart.plot(byricart2, nn = T, main = "Optimal Tree for Industry Diversification (BY Risky)")
byricart2$variable.importance #Technology, Financial_services, Consumer_defensive

#Plot Higher Resolution (Will save image in working directory)
jpeg("byricart2.jpeg", units="in", width=5, height=5, res=500)
rpart.plot(byricart2, nn = T, main = "Optimal Tree for Industry Diversification (BY Risky)")
dev.off()

print(byricart2)
#Optimal Tree path (Terminal Node 223): (6.4% return, 5 data points) Technology>=25, Technology<55, Technology<54, Utilities<1.2, Utilities>=0.015, Financial_services<13, Technology>=44
#Summarised Tree: 44<=Technology<54, 0.015<=Utilities<1.2, Financial_services<13
printcp(byricart2, digits = 3)
#Root Node Error: 5.19
#TrainSet MSE: 5.19*0.451 = 2.34
#CV MSE: 5.19*0.703 = 3.65

#-----------1.4 BAD year NOT Risky (Industry Diversification)---------------

set.seed(2014)
bynotriskyi <- bynotrisky[,c(29,32,6:16)]
bynotricart1 <- rpart(avg_return ~., data = bynotriskyi, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
CVerror.cap <- bynotricart1$cptable[which.min(bynotricart1$cptable[,"xerror"]), "xerror"] + bynotricart1$cptable[which.min(bynotricart1$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (bynotricart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(bynotricart1$cptable[i,1] * bynotricart1$cptable[i-1,1]), 1)
cpbynotri1 <- cp.opt
bynotricart2 <- prune(bynotricart1, cp = cpbynotri1)
printcp(bynotricart2, digits = 3)
rpart.plot(bynotricart2, nn = T, main = "Optimal Tree for Industry Diversification (BY Not Risky)")
bynotricart2$variable.importance #Healthcare, Communication_services, Industrials

#Plot Higher Resolution (Will save image in working directory)
jpeg("bynotricart2.jpeg", units="in", width=5, height=5, res=500)
rpart.plot(bynotricart2, nn = T, main = "Optimal Tree for Industry Diversification (BY Not Risky)")
dev.off()

print(bynotricart2)
#Optimal Tree path (Terminal Node 31): (9% return, 2 data points) Healthcare>=10, Energy<1.6, Technology>=30, Industrials<5.8
#2nd optimal tree path (Terminal Node 27): (5.4% return, 10 data points (more reliable)) Healthcare>=10, energy>=1.6, industrials>=13, Communication_services>=4.2
printcp(bynotricart2, digits = 3)
#Root Node Error: 22.6
#TrainSet MSE: 8.633
#CV MSE: 13.4696

#---------2.1 Good Year Risky (Asset Class Diversification)------------

set.seed(2014)
gyriskyd <- gyrisky[,c(28,19:24)]
gyrdcart1 <- rpart(avg_return ~., data = gyriskyd, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
CVerror.cap <- gyrdcart1$cptable[which.min(gyrdcart1$cptable[,"xerror"]), "xerror"] + gyrdcart1$cptable[which.min(gyrdcart1$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (gyrdcart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(gyrdcart1$cptable[i,1] * gyrdcart1$cptable[i-1,1]), 1)
cpgyrd1 <- cp.opt
gyrdcart2 <- prune(gyrdcart1, cp = cpgyrd1)
printcp(gyrdcart2, digits = 3)
rpart.plot(gyrdcart2, nn = T, main = "Optimal Tree for Portfolio Diversification (GY Risky)")
gyrdcart2$variable.importance #Portfolio_stocks, Portfolio_cash, Portfolio_Bonds


#Plot Higher Resolution (Will save image in working directory)
jpeg("gyrdcart2.jpeg", units="in", width=5, height=5, res=500)
rpart.plot(gyrdcart2, nn = T, main = "Optimal Tree for Portfolio Diversification (GY Risky)")
dev.off()

print(gyrdcart2)
#Optimal Tree path (terminal node 3): Outlier (68% return likely anomaly, insufficient data points within (2))
#2nd optimal tree path (terminal node 135): (44% return, 1 data points)
#3rd optimal tree path (terminal node 23): (41% return, 3 data points) Portfolio_stocks >= 32, Portfolio_preferred>=1.9, Portfolio_preferred<2.6, Portfolio_stocks<97
#Summarised Tree: 32<=Portfolio_stocks<97, 1.9<=Portfolio_preferred<2.6
printcp(gyrdcart2, digits = 3)
#Root Node Error: 20
#TrainSet MSE: 20*0.414 = 8.28
#CV MSE: 20*0.626 = 12.52

#------------2.2 GOOD year NOT Risky (Asset Class Diversification)---------------

set.seed(2014)
gynotriskyd <- gynotrisky[,c(28,19:24)]
gynotrdcart1 <- rpart(avg_return ~., data = gynotriskyd, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
CVerror.cap <- gynotrdcart1$cptable[which.min(gynotrdcart1$cptable[,"xerror"]), "xerror"] + gynotrdcart1$cptable[which.min(gynotrdcart1$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (gynotrdcart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(gynotrdcart1$cptable[i,1] * gynotrdcart1$cptable[i-1,1]), 1)
cpgynotrd1 <- cp.opt
gynotrdcart2 <- prune(gynotrdcart1, cp = cpgynotrd1)
printcp(gynotrdcart2, digits = 3)
rpart.plot(gynotrdcart2, nn = T, main = "Optimal Tree for Portfolio Diversification (GY Not Risky)")
gynotrdcart2$variable.importance #Portfolio_stocks, Portfolio_cash, Portfolio_others

#Plot Higher Resolution (Will save image in working directory)
jpeg("gynotrdcart2.jpeg", units="in", width=5, height=5, res=500)
rpart.plot(gynotrdcart2, nn = T, main = "Optimal Tree for Portfolio Diversification (GY Not Risky)")
dev.off()

print(gynotrdcart2)
#Optimal Tree path (Terminal node 15): (35% return, 4 data points) Portfolio_stocks<100, Portfolio_others>=0.015, Portfolio_stocks<78
#Summarised Tree: Portfolio_others>=0.015, Portfolio_stocks<78
printcp(gynotrdcart2, digits = 3)
#Root Node Error: 20.3
#TrainSet MSE: 20.3*0.331 = 6.72
#CV MSE: 20.3*0.504 = 10.23

#-----------2.3 Bad year Risky (Asset Class Diversification)--------------

set.seed(2014)
byriskyd <- byrisky[,c(29,20:25)]

byrdcart1 <- rpart(avg_return ~., data = byriskyd, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
CVerror.cap <- byrdcart1$cptable[which.min(byrdcart1$cptable[,"xerror"]), "xerror"] + byrdcart1$cptable[which.min(byrdcart1$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (byrdcart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(byrdcart1$cptable[i,1] * byrdcart1$cptable[i-1,1]), 1)
cpbyrd1  <- cp.opt
byrdcart2 <- prune(byrdcart1, cp = cpbyrd1 )
printcp(byrdcart2, digits = 3)
rpart.plot(byrdcart2, nn = T, main = "Optimal Tree for Portfolio Diversification (BY Risky)")
byrdcart2$variable.importance #Portfolio_stocks, Portfolio_cash, Portfolio_bonds


#Plot Higher Resolution (Will save image in working directory)
jpeg("byrdcart2.jpeg", units="in", width=5, height=5, res=500)
rpart.plot(byrdcart2, nn = T, main = "Optimal Tree for Portfolio Diversification (BY Risky)")
dev.off()

print(byrdcart2)
#Optimal Tree path (terminal node 103259): (6.8% return, 2 data points) Summarised tree: Portfolio_bonds <0.015, 98<=Portfolio_stocks<99, 1.5<=Portfolio_cash<=1.8 (Portfolio_cash~1.8)
printcp(byrdcart2, digits = 3)
#Root Node Error: 5.19
#TrainSet MSE: 5.19*0.533 = 2.77
#CV MSE: 5.19*0.801 = 4.16

#-----------2.4 Bad year NOT Risky (Asset Class Diversification)----------------

set.seed(2014)
bynotriskyd <- bynotrisky[,c(29,20:25)]

bynotrdcart1 <- rpart(avg_return ~., data = bynotriskyd, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
CVerror.cap <- bynotrdcart1$cptable[which.min(bynotrdcart1$cptable[,"xerror"]), "xerror"] + bynotrdcart1$cptable[which.min(bynotrdcart1$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (bynotrdcart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(bynotrdcart1$cptable[i,1] * bynotrdcart1$cptable[i-1,1]), 1)
cpbynotrd1 <- cp.opt
bynotrdcart2 <- prune(bynotrdcart1, cp = cpbynotrd1)
printcp(bynotrdcart2, digits = 3)
rpart.plot(bynotrdcart2, nn = T, main = "Optimal Tree for Portfolio Diversification (BY Not Risky)")
bynotrdcart2$variable.importance #Portfolio_cash, Portfolio_stocks, Portfolio_others

#Plot Higher Resolution (Will save image in working directory)
jpeg("bynotrdcart2.jpeg", units="in", width=5, height=5, res=500)
rpart.plot(bynotrdcart2, nn = T, main = "Optimal Tree for Portfolio Diversification (BY Not Risky)")
dev.off()

print(bynotrdcart2)
#Optimal Tree path (Terminal node 251): (9% return, 2 data points) 0.005<=Portfolio_cash<15, Portfolio_preferred<0.11, Portfolio_stocks~96
printcp(bynotrdcart2, digits = 3)
#Root Node Error: 8.2
#TrainSet MSE: 8.2*0.219 = 1.80
#CV MSE: 8.2*0.387 = 3.17
