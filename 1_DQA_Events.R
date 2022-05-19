# Load libraries
library(dlookr)
library(dplyr)
library(ggplot2)
library(data.table)
library(stringr)
library(tidyr)
library(leaflet)
library(lubridate)
library(scales)
library(ggthemes)
library(leaflet)
library(gridExtra)
library(ggmap)
library(readxl)
library(tidyverse)
library(plotly)
library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)
library(gsubfn)
library(naniar)
library(modelr)
library(ggpubr)

## ----DQA1_cleaning and removing duplicates------------------------------------------------------------------
# with 
df <- df %>%
  mutate('Start_Date_vod__c'= ymd(df$'Start_Date_vod__c'),
         'End_Date_vod__c'= ymd(df$'End_Date_vod__c'),
         'CreatedDate'= ymd(sub("T.*"," ", df$'CreatedDate')),
         'LastModifiedDate'= ymd(sub("T.*"," ", df$'LastModifiedDate')),
         'LastActivityDate'= ymd(sub("T.*"," ", df$'LastActivityDate')))
    
df <- filter(df,df$UCB_Event_Status__c=="Closed")  # filter just "Closed" events
df <- data.table(df)  # convert tbl_df to data.table format
df$UCB_Event_Products__c[is.na(df$UCB_Event_Products__c)]<-"not_avaliable"
#Restoring values "not_avaliable" with NA 
df2 <- df %>%
    replace_with_na(replace=list(UCB_Event_Products__c="not_avaliable"))
# removing duplicates by "UCB_External_Id__c"
df.unique <- df2[!duplicated(df2$"UCB_External_Id__c")] 

## ----DQA2_Missing_Alltime------------------------------------------------------------------
missing_values <- df.unique%>%
  summarize_each(list(~(sum(is.na(.))+sum(is.null(.)))/n()))
# re-shape data frame
missing_values <- gather(missing_values, key="feature", value="missing_pct")
# plot all features with missing data percentage
png(filename = "./figs/missing_values.png", width=600, height=500, res=120)
missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="black")+
  coord_flip()+theme_bw()
dev.off()
#show the graphes
missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="black")+
  coord_flip()+theme_bw()

## ----DQA3.1_events' frequency----------------------------------------------------
datetime <- df.unique$"Start_Date_vod__c"#remove duplicate call names
df.unique.freq <- df.unique %>%
  mutate(datetime2 = (df.unique$"Start_Date_vod__c")) %>%
  select(datetime2,"Owner.Name")
# rename feature names
df.unique.freq <- df.unique.freq %>%
  rename(Owner = 'Owner.Name')
# present total events over time by histogram plots
png(filename = "./figs/events_plot_%02d.png", width=600, height=500, res=120)
df.unique.freq %>%
  ggplot(aes(datetime2))+geom_histogram(binwidth=1)+ ggtitle("Total Events/per day")+xlab("Time")+ylab("# Events") #1 day #1 day
df.unique.freq %>%
  ggplot(aes(datetime2))+geom_histogram(binwidth=7)+ ggtitle("Total Events/per week")+xlab("Time")+ylab("# Events") #1 day
df.unique.freq %>%
  ggplot(aes(datetime2))+geom_histogram(binwidth=30)+ ggtitle("Total Events/per month")+xlab("Time")+ylab("# Events") #1 day
dev.off()
#show the graphes
df.unique.freq %>%
  ggplot(aes(datetime2))+geom_histogram(binwidth=1)+ ggtitle("Total Events/per day")+xlab("Time")+ylab("# Events") #1 day #1 day
df.unique.freq %>%
  ggplot(aes(datetime2))+geom_histogram(binwidth=7)+ ggtitle("Total Events/per week")+xlab("Time")+ylab("# Events") #1 day
df.unique.freq %>%
  ggplot(aes(datetime2))+geom_histogram(binwidth=30)+ ggtitle("Total Events/per month")+xlab("Time")+ylab("# Events") #1 day

## ----DQA3.2_day_of_week_effect and seasonality_wday------------------------------------
#Day of week and fit the model with lm
daily <- df.unique.freq %>%
  mutate(datetime2) %>%
  group_by(datetime2) %>%
  summarize(n=n())
daily <- daily %>%
  mutate(wday=wday(datetime2,label=TRUE))

# Warning, check your data and accordingly chack the datatime below
term2018 <- function(datetime2){
  cut(datetime2,
      breaks=ymd(20160901,20161215,20170115,20170701,20170901,20171215,20180115,20180701,20180901,20181215,20190115,20190701),
      labels=c("2016fall","2016decem-jan","2017spring","2017summer","2017fall","2017decem-jan","2018spring","2018summer","2018fall","2018decem-jan","2019spring")
  )
}
  
daily <- daily %>% 
  mutate(term=term2018(datetime2))
mod1 <- lm(n~wday,data = daily)
mod2 <- lm(n~wday*term,data = daily)
png(filename = "./figs/events_wday_term_%02d.png", width=1200, height=1000, res=150) #start export
grid <- daily %>% 
  data_grid(wday,term) %>% 
  add_predictions(mod2,"n")
ggplot(daily,aes(wday,n))+
  geom_boxplot()+
  geom_point(data=grid,color="red",size=4)+
  facet_wrap(~term,ncol = 4)
dev.off() # finish export

#show the graphes
grid <- daily %>% 
  data_grid(wday,term) %>% 
  add_predictions(mod2,"n")
ggplot(daily,aes(wday,n))+
  geom_boxplot()+
  geom_point(data=grid,color="red",size=4)+
  facet_wrap(~term,ncol = 4)+
  ylim(-2,6)

## ----DQA4._attendee distribution and outlier detection----------------
#statistics
print(paste("mean: ",as.character(mean(df.unique$UCB_Calculated_Number_of_Actual_Attendee__c)),"std dev: ",as.character(sd(as.character(df.unique$UCB_Calculated_Number_of_Actual_Attendee__c))))) # mean value and st dev
mean_value <- (mean(df.unique$UCB_Calculated_Number_of_Actual_Attendee__c))
std_value <- (sd(df.unique$UCB_Calculated_Number_of_Actual_Attendee__c))
  
png(filename = "./figs/attendee_histogram_%02d.png", width=600, height=500, res=120) # start export
require(scales)
  df.unique %>%
    ggplot(aes(x = UCB_Calculated_Number_of_Actual_Attendee__c)) +
    geom_histogram(stat = "count") +
    theme_minimal() +
    scale_x_continuous(name = "#attendees",limits=c(0,150),
                       breaks = 1*c(mean_value+2*std_value, mean_value-2*std_value),
                       minor_breaks = 1*c(mean_value+1*std_value, mean_value-1*std_value)) +
    ggtitle("Distribution of attendees with +/- 2 std dev")
dev.off() # finish export
#show the graphes
  df.unique %>%
    ggplot(aes(x = UCB_Calculated_Number_of_Actual_Attendee__c)) +
    geom_histogram(stat = "count") +
    theme_minimal() +
    scale_x_continuous(name = "#attendees",limits=c(0,150),
                       breaks = 1*c(mean_value+2*std_value, mean_value-2*std_value),
                       minor_breaks = 1*c(mean_value+1*std_value, mean_value-1*std_value)) +
    ggtitle("Distribution of attendees with +/- 2 std dev")

#  Let's see the plots before and after outlier removal
  png(filename = "./figs/attendee_outlier_%02d.png", width=600, height=500, res=120) # start export
  df.unique %>% 
    plot_outlier(UCB_Calculated_Number_of_Actual_Attendee__c)
  dev.off() # finish export
# show the graphes
  df.unique %>% 
    plot_outlier(UCB_Calculated_Number_of_Actual_Attendee__c)
  
## ----DQA5_timeliness of events------------------
TimeGap <- df.unique$End_Date_vod__c
df.unique.timegap <- df.unique %>%
  mutate(TimeGap = pmax(0,df.unique$"Start_Date_vod__c"-df.unique$"CreatedDate")) #%>%
    
png(filename = "./figs/timeliness_plot_%02d.png", width=600, height=500, res=120) # start export
df.unique.timegap %>%
  ggplot(aes(TimeGap))+geom_histogram(stat = "count")+ggtitle("events~TimeGap")+xlab("Delayed Days")+ylab("#events")  #1 day #1 day
dev.off() # finish export

df.unique.timegap %>%
  ggplot(aes(TimeGap))+geom_histogram(stat = "count") +ggtitle("events~TimeGap")+xlab("Delayed Days")+ylab("#events")+ xlim(-10,240) #1 day #

png(filename = "./figs/timeliness_outlier_%02d.png", width=600, height=500, res=120) # start export
  df.unique.timegap %>% 
    plot_outlier(TimeGap)
dev.off() # finish export
# show the graphes
 df.unique.timegap %>% 
    plot_outlier(TimeGap)