sleepTimeForStatusBar<-2
library(DT)
library(shinydashboard)
library(cricketdata)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(shiny)
library(DT)
library(shinydashboard)
library(tree)
library(plotly)
library(pscl)

mt20 <- fetch_cricinfo("T20", "Men", "Batting")

#REMOVE COUNTRIES WHICH HAVE LESS THEN 20 PLAYERS 
tab <- table(mt20$Country)
mt20_gt20<-mt20[mt20$Country %in% names(tab)[tab>20],]
mt20_gt20$Country <- as.factor(mt20_gt20$Country)

#NEED THIS TABLE FOR CLUSTERING ANALYSIS
cldat<-mt20_gt20 %>% dplyr::filter(Runs>1000) %>% select("Player", "StrikeRate", "Runs")
cldat<-cldat %>% remove_rownames %>% column_to_rownames(var="Player")



compute_data<-function(updateProgress=NULL){
  
  #CREATE DATASET FOR THE INTERACTIVE PLOT
  #CREATE SUMMARY DATASET FOR COUNTRIES
  updateProgress(detail = "1/4 Create Summary datasets")
  Sys.sleep(sleepTimeForStatusBar)
  CountrySummary<-
    mt20_gt20 %>%
    replace_na(list(Runs=0)) %>% 
    group_by(Country) %>% 
    summarise(
      TotalPlayers=dplyr::n(), 
      TotalRuns=sum(Runs), 
      TotalMatches=sum(Matches)
    )
    
  #GET COUNTRY CODES
  updateProgress(detail = "2/4 Get Country codes from API")
  Sys.sleep(sleepTimeForStatusBar)
  df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
  df <- df %>% select(COUNTRY, CODE)
  colnames(df)<-c("Country", "CODE")
  
  #JOIN WITH THIS DATASET TO INCLUDE COUNTRY CODE  
  updateProgress(detail = "3/4 Join code with main dataset")
  Sys.sleep(sleepTimeForStatusBar)
  CountrySummaryWCode<-inner_join(CountrySummary, df, by="Country")
  
  #CREATE HOVER TEXT 
  updateProgress(detail = "4/4 Creating hover text")
  Sys.sleep(sleepTimeForStatusBar)
  CountrySummaryWCode$hover <- 
    with(CountrySummaryWCode, paste(Country, '<br>', 
                                    "Total Players", TotalPlayers, "<br>", 
                                    "Total Matches", TotalMatches, "<br>",
                                    "Total Runs", TotalRuns
    )
)


}