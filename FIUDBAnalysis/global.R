library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(shiny)
library(gridExtra)
library(leaflet)
library(lubridate)
path = "/Users/apoorvahavanur/Documents/School/2017-2018/Other/TCinGC/fiu/FIUExport/"
ctr_df = read.csv(paste(path,"CTRs.csv",sep=''))
str_df = read.csv(paste(path,"STRs.csv",sep=''))
case_df = read.csv(paste(path,"Cases.csv",sep=''))
immigration_df = read.csv(paste(path,"ImmigrationRecords.csv",sep=''))
customs_df = read.csv(paste(path,"CustomsRecords.csv",sep=''))
pit_df = read.csv(paste(path,"PersonInTransaction.csv",sep=''))
ctr_df$dateOfTransaction = as.Date(strptime(ctr_df$dateOfTransaction, "%m/%d/%Y %H:%M:%S"))
ctr_df$cashAmount = as.numeric(gsub("[\\$,]", "", as.character(ctr_df$cashAmount)))
ctr_df$fullNameOfFinancialInstitution = as.character(toupper(ctr_df$fullNameOfFinancialInstitution))
ctr_df$cashAmount[which(is.na(ctr_df$cashAmount))] = 0

pit_df$cashAmount =  as.numeric(gsub("[\\$,]", "", as.character(pit_df$cashAmount)))
pit_df$cashAmount[which(is.na(pit_df$cashAmount))] = 0
pit_df_acct = separate_rows(pit_df,accountNumbers,sep="/")
pit_df_acct = separate_rows(pit_df_acct,accountNumbers,sep=",")
pit_acct_count = pit_df_acct %>% group_by(CTRID) %>% summarize(count = n())

str_df$startDateOfSuspiciousActivity = as.Date(strptime(str_df$startDateOfSuspiciousActivity, "%m/%d/%Y %H:%M:%S"))
str_df$endDateOfSuspiciousActivity = as.Date(strptime(str_df$endDateOfSuspiciousActivity, "%m/%d/%Y %H:%M:%S"))
str_df$amountOfCash =  as.numeric(gsub("[\\$,]", "", as.character(str_df$amountOfCash)))
str_df$strDateGenerate = as.Date(strptime(str_df$strDateGenerate, "%m/%d/%Y %H:%M:%S"))
str_df$duration = str_df$endDateOfSuspiciousActivity - str_df$startDateOfSuspiciousActivity
str_df$narrative = as.character(str_df$narrative)
str_df$duration[which(is.na(str_df$duration))] = 0
str_df$duration[which(str_df$duration < 0)] = 0


stracct_df = separate_rows(str_df,accountNumbers,sep="/")
stracct_df = separate_rows(stracct_df,accountNumbers,sep=",")
stracct_agg = stracct_df %>% group_by(STRID) %>% summarise(count = n()) 
