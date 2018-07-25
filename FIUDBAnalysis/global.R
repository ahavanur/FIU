library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(shiny)
library(gridExtra)
library(leaflet)
library(lubridate)
library(tidytext)
library(stopwords)
library(wordcloud)
library(RColorBrewer)
library(GOsummaries)
library(textstem)
library(stringdist)
library(stringr)

set.seed(1234)
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

str_df$fullNameOfFinancialInstitution = as.character(toupper(str_df$fullNameOfFinancialInstitution))
str_df$startDateOfSuspiciousActivity = as.Date(strptime(str_df$startDateOfSuspiciousActivity, "%m/%d/%Y %H:%M:%S"))
str_df$endDateOfSuspiciousActivity = as.Date(strptime(str_df$endDateOfSuspiciousActivity, "%m/%d/%Y %H:%M:%S"))
str_df$amountOfCash =  as.numeric(gsub("[\\$,]", "", as.character(str_df$amountOfCash)))
str_df$strDateGenerate = as.Date(strptime(str_df$strDateGenerate, "%m/%d/%Y %H:%M:%S"))
str_df$duration = str_df$endDateOfSuspiciousActivity - str_df$startDateOfSuspiciousActivity
str_df$narrative = as.character(str_df$narrative)
str_df$duration[which(is.na(str_df$duration))] = 0
str_df$duration[which(str_df$duration < 0)] = 0
str_df[which.max(str_df$strDateGenerate),'strDateGenerate'] <- "2017-01-27"

stracct_df = separate_rows(str_df,accountNumbers,sep="/")
stracct_df = separate_rows(stracct_df,accountNumbers,sep=",")
stracct_agg = stracct_df %>% group_by(STRID) %>% summarise(count = n()) 


make_stopwords <- function(){
  stops = stopwords("en")
  css_chars = c("br", "nbsp", "amp")
  return(c(stops, css_chars, letters))
}
common_stopwords = make_stopwords()
restrict_banks = function(df, input_banks) {
  #choosing banks
  if (length(input_banks) == 0) {
    banks = unique(toupper(df$fullNameOfFinancialInstitution))
  }
  else {
    banks = input_banks
  }
  rows = which(toupper(df$fullNameOfFinancialInstitution) %in% banks)
  return(rows)
}

restrict_range = function(df, field1, field2, ranges) {
  rows_field1 <- which(df[,field1] >= ranges[1])
  rows_field2 <- which(df[,field2] <= ranges[2])
  return(intersect(rows_field1, rows_field2))
}

str_occupations = c()
str_narratives = c()

restrict_terms = function(df,field,terms, threshold) {
  if (length(terms) != 0) {
    df[,field] <- lemmatize_strings(toupper(df[,field]))
    distances = as.data.frame(1-stringdistmatrix(df[,field], toupper(terms), method = "jw"))
    distances$max_similarity = apply(distances,1,max)
    return(unique(df$original_index[which(distances$max_similarity > threshold)]))
  }
  else {
    return(unique(df$original_index))
  }
}

create_ui_select_inputs = function(df, field){
  new_ui_list = str_replace_all(df[,field],"[^[:graph:]]", " ")
  new_ui_list = str_replace_all(new_ui_list,"[[:punct:]]", " ")
  new_ui_list = unlist(strsplit(new_ui_list, ">"))
  new_ui_list = unlist(strsplit(new_ui_list, "<"))
  new_ui_list = unlist(strsplit(new_ui_list, " "))
  new_ui_list = unlist(strsplit(new_ui_list, "-"))
  new_ui_list = unlist(strsplit(new_ui_list, "/"))
  new_ui_list = unlist(strsplit(new_ui_list, ","))
  if (length(which(new_ui_list %in% common_stopwords)) != 0)
    new_ui_list = new_ui_list[-which(new_ui_list %in% common_stopwords)]
  new_ui_list = toupper(lemmatize_strings(new_ui_list))
  new_ui_list = tolower(names(sort(table(new_ui_list),decreasing=TRUE)))
  return(new_ui_list)
}

str_ui_occupations = create_ui_select_inputs(str_df, 'occupationOrTypeOfBusiness')
str_ui_narratives = create_ui_select_inputs(str_df, 'narrative')
str_ui_lastname = create_ui_select_inputs(str_df, 'lastNameOrNameOfEntity')
str_ui_firstname = create_ui_select_inputs(str_df, 'firstName')

ctr_ui_lastname = create_ui_select_inputs(pit_df, 'lastNameOrNameOfEntity')
ctr_ui_firstname = create_ui_select_inputs(pit_df, 'firstName')
ctr_ui_occupations = create_ui_select_inputs(pit_df, 'occupationOrTypeOfBusiness')


str_stringed = str_df[,c("firstName","lastNameOrNameOfEntity", "occupationOrTypeOfBusiness", "narrative")]
str_stringed$original_index = seq(1,nrow(str_stringed))
str_stringed$narrative = str_replace_all(str_stringed$narrative,"[^[:graph:]]", " ")
str_stringed$occupationOrTypeOfBusiness = str_replace_all(str_stringed$occupationOrTypeOfBusiness,"[^[:graph:]]", " ")
str_stringed = separate_rows(str_stringed, "occupationOrTypeOfBusiness", sep="/")
str_stringed = separate_rows(str_stringed, "occupationOrTypeOfBusiness", sep=" ")
str_stringed = separate_rows(str_stringed, "occupationOrTypeOfBusiness", sep="-")
str_stringed = str_stringed[-which(str_stringed$occupationOrTypeOfBusiness %in% common_stopwords),]
str_stringed = str_stringed %>% unnest_tokens(word, narrative)
str_stringed = str_stringed[-which(str_stringed$word %in% common_stopwords),]
str_stringed$occupationOrTypeOfBusiness = toupper(lemmatize_strings(str_stringed$occupationOrTypeOfBusiness))
str_stringed$word = toupper(lemmatize_strings(str_stringed$word))

pit_stringed = pit_df[,c("CTRID","firstName","lastNameOrNameOfEntity", "occupationOrTypeOfBusiness")]
pit_stringed$original_index = seq(1,nrow(pit_stringed))
pit_stringed$occupationOrTypeOfBusiness = str_replace_all(pit_stringed$occupationOrTypeOfBusiness,"[^[:graph:]]", " ")
pit_stringed = separate_rows(pit_stringed, "occupationOrTypeOfBusiness", sep="/")
pit_stringed = separate_rows(pit_stringed, "occupationOrTypeOfBusiness", sep=" ")
pit_stringed = separate_rows(pit_stringed, "occupationOrTypeOfBusiness", sep="-")
pit_stringed = pit_stringed[-which(pit_stringed$occupationOrTypeOfBusiness %in% common_stopwords),]
pit_stringed$occupationOrTypeOfBusiness = toupper(lemmatize_strings(pit_stringed$occupationOrTypeOfBusiness))
