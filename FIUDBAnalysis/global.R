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
library(ggmap)
library(geosphere)

set.seed(1234)
path = "/Users/apoorvahavanur/Documents/School/2017-2018/Other/TCinGC/fiu/FIUExport/"
ctr_df = read.csv(paste(path,"CTRs.csv",sep=''))
str_df = read.csv(paste(path,"STRs.csv",sep=''))
case_df = read.csv(paste(path,"Cases.csv",sep=''))
immigration_df = read.csv(paste(path,"ImmigrationRecords.csv",sep=''))
customs_df = read.csv(paste(path,"CustomsRecords.csv",sep=''))
pit_df = read.csv(paste(path,"PersonInTransaction.csv",sep=''))
flights_df = read.csv(paste(path,"flights.csv",sep=''))
city_df = read.csv(paste(path,"CityLocations.csv",sep=''))

immigration_df$flight = paste(immigration_df$Carrier, immigration_df$Voyage)
immigration_df$fullflight = paste(immigration_df$Carrier, immigration_df$Voyage, immigration_df$Travel.Status)
immigration_df$Travel.Date = as.Date(strptime(immigration_df$Travel.Date, "%m/%d/%y"))
flights_df$Type = toupper(flights_df$Type)
flights_df$fullflight = paste(flights_df$Flight, flights_df$Type)
flightmatch_df = as.data.frame(1-stringdistmatrix(immigration_df$fullflight, flights_df$fullflight, method = "jw"))
flightmatch_df$bestmatch = flights_df$fullflight[apply(flightmatch_df,1,which.max)]
flightmatch_df = flights_df[match(flightmatch_df$bestmatch, flights_df$fullflight),]
immigration_df = cbind(immigration_df, flightmatch_df)
immigration_df = immigration_df[,c("ImmigrationID","CASEID","First.and.Middle.Name","Last.Name","Citizenship","Employer.Sponsor","Travel.Date","Travel.Status","flight","Departs","Arrives")]

depart_loc_df = city_df[match(immigration_df$Departs,city_df$loc),]
immigration_df$depart_lat = depart_loc_df$lat
immigration_df$depart_lon = depart_loc_df$lon

arrive_loc_df = city_df[match(immigration_df$Arrives,city_df$loc),]

immigration_df$arrive_lat = arrive_loc_df$lat
immigration_df$arrive_lon = arrive_loc_df$lon


customs_df$Date.Of.Report = as.Date(strptime(customs_df$Date.Of.Report, "%m/%d/%y"))
# arrive_lat = quantile(unique(immigration_df$arrive_lat), na.rm=TRUE,0.75)[[1]]
# arrive_lon = quantile(unique(immigration_df$arrive_lon), na.rm=TRUE,0.75)[[1]]
# immigration_df_coords = data.frame(group = immigration_df$Travel.Status,
#                                    lat = c(immigration_df$depart_lat, immigration_df$arrive_lat),
#                                    long = c(immigration_df$depart_lon, immigration_df$arrive_lon))
# arrivals = immigration_df[which(immigration_df$Travel.Status == "ARRIVAL"),]
# departures = immigration_df[which(immigration_df$Travel.Status == "DEPARTURE"),]
# 
# inter_arrivals = gcIntermediate(cbind(arrivals$depart_lon, arrivals$depart_lat), cbind(arrivals$arrive_lon,arrivals$arrive_lat),
#                n=1000,
#                addStartEnd=TRUE,
#                sp=TRUE)
# inter_departures = gcIntermediate(cbind(departures$depart_lon, departures$depart_lat), cbind(departures$arrive_lon,departures$arrive_lat),
#                                 n=2,
#                                 addStartEnd=TRUE,
#                                 sp=TRUE)

# lines(inter)                 
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
    distances = as.data.frame(1-stringdistmatrix(df[,field], toupper(terms), method = "jw"))
    distances$max_similarity = apply(distances,1,max)
    return(unique(df$original_index[which(distances$max_similarity > threshold)]))
  }
  else {
    return(seq(1,max(df$original_index)))
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
  if (length(which(new_ui_list %in% common_stopwords)) != 0) {
    new_ui_list = new_ui_list[-which(new_ui_list %in% common_stopwords)] }
  new_ui_list = toupper(lemmatize_strings(new_ui_list))
  new_ui_list = tolower(names(sort(table(new_ui_list),decreasing=TRUE)))
  return(new_ui_list)
}

str_ui_occupations = create_ui_select_inputs(str_df, 'occupationOrTypeOfBusiness')
str_ui_narratives = create_ui_select_inputs(str_df, 'narrative')
str_ui_characterizations = create_ui_select_inputs(str_df, 'summaryCharacterization')
str_ui_lastname = create_ui_select_inputs(str_df, 'lastNameOrNameOfEntity')
str_ui_firstname = create_ui_select_inputs(str_df, 'firstName')

ctr_ui_lastname = create_ui_select_inputs(pit_df, 'lastNameOrNameOfEntity')
ctr_ui_firstname = create_ui_select_inputs(pit_df, 'firstName')
ctr_ui_occupations = create_ui_select_inputs(pit_df, 'occupationOrTypeOfBusiness')


str_stringed = str_df[,c("firstName","lastNameOrNameOfEntity", "occupationOrTypeOfBusiness", "summaryCharacterization", "narrative")]
str_stringed$original_index = seq(1,nrow(str_stringed))
str_stringed$narrative = str_replace_all(str_stringed$narrative,"[^[:graph:]]", " ")
str_stringed$occupationOrTypeOfBusiness = str_replace_all(str_stringed$occupationOrTypeOfBusiness,"[^[:graph:]]", " ")
str_stringed$narrative = toupper(lemmatize_strings(str_stringed$narrative))
str_stringed$firstName = toupper(lemmatize_strings(str_stringed$firstName))
str_stringed$lastNameOrNameOfEntity = toupper(lemmatize_strings(str_stringed$lastNameOrNameOfEntity))
str_stringed$summaryCharacterization = toupper(lemmatize_strings(str_stringed$summaryCharacterization))
str_stringed$occupationOrTypeOfBusiness = toupper(lemmatize_strings(str_stringed$occupationOrTypeOfBusiness))

str_stringed = separate_rows(str_stringed, "occupationOrTypeOfBusiness", sep="/")
str_stringed = separate_rows(str_stringed, "occupationOrTypeOfBusiness", sep=" ")
str_stringed = separate_rows(str_stringed, "occupationOrTypeOfBusiness", sep="-")
str_stringed = separate_rows(str_stringed, "summaryCharacterization", sep=" ")
str_stringed = str_stringed[-which(str_stringed$summaryCharacterization %in% toupper(common_stopwords)),]
str_stringed = str_stringed[-which(str_stringed$occupationOrTypeOfBusiness %in% toupper(common_stopwords)),]
str_stringed = str_stringed %>% unnest_tokens(word, narrative)
str_stringed = str_stringed[-which(str_stringed$word %in% common_stopwords),]
str_stringed = str_stringed[which(is.na(as.numeric(as.character(gsub("[[:punct:]]", "", str_stringed$word))))),]


pit_stringed = pit_df[,c("CTRID","firstName","lastNameOrNameOfEntity", "occupationOrTypeOfBusiness")]
pit_stringed$original_index = seq(1,nrow(pit_stringed))
pit_stringed$occupationOrTypeOfBusiness = str_replace_all(pit_stringed$occupationOrTypeOfBusiness,"[^[:graph:]]", " ")
pit_stringed = separate_rows(pit_stringed, "occupationOrTypeOfBusiness", sep="/")
pit_stringed = separate_rows(pit_stringed, "occupationOrTypeOfBusiness", sep=" ")
pit_stringed = separate_rows(pit_stringed, "occupationOrTypeOfBusiness", sep="-")
pit_stringed = pit_stringed[-which(pit_stringed$occupationOrTypeOfBusiness %in% common_stopwords),]
pit_stringed$occupationOrTypeOfBusiness = toupper(lemmatize_strings(pit_stringed$occupationOrTypeOfBusiness))
