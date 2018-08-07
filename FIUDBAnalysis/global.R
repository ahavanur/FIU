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
library(shinyFiles)
set.seed(1234)


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
  new_ui_list = toupper((new_ui_list))
  if (length(which(new_ui_list %in% common_stopwords)) != 0) {
    new_ui_list = new_ui_list[-which(new_ui_list %in% toupper(common_stopwords))] }
  new_ui_list = tolower(names(sort(table(new_ui_list),decreasing=TRUE)))
  return(new_ui_list)
}
