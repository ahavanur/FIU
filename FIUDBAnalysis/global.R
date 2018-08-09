load_or_install<-function(package_names)
{
  for(package_name in package_names)
  {
    if(!is_installed(package_name))
    {
      install.packages(package_name,repos="http://lib.stat.cmu.edu/R/CRAN")
    }
    load_or_install(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)
  }
}

load_or_install("dplyr")
load_or_install("tidyr")
load_or_install("ggplot2")
load_or_install("scales")
load_or_install("ggrepel")
load_or_install("shiny")
load_or_install("gridExtra")
load_or_install("leaflet")
load_or_install("lubridate")
load_or_install("tidytext")
load_or_install("stopwords")
load_or_install("wordcloud")
load_or_install("RColorBrewer")
load_or_install("GOsummaries")
load_or_install("textstem")
load_or_install("stringdist")
load_or_install("stringr")
load_or_install("ggmap")
load_or_install("geosphere")
load_or_install("shinyFiles")
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

fuzzyNameMatchIdx = function(name_vec, name, threshold=0.75) {
  return(which(1 - stringdist(trimws(name_vec),trimws(name), method="jw") >= threshold))
}

tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }"
)