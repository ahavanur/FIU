#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(shiny)
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
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$ctr_ui <- 
    renderUI(
  if (input$ctr_bank_iso) {
    if (length(input$ctr_banks) != 0) {
      plotOutput("ctr_plot", height=as.character(600*length(input$ctr_banks))) 
    }
    else {
      plotOutput("ctr_plot", height="1800")
    }
  }
  else {
    plotOutput("ctr_plot", height="800")
    }
  )
  output$ctr_plot <- renderPlot({
    ctr_display = ctr_df
    #selecting banks
    if (length(input$ctr_banks) == 0) {
      banks = unique(toupper(ctr_display$fullNameOfFinancialInstitution))
    }
    else {
      banks = input$ctr_banks
    }
    ctr_display = ctr_display[which(toupper(ctr_display$fullNameOfFinancialInstitution) %in% c(banks, "")),]
    #selecting direction
    if (length(input$ctr_flow) == 0) {
      directions = c("DEPOSIT", "WITHDRAWAL")
    }
    else {
      directions = input$ctr_flow
    }
    ctr_display = ctr_display[which(toupper(ctr_display$cashDirection) %in% directions),]
    #restricting date range
    ctr_display = ctr_display[which(ctr_display$dateOfTransaction >= input$ctr_dates[1]),]
    ctr_display = ctr_display[which(ctr_display$dateOfTransaction <= input$ctr_dates[2]),]
    #restricting ctr amounts 
    ctr_display = ctr_display[which(ctr_display$cashAmount >= input$ctr_min_cash),]
    ctr_display = ctr_display[which(ctr_display$cashAmount <= input$ctr_max_cash),]
    #restricting accounts range
    range_account_ctrs = pit_acct_count$CTRID[which(pit_acct_count$count %in% seq(input$ctr_min_accts, input$ctr_max_accts))]
    ctr_display = ctr_display[which(ctr_display$CTRID %in% range_account_ctrs),]
    ctr_display$cashDirection = toupper(ctr_display$cashDirection)
    #putting it all together
    ctr_by_month = ctr_display %>%
      mutate(month = format(dateOfTransaction, "%m"), year = format(dateOfTransaction, "%Y")) %>%
      group_by(month, year, fullNameOfFinancialInstitution, cashDirection) %>%
      summarise(total = sum(cashAmount), count = n())
    ctr_by_month$date = as.Date(paste(ctr_by_month$year, ctr_by_month$month, "01", sep="-"), "%Y-%m-%d", origin = "1960-10-01")
    deposits = ctr_by_month[which(toupper(ctr_by_month$cashDirection) == 'DEPOSIT'),]
    withdrawals = ctr_by_month[which(toupper(ctr_by_month$cashDirection) == 'WITHDRAWAL'),]

    withdrawals$total = -1 * withdrawals$total 
    withdrawals$count = -1 * withdrawals$count
    
    net = merge(deposits, withdrawals, by = c("date", "fullNameOfFinancialInstitution"))
    net$total = net$total.x + net$total.y
    net$count = net$count.x + net$count.y
    #rnge = seq(min(rnge), max(rnge), length.out=21)
    agg_deposits = deposits %>% group_by(date) %>% summarise(total = sum(total), count = sum(count))
    agg_withdrawals = withdrawals %>% group_by(date) %>% summarise(total = sum(total), count = sum(count))
    if (length(directions != 1)) {
      rnge_df = ctr_by_month
    }
    else{
      if ("DEPOSIT" %in% directions) {
        rnge_df = agg_deposits
      }
      else {
        rnge_df = agg_withdrawals
        rnge_df$total = rnge_df$total*-1
      }
    }
    rnge = seq(-1.5*round(max(rnge_df$total, na.rm = TRUE)), max(rnge_df$total)*1.1, 1)
    rnge = round(rnge[(round(rnge%%10^(floor(log10(max(rnge)))))%%(10^(floor(log10(max(rnge))))))==0])
    w_d_total <- ggplot(NULL, aes(date, total)) + 
      geom_bar(stat = "identity", aes(fill = fullNameOfFinancialInstitution), data = deposits, fill = "forest green") +
      geom_bar(stat = "identity", aes(fill = fullNameOfFinancialInstitution), data = withdrawals, fill = "red") + 
      theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(breaks=rnge, label=comma)
    if (input$ctr_net) {
      w_d_total <- w_d_total + 
        stat_summary(data = net, fun.y = sum, geom = 'line', color = "dark blue", size = 2) +
        scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) + ylab("Total Amount ($)") + ggtitle("CTR Cash Flow Per Bank", subtitle = "Deposits in Green, Withdrawals in Red, Net in Blue")
        }
    else {
      w_d_total <- w_d_total +
        scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) + ylab("Total Amount ($)") + ggtitle("CTR Cash Flow Per Bank", subtitle = "Deposits in Green, Withdrawals in Red")
    }
    if (input$ctr_bank_iso) {
      deposits_iso_use = deposits
      withdrawals_iso_use = withdrawals
      w_d_total = w_d_total + facet_wrap(~fullNameOfFinancialInstitution, dir='v', scale = 'free')
    }
    else {
      deposits_iso_use = agg_deposits
      withdrawals_iso_use = agg_withdrawals
    }
    if ("DEPOSIT" %in% directions) {
      w_d_total = w_d_total + geom_text(aes(label = paste("$",as.character(total), sep="")), data = deposits_iso_use, stat = 'identity', angle = 90, position = position_stack(vjust = .5)) +
        geom_text(aes(label = count), data = deposits_iso_use, stat = 'identity', vjust = -1)
    }
    if ("WITHDRAWAL" %in% directions) {
      w_d_total = w_d_total + geom_text(aes(label = paste("$",as.character(-1*total), sep="")), data = withdrawals_iso_use, stat = 'identity', angle = 90, position = position_stack(vjust = .5))+
        geom_text(aes(label = -1*count), data = withdrawals_iso_use, stat = 'identity', vjust=1)
    }
    w_d_total
  })
  
})
