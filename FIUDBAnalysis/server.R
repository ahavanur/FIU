#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

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
  restrictCTRBanks <- reactive({
    input_banks = input$ctr_banks
    return(restrict_banks(ctr_df, input_banks))
  })
  
  restrictCTRDirection <- reactive({
    if (length(input$ctr_flow) == 0) {
      directions = c("DEPOSIT", "WITHDRAWAL")
    }
    else {
      directions = input$ctr_flow
    }
    return(which(toupper(ctr_display$cashDirection) %in% directions))
  })
  output$ctr_plot <- 
    renderPlot({
        ctr_display = ctr_df
        #selecting banks
        # if (length(input$ctr_banks) == 0) {
        #   banks = unique(toupper(ctr_display$fullNameOfFinancialInstitution))
        # }
        # else {
        #   banks = input$ctr_banks
        # }
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
        if (nrow(ctr_display) == 0) {
          h3("No Data Matched Search Criteria!")
        }
        else {
          #aggregating by month
          ctr_by_month = ctr_display %>%
            mutate(month = format(dateOfTransaction, "%m"), year = format(dateOfTransaction, "%Y")) %>%
            group_by(month, year, fullNameOfFinancialInstitution, cashDirection) %>%
            summarise(total = sum(cashAmount), count = n())
          ctr_by_month$date = as.Date(paste(ctr_by_month$year, ctr_by_month$month, "01", sep="-"), "%Y-%m-%d", origin = "1960-10-01")
          #seperating deposits for withdrawals
          deposits = ctr_by_month[which(toupper(ctr_by_month$cashDirection) == 'DEPOSIT'),]
          withdrawals = ctr_by_month[which(toupper(ctr_by_month$cashDirection) == 'WITHDRAWAL'),]

          withdrawals$total = -1 * withdrawals$total 
          withdrawals$count = -1 * withdrawals$count
          
          net = merge(deposits, withdrawals, by = c("date", "fullNameOfFinancialInstitution"))
          net$total = net$total.x + net$total.y
          net$count = net$count.x + net$count.y
          agg_deposits = deposits %>% group_by(date) %>% summarise(total = sum(total), count = sum(count))
          agg_withdrawals = withdrawals %>% group_by(date) %>% summarise(total = sum(total), count = sum(count))
          #this is a series of incredibly stupid lines in order to get the label marks just right
          if (length(directions) != 1) {
            rnge_df = agg_deposits
          } else if ("DEPOSIT" %in% directions) {
              rnge_df = agg_deposits
            }
          else {
              rnge_df = agg_withdrawals
              rnge_df$total = rnge_df$total*-1
          }
          rnge = seq(-1.5*round(max(rnge_df$total, na.rm = TRUE)), max(rnge_df$total)*1.1, 1)
          max_rnge = max(rnge) - floor(max(rnge)%%(10^floor(log10(max(rnge)))))
          min_rnge = -1*(abs(min(rnge)) - floor(abs(min(rnge))%%(10^floor(log10(abs(min(rnge)))))))
          rnge = round(rnge[(round(rnge%%10^(floor(log10(max(rnge)))))%%(10^(floor(log10(max(rnge))))))==0])
          rnge = c(min_rnge, rnge, max_rnge)
          
          
          w_d_total <- ggplot(NULL, aes(date, total)) + 
            geom_bar(stat = "identity", aes(fill = fullNameOfFinancialInstitution), data = deposits, fill = "forest green") +
            theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1), axis.text.y = (element_text(size=15)))+ scale_y_continuous(breaks=rnge, label=comma) +
            scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) + ylab("Total Amount ($)") + xlab("Date")
          #add withdrawals
          if (nrow(withdrawals) != 0) {
            w_d_total <- w_d_total + geom_bar(stat = "identity", aes(fill = fullNameOfFinancialInstitution), data = withdrawals, fill = "red")
          } 
          #add net 
          if (input$ctr_net) {
            w_d_total <- w_d_total + 
              stat_summary(data = net, fun.y = sum, geom = 'line', color = "dark blue", size = 2) +
              ggtitle("CTR Cash Flow Per Bank", subtitle = "Deposits in Green, Withdrawals in Red, Net in Blue")
            }
          else {
            w_d_total <- w_d_total + ggtitle("CTR Cash Flow Per Bank", subtitle = "Deposits in Green, Withdrawals in Red")
          }
          #showing all banks seperately
          if (input$ctr_bank_iso) {
            deposits_iso_use = deposits
            withdrawals_iso_use = withdrawals
            w_d_total <- w_d_total + facet_wrap(~fullNameOfFinancialInstitution, dir='v', scale = 'free')
          }
          else {
            deposits_iso_use = agg_deposits
            withdrawals_iso_use = agg_withdrawals
          }
          #adding labels to each bar 
          if ("DEPOSIT" %in% directions) {
            w_d_total = w_d_total + geom_text(aes(label = paste("$",as.character(total), sep="")), data = deposits_iso_use, stat = 'identity', angle = 90, position = position_stack(vjust = .5)) +
              geom_text(aes(label = count), data = deposits_iso_use, stat = 'identity', vjust = -1)
          }
          if ("WITHDRAWAL" %in% directions & nrow(withdrawals) != 0) {
            w_d_total = w_d_total + geom_text(aes(label = paste("$",as.character(-1*total), sep="")), data = withdrawals_iso_use, stat = 'identity', angle = 90, position = position_stack(vjust = .5))+
              geom_text(aes(label = -1*count), data = withdrawals_iso_use, stat = 'identity', vjust=1.5)
          }
          w_d_total
      }})
  pt1 <- reactive({
    if (!input$imm_permit) return(NULL)
    qplot(rnorm(500),fill=I("red"),binwidth=0.2,main="plotgraph1")
  })
  output$imm_ui <-
    renderUI(
      plotOutput("imm_plot_1"))
  output$imm_plot_1 <- 
    renderPlot({
      a = list(pt1(), pt1())
      grid.arrange(grobs=a)})
  
  output$str_ui <- renderUI(
      if (input$str_bank_iso) {
        if (length(input$str_banks) != 0) {
          plotOutput("str_plot", height=as.character(600*length(input$str_banks))) 
        }
        else {
          plotOutput("str_plot", height="1200")
        }
      }
    else {
      plotOutput("str_plot", height="800")
    }
  )
  
  
  restrictSTRBanks <- reactive({
    input_banks = input$str_banks
    return(restrict_banks(str_df, input_banks))
  })
  
  restrictSTRDatesGenerate <- reactive({
    dates <- input$str_generate_dates
    field1 <- 'strDateGenerate'
    field2 <- 'strDateGenerate'
    return(restrict_range(str_df, field1, field2, dates))
  })
  
  restrictSTRDatesActivity <- reactive({
    dates <- input$str_sus_dates
    field1 <- 'startDateOfSuspiciousActivity'
    field2 <- 'endDateOfSuspiciousActivity'
    return(restrict_range(str_df, field1, field2, dates))
  })
  
  restrictSTRcashAmount <- reactive ({
    cash_range <- c(input$str_min_cash, input$str_max_cash)
    field1 <- 'amountOfCash'
    field2 <- 'amountOfCash'
    return(restrict_range(str_df, field1, field2, cash_range))
  })
  
  restrictSTRaccts <- reactive ({
    strs_in_range = stracct_agg$STRID[which(stracct_agg$count <= input$str_max_accts & stracct_agg$count >= input$str_min_accts)]
    return(which(str_df$STRID %in% strs_in_range))
  })
  
  restrictSTRAdmissions <- reactive ({
    if (input$str_admission) {
     return(which(str_df$admissionOrConfession == 'Yes'))
    }
    return(seq(1,nrow(str_df)))
  })
  
  restrictSTR <- reactive ({
    str_rows_banks <- restrictSTRBanks()
    str_rows_gen_dates <- restrictSTRDatesGenerate()
    str_rows_sus_dates <- restrictSTRDatesActivity()
    str_rows_cash <- restrictSTRcashAmount()
    str_rows_accts <- restrictSTRaccts()
    str_rows_admin <- restrictSTRAdmissions()
    vals <- list(str_rows_banks,str_rows_gen_dates,str_rows_sus_dates,str_rows_cash,str_rows_accts,str_rows_admin)
    return(str_df[Reduce(intersect, vals),])
  })
  
  plot_str_hist <- reactive({
    str_display <- restrictSTR()
    
    str_by_month = str_display %>% mutate(month = format(strDateGenerate, "%m"), year = format(strDateGenerate, "%Y")) %>% 
      group_by(month, year, fullNameOfFinancialInstitution) %>% summarise(total = sum(amountOfCash), count = n())
    
    str_by_month$date = as.Date(paste(str_by_month$year, str_by_month$month, "01", sep="-"), "%Y-%m-%d", origin = "1960-10-01")
    agg_str = str_by_month %>% group_by(date) %>% summarise(total = sum(total), count = sum(count))
    
    str_hist <- ggplot(str_by_month, aes(date, total)) + 
      geom_bar(stat='identity', aes(fill = fullNameOfFinancialInstitution), fill = "forest green") + 
      theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1), axis.text.y = (element_text(size=15)))  +
      scale_y_continuous(label=comma) + scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) + 
      ylab("Total Amount ($)") + xlab("STR Generation Date")
    
    if (input$str_bank_iso) {
      str_hist <- str_hist + facet_wrap(~fullNameOfFinancialInstitution, dir='v', scale = 'free')
      agg_str_iso = str_by_month
    }
    else {
      agg_str_iso = agg_str
    }
    str_hist <- str_hist + 
      geom_text(aes(label = paste("$",as.character(total), sep="")), data = agg_str_iso, stat = 'identity', angle = 90, position = position_stack(vjust = .5)) +
      geom_text(aes(label = count), data = agg_str_iso, stat = 'identity', vjust = -1)
    
    str_hist
  })
  
  output$str_plot <- 
    renderPlot({
      a = list(pt1(), plot_str_hist())
      grid.arrange(grobs=a)
      })
    
  
  
})
