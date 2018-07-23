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
    return(which(toupper(ctr_df$cashDirection) %in% directions))
  })
  
  restrictCTRDatesGenerate <- reactive({
    dates <- input$ctr_dates
    field1 <- 'dateOfTransaction'
    field2 <- 'dateOfTransaction'
    return(restrict_range(ctr_df, field1, field2, dates))
  })
  
  restrictCTRcashAmount <- reactive ({
    cash_range <- c(input$ctr_min_cash, input$ctr_max_cash)
    field1 <- 'cashAmount'
    field2 <- 'cashAmount'
    return(restrict_range(ctr_df, field1, field2, cash_range))
  })
  
  restrictCTRaccts <- reactive ({
    range_account_ctrs = pit_acct_count$CTRID[which(pit_acct_count$count %in% seq(input$ctr_min_accts, input$ctr_max_accts))]
    return(which(ctr_df$CTRID %in% range_account_ctrs))
  })
  
  restrictCTR <- reactive ({
    ctr_rows_banks <- restrictCTRBanks()
    ctr_rows_direction <- restrictCTRDirection()
    ctr_rows_dates <- restrictCTRDatesGenerate()
    ctr_rows_cash <- restrictCTRcashAmount()
    ctr_rows_accts <- restrictCTRaccts()
    vals <- list(ctr_rows_banks,ctr_rows_direction,ctr_rows_dates,ctr_rows_cash,ctr_rows_accts)
    ctr_reduced = ctr_df[Reduce(intersect,vals),] %>% mutate(month = format(dateOfTransaction, "%m"), year = format(dateOfTransaction, "%Y"))
    ctr_reduced$date = as.Date(paste(ctr_reduced$year, ctr_reduced$month, "01", sep="-"), "%Y-%m-%d", origin = "1960-10-01")
    return(ctr_reduced)
  })
  
  output$ctr_plot <- 
    renderPlot({
        ctr_display <- restrictCTR()
        
        if (length(input$ctr_flow) == 0) {
          directions = c("DEPOSIT", "WITHDRAWAL")
        }
        else {
          directions = input$ctr_flow
        }
        
        if (nrow(ctr_display) == 0) {
          h3("No Data Matched Search Criteria!")
        }
        else {
          #aggregating by month
          ctr_by_month = ctr_display %>%
            group_by(date, fullNameOfFinancialInstitution, cashDirection) %>%
            summarise(total = sum(cashAmount), count = n())
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
          #monstrosity over
          
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
          plotOutput("str_plot", height=as.character(500*(length(input$str_banks))))
        }
        else {
          plotOutput("str_plot", height="1500")
        }
      }
    else {
      plotOutput("str_plot", height="1200")
    }
  )
  
  
  restrictSTRbanks <- reactive({
    input_banks = input$str_banks
    return(restrict_banks(str_df, input_banks))
  })
  
  restrictSTRdatesGenerate <- reactive({
    dates <- input$str_generate_dates
    field1 <- 'strDateGenerate'
    field2 <- 'strDateGenerate'
    return(restrict_range(str_df, field1, field2, dates))
  })
  
  restrictSTRdatesActivity <- reactive({
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
  
  restrictSTRadmissions <- reactive ({
    if (input$str_admission) {
     return(which(str_df$admissionOrConfession == 'Yes'))
    }
    return(seq(1,nrow(str_df)))
  })
  
  getOccupations <- eventReactive(input$str_occupation_button, {
    str_occupations <<- c(str_occupations, input$str_occupation)
  })
  
  restrictSTRoccupations <- reactive ({
    getOccupations()
    str_occupations <<- str_occupations[!(str_occupations=="")]
    if (length(str_occupations) != 0) {
      temp = str_df
      temp$original_index = seq(1,nrow(temp))
      temp = separate_rows(temp, "occupationOrTypeOfBusiness", sep="/")
      temp = separate_rows(temp, "occupationOrTypeOfBusiness", sep=" ")
      temp = separate_rows(temp, "occupationOrTypeOfBusiness", sep="-")
      temp$occupationOrTypeOfBusiness = lemmatize_strings(toupper(temp$occupationOrTypeOfBusiness))
      distances = as.data.frame(1-stringdistmatrix(temp$occupationOrTypeOfBusiness, toupper(str_occupations), method = "jw"))
      distances$max_similarity = apply(distances,1,max)
      return(unique(temp$original_index[which(distances$max_similarity > 0.75)]))
    }
    else {
      return(seq(1,nrow(str_df)))
    }
  })
  
  
  restrictSTR <- reactive ({
    str_rows_banks <- restrictSTRbanks()
    str_rows_gen_dates <- restrictSTRdatesGenerate()
    str_rows_sus_dates <- restrictSTRdatesActivity()
    str_rows_cash <- restrictSTRcashAmount()
    str_rows_accts <- restrictSTRaccts()
    str_rows_admin <- restrictSTRadmissions()
    str_occupation_rows <- restrictSTRoccupations()
    vals <- list(str_rows_banks,str_rows_gen_dates,str_rows_sus_dates,str_rows_cash,str_rows_accts,str_rows_admin,str_occupation_rows)
    str_reduced <- str_df[Reduce(intersect, vals),] %>% mutate(month = format(strDateGenerate, "%m"), year = format(strDateGenerate, "%Y"))
    str_reduced$date <- as.Date(paste(str_reduced$year, str_reduced$month, "01", sep="-"), "%Y-%m-%d", origin = "1960-10-01")
    View(str_reduced)
    return(str_reduced)
  })
  
  plot_str_hist <- reactive({
    str_display <- restrictSTR()
    str_by_month = str_display %>% group_by(date, fullNameOfFinancialInstitution) %>% summarise(total = sum(amountOfCash), count = n())
    
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
    print(paste(as.character(str_occupations), collapse=", "))
    str_hist <- str_hist + ggtitle("STR Amounts By Month", subtitle = paste("Showing Occupations: ", paste(as.character(str_occupations), collapse=", ")))
    str_hist
  })
  
  create_narrative_counts <- reactive({
    str_display <- restrictSTR()
    narrativetext = str_display[,c('STRID', 'narrative')]
    narrative_counts = narrativetext %>% unnest_tokens(word, narrative) %>% count(STRID, word, sort = TRUE) %>%
      ungroup()
    return(narrative_counts)
  })
  
  tf_idf_df <- reactive({
    narrative_counts = create_narrative_counts()
    numbers = narrative_counts$word[which(!is.na(as.numeric(as.character(gsub("[[:punct:]]", "", narrative_counts$word)))))]
    stopwords_full = c(common_stopwords, numbers)
    narrative_counts = narrative_counts[-which(narrative_counts$word %in% stopwords_full),]
    narrative_counts$word = lemmatize_words(narrative_counts$word)
    total_words <- narrative_counts %>% group_by(STRID) %>% summarize(total = sum(n))
    narrative_counts <- left_join(narrative_counts, total_words, by = 'STRID')
    narrative_counts <- narrative_counts %>%
      bind_tf_idf(word, STRID, n)
    narrative_counts = narrative_counts %>% mutate(word = word) %>% group_by(word) %>% 
      summarise(avg_tf_idf = mean(tf_idf), count = n())
    narrative_counts$freq = narrative_counts$avg_tf_idf*narrative_counts$count
    return(narrative_counts)
  })

  
  plot_str_cloud <- reactive({
    narrative_counts <- tf_idf_df()
    cloud = plotWordcloud(narrative_counts$word, freq = narrative_counts$freq,rot.per = 0, 
                          colors=brewer.pal(8, "Accent"), scale = 0.1, tryfit = TRUE, max_min = c(1, 0.05), 
                          min.freq = min(narrative_counts$freq), max.words = 25, grob = TRUE, dimensions = unit(c(2, 1), "npc"))
    cloud})
  
  output$str_plot <- 
    renderPlot({
      a = list(plot_str_hist(), plot_str_cloud())
      grid.arrange(grobs=a, heights=c(600,500))
      })
    
  
  
})
