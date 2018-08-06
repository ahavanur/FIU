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
  
  restrictCTRoccupations <- reactive({    
    range_occupation_ctrs = restrict_terms(pit_stringed,"occupationOrTypeOfBusiness", input$ctr_occupation,0.75)
    return(which(ctr_df$CTRID %in% unique(pit_stringed$CTRID[range_occupation_ctrs])))
  })
  
  restrictCTRlastnames <- reactive({
    range_lastname_ctrs = restrict_terms(pit_stringed,"lastNameOrNameOfEntity", input$ctr_lastname,0.75)
    return(which(ctr_df$CTRID %in% unique(pit_stringed$CTRID[range_lastname_ctrs])))
  })
  
  restrictCTRfirstnames <- reactive({
    range_firstname_ctrs = restrict_terms(pit_stringed,"firstName", input$ctr_firstname,0.75)
    return(which(ctr_df$CTRID %in% unique(pit_stringed$CTRID[range_firstname_ctrs])))
  })

  
  restrictCTR <- reactive ({
    ctr_rows_banks <- restrictCTRBanks()
    ctr_rows_direction <- restrictCTRDirection()
    ctr_rows_dates <- restrictCTRDatesGenerate()
    ctr_rows_cash <- restrictCTRcashAmount()
    ctr_rows_accts <- restrictCTRaccts()
    ctr_occupation_rows <- restrictCTRoccupations()
    ctr_lastname_rows <- restrictCTRlastnames()
    ctr_firstname_rows <- restrictCTRfirstnames()
    vals <- list(ctr_rows_banks,ctr_rows_direction,ctr_rows_dates,ctr_rows_cash,ctr_rows_accts,ctr_occupation_rows,ctr_lastname_rows,ctr_firstname_rows)
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
  
  restrictImmigrationDates <- reactive({
    dates <- input$imm_dates
    field1 <- 'Travel.Date'
    field2 <- 'Travel.Date'
    valid_dates = restrict_range(immigration_df, field1, field2, dates)
    return(valid_dates)
  })
  
  restrictImmigrationCountries <- reactive({
    if (length(input$imm_cities) != 0)
      return(which(immigration_df$Departs %in% input$imm_countries | immigration_df$Arrives %in% input$imm_countries))
    else {
      return(seq(1,nrow(immigration_df)))
    }
  })
  
  restrictImmigrationLastNames <- reactive({
    if (length(input$imm_lastnames) != 0)
      return(which(immigration_df$Last.Name %in% input$imm_lastnames))
    else {
      return(seq(1,nrow(immigration_df)))
    }
  })
  
  restrictImmigrationFirstNames <- reactive({
    if (length(input$imm_firstnames) != 0)
      return(which(immigration_df$First.and.Middle.Name %in% input$imm_firstnames))
    else {
      return(seq(1,nrow(immigration_df)))
    }
  })
  
  restrictImmigrationAirlines <- reactive({
    temp_df = immigration_df
    temp_df$original_index = seq(1,nrow(immigration_df))
    return(restrict_terms(temp_df, "flight", input$imm_airlines, 0.75))
  })
  
  restrictImmigrationCASEID <- reactive({
    if (is.null(input$imm_caseid)) {
      return(which(as.numeric(immigration_df$CASEID) == as.numeric(input$imm_caseid)))}
    else{
      return(seq(1,nrow(immigration_df)))
    }
  })
  
  restrictImmigrationCitizenship <- reactive({
    temp_df = immigration_df
    temp_df$original_index = seq(1,nrow(immigration_df))
    return(restrict_terms(temp_df, "Citizenship", input$imm_citizenships, 0.75))
  })
  
  restrictImmigration <- reactive({
    imm_date_rows <- restrictImmigrationDates()
    imm_country_rows <- restrictImmigrationCountries()
    imm_lastname_rows <- restrictImmigrationLastNames()
    imm_firstname_rows <- restrictImmigrationFirstNames()
    imm_airline_rows <- restrictImmigrationAirlines()
    imm_caseid_rows <- restrictImmigrationCASEID()
    imm_citizenship_rows <- restrictImmigrationCitizenship()
    vals <- list(imm_date_rows,imm_country_rows,imm_lastname_rows,imm_firstname_rows,imm_airline_rows,imm_caseid_rows,imm_citizenship_rows)
    imm_reduced = immigration_df[Reduce(intersect,vals),] %>% mutate(month = format(Travel.Date, "%m"), year = format(Travel.Date, "%Y"))
    imm_reduced$date = as.Date(paste(imm_reduced$year, imm_reduced$month, "01", sep="-"), "%Y-%m-%d", origin = "1960-10-01")
    return(imm_reduced)
  })
  
  
  output$imm_map <- renderLeaflet({
    immigration_display <- restrictImmigration()
    leaflet() %>% addTiles() %>% setView(lng = arrive_lon, lat = arrive_lat, zoom =4) %>% 
      addCircleMarkers(data = immigration_display, lng = ~depart_lon, lat = ~depart_lat, clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = FALSE, zoomToBoundsOnClick = FALSE))
  })
  
  restrictCustomsDates <- reactive({
    dates <- input$imm_dates
    field1 <- 'Date.Of.Report'
    field2 <- 'Date.Of.Report'
    valid_dates = restrict_range(customs_df, field1, field2, dates)
    return(valid_dates)
  })

  restrictCustomsLastNames <- reactive({
    if (length(input$imm_lastnames) != 0)
      return(which(customs_df$Last.Name %in% input$imm_lastnames))
    else {
      return(seq(1,nrow(customs_df)))
    }
  })
  
  restrictCustomsFirstNames <- reactive({
    if (length(input$imm_firstnames) != 0)
      return(which(customs_df$First.Name %in% input$imm_firstnames))
    else {
      return(seq(1,nrow(customs_df)))
    }
  })
  
  restrictCustomsAirlines <- reactive({
    temp_df = customs_df
    temp_df$original_index = seq(1,nrow(customs_df))
    return(restrict_terms(temp_df, "Flight", input$imm_airlines, 0.75))
  })
  
  restrictCustomsCASEID <- reactive({
    if (is.null(input$imm_caseid)) {
      return(which(as.numeric(customs_df$CASEID) == as.numeric(input$imm_caseid)))}
    else{
      return(seq(1,nrow(customs_df)))
    }
  })

  restrictCustomsAmount <- reactive({
    cash_range <- c(input$imm_min_cash, input$imm_max_cash)
    field1 <- 'Currency.Amount'
    field2 <- 'Currency.Amount'
    return(restrict_range(customs_df, field1, field2, cash_range))
  })
  
  
  restrictCustoms <- reactive({
    cus_date_rows <- restrictCustomsDates()
    cus_lastname_rows <- restrictCustomsLastNames()
    cus_firstname_rows <- restrictCustomsFirstNames()
    cus_airline_rows <- restrictCustomsAirlines()
    cus_caseid_rows <- restrictCustomsCASEID()
    cus_currency_rows <- restrictCustomsAmount()
    vals <- list(cus_date_rows,cus_lastname_rows,cus_firstname_rows,cus_airline_rows,cus_caseid_rows,cus_currency_rows)
    cus_reduced = customs_df[Reduce(intersect,vals),] %>% mutate(month = format(Date.Of.Report, "%m"), year = format(Date.Of.Report, "%Y"))
    cus_reduced$date = as.Date(paste(cus_reduced$year, cus_reduced$month, "01", sep="-"), "%Y-%m-%d", origin = "1960-10-01")
    return(cus_reduced)
  })
  
  output$imm_plot <- renderPlot({
    customs_display <- restrictCustoms()
    customs_display$fullname <- paste(customs_display$First.Name, customs_display$Last.Name)
    customs_display$direction <- ifelse(toupper(customs_display$Shipment.Point.Of.Arrival) == "PALAU", "Arriving", "Leaving")
    cus_by_month <- customs_display %>% group_by(date, fullname,direction) %>% summarise(total = sum(Currency.Amount), count = n())
    agg_cus = cus_by_month %>% group_by(date) %>% summarise(total = sum(total), count = sum(count))
    
    arriving = cus_by_month[which(cus_by_month$direction == 'Arriving'),]
    departing = cus_by_month[which(cus_by_month$direction == 'Leaving'),]
    
    departing$total = -1 * departing$total 
    departing$count = -1 * departing$count
    
    net = merge(arriving, departing, by = c("date", "fullname"))
    net$total = net$total.x + net$total.y
    net$count = net$count.x + net$count.y
    agg_arriving = arriving %>% group_by(date) %>% summarise(total = sum(total), count = sum(count))
    agg_departing = departing %>% group_by(date) %>% summarise(total = sum(total), count = sum(count))
    #this is a series of incredibly stupid lines in order to get the label marks just right
    rnge_df = agg_cus
    rnge = seq(-1.5*round(max(rnge_df$total, na.rm = TRUE)), max(rnge_df$total)*1.1, 1)
    max_rnge = max(rnge) - floor(max(rnge)%%(10^floor(log10(max(rnge)))))
    min_rnge = -1*(abs(min(rnge)) - floor(abs(min(rnge))%%(10^floor(log10(abs(min(rnge)))))))
    rnge = round(rnge[(round(rnge%%10^(floor(log10(max(rnge)))))%%(10^(floor(log10(max(rnge))))))==0])
    rnge = c(min_rnge, rnge, max_rnge)
    #monstrosity over
    
    a_d_total <- ggplot(NULL, aes(date, total)) + 
      geom_bar(stat = "identity", aes(fill = fullname), data = arriving, fill = "forest green") +
      theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1), axis.text.y = (element_text(size=15)))+ scale_y_continuous(breaks=rnge, label=comma) +
      scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%Y")) + ylab("Total Amount ($)") + xlab("Date")
    #add withdrawals
    if (nrow(departing) != 0) {
      a_d_total <- a_d_total + geom_bar(stat = "identity", aes(fill = fullname), data = departing, fill = "red")
    } 
    #add net 
    if (input$cus_net) {
      a_d_total <- a_d_total + 
        stat_summary(data = net, fun.y = sum, geom = 'line', color = "dark blue", size = 2) +
        ggtitle("Customs Cash Flow", subtitle = "Arriving Money in Green, Departing in Red, Net in Blue")
    }
    else {
      a_d_total <- a_d_total + ggtitle("CTR Cash Flow Per Bank", subtitle = "Arriving Money in Green, Departing in Red")
    }
    #showing all banks seperately
    if (input$cus_person_iso) {
      departing_iso_use = departing
      arriving_iso_use = arriving
      a_d_total <- a_d_total + facet_wrap(~fullname, dir='v', scale = 'free')
    }
    else {
      arriving_iso_use = agg_arriving
      departing_iso_use = agg_departing
    }
    #adding labels to each bar 
    a_d_total = a_d_total + geom_text(aes(label = paste("$",as.character(total), sep="")), data = arriving_iso_use, stat = 'identity', angle = 90, position = position_stack(vjust = .5)) +
      geom_text(aes(label = count), data = arriving_iso_use, stat = 'identity', vjust = -1)
    a_d_total = a_d_total + geom_text(aes(label = paste("$",as.character(-1*total), sep="")), data = departing_iso_use, stat = 'identity', angle = 90, position = position_stack(vjust = .5))+
      geom_text(aes(label = -1*count), data = departing_iso_use, stat = 'identity', vjust=1.5)
    a_d_total
  })
  
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
    valid_dates <- restrict_range(str_df, field1, field2, dates)
    if (all(input$str_generate_dates == c(min(str_df$strDateGenerate,na.rm=TRUE), today()))) {
      #if the dates are the defaults, then include the NA valued dates
      na_dates = which(is.na(str_df$strDateGenerate))
      return(c(na_dates, valid_dates))
    }
    else{
      return(valid_dates)
    }
  })
  
  restrictSTRdatesActivity <- reactive({
    dates <- input$str_sus_dates
    field1 <- 'startDateOfSuspiciousActivity'
    field2 <- 'endDateOfSuspiciousActivity'
    valid_dates = restrict_range(str_df, field1, field2, dates)
    if (all(input$str_sus_dates == c(min(str_df$startDateOfSuspiciousActivity,na.rm=TRUE), today()))) {
      #if the dates are the defaults, then include the NA valued dates
      na_dates = which(is.na(str_df$startDateOfSuspiciousActivity) | is.na(str_df$endDateOfSuspiciousActivity))
      return(c(na_dates, valid_dates))
    }
    return(valid_dates)
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
  
  restrictSTRoccupations <- reactive ({
    return(restrict_terms(str_stringed, "occupationOrTypeOfBusiness", input$str_occupation, 0.75))
  })
  
  restrictSTRnarratives <- reactive ({
    return(restrict_terms(str_stringed, "word", input$str_narrative, 0.75))
  })
  
  restrictSTRlastnames <- reactive ({
    return(restrict_terms(str_stringed, "lastNameOrNameOfEntity", input$str_lastname, 0.75))
  })
  
  restrictSTRfirstnames <- reactive ({
    return(restrict_terms(str_stringed, "firstName", input$str_firstname, 0.75))
  })
  
  restrictSTRcharacterization <- reactive ({
    return(restrict_terms(str_stringed, "summaryCharacterization", input$str_characterization, 0.75))
  })
  
  restrictSTR <- reactive ({
    str_rows_banks <- restrictSTRbanks()
    str_rows_gen_dates <- restrictSTRdatesGenerate()
    str_rows_sus_dates <- restrictSTRdatesActivity()
    str_rows_cash <- restrictSTRcashAmount()
    str_rows_accts <- restrictSTRaccts()
    str_rows_admin <- restrictSTRadmissions()
    str_occupation_rows <- restrictSTRoccupations()
    str_narrative_rows <- restrictSTRnarratives()
    str_characterization_rows <- restrictSTRcharacterization()
    str_lastname_rows <- restrictSTRlastnames()
    str_firstname_rows <- restrictSTRfirstnames()
    vals <- list(str_rows_banks,str_rows_gen_dates,str_rows_sus_dates,str_rows_cash,str_rows_accts,str_rows_admin,str_occupation_rows, str_narrative_rows,str_characterization_rows, str_lastname_rows,str_firstname_rows)
    str_reduced <- str_df[Reduce(intersect, vals),] %>% mutate(month = format(strDateGenerate, "%m"), year = format(strDateGenerate, "%Y"))
    str_reduced$date <- as.Date(paste(str_reduced$year, str_reduced$month, "01", sep="-"), "%Y-%m-%d", origin = "1960-10-01")
    return(str_reduced)
  })
  
  plot_str_hist <- reactive({
    str_display <- restrictSTR()
    # x <- rbind(str_display, str_df)
    # View(x[! duplicated(x, fromLast=TRUE) & seq(nrow(x)) <= nrow(str_display), ])
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
    subtitle_count = paste("Total STRs Shown:",as.character(nrow(str_display)),"\n")
    subtitle_amount = paste("Total STR Amounts Shown:", as.character(sum(str_by_month$total)),"\n")
    if (length(input$str_occupation) != 0) {
      subtitle_occupations = paste("Occupations:", paste(input$str_occupation, collapse=", "), "\n")
    } 
    else {
      subtitle_occupations = NULL
    }
    if (length(input$str_narrative) != 0) {
      subtitle_narratives = paste("Narrative Keywords:", paste(input$str_narrative,collapse=", "), "\n")
    }
    else {
      subtitle_narratives = NULL
    }
    if (length(input$str_firstname) != 0) {
      subtitle_firstname = paste("First Names:", paste(input$str_firstname,collapse=", "), "\n")
    }
    else {
      subtitle_firstname = NULL
    }
    if (length(input$str_lastname) != 0) {
      subtitle_lastname = paste("Last Names:", paste(input$str_lastname,collapse=", "), "\n")
    }
    else {
      subtitle_lastname = NULL
    }
    
    if (length(input$str_characterization) != 0) {
      subtitle_characterization = paste("Characterizations:", paste(input$str_characterization,collapse=", "), "\n")
    }
    else {
      subtitle_characterization = NULL
    }
    subtitlefull = paste(subtitle_count,subtitle_amount,subtitle_occupations, subtitle_narratives, subtitle_firstname, subtitle_lastname,subtitle_characterization, sep = "")
    str_hist <- str_hist + ggtitle("STR Amounts By Month", subtitle = subtitlefull)
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
    if (max(narrative_counts$freq) <= 0) {
      narrative_counts$freq <- narrative_counts$count
    }
    cloud = plotWordcloud(narrative_counts$word, freq = narrative_counts$freq, rot.per = 0, 
                          colors=brewer.pal(8, "Accent"), scale = 0.1, tryfit = TRUE, max_min = c(1, 0.05), 
                          min.freq = min(narrative_counts$freq), max.words = 25, grob = TRUE, dimensions = unit(c(2, 1), "npc"))
    cloud})
  
  output$str_plot <- 
    renderPlot({
      a = list(plot_str_hist(), plot_str_cloud())
      grid.arrange(grobs=a, heights=c(600,500))
      })
    
  
  
})
