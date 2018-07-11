library(markdown)
navbarPage("Navigation",
           tabPanel("Home", verbatimTextOutput("home screen")),
           tabPanel("CTR Summary",
                    sidebarLayout(
                      sidebarPanel(
                        checkboxGroupInput("ctr_banks", "Banks", choices = 
                                             list("Bank of Hawaii" = "BANK OF HAWAII",
                                                  "Bank of Guam" = "BANK OF GUAM",
                                                  "BankPacific" = "BANKPACIFIC, LTD."),
                                           selected = 0),
                        checkboxGroupInput("ctr_flow", "Direction of Cash", choices = 
                                             list("Withdrawals" = "WITHDRAWAL",
                                                  "Deposits" = "DEPOSIT")),
                        checkboxInput("ctr_net", "Show Net Cash Direction", value = FALSE),
                        dateRangeInput("ctr_dates", "Date Range"),
                        numericInput("ctr_min_cash", 
                                     "Min. Cash Amount", 
                                     value = 0),
                        numericInput("ctr_max_cash", 
                                   "Max. Cash Amount", 
                                   value = 100),
                        numericInput("ctr_min_ppl", 
                                     "Min. People Involved", 
                                     value = 0),
                        numericInput("ctr_max_cash", 
                                     "Max. People Involved", 
                                     value = 100)),
                      mainPanel(
                        plotOutput("plot")
                      ))),
           tabPanel("Summary",
                    verbatimTextOutput("summary")
           )
           )
