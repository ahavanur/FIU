library(markdown)

navbarPage("Navigation",
           tabPanel("Home", verbatimTextOutput("home screen")),
           tabPanel("CTR Summary",
                    fluidPage(
                      sidebarLayout(
                      sidebarPanel(
                        checkboxGroupInput("ctr_banks", "Banks", choices = 
                                             list("Bank of Hawaii" = "BANK OF HAWAII",
                                                  "Bank of Guam" = "BANK OF GUAM",
                                                  "BankPacific" = "BANKPACIFIC, LTD."),
                                           selected = 0),
                        checkboxGroupInput("ctr_flow", "Direction of Cash", choices = 
                                             list("Deposits" = "DEPOSIT",
                                                  "Withdrawals" = "WITHDRAWAL")),
                        dateRangeInput("ctr_dates", "Date Range", start = '2015-01-01', end = max(ctr_df$dateOfTransaction, na.rm = TRUE), format = 'mm-dd-yyyy', separator = "-"),
                        numericInput("ctr_min_cash", 
                                     "Min. Cash Amount", 
                                     value = min(ctr_df$cashAmount)),
                        numericInput("ctr_max_cash", 
                                   "Max. Cash Amount", 
                                   value = 10^(ceiling(log10(max(ctr_df$cashAmount)))+1)), #shoutout to 15-112 HW 1 
                        numericInput("ctr_min_accts", 
                                     "Min. Accounts Involved", 
                                     value = 0),
                        numericInput("ctr_max_accts", 
                                     "Max. Accounts Involved", 
                                     value = 10^ceiling(log10(max(pit_acct_count$count)))),
                        checkboxInput("ctr_bank_iso", "Show Banks Individually", value = FALSE),
                        checkboxInput("ctr_net", "Show Net Cash Direction", value = FALSE), width = 2),
                      mainPanel(
                        uiOutput("ctr_ui"),width = 10
                      )))),
           tabPanel("Summary",
                    verbatimTextOutput("summary")
           )
           )
