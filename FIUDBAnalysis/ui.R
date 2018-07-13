library(markdown)

navbarPage("Navigation",
           tabPanel("Home", verbatimTextOutput("home screen")),
           tabPanel("CTR Summary", fluidPage(sidebarLayout(
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
           tabPanel("Immigration & Customs Summary", fluidPage(sidebarLayout(
                      sidebarPanel(
                        dateRangeInput("imm_dates", "Date Range", start = '2015-01-01', end = today(), format = 'mm-dd-yyyy', separator = "-"),
                        checkboxInput("imm_permit", "Holds Permit", value = FALSE), width = 2)
                      ,
                      mainPanel(uiOutput('imm_ui'), width = 10)))),
           tabPanel("STR Summary", fluidPage(sidebarLayout(
                      sidebarPanel(
                        checkboxGroupInput("str_banks", "Banks", choices = 
                                             list("Bank of Hawaii" = "BANK OF HAWAII",
                                                  "Bank of Guam" = "BANK OF GUAM",
                                                  "BankPacific" = "BANKPACIFIC, LTD."),
                                           selected = 0),
                        dateRangeInput("str_dates", "Date Range", start = '2015-01-01', end = max(str_df$strDateGenerate, na.rm = TRUE), format = 'mm-dd-yyyy', separator = "-"),
                        numericInput("str_min_cash", 
                                     "Min. Cash Amount", 
                                     value = min(str_df$amountOfCash,na.rm=TRUE)),
                        numericInput("str_max_cash", 
                                     "Max. Cash Amount", 
                                     value = 10^(ceiling(log10(max(str_df$amountOfCash,na.rm=TRUE)))+1)), #shoutout to 15-112 HW 1 
                        numericInput("str_min_accts", 
                                     "Min. Accounts Involved", 
                                     value = 0),
                        numericInput("str_max_accts", 
                                     "Max. Accounts Involved", 
                                     value = 10^ceiling(log10(max(stracct_agg$count)))),
                        selectInput("str_summary_type", "Summary Characterization", choices = )
                        checkboxInput("str_admission", "Admission of Guilt", value=FALSE)
                      ),
                      mainPanel(uiOutput("str_ui"), width = 10)
           )))
           )