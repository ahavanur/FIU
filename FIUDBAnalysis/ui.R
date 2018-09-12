library(markdown)

navbarPage("Navigation",
           tabPanel("Main Page", h1("Welcome to the FIU Raider"), 
                    h2("About this tool"),
                    h5("RAIDER (Report Analytics & Intelligence Data Examination in R) is a tool designed to help the FIU better understand the visualize intelligence reports and data collected from partner agencies."),
                    h2("Tabs Explanation"),
                    h5("Each tab represents a different source of intelligence, and provides filtering and querying options, and employs a variety of visualization techniques"),
                    h2("Getting Started"),
                    h5("To begin, please click on the button below to choose a folder that contains the exported files of the FIU Database. Before you begin, select the export folder from the directory below."), 
                    shinyDirButton("folder_path", "Chose folder", "Import FIU Export Files"),
                    h6("FIU RAIDER was developed by Apoorva Havanur (https://github.com/ahavanur/FIU) during the summer of 2018 while working as a consultant for the Palau FIU. Contact yhavanur@gmail.com for questions or concerns.")
                    ),
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
                        dateRangeInput("ctr_dates", "Date Range", start = '2015-01-01', end = today(), format = 'mm-dd-yyyy', separator = "-"),
                        numericInput("ctr_min_cash", 
                                     "Min. Cash Amount", 
                                     value = 0),
                        numericInput("ctr_max_cash", 
                                   "Max. Cash Amount", 
                                   value = NULL), 
                        numericInput("ctr_min_accts", 
                                     "Min. Accounts Involved", 
                                     value = 0),
                        numericInput("ctr_max_accts", 
                                     "Max. Accounts Involved", 
                                     value = NULL),
                        checkboxInput("ctr_bank_iso", "Show Banks Individually", value = FALSE),
                        checkboxInput("ctr_net", "Show Net Cash Direction", value = FALSE),
                        selectizeInput("ctr_firstname", "First Name", NULL,  multiple=TRUE),
                        selectizeInput("ctr_lastname", "Last Name", NULL, multiple=TRUE),
                        selectizeInput("ctr_occupation", "Occupation", NULL,  multiple=TRUE), 
                        width = 2),
                      mainPanel(
                        h2("Summarizing CTR History Over Time"),
                        h5("The plot below displays CTRs for each month over time. Use the buttons on the side to filter for specific individuals, dates, banks, etc. If an error message appears, it means that the filters you've selected have resulted in no matching CTRs. Remove the filters and try again."),
                        uiOutput("ctr_ui"),width = 10
                      )))),
           tabPanel("Immigration & Customs Summary", fluidPage(sidebarLayout(
                      sidebarPanel(
                        dateRangeInput("imm_dates", "Date Range", start = '2015-01-01', end = today(), format = 'mm-dd-yyyy', separator = "-"),
                        selectizeInput("imm_cities", "Cities", NULL, multiple=TRUE),
                        selectizeInput("imm_firstnames", "First Name", NULL, multiple=TRUE),
                        selectizeInput("imm_lastnames", "Last Name", NULL, multiple=TRUE),
                        checkboxInput("cus_person_iso", "Show People Individually", value = FALSE),
                        selectizeInput("imm_airlines", "Airlines", NULL, multiple=TRUE),
                        selectInput("imm_caseid", "CASE ID", choices= c("all cases")),
                        numericInput("imm_min_cash", 
                                     "Min. Cash Amount", 
                                     value = 0),
                        numericInput("imm_max_cash", 
                                     "Max. Cash Amount", 
                                     value = NULL),width=2),
                      mainPanel(
                        h2("International and Currency Movement"),
                        h5("The map below shows the movement patterns identified in the immigration and customs data collected, while the graph below shows the flow of currency (calculated in USD) in and out of Palau. Since immigration and customs are the newest data available in the FIUDB, these graphs may not appear due to lack of data"),
                        leafletOutput("imm_map"),uiOutput("imm_plot_ui"), width = 10)))),
           tabPanel("STR Summary", fluidPage(sidebarLayout(
                      sidebarPanel(
                        checkboxGroupInput("str_banks", "Banks", choices = 
                                             list("Bank of Hawaii" = "BANK OF HAWAII",
                                                  "Bank of Guam" = "BANK OF GUAM"),
                                           selected = 0),
                        dateRangeInput("str_generate_dates", "STR Generation Date Range", start = NULL, end = today(), format = 'mm-dd-yyyy', separator = "-"),
                        dateRangeInput("str_sus_dates", "STR Suspicion Date Range", start = NULL, end = today(), format = 'mm-dd-yyyy', separator = "-"),
                        numericInput("str_min_cash", 
                                     "Min. Cash Amount", 
                                     value = NULL),
                        numericInput("str_max_cash", 
                                     "Max. Cash Amount", 
                                     value = NULL), #shoutout to 15-112 HW 1 
                        numericInput("str_min_accts", 
                                     "Min. Accounts Involved", 
                                     value = 0),
                        numericInput("str_max_accts", 
                                     "Max. Accounts Involved", 
                                     value = NULL),
                        checkboxInput("str_admission", "Admission of Guilt", value=FALSE),
                        checkboxInput("str_bank_iso", "Show Banks Individually", value = FALSE),
                        selectizeInput("str_firstname", "First Name", NULL,  multiple=TRUE),
                        selectizeInput("str_lastname", "Last Name",NULL, multiple=TRUE),
                        selectizeInput("str_occupation", "Occupation", NULL,  multiple=TRUE),
                        selectizeInput("str_characterization", "Characterization", NULL,  multiple=TRUE),
                        selectizeInput("str_narrative", "Narrative Terms",NULL, multiple=TRUE),
                        width = 2
                      ),
                      mainPanel(
                        h2("Summarizing STR History Over Time"),
                        h5("The plot below displays STRs for each month over time. Use the buttons on the side to filter for specific individuals, dates, banks, etc. The word cloud below identifies keywords in the narratives of the displayed STRs. If an error message appears, it means that the filters you've selected have resulted in no matching STRs. Remove the filters and try again."),
                        uiOutput("str_ui"), width = 10)
           ))),
           tabPanel("Individual Report", fluidPage(sidebarLayout(
             sidebarPanel(
               selectizeInput("indiv_name", "Name", NULL,  multiple=FALSE),
               selectizeInput("indiv_assc", "Track Associates", NULL,  multiple=TRUE),
               selectizeInput("indiv_accts", "Track Accounts", NULL,  multiple=TRUE),
               width=2),
             mainPanel(
               h2("Overall Summary of Individual(s)"),
               h5("This page allows you to get a brief snapshot of an individual and their known associates and buisness dealings. Start by selecting a Name, and the plots will then be produced. You can then select accounts associated with that person (drawn from STR and CTR filings), and potential known assocaites (other names linked to those accounts) and see a comprehensive picture involving all parts involved."),
               uiOutput("indiv_plots"),
               plotOutput("indiv_plot_ctr"),
               plotOutput("indiv_plot_str"),
               leafletOutput("indiv_imm_map"),width=10)
             )))
           )
