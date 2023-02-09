# Create UI
ui <- navbarPage(
  title = "Commodity Retail Prices Pilipinas",
  collapsible=TRUE,
  
  #-----------TAB1:---------------- #label; variables depending on which tab they appear
  tabPanel(title = "Statistics",
           fluidPage(
             sidebarLayout(
               sidebarPanel(width=2,
                            selectInput('T1_Regio_Natio', 'Select Administative level', 
                                        choices = list("Subregional" = "subreg", "Subnational" = "subnat"), selected = "subnat"),
                            uiOutput("T1_regio_select"),  #show regions list
                            checkboxGroupInput("T1_CommGrp","Choose commodity  Group(s):", commod_names, selected = "BeansLegumes"),
                            actionLink("selectall","Select All"),
                            sliderInput("T1Date", "Select Date Range:",
                                        min = as.Date("2012-01-01","%Y-%m-%d"),
                                        max = as.Date("2021-12-01","%Y-%m-%d"),
                                        value= c(as.Date("2015-01-01","%Y-%m-%d"),as.Date("2019-01-01","%Y-%m-%d")),
                                        timeFormat="%Y-%m-%d")
               ),
               mainPanel(width =10,
                         tabsetPanel(
                           tabPanel("Plot",
                                    fluidRow(
                                      column(6,plotOutput("Pricebar_high")),
                                      column(6,plotOutput("Pricebar_low"))
                                    ),
                                    br(),
                                    br(),
                                    fluidRow(
                                      column(6,plotOutput("Pricebar_volat")),
                                      column(6,plotOutput("Pricebar_stabi"))
                                    ),
                                    br(),
                                    fluidRow(
                                      column(6,plotOutput("Pricebar_diffhigh")),
                                      column(6,plotOutput("Pricebar_difflow"))
                                    ),
                                    br(),
                                    DT::dataTableOutput("text")
                           )
                         )
               )
             )
           )
  ),
  #-------------TAB2:----------------
  tabPanel(title = "Price trends",
           sidebarLayout(
             sidebarPanel(width = 2,
                          selectInput("T2Var0", "Select Commodity Group", choices = commod_names), 
                          selectInput("T2Var1", "Select Commodity",choices=""),
                          selectInput("T2Var2", "Select Admininstative level", choices = "Province"),
                          selectInput("T2Var3", "Select Specific Unit", choices = "Abra"),
                          uiOutput("condition_T2Var4"),  #only show if yes
                          sliderInput("T2Var5", "Select Date Range:",
                                      min = as.Date("2012-01-01","%Y-%m-%d"),
                                      max = as.Date("2021-12-01","%Y-%m-%d"),
                                      value= c(as.Date("2015-01-01","%Y-%m-%d"),as.Date("2019-01-01","%Y-%m-%d")),
                                      timeFormat="%Y-%m-%d")
             ),
             mainPanel(width=10,
               tabsetPanel(
                 tabPanel("Plot",
                          fluidRow(plotOutput("Priceplot")),
                          ),
                 tabPanel("Map",
                          fluidRow(leafletOutput("PriceMap"))
                          )
               )
             )
           )
  ),
  
  
  
  #---------------------TAB3:-----------
  tabPanel(title = "Data table","content 3"),
  #----------------TAB4:------------------
  tabPanel(title = "About","content 3")
)


