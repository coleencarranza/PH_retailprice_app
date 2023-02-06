library(shiny)
library(ggplot2)
library(scales)
library(DT)
library(dplyr)
library(leaflet)
library(sf)
library(magrittr)
library(geojsonio)
library(htmltools)
library(htmlwidgets)
library(stringi)
library(RColorBrewer)

# Read retail price data script:
source("./retail_prices_data.R")


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
                          fluidRow(
                            column(6,plotOutput("Pricebar_volat")),
                            column(6,plotOutput("Pricebar_stabi"))
                          ),
                          fluidRow(
                            column(6,plotOutput("Pricebar_diffhigh")),
                            column(6,plotOutput("Pricebar_difflow"))
                          ),
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
             sidebarPanel(width = 3,
                          selectInput("T2Var0", "Select Commodity Group", choices = commod_names), 
                          selectInput("T2Var1", "Select Commodity",choices=""),
                          selectInput("T2Var2", "Select Admininstative level", choices = ""),
                          selectInput("T2Var3", "Select Specific Unit", choices = ""),
                          selectInput("T2Var4", "Show subsequent sub-units?",
                                      choices = list("Yes" = "w_sub","No" = "wo_sub"),selected = "wo_sub"),
                          uiOutput("condition_T2Var4"),  #only show if yes
                          sliderInput("T2Var5", "Select Date Range:",
                                      min = as.Date("2012-01-01","%Y-%m-%d"),
                                      max = as.Date("2021-12-01","%Y-%m-%d"),
                                      value= c(as.Date("2015-01-01","%Y-%m-%d"),as.Date("2019-01-01","%Y-%m-%d")),
                                      timeFormat="%Y-%m-%d")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput("Priceplot"),leafletOutput("PriceMap")
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




# Define server logic 
server <- function(input, output, session) {
  #================--------TAB 1----------=================
  #T1 reactive UI
  #Select/deselect
  observe({
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    { updateCheckboxGroupInput(session,"T1_CommGrp","Choose commodity  Group(s):",choices=commod_names,selected=commod_names)
    } else{
      updateCheckboxGroupInput(session,"T1_CommGrp","Choose commodity  Group(s):",choices=commod_names)
    }
  })
  
  #renderUI for conditional admin_level
  output$T1_regio_select <- renderUI({
    if(input$T1_Regio_Natio == "subreg"){
      selectInput("regio_list", "Select region:", regions, selected = "Region I")}
    })
  
  #T1 DATA WRANGLING:
 #select provinces  sub national or subregional depends on input
  t1prov_sub <- reactive({  #vector of provinces
    if(input$T1_Regio_Natio=="subreg"){
      prov <- subset(ph_adm,  ADM1_EN %in% input$regio_list) %>% select(ADM2_EN) %>% unlist()
    }else{
      prov <- ph_adm$ADM2_EN
    }
    })
  
  #Price summarize based on  dynamic inputs:
  price_dfsub <- reactive({
    provs <-t1prov_sub()
    #function
    per_commod_summ<-function(x){
      x %>%
      dplyr::filter(Date >= input$T1Date[1] & Date <= input$T1Date[2]) %>%
        dplyr::filter(Admin_level == "Province") %>%
        dplyr::filter(Region.Province %in% provs) %>%
        group_by(Commodity, .drop = FALSE) %>%
        summarise(across(Price, list(average=mean,variability = sd), na.rm = TRUE)) %>%
        mutate_at(2:3, funs(round(., 2))) 
      
    }
    
    price_diff_time <- function(x){  
      x %>%
        dplyr::filter(Date >= input$T1Date[1] & Date <= input$T1Date[2]) %>%
        dplyr::filter(Admin_level == "Province") %>%
        dplyr::filter(Region.Province %in% provs) %>%
        dplyr::filter(Date == max(Date) | Date == min(Date)) %>%
        group_by(Commodity,Date) %>%
        summarise(across(Price, mean, na.rm = TRUE), .groups="drop_last") %>%
        mutate(Price_diff = Price - lag(Price)) %>%
        select(Commodity,Price_diff)%>%
        mutate_at(2, funs(round(., 2))) %>%
        drop_na(Price_diff)
    }
    
    diff <- df[which(commod_names %in% input$T1_CommGrp)] %>%
      lapply(.,  price_diff_time) %>%
      bind_rows()
    
    summ <-df[which(commod_names %in% input$T1_CommGrp)] %>%  
      lapply(., per_commod_summ) %>%
      bind_rows()
    
    dfs<-left_join(summ,diff,by="Commodity") %>%
      drop_na(Price_average)
  })
  

  
    #----------Horizontal bar plots------------------
  #most expensive
  output$Pricebar_high <- renderPlot({
    bars <- price_dfsub() %>%
      top_n(n = 7, wt = Price_average)
    
    ggplot(bars, aes(x = reorder(Commodity,Price_average), y = Price_average)) +
      geom_bar(stat = "identity",fill="indianred3")+
      labs(y = "Average suggested retail price (SRP) in PhP", x = "Commodity",
           title = "Most expensive commodities") +
      coord_flip() +
      theme_bw(base_size =15)
  })
  #cheapest:
  output$Pricebar_low <- renderPlot({
    bars <- price_dfsub() %>%
        top_n(n = -7, wt = Price_average)
    
    ggplot(bars, aes(x = reorder(Commodity,Price_average,decreasing=T), y = Price_average)) +
      geom_bar(stat = "identity",fill="forestgreen")+
      labs(y = "Average suggested retail price (SRP) in PhP", x = "Commodity",
           title = "Cheapest commodities") +
      coord_flip() +
      theme_bw(base_size =15)
  })
  #most stable price:
  output$Pricebar_stabi <- renderPlot({
    bars <- price_dfsub() %>%
      top_n(n = -7, wt = Price_variability)
    
    ggplot(bars, aes(x = reorder(Commodity,Price_variability,decreasing=TRUE), y = Price_variability)) +
      geom_bar(stat = "identity",fill="lightskyblue3")+
      labs(y = "Retail prices standard deviation in PhP", x = "Commodity",
           title = "Most stable prices") +
      coord_flip() +
      theme_bw(base_size =15)
  })
  #Most variable/volatile price:
  output$Pricebar_volat <- renderPlot({
    bars <- price_dfsub() %>%
      top_n(n = 7, wt =Price_variability)
    
    ggplot(bars, aes(x = reorder(Commodity,Price_variability), y = Price_variability)) +
      geom_bar(stat = "identity",fill="violetred4")+
      labs(y = "Retail prices standard deviation in PhP", x = "Commodity",
           title = "Most volatile prices") +
      coord_flip() +
      theme_bw(base_size =15)
  })
  #largest price diff:
  output$Pricebar_diffhigh <-renderPlot({
    bars <- price_dfsub() %>%
      top_n(n = 7, wt = Price_diff)
    
    ggplot(bars, aes(x = reorder(Commodity,Price_diff), y = Price_diff)) +
      geom_bar(stat = "identity",fill="lightgoldenrod") +
      labs(y = "in PhP", x = "Commodity",
           title = "Largest price change") +
      coord_flip() +
      theme_bw(base_size = 15)
  })
  #Smallest price diff:
  output$Pricebar_difflow <- renderPlot({
    bars <- price_dfsub() %>%
      top_n(n = -7,wt = Price_diff)
    
    ggplot(bars, aes(x = reorder(Commodity,Price_diff,decreasing=T), y =Price_diff)) +
      geom_bar(stat = "identity",fill="tan")+
      labs(y = "in PhP", x = "Commodity",
           title = "Smallest price change") +
      coord_flip() +
      theme_bw(base_size =15)
  })

  #table 
  output$text <- DT::renderDataTable(price_dfsub())
  # output$text <-renderText(t1prov_sub())

  #===============--------TAB 2----------================
  #T2 reactive UI:
  selectedvariable0 <- reactive({
    df[[grep(input$T2Var0,commod_names)]] #dataframe
  })
  #Update commodity
  observeEvent(selectedvariable0(), {#Update Commodity dropdown
    choices <- unique(selectedvariable0()$Commodity)
    updateSelectInput(inputId = "T2Var1", choices = choices) 
  })
  
  selectedvariable1 <- reactive({
    req(input$T2Var0)
    filter(selectedvariable0(), Commodity == input$T2Var1)
  })
  #Update Admin level
  observeEvent(selectedvariable1(), {#Update Admin_level dropdown
    choices <- unique(selectedvariable1()$Admin_level)
    updateSelectInput(inputId = "T2Var2", choices = choices)
  })
  
  selectedvariable2 <- reactive({
    req(input$T2Var1)
    filter(selectedvariable1(), Admin_level == input$T2Var2)
  })
  #Update Region/Province
  observeEvent(selectedvariable2(), { #Update specific province/Region
    choices <- unique(selectedvariable2()$Region.Province)
    updateSelectInput(inputId = "T2Var3", choices = choices)
  })
  
  
  # renderUI for conditional checkbox subUnits
  output$condition_T2Var4 <- renderUI({
    if(input$T2Var4=="w_sub"){
      checkboxInput("show_subUnits", "Plot sub-units?", FALSE)
    }
  }) 
  
  selectedvariable3 <- reactive({
    req(input$T2Var2)
    filter(selectedvariable2(), Region.Province == input$T2Var3)
  })
  #Update Date
  observeEvent(selectedvariable3(), {
    choices <- unique(selectedvariable3()$Date)
    updateSliderInput(inputId = "T2Var5", min = min(choices), max = max(choices))
  })
  
  
  #T2 DATA WRANGLING:
  #1.NON-spatial:
  dfsub <- reactive({
    dfsub <- df[[grep(input$T2Var0,commod_names)]] %>%
      dplyr::filter(Commodity == input$T2Var1 & Date >= input$T2Var5[1] & Date <= input$T2Var5[2])
    
    if(input$T2Var2 == "Region"){
      reg.prov <- ph_adm %>% 
        filter(ADM1_EN == word(input$T2Var3,1,2))%>%
        select(ADM2_EN) %>%
        unlist %>%
        append(input$T2Var3)
      dfsub %>%
        filter(Region.Province == reg.prov)
    }else if(input$T2Var2 == "Country"){
      coun.reg <- ph_adm %>%
        filter(ADM0_EN == word(input$T2Var3,1,2)) %>%
        select(ADM1_EN) %>%
        unlist %>%
        append(input$T2Var3)
      dfsub %>% 
        filter(Region.Province == coun.reg)
    }else if(input$T2Var2 == "Province"){
      dfsub %>%
        filter(Region.Province == input$T2Var3)
    }
    
  })
  # #2.Spatial (sf objects)
  geom_sub <-reactive({
    if(input$T2Var2 == "Region"){
      ph_geom %>%  filter(ADM1_EN == word(input$T2Var3,1,2))
    }else if(input$T2Var2 == "Province"){
      ph_geom %>% filter(ADM2_EN == word(input$T2Var3,1,2))
    }else{
      ph_geom
    }
    
  })

  #Centroid for zooming purposes
  centr <- reactive ({
    g <-  geom_sub()
    g %>% st_union() %>%st_centroid %>% unlist
  })

  #T2 PLOTS:
  #Time series
  output$Priceplot <- renderPlot({
    toplot<-dfsub()
    
    if(input$T2Var2 == "Province"){ # province and not subunits plot
      p <- ggplot(toplot, aes(x = Date,y = Price)) +
        geom_line() +
        geom_point() +
        scale_x_date(limits = input$DateRange,labels = date_format("%b-%Y")) +
        labs(x = "Date", y = "Suggested Retail Price (SRP) in PhP",
             title=paste(input$T2Var1,input$T2Var3, sep = " - ")) +
        theme_bw()
       
    }else{
      p <- ggplot(toplot, aes(x = Date,y = Price,group = Region.Province)) +
        geom_line() +
        geom_point() +
        scale_x_date(limits = input$DateRange,labels = date_format("%b-%Y")) +
        labs(x = "Date", y = "Suggested Retail Price (SRP) in PhP",
             title=paste(input$T2Var1,input$T2Var3, sep = " - ")) +
        theme_bw()
    }
    p
  })
  #   
  #   #Interactive map
  #   output$PriceMap <-renderLeaflet({
  #     cn <-centr()
  #     poly <- geom_sub()
  #     
  #     m <- leaflet() %>%
  #     addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
  #     setView(lng = cn[[1]], lat = cn[[2]], zoom = 5)%>%
  #     addPolygons(data = poly, weight=1)
  #     m
  #     
  # })

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)





