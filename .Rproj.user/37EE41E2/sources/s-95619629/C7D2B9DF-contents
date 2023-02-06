library(shiny)
library(ggplot2)
library(scales)
library(DT)
library(dplyr)
library(leaflet)
##--------------------DATA -----------------
## Read retail price data script:
source("./retail_prices_data.R")


# Create UI
ui <- fluidPage(
  # Application title
  titlePanel("Retail Prices of Commodities in the Philippines"),
  p("Data source: "),
  tags$ul(
    tags$li("Philippine Statistics Authority (PSA)",
            tags$a("OpenSTAT Database", href="https://openstat.psa.gov.ph/")
    )
  ),
  tags$br(),
  tags$hr(),
  #side bar options:
  sidebarLayout(
    sidebarPanel(width = 2,
      selectInput("Var0", "Select Commodity Group", choices = unique(df$Group)), 
      selectInput("Var1", "Select Commodity",""),
      selectInput("Var2", "Select Region", choices = df %>% distinct(Region.Province)),
      
      sliderInput("Var3",
                  "Select Date Range:",
                  min = min(df$Date),
                  max = max(df$Date),
                  value=c(min(df$Date),
                          max(df$Date)),
                  timeFormat="%d-%m-%Y")
      ),
    
    mainPanel(plotOutput("Priceplot"),leafletOutput("PriceMap"))
  )
)



# Define server logic 
server <- function(input, output, session) {
  
    selectedVar0 <- reactive({
      filter(df, Group == input$Var0)
    })
    #update selection Var1 
    observeEvent(selectedVar0(), {
      choices <- unique(selectedVar0()$Commodity)
      updateSelectInput(inputId = "Var1", choices = choices) 
    })
    
    selectedVar1 <- reactive({
      req(input$Var1)
      filter(selectedVar0(), Commodity == input$Var1)
    })
    #Update selection Var2
    observeEvent(selectedVar1(), {
      choices <- unique(selectedVat1()$Admin_level)
      updateSelectInput(inputId = "Var2", choices = choices)
    })

  #Data subset based on inputs:
  dfsplot<-reactive({
    df_sub %>%
    dplyr::filter(Commodity == input$Commodity,
           Region.Province == input$Admin_area) %>%
    select(Date,Price)
  })
  
  #Output plots
  #TIme series
  output$Priceplot <- renderPlot({
    dataset <- dfsplot()
    
    ggplot(dataset, aes(x = Date,
               y = Price)) +
      geom_line() +
      scale_x_date(limits = input$DateRange,labels = date_format("%b-%Y")) +
      labs(x = "Date", y = "Suggested Retail Price (SRP) in PhP")+
      theme_bw()
    })
  
  #Interactive map:
  df_yr<-df %>%
    filter(between(Date, as.Date("2012-11-15"),as.Date("2012-12-15"))) %>%
    filter(ADM1_EN == "Region V") %>%
    filter(Commodity == unique(df$Commodity)[1])
  
  
  
  minPr<-min(df_yr$Price,na.rm = TRUE)
  maxPr<- max(df_yr$Price,na.rm = TRUE)
  
  paletteLayers <- colorBin(palette = "RdBu", domain = c(minPr, maxPr), bins = 7 , pretty=FALSE)
  
  
  df_geom<-ph_geom %>%
    filter(ADM1_EN == "Region V") %>%
    left_join(df_yr)
  
  centr<- df_geom %>%
    st_union() %>%
    st_centroid %>%
    unlist
  output$PriceMap <-renderLeaflet({
    
    m<- leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = centr[[1]], lat = centr[[2]], zoom = 7.5) %>%
      addPolygons(data = df_geom,
                  color = 'white',
                  weight = 1,
                  smoothFactor = .3,
                  fillOpacity = .75,
                  fillColor = ~paletteLayers(df_geom$Price),
                  label = ~df_yr$Price,
                  labelOptions = labelOptions(
                    style = list(color = 'gray30'),
                    textsize = '10px'),
                  highlightOptions = highlightOptions(
                    weight = 3,
                    color = 'dodgerblue'
                  )
      )
  })
 
  
}

# Run the application 
shinyApp(ui = ui, server = server)





