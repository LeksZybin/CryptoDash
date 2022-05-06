start_of_execution <- Sys.time()

# Load Data ---------------------------------------------------------------
## 1 - Crypto IDOs
source("/Users/aleksandrzybin/Documents/R_projects/crypto_tracker/crypto_IDOS/crypto_IDOs_graph_database.R")
#load("/Users/aleksandrzybin/Documents/R_projects/crypto_tracker/crypto_IDOS/idos.RData"); load("/Users/aleksandrzybin/Documents/R_projects/crypto_tracker/crypto_IDOS/platforms.RData")
## 2 - Portfolio value
source("/Users/aleksandrzybin/Documents/R_projects/crypto_tracker/crypto_IDOS/crypto_portfolio.R")
# Load Libraries ---------------------------------------------------------------
#Data editing and analysis
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(plotly)
#Shiny Dasboard
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DT)
#Turn off scientific notations
options(scipen = 999)
#library(dashboardthemes)

# Data Month Range ---------------------------------------------------------------
idos <- idos[idos$Ended %in%
               floor_date(as.Date(Sys.Date()) %m-% months(12), "month"):(Sys.Date() %m+% months(1)),]
month_range <-
  c(na.omit(idos[idos$Type == "IDO" &
                   idos$TGE_Platform != "Not Set",]$Ended), Sys.Date() %m+% months(1))
month_range <-
  unique(ceiling_date(seq(min(month_range), max(month_range), by = "day"), 'month') -
           1)
month_range <-
  paste0(as.character(lubridate::month(
    month_range, label = TRUE, abbr = TRUE
  )), "-", year(month_range))
#Sidebar ---------------------------------------------------------------
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Portfolio Performance", tabName = "Portfolio_performance", icon = icon("fas fa-briefcase")),
  menuItem("New Projects", icon = icon("baby"),
           sliderTextInput(
             inputId   = "range",
             label     = "Dates:",
             choices   = month_range,
             selected  = c(month_range[1], month_range[length(month_range)]),
             grid      = TRUE,
             width     = "100%"
           ),
           menuSubItem("IDO Market", tabName = "Market_Dash", icon = icon("fas fa-industry")),
           menuSubItem("IDO Platform", tabName = "Platform_Dash", icon = icon("fas fa-search-dollar"))
  )
))
#Tabitem 1 UI ---------------------------------------------------------------
tabitem_1 <- tabItem(tabName = "Market_Dash", 
                     fluidPage(box(width=12, verticalLayout(
                                               fluidRow(sidebarPanel(
                                                 selectInput(
                                                   inputId   = "y_axis",
                                                   label     = "Select Y:",
                                                   choices   = list("ATH_ROI_USD", "ROI_USD"),
                                                   selected  = "ATH_ROI_USD"
                                               ),
                                                 selectInput(
                                                   inputId   = "x_axis",
                                                   label     = "Select X:",
                                                   choices   = list("Category", "TGE_Platform"),
                                                   selected  = "TGE_Platform"
                                               ),
                                                 sliderTextInput(
                                                   inputId   = "rank",
                                                   label     = "Rank:",
                                                   choices   = min(platforms$Rank):max(platforms$Rank),
                                                   selected  = c(min(platforms$Rank), max(platforms$Rank))
                                               )
                                             ),
                                             mainPanel(plotlyOutput("plotgraph1"))),
                                             plotlyOutput("plotgraph3")))
                                           ))
#Tabitem 2 UI ---------------------------------------------------------------
tabitem_2 <- tabItem(tabName = "Platform_Dash", 
          fluidRow(
          box(width=12,
            verticalLayout(
              sidebarPanel(splitLayout(
                selectInput(
                  inputId   = "PLATFORM",
                  label     = "Select Platform",
                  choices   = unique(idos[idos$Type == "IDO",]$TGE_Platform),
                  selected  = "TrustPad"
                )
              )),
            splitLayout(
              cellWidths = c("50%", "50%"),
              plotlyOutput("plotgraph2"),
              DT::dataTableOutput("table1", height="10em")
            )
            )
          )
        )
  )
#Tabitem 3 UI ---------------------------------------------------------------
tabitem_3 <- tabItem(tabName = "Portfolio_performance", 
                     fixedRow(column(width = 4, valueBoxOutput("portfolio_value_Box", width = NULL),
                                     box(width = NULL, title = "Coins Share", plotlyOutput(height = "150px", "plotgraph4"))),
                      column(width = 8, tabBox(width = NULL, title = "PnL", selected = "USD",
                                               tabPanel("USD",plotlyOutput(height = "250px","PNLgraph")),
                                               tabPanel("RUB",plotlyOutput(height = "250px","PNLgraph_RUB"))
                                               ))
                     ),
                     fluidRow(
                       column(width = 4, box(title = "News", width = NULL,
                         div(style = 'overflow-x: scroll; height: 500px', DT::dataTableOutput('news'))
                       )
                       ),
                       column(width = 8, box(title = "Current Market", width = NULL, 
                         div(style = 'overflow-x: scroll; height: 500px', DT::dataTableOutput("coins_table"))))
                       )
                     )
#Body ---------------------------------------------------------------
body <- dashboardBody(
    tabItems(
      tabitem_1,
      tabitem_2,
      tabitem_3
    )
  )

#UI Setup---------------------------------------------------------------
ui <- dashboardPage(skin = "blue", dashboardHeader(title = "Sort of a Crypto Dash"),
                    sidebar,
                    body)
#Server Setup ---------------------------------------------------------------

server <- function(input, output, session) {
  platform = reactive({
    input$PLATFORM
  })
  y_axis = reactive({
    input$y_axis
  })
  x_axis = reactive({
    input$x_axis
  })
  range = reactive({
    input$range
  })
  rank = reactive({
    input$rank
  })
  observe({
    lower_date <-
      as.Date(parse_date_time(range()[1], orders = "%b-%Y"))
    upper_date <-
      as.Date(parse_date_time(range()[2], orders = "%b-%Y"))
    range <- lower_date:upper_date
    rank <- rank()[1]:rank()[2]
    
    updateSelectInput(session, "PLATFORM",
                      choices = unique(idos[idos$Ended %in% range &
                                              idos$Type == "IDO" &
                                              idos$TGE_Platform %in% platforms[platforms$Rank %in% rank, ]$Name,]$TGE_Platform))
  })
#TabItem1 Server ---------------------------------------------------------------
  output$plotgraph1 <- renderPlotly({
    lower_date <-
      as.Date(parse_date_time(range()[1], orders = "%b-%Y"))
    upper_date <-
      as.Date(parse_date_time(range()[2], orders = "%b-%Y"))
    range <- lower_date:upper_date
    rank <- rank()[1]:rank()[2]
    idos_filter <- idos
    idos_filter <-
      unique(idos[idos$Ended %in% range &
                    idos$Type == "IDO",]) %>%
      .[.$TGE_Platform %in% platforms[platforms$Rank %in% rank, ]$Name, ]
    fig <-
      plot_ly(
        idos_filter,
        y = ~ get(y_axis()),
        color = ~ get(x_axis()),
        type = "box"
      ) %>% layout(yaxis = list(title = gsub("_", " ", y_axis()))) %>%
      hide_legend()
    fig
  })
  output$plotgraph3 <- renderPlotly({
    lower_date <-
      as.Date(parse_date_time(range()[1], orders = "%b-%Y"))
    upper_date <-
      as.Date(parse_date_time(range()[2], orders = "%b-%Y"))
    range <- lower_date:upper_date
    rank <- rank()[1]:rank()[2]
    idos_filter <- idos
    idos_filter <-
      unique(idos[idos$Ended %in% range &
                    idos$Type == "IDO",])
    idos_filter$Ended <- floor_date(idos_filter$Ended, "month")
    idos_filter <- idos_filter %>% .[.$TGE_Platform %in% platforms[platforms$Rank %in% rank, ]$Name, ]
    idos_filter <- idos_filter[,c("ATH_ROI_USD","ROI_USD","Ended")] %>% 
      gather(Measure,Value,-c(Ended))
    fig <-
      plot_ly(
        idos_filter,
        y = ~ Value,
        x = ~ Ended,
        color = ~ Measure,
        type = "box"
      ) %>%
      layout(
        boxmode = "group",
        title = "Monthly ROI",
        tickformat = "%b-%Y"
      )
    fig
    
  })
#TabItem2 Server ---------------------------------------------------------------
  output$plotgraph2 <- renderPlotly({
    lower_date <-
      as.Date(parse_date_time(range()[1], orders = "%b-%Y"))
    upper_date <-
      as.Date(parse_date_time(range()[2], orders = "%b-%Y"))
    range <- lower_date:upper_date
    rank <- rank()[1]:rank()[2]
    idos_filter <- idos
    idos_filter <-
      unique(idos[idos$Ended %in% range &
                    idos$Type == "IDO",])
    idos_filter <-
      idos_filter[idos_filter$TGE_Platform == platform(),] %>%
      .[.$TGE_Platform %in% platforms[platforms$Rank %in% rank, ]$Name, ]
    idos_filter <- idos_filter[,c("TGE_Platform","ATH_ROI_USD","ROI_USD","Category")] %>% 
      gather(Measure,Value,-c(TGE_Platform,Category))
    idos_filter 
    fig <-
      plot_ly(
        idos_filter,
        y = ~ Value,
        x = ~ Category,
        color = ~ Measure,
        type = "box"
      ) %>%
      layout(
        boxmode = "group",
        title = platform()
      )
    fig
  })
  output$table1 <- DT::renderDataTable({
    lower_date <-
      as.Date(parse_date_time(range()[1], orders = "%b-%Y"))
    upper_date <-
      as.Date(parse_date_time(range()[2], orders = "%b-%Y"))
    range <- lower_date:upper_date
    rank <- rank()[1]:rank()[2]
    idos_filter <- idos
    idos_filter <-
      unique(idos[idos$Ended %in% range & idos$Type == "IDO",])
    idos_filter <-
      idos_filter[idos_filter$TGE_Platform == platform(),] %>%
      .[.$TGE_Platform %in% platforms[platforms$Rank %in% rank, ]$Name, ]
    idos_filter <-
      idos[idos$Ended %in% range &
             idos$Type == "IDO" & idos$TGE_Platform == platform(),
           c("Name", "Category", "ATH_ROI_USD","ROI_USD", "Ended")]
    idos_filter$Name <- paste0("<a href='",  "https://cryptorank.io/ico/", gsub(" ","-",tolower(trimws(idos_filter$Name, whitespace = "[A-Z/]+", which = "right"))), "'>",trimws(idos_filter$Name, whitespace = "[A-Z/]+", which = "right"),"</a>")
    DT::datatable(idos_filter,
                  escape = FALSE,
    options = list(pageLength = 10)
    )
  })
#TabItem3 Server ---------------------------------------------------------------
  output$portfolio_value_Box <- renderValueBox({
    valueBox(
      paste0("$", doll_value, " | ₽", rub_value,"K"), portfolio_change_perc, icon = icon("fas fa-funnel-dollar"),
      color = "light-blue",
      width = 12
    )  
    })
  output$PNLgraph <- renderPlotly({
    PnL_plot
  })
  output$PNLgraph_RUB <- renderPlotly({
    PnL_RUB_plot
  })
  output$plotgraph4 <- renderPlotly({
    coins_share
  })
  output$news <- DT::renderDataTable({
    DT::datatable(df_news, 
                  escape = FALSE,
                  options = list(dom = 't'),
                  rownames = FALSE)
  })
  output$coins_table <- DT::renderDataTable({coins_output_table})
}

end_of_execution <- Sys.time()
time_of_execution <- end_of_execution - start_of_execution; rm(start_of_execution, end_of_execution)
print(time_of_execution)

#Run The Application ---------------------------------------------------------------
shinyApp(ui = ui, server = server)




