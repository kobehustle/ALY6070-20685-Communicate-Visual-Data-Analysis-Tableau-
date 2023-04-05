install.packages("shinydashboard")
install.packages("tigris")
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(shinydashboard)
library(tigris)
library(readr)
library(knitr)
library(readr)
library(tibble)
library(stringr)
library(gridExtra)
library(scales)
library(lubridate)
library(ggrepel)
library(rgdal)
library(psych)

setwd("C:\\Users\\14083\\Desktop\\0. Winter course in 2023 Northeastern\\ALY 6070\\Module 4\\assignment\\data")
execut <- as_tibble(data.table::fread("U.S. Executions_cleaned.csv"),header=TRUE,stringsAsFactors = FALSE, na.strings=c("NA", ""))
states <- readRDS("states.rds")

str(execut)
execut <- na.omit(execut)
execut %>% rename(state=State)

#Data preparation
statesPop <- read_csv(str_c("NST-EST2022-POPCHG2020_2022.csv"))
str(statesPop)
statesPop

statesPop
statesPop <- statesPop %>% select(NAME, POPESTIMATE2021)
statesPop <- statesPop %>% filter(!NAME %in% c("United States", "Puerto Rico Commonwealth"))
statesPop <- statesPop %>% rename(state= NAME)
statesPop$state <- as.factor(statesPop$state)
statesPop
colnames(statesPop)[2] <- "population"


execut_new <- execut %>% rename(state = State)
ExecuteByState <- execut_new %>% group_by(state) %>% summarize(stateExecution=n())
ExecuteByState <-left_join(ExecuteByState, statesPop, by="state")
ExecuteByState$Per100000000 <- round((ExecuteByState$stateExecution/ExecuteByState$population)*100000000)
ExecuteByState

execute <-  execut_new
execute <- left_join(execute, statesPop, by="state")
str(execute)

# Data Preperation for pie chart
ExecuteRatio <- as.data.frame(table(execute$Race))
ExecuteRatio <-  ExecuteRatio %>% rename(Race = Var1)
ExecuteRatio <-  ExecuteRatio %>% rename(Num = Freq)
ExecuteRatio

rownames(ExecuteRatio) <- ExecuteRatio$Race

ExecuteRatio

header <- dashboardHeader(title = "US Execution 1977-2022", titleWidth=1000)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(h4("Dashboard"), tabName = "dashboard"),
    menuItem(h4("Data "), href = "https://www.kaggle.com/datasets/johnny1994/execution-data-in-us"), newtab = FALSE))

body <- dashboardBody(
  tabItems(
    tabItem(tabName= "dashboard",
            fluidRow(
              tabBox(id = "heejae", width=20, height=700,
                            tabPanel("Map",
                              br(),
                              fluidRow(h4("Relative Execution numbers by State", align='center')),
                              br(),
                              fluidRow(leafletOutput("usmap", height=700))),
                     tabPanel("Bar chart",
                              br(),
                              h4("Most Executions by State", align="center"),
                              sliderInput(inputId = "num",
                                          label = "Choose Top N states with most executions",
                                          value = 10,
                                          min = 1,
                                          max = (n_distinct(execute$state)-1)),
                              fluidRow(plotlyOutput("plot1")),
                              fluidRow(plotlyOutput("plot2"))),
                     tabPanel("Table",
                              br(),
                              fluidRow(h4("The default sort of this table is by descending number of executions", align='center')),
                              br(),
                              fluidRow(DT::dataTableOutput(outputId="table"))),
                     tabPanel("Pie Chart",
                              br(),
                              fluidRow(h4("Execution Ratio by Race", align='center')),
                              br(),
                              fluidRow(plotlyOutput("plot3"))))
              
            )    )  ))

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  execution_select <- reactive({execute %>% group_by(state, population) %>% summarise(Executions = n()) %>% ungroup() %>% mutate(Per0.1B = round((Executions/population)*100000000))})

  output$plot1<-renderPlotly({
    ggplotly(execution_select() %>% top_n(input$num, wt=Executions) %>%
               ggplot(aes(x=reorder(state, Executions), y=Executions, fill=Executions, text=state)) +
               geom_bar(stat='identity') + coord_flip() +
               labs(x='', y='Number of Executions', title="Number of Executions by State") + 
               scale_fill_gradient(low="yellow", high="red") +
               theme(legend.position="none"),
             tooltip=c("text", "y"))
  })
  
  output$plot2<-renderPlotly({
    ggplotly(execution_select() %>% top_n(input$num, wt=Per0.1B) %>%
               ggplot(aes(x=reorder(state, Per0.1B), y=Per0.1B, fill= Per0.1B, text=state)) +
               geom_bar(stat='identity') + coord_flip() +
               labs(x='', y='Executions Per0.1B', title="Relative number of Executions") +
               scale_fill_gradient(low="green", high="red") +
               theme(legend.position="none"),
             tooltip=c("text", "y"))
  })
  
  output$plot3<-renderPlotly({
    ggplotly(plot_ly(ExecuteRatio, labels = ~Race, values = ~Num, type = 'pie'))
  })
  
  output$table <- DT::renderDataTable(DT::datatable({execution_select() %>% arrange(desc(Per0.1B))}))
  
  states1 <- reactive({tigris::geo_join(states, execution_select(), "NAME", "state", how="inner")})
  
  state_popup <- reactive({paste0("<strong>State: </strong>",
                                  states1()$NAME,
                                  "<br><strong>Executions Per0.1B inhabitants </strong>",
                                  states1()$Per0.1B) %>%
      lapply(htmltools::HTML)})
  
  pal <- reactive({colorNumeric("Reds", domain=states1()$Per0.1B)})
  
  output$usmap <- renderLeaflet({states1() %>%
      leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-98.483330, 38.712046, zoom = 4) %>%
      addPolygons(data = states1(),
                  fillColor = ~pal()(states1()$Per0.1B),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = state_popup(),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal(),
                values = states1()$Per0.1B,
                position = "topright",
                title = "Per0.1B")})
  
}
#}
shinyApp(ui = ui, server = server)