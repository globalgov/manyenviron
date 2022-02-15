#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(migraph)

# Set working directory in this folder first using setwd() function
references <- read_csv2("references.csv")
references$action <- stringr::str_replace_na(references$action, "other")

# Prepare dashboard interface
ui <- dashboardPage(
    dashboardHeader(title = "Lineage Chain of Environmental Treaties", titleWidth = "400"),
    dashboardSidebar(
        sidebarMenu(
            checkboxGroupInput("ref_choices", 
                               "Select relation type:",
                               choices = c("Amends" = "Amends",
                                           "Cites" = "Cites",
                                           "Enables" = "Enables",
                                           "Supersedes" = "Supersedes"),
                               selected = c("Amends", "Cites", "Enables", "Supersedes")),
            selectInput("known",
                        "Select known agreement:",
                        choices = c("choose" = "","UNCLOS", "CBD", "CITES", "CLC", "LRTAP",
                                    "MARPOL", "OSPAR", "PARIS", "PIC", "RAMSA",
                                    "UNFCCC", "VIENNA"),
                        selected = "MARPOL",
                        multiple = T),
            selectInput("actions",
                        "OR select activity:",
                        choices = c("choose" = "" ,"agriculture", "alliance", "biodiversity", "climate change", "delimitation",
                                    "economic integration", "energy", "fishing", "forestry", "health","management", "research",
                                    "trade", "waste", "other"),
                        selected = "choose",
                        multiple = T)
            
        )),
    dashboardBody(
        plotOutput("distPlot", height = "550px")
    )
)

# Connect with the data
server <- function(input, output){
    filteredData <- reactive({
        references <- references %>%
            dplyr::filter(RefType %in% input$ref_choices)
    })
    
    filteredData2 <- reactive({
        references <- references %>%
            dplyr::filter(RefType %in% input$ref_choices) %>% 
            dplyr::filter(familyLineage %in% input$known)
    })
    
    filteredData3 <- reactive({
        references <- references %>%
            dplyr::filter(RefType %in% input$ref_choices) %>% 
            dplyr::filter(action %in% input$actions)
    })
    filteredData4 <- reactive({
        references <- references %>%
            dplyr::filter(RefType %in% input$ref_choices) %>% 
            dplyr::filter(action %in% input$actions) %>% 
            dplyr::filter(familyLineage %in% input$known)
    })
    
    
    output$distPlot <- renderPlot({
        if(is.null(input$known) & is.null(input$actions)){
            migraph::gglineage(filteredData())
        }
        
        else if(!is.null(input$known) & is.null(input$actions)){
            migraph::gglineage(filteredData2())
        }
        
        else if(is.null(input$known) & !is.null(input$actions)){
            migraph::gglineage(filteredData3())
        }
        
        else {
            migraph::gglineage(filteredData4())
        }
    })
}

shinyApp(ui, server)