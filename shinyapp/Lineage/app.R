# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/

# First step: load packages
library(shiny)
library(shinydashboard)
library(dplyr)

# Set working directory in this folder first using setwd() function
# setwd()
references <- readr::read_csv("references.csv")
references <- references[, -1]

references <- references %>% 
    dplyr::mutate(year = stringr::str_extract(Treaty2, "[:digit:]{4}")) %>% 
    dplyr::mutate(known_agr1 = stringr::str_extract(Treaty1, "UNCLOS|CBD|CCAMLR|CITES|CLC|CRAMRA|CECE|LRTAP|MARPOL|NAAEC|OLDEPESCA|OPRC|OSPAR|PARIS|PIC|RAMSA|UNCCD|UNFCCC|VIENNA")) %>% 
    dplyr::mutate(known_agr2 = stringr::str_extract(Treaty2, "UNCLOS|CBD|CCAMLR|CITES|CLC|CRAMRA|CECE|LRTAP|MARPOL|NAAEC|OLDEPESCA|OPRC|OSPAR|PARIS|PIC|RAMSA|UNCCD|UNFCCC|VIENNA")) 


references$known_agr <- dplyr::coalesce(references$known_agr1, references$known_agr2)

references$action <- dplyr::coalesce(references$Action1, references$Action2)

references$year <- as.numeric(references$year)

references$year_range <- ifelse(references$year <= 1969, "Before 1970",
                                (ifelse((references$year <= 1980) & (references$year >= 1970), "1970-1980",
                                        (ifelse((references$year <= 1990) & (references$year >= 1981), "1981-1990",
                                                (ifelse((references$year <= 2000) & (references$year >= 1991), "1991-2000",
                                                        (ifelse((references$year <= 2010) & (references$year >= 2001), "2001-2010",
                                                                (ifelse((references$year <= 2020) & (references$year >= 2011), "2011-2020", NA)))))))))))


references <- references %>% 
    dplyr::select(Treaty1, Treaty2, RefType, TitleTreaty_1, TitleTreaty_2, known_agr, action, year, year_range)

# Step two: prepare dashboard interface
ui <- dashboardPage(
    dashboardHeader(title = "Environmental Treaties Lineage", titleWidth = "400"),
    dashboardSidebar(
        sidebarMenu(
            checkboxGroupInput("ref_choices", 
                               "Select relation type:",
                               choices = c("Amends" = "Amends",
                                           "Cites" = "Cites",
                                           "Enables" = "Enables",
                                           "Supersedes" = "Supersedes"),
                               selected = "Cites"),
            selectInput("actions", "Select activity:",
                        choices = c("choose" = "" ,"agriculture", "alliance", "biodiversity", "climate change", "delimitation",
                                    "economic integration", "energy", "finance", "fishing", "forestry", "health",
                                    "human rights", "investement", "management", "military", "research", "security",
                                    "space", "trade", "waste"),
                        selected = "choose",
                        multiple = T),
            selectInput("known", "Select known agreement:",
                        choices = c("choose" = "","UNCLOS", "CBD", "CCAMLR", "CITES", "CLC", "CRAMRA", "CECE", "LRTAP",
                                    "MARPOL", "NAAEC", "OLDEPESCA", "OPRC", "OSPAR", "PARIS", "PIC", "RAMSA",
                                    "UNCCD", "UNFCCC", "VIENNA"),
                        selected = "choose",
                        multiple = T),
            checkboxGroupInput("year_choices", 
                               "Select period:",
                               choices = c("Before 1970","1970-1980","1981-1990","1991-2000","2001-2010", "2011-2020"),
                               selected = "1970-1980") 
        )),
    dashboardBody(
        plotOutput("distPlot", height = "550px")
    )
)

# Step three: connect with the data
server <- function(input, output){
    filteredData <- reactive({
        references <- references %>%
            dplyr::filter(year_range %in% input$year_choices) %>%
            dplyr::filter(RefType %in% input$ref_choices)
    })
    
    filteredData2 <- reactive({
        references <- references %>%
            dplyr::filter(year_range %in% input$year_choices) %>%
            dplyr::filter(RefType %in% input$ref_choices) %>% 
            dplyr::filter(known_agr %in% input$known)
    })
    
    filteredData3 <- reactive({
        references <- references %>%
            dplyr::filter(year_range %in% input$year_choices) %>%
            dplyr::filter(RefType %in% input$ref_choices) %>% 
            dplyr::filter(action %in% input$actions)
    })
    filteredData4 <- reactive({
        references <- references %>%
            dplyr::filter(year_range %in% input$year_choices) %>%
            dplyr::filter(RefType %in% input$ref_choices) %>% 
            dplyr::filter(action %in% input$actions) %>% 
            dplyr::filter(known_agr %in% input$known)
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
