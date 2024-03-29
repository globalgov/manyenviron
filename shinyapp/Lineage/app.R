# First step: load packages
# library(shiny)
# library(shinydashboard)
# library(dplyr)

references <- manyenviron::references
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
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Environmental Treaties Lineage", titleWidth = "400"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
            shiny::checkboxGroupInput("ref_choices", 
                               "Select relation type:",
                               choices = c("Amends" = "Amends",
                                           "Cites" = "Cites",
                                           "Enables" = "Enables",
                                           "Supersedes" = "Supersedes"),
                               selected = "Cites"),
            shiny::selectInput("actions", "Select activity:",
                        choices = c("choose" = "" ,"agriculture", "alliance", "biodiversity", "climate change", "delimitation",
                                    "economic integration", "energy", "finance", "fishing", "forestry", "health",
                                    "human rights", "investement", "management", "military", "research", "security",
                                    "space", "trade", "waste"),
                        selected = "choose",
                        multiple = T),
            shiny::selectInput("known", "Select known agreement:",
                        choices = c("choose" = "","UNCLOS", "CBD", "CCAMLR", "CITES", "CLC", "CRAMRA", "CECE", "LRTAP",
                                    "MARPOL", "NAAEC", "OLDEPESCA", "OPRC", "OSPAR", "PARIS", "PIC", "RAMSA",
                                    "UNCCD", "UNFCCC", "VIENNA"),
                        selected = "choose",
                        multiple = T),
            shiny::checkboxGroupInput("year_choices", 
                               "Select period:",
                               choices = c("Before 1970","1970-1980","1981-1990","1991-2000","2001-2010", "2011-2020"),
                               selected = "1970-1980") 
        )),
    shinydashboard::dashboardBody(
        shiny::plotOutput("distPlot", height = "550px")
    )
)

# Step three: connect with the data
server <- function(input, output){
    filteredData <- shiny::reactive({
        references <- references %>%
            dplyr::filter(year_range %in% input$year_choices) %>%
            dplyr::filter(RefType %in% input$ref_choices)
    })
    
    filteredData2 <- shiny::reactive({
        references <- references %>%
            dplyr::filter(year_range %in% input$year_choices) %>%
            dplyr::filter(RefType %in% input$ref_choices) %>% 
            dplyr::filter(known_agr %in% input$known)
    })
    
    filteredData3 <- shiny::reactive({
        references <- references %>%
            dplyr::filter(year_range %in% input$year_choices) %>%
            dplyr::filter(RefType %in% input$ref_choices) %>% 
            dplyr::filter(action %in% input$actions)
    })
    filteredData4 <- shiny::reactive({
        references <- references %>%
            dplyr::filter(year_range %in% input$year_choices) %>%
            dplyr::filter(RefType %in% input$ref_choices) %>% 
            dplyr::filter(action %in% input$actions) %>% 
            dplyr::filter(known_agr %in% input$known)
    })
    
    
    output$distPlot <- shiny::renderPlot({
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

shiny::shinyApp(ui, server)
