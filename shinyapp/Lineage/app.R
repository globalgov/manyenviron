# First step: load packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(manynet)

references <- manyenviron::references$ECOLEX_REF
# references <- references[, -1]

references <- references |> 
    dplyr::mutate(year = ifelse(RefType == "Amended by" |
                                  RefType == "Cited by" |
                                  RefType == "Superseded by" |
                                  RefType == "Enabled by",
                                stringr::str_extract(treatyID2, "[:digit:]{4}"),
                                stringr::str_extract(treatyID1, "[:digit:]{4}"))) |> 
    dplyr::mutate(known_agr1 = stringr::str_extract(treatyID1, "UNCLOS|CBD|CCAMLR|CITES|CLC|CRAMRA|CECE|LRTAP|MARPOL|NAAEC|OLDEPESCA|OPRC|OSPAR|PARIS|PIC|RAMSA|UNCCD|UNFCCC|VIENNA")) |> 
    dplyr::mutate(known_agr2 = stringr::str_extract(treatyID2, "UNCLOS|CBD|CCAMLR|CITES|CLC|CRAMRA|CECE|LRTAP|MARPOL|NAAEC|OLDEPESCA|OPRC|OSPAR|PARIS|PIC|RAMSA|UNCCD|UNFCCC|VIENNA")) 


references$known_agr <- dplyr::coalesce(references$known_agr1, references$known_agr2)

references$year <- as.numeric(references$year)

references$year_range <- ifelse(references$year <= 1969, "Before 1970",
                                (ifelse((references$year <= 1980) & (references$year >= 1970), "1970-1980",
                                        (ifelse((references$year <= 1990) & (references$year >= 1981), "1981-1990",
                                                (ifelse((references$year <= 2000) & (references$year >= 1991), "1991-2000",
                                                        (ifelse((references$year <= 2010) & (references$year >= 2001), "2001-2010",
                                                                (ifelse((references$year <= 2020) & (references$year >= 2011), "2011-2020", NA)))))))))))

# code agreement activity
activity <- paste(c("agriculture", "alliance", "biodiversity", "climate change",
                    "delimitation", "economic integration", "energy", "finance", "fishing", "forestry",
                    "health", "human rights", "investment", "management", "military", "research",
                    "security", "space", "trade", "waste"), collapse="|")
agr_action <- manyenviron::agreements$ECOLEX |>
  dplyr::mutate(action = stringr::str_extract(Lineage, activity)) |>
  dplyr::select(treatyID, action)
references <- dplyr::left_join(references, agr_action,
                               by = join_by("treatyID1" == "treatyID")) |>
  dplyr::rename("action1" = "action") |>
  dplyr::mutate(action1 = stringr::str_replace_na(action1, "other"))
references <- dplyr::left_join(references, agr_action,
                               by = join_by("treatyID2" == "treatyID")) |>
  dplyr::rename("action2" = "action") |>
  dplyr::mutate(action2 = stringr::str_replace_na(action2, "other"))
references <- references %>%
  dplyr::mutate(action = ifelse(RefType == "Amended by" |
                                  RefType == "Cited by" |
                                  RefType == "Superseded by" |
                                  RefType == "Enabled by", action2, action1))

references <- references |>
  dplyr::select(treatyID1, treatyID2, RefType, known_agr, action, year, year_range) |>
  dplyr::distinct()

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
        references <- references |>
          dplyr::filter(year_range %in% input$year_choices) |>
          dplyr::filter(RefType %in% input$ref_choices) |>
          manynet::as_tidygraph() |>
          manynet::mutate(nyear = as.numeric(stringr::str_extract(name, "[:digit:]{4}")))
    })
    
    filteredData2 <- shiny::reactive({
        references <- references |>
          dplyr::filter(year_range %in% input$year_choices) |>
          dplyr::filter(RefType %in% input$ref_choices) |> 
          dplyr::filter(known_agr %in% input$known) |>
          manynet::as_tidygraph() |>
          manynet::mutate(nyear = as.numeric(stringr::str_extract(name, "[:digit:]{4}")))
    })
    
    filteredData3 <- shiny::reactive({
        references <- references |>
          dplyr::filter(year_range %in% input$year_choices) |>
          dplyr::filter(RefType %in% input$ref_choices) |> 
          dplyr::filter(action %in% input$actions) |>
          manynet::as_tidygraph() |>
          manynet::mutate(nyear = as.numeric(stringr::str_extract(name, "[:digit:]{4}")))
    })
    filteredData4 <- shiny::reactive({
        references <- references |>
          dplyr::filter(year_range %in% input$year_choices) |>
          dplyr::filter(RefType %in% input$ref_choices) |> 
          dplyr::filter(action %in% input$actions) |>
          dplyr::filter(known_agr %in% input$known) |>
          manynet::as_tidygraph() |>
          manynet::mutate(nyear = as.numeric(stringr::str_extract(name, "[:digit:]{4}")))
    })
    
    
    output$distPlot <- shiny::renderPlot({
        if(is.null(input$known) & is.null(input$actions)){
            manynet::autographr(filteredData(), layout = "lineage", rank = "nyear")
        }
        
        else if(!is.null(input$known) & is.null(input$actions)){
            manynet::autographr(filteredData2(), layout = "lineage", rank = "nyear")
        }
        
        else if(is.null(input$known) & !is.null(input$actions)){
            manynet::autographr(filteredData3(), layout = "lineage", rank = "nyear")
        }
        
        else {
            manynet::autographr(filteredData4(), layout = "lineage", rank = "nyear")
        }
    })
}

shiny::shinyApp(ui, server)
