# # load packages
# # library(shiny)
# # library(shinydashboard)
# # library(dplyr)
# # library(ggplot2)
# # library(tidygraph)
#library(manynet) # needs to be loaded
#library(Rgraphviz)  # needs to be loaded
# get memberships dataset
memberships <- readRDS("memberships.Rds")
# memberships <- manyenviron::memberships$IEADB_MEM |>
#   dplyr::select("stateID", "manyID", "Title") |>
#   dplyr::mutate(type = manypkgs::code_type(Title),
#                 agr_type = dplyr::case_when(stringr::str_detect(type, "A") ~ "Agreement",
#                                             stringr::str_detect(type, "P") ~ "Protocol",
#                                             stringr::str_detect(type, "E") ~ "Amendment",
#                                             stringr::str_detect(type, "N") ~ "Notes",
#                                             stringr::str_detect(type, "S") ~ "Strategy",
#                                             stringr::str_detect(type, "R") ~"Resolution",
#                                             .default = NA),
#                 category = ifelse(stringr::str_detect(manyID, "-"),
#                                   "Bilateral", "Multilateral"),
#                 known_agr = stringr::str_extract(manyID, "UNCLOS|CBD|CCAMLR|CITES|CLC|CRAMRA|
#                                                  |CECE|LRTAP|MARPOL|NAAEC|OLDEPESCA|OPRC|OSPAR|
#                                                  |PARIS|PIC|RAMSA|UNCCD|UNFCCC|VIENNA"))
# get titles for clicking
titles <- readRDS("titles.Rds")
# titles <- memberships |>
#   dplyr::distinct(manyID, .keep_all = TRUE) |>
#   dplyr::select(manyID, Title) |>
#   dplyr::distinct()
# Define UI 
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Type of Environmental Treaties",
                                  titleWidth = "400"),
  shinydashboard::dashboardSidebar(width = 350, shinydashboard::sidebarMenu(
          selectInput("country", "Select party(s):",
                      choices = c("choose" = "", stringr::str_sort(unique(memberships$stateID))),
                      selected = "choose", multiple = T),
          shiny::checkboxGroupInput("agr_type", "Select treaty type:",
                                    choices = c("Agreement", "Protocol", "Amendment",
                                                "Notes", "Strategy", "Resolution"),
                                    selected = "Agreement"),
          selectInput("category", "Select category:",
                      choices = c("choose" = "", "Bilateral", "Multilateral"),
                      selected = "choose", multiple = T),
          selectInput("known", "Select known agreement:",
                      choices = c("choose" = "", "UNCLOS", "CBD", "CCAMLR", "CITES",
                                  "CLC", "CRAMRA", "CECE", "LRTAP", "MARPOL", "NAAEC",
                                  "OLDEPESCA", "OPRC", "OSPAR", "PARIS", "PIC", "RAMSA",
                                  "UNCCD", "UNFCCC", "VIENNA"),
                      selected = "choose", multiple = T),
          shinydashboard::menuItem(shiny::sliderInput("range", "Dates",
                                                      value = c(1951, 1952),
                                                      min = 1950, max = 2020,
                                                      width = 350, sep = "")),
          checkboxInput("treatylabel", "Display treaty node labels", TRUE),
          checkboxInput("countrylabel", "Display country node labels", TRUE)),
          wellPanel(style = "background: #222D32; border-color: #222D32",
                    textOutput("click_info"), tags$head(tags$style(".shiny-output-error{visibility: hidden}")),
                    tags$head(tags$style(".shiny-output-error:after{content: 'Treaty title not found, please try again with another node.';
visibility: visible}")))),
  shinydashboard::dashboardBody(shiny::plotOutput("distPlot", height = "650px",
                                                  click = "plot_click"))
)
# Define server
server <- function(input, output) {
    filteredData <- shiny::reactive({ # filter data
      memberships |> 
        dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) |>
        dplyr::filter(if (!is.null(input$country)) stateID %in% input$country  else TRUE,
                      if (!is.null(input$category)) category %in% input$category  else TRUE,
                      if (!is.null(input$known)) known_agr %in% input$known  else TRUE,
                      year >= input$range[1] & year <= input$range[2],
                      agr_type %in% input$agr_type) |>
        manynet::as_tidygraph() |>
        dplyr::mutate(type = ifelse(grepl("[0-9]", name), TRUE, FALSE),
                      name = dplyr::case_when(input$treatylabel == FALSE & type == TRUE ~ "",
                                              input$countrylabel == FALSE & type == FALSE ~ "",
                                              .default = name)) |>
        manynet::autographr(layout = "hierarchy", center = "events",
                            node_color = "type", node_size = 3) +
        manynet::scale_color_iheid(guide = "none")
    })
    output$distPlot <- renderPlot({filteredData()})
    output$click_info <- renderText({
      point <- nearPoints(ggplot2::ggplot_build(filteredData())$data[[1]],
                          input$plot_click, addDist = TRUE)
      titlet <- as.character(titles[titles$manyID %in% point$label, 2])
      if (titlet == "character(0)" | is.na(titlet)) {
        print("Please click on a node representing a treaty to display its title.")
      } else if (!(titlet == "character(0)")) {
        print(titlet)
      }
    })
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
