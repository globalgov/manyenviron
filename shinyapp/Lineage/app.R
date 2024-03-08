# load packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(manynet)
# get main data
references <- manyenviron::references$ECOLEX_REF |>
  dplyr::mutate(year = as.numeric(ifelse(RefType == "Amended by" | RefType == "Cited by" |
                                           RefType == "Superseded by" | RefType == "Enabled by",
                                         stringr::str_extract(treatyID2, "[:digit:]{4}"),
                                         stringr::str_extract(treatyID1, "[:digit:]{4}"))),
                known_agr1 = stringr::str_extract(treatyID1, "UNCLOS|CBD|CCAMLR|CITES|CLC|CRAMRA|CECE|LRTAP|MARPOL|NAAEC|OLDEPESCA|OPRC|OSPAR|PARIS|PIC|RAMSA|UNCCD|UNFCCC|VIENNA"),
                known_agr2 = stringr::str_extract(treatyID2, "UNCLOS|CBD|CCAMLR|CITES|CLC|CRAMRA|CECE|LRTAP|MARPOL|NAAEC|OLDEPESCA|OPRC|OSPAR|PARIS|PIC|RAMSA|UNCCD|UNFCCC|VIENNA"),
                known_agr = dplyr::coalesce(known_agr1, known_agr2),
                year_range = case_when(year <= 1969 ~ "Before 1970",
                                       year <= 1980 & year >= 1970 ~ "1970-1980",
                                       year <= 1990 & year >= 1981 ~ "1981-1990",
                                       year <= 2000 & year >= 1991 ~ "1991-2000",
                                       year <= 2010 & year >= 2001 ~ "2001-2010",
                                       year <= 2020 & year >= 2011 ~ "2011-2020",
                                       .default = NA))
# code agreement activity
activity <- paste(c("agriculture", "alliance", "biodiversity", "climate change",
                    "delimitation", "economic integration", "energy", "finance", "fishing", "forestry",
                    "health", "human rights", "investment", "management", "military", "research",
                    "security", "space", "trade", "waste"), collapse="|")
agr_action <- manyenviron::agreements$ECOLEX |>
  dplyr::mutate(action = stringr::str_extract(Lineage, activity)) |>
  dplyr::select(manyID, action)
references <- dplyr::left_join(references, agr_action,
                               by = join_by("treatyID1" == "manyID")) |>
  dplyr::rename("action1" = "action") |>
  dplyr::mutate(action1 = stringr::str_replace_na(action1, "other")) |>
  dplyr::distinct() |>
  dplyr::left_join(agr_action, by = join_by("treatyID2" == "manyID")) |>
  dplyr::rename("action2" = "action") |>
  dplyr::mutate(action2 = stringr::str_replace_na(action2, "other"),
                action = ifelse(RefType == "Amended by" |
                                  RefType == "Cited by" |
                                  RefType == "Superseded by" |
                                  RefType == "Enabled by", action2, action1)) |>
  dplyr::select(treatyID1, treatyID2, RefType, known_agr, action, year, year_range) |>
  dplyr::distinct()
# get titles for hoovering
titles <- manyenviron::agreements$ECOLEX |> 
  dplyr::select(manyID, Title) |>
  bind_rows(dplyr::select(manyenviron::agreements$HUGGO, manyID, Title)) |>
  distinct(manyID, .keep_all = TRUE) |>
  distinct(Title, .keep_all = TRUE) |>
  dplyr::right_join(data.frame("manyID" = node_names(as_tidygraph(references)))) |>
  rename(name = manyID)
# prepare dashboard interface
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Environmental Treaties Lineage", titleWidth = "400"),
  shinydashboard::dashboardSidebar(width = 350, shinydashboard::sidebarMenu(
    shiny::checkboxGroupInput("ref_choices", "Select relation type:",
                              choices = c("Amends" = "Amends", "Cites" = "Cites",
                                          "Enables" = "Enables", "Supersedes" = "Supersedes"),
                              selected = "Amends"),
    shiny::selectInput("actions", "Select activity:",
                       choices = c("choose" = "" ,"agriculture", "alliance", "biodiversity", "climate change", "delimitation",
                                    "economic integration", "energy", "finance", "fishing", "forestry", "health",
                                    "human rights", "investement", "management", "military", "research", "security",
                                    "space", "trade", "waste"), selected = "choose", multiple = T),
    shiny::selectInput("known", "Select known agreement:",
                        choices = c("choose" = "","UNCLOS", "CBD", "CCAMLR", "CITES", "CLC", "CRAMRA", "CECE", "LRTAP",
                                    "MARPOL", "NAAEC", "OLDEPESCA", "OPRC", "OSPAR", "PARIS", "PIC", "RAMSA",
                                    "UNCCD", "UNFCCC", "VIENNA"), selected = "choose", multiple = T),
    shiny::checkboxGroupInput("year_choices", "Select period:",
                               choices = c("Before 1970","1970-1980","1981-1990","1991-2000","2001-2010", "2011-2020"),
                               selected = "Before 1970"))),
  shinydashboard::dashboardBody(div(style = "position:relative",
                                    plotOutput("distPlot", height = "700px",
                                               hover = hoverOpts("plot_hover",
                                                                 delay = 50,
                                                                 delayType = "throttle")),
                                    uiOutput("hover_info"))))

# Step three: connect with the data
server <- function(input, output) {
  filteredData <- shiny::reactive({
    references <- references |>
      dplyr::filter(year_range %in% input$year_choices,
                    RefType %in% input$ref_choices,
                    if (!is.null(input$actions)) action %in% input$actions  else TRUE,
                    if (!is.null(input$known)) known_agr %in% input$known  else TRUE) |>
      manynet::as_tidygraph() |>
      manynet::mutate(nyear = as.numeric(stringr::str_extract(name, "[:digit:]{4}")))
    })
    output$distPlot <- shiny::renderPlot({
      manynet::autographr(filteredData(), layout = "lineage", rank = "nyear",
                          edge_color = "RefType") +
        theme(legend.position = "bottom")
    })
    output$hover_info <- renderUI({
      hover <- input$plot_hover
      ggdata <- manynet::autographr(filteredData(), layout = "lineage",
                                    rank = "nyear", edge_color = "RefType") +
        theme(legend.position = "bottom")
      point <- nearPoints(ggplot2::ggplot_build(ggdata)$data[[1]],
                          hover, addDist = TRUE)
      titlet <- as.character(titles[titles$name %in% point$label, 2])
      wellPanel(style = "position:relative; background: #F0F8FF; border-color: #FFFFFF; ",
                if (titlet == "character(0)") "Hoover over nodes to see the treaty title" else titlet,
                tags$head(tags$style(".shiny-output-error{visibility: hidden}")),
                tags$head(tags$style(".shiny-output-error:after{content: 'Title not found, please try another node.';
visibility: visible}")))
    })
}

shiny::shinyApp(ui = ui, server = server)
