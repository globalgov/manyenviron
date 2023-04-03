# library(shiny)
# library(shinydashboard)
# library(dplyr)
# library(ggplot2)

# Prepare memberships dataset
memberships <- manyenviron::memberships$IEADB_MEM[,c(1,2,3)]
memberships$type <- manypkgs::code_type(memberships$Title)
memberships$agr_type <- ifelse(stringr::str_detect(memberships$type, "A"), "Agreement",
                               (ifelse(stringr::str_detect(memberships$type, "P"), "Protocol",
                                       (ifelse(stringr::str_detect(memberships$type, "E"), "Amendment",
                                               (ifelse(stringr::str_detect(memberships$type, "N"), "Notes",
                                                       (ifelse(stringr::str_detect(memberships$type, "S"), "Strategy",
                                                               (ifelse(stringr::str_detect(memberships$type, "R"), "Resolution",NA)))))))))))

memberships <- memberships %>%
  dplyr::mutate(category = NA)

i <- 0
manyID <- NA
for(i in 1:nrow(memberships)){
  manyID <- as.character(memberships[i, 1])
  if(sum(memberships$manyID == manyID) > 2) {
    memberships[i, 6] <- "Multilateral"
  }
  else if(sum(memberships$manyID == manyID) == 2){
    memberships[i, 6] <- "Bilateral"
  }
  else{
  memberships[i, 6] <- "Other"
}
}

titles <- memberships %>%
  dplyr::distinct(manyID, .keep_all = TRUE) %>%
  dplyr::select(manyID, Title)


# Define UI 
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Type of Environmental Treaties", titleWidth = "400"),
  shinydashboard::dashboardSidebar(
        width = 350,
        shinydashboard::sidebarMenu(
          selectInput("country",
                      "Select party(s):",
                      choices = c("choose" = "", stringr::str_sort(memberships$stateID)),
                      selected = "choose",
                      multiple = T),
          shiny::checkboxGroupInput("agr_type", 
                               "Select treaty type:",
                               choices = c("Agreement", "Protocol",
                                           "Amendment", "Notes", "Strategy", "Resolution"),
                               selected = "Agreement"),
          selectInput("category",
                      "Select category:",
                      choices = c("choose" = "", "Bilateral", "Multilateral", "Other"),
                      selected = "choose",
                      multiple = T),
          shinydashboard::menuItem(shiny::sliderInput("num", "Dates", value = 1960, min = 1950, max = 2020, width = 350))
        ),
        wellPanel(
          style = " background: #222D32; border-color: #222D32; margin-left: 20px",
          textOutput("click_info"),
          tags$head(tags$style(".shiny-output-error{visibility: hidden}")),
          tags$head(tags$style(".shiny-output-error:after{content: 'No treaties found. Please try again with different inputs.';
visibility: visible}"))
        )),
  shinydashboard::dashboardBody(
        shiny::plotOutput("distPlot", height = "550px",
        click = "plot_click"))
)
# Define server
server <- function(input, output){
    filteredData <- shiny::reactive({
        memberships <- memberships %>% 
            dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>% 
            dplyr::filter(year %in% input$num) %>% 
            dplyr::filter(agr_type %in% input$agr_type) %>% 
            migraph::as_tidygraph()
})
    
    filteredData2 <- shiny::reactive({
      memberships <- memberships %>% 
        dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>% 
        dplyr::filter(year %in% input$num) %>% 
        dplyr::filter(agr_type %in% input$agr_type) %>% 
        dplyr::filter(category %in% input$category) %>%
      migraph::as_tidygraph()
    })
    filteredData3 <- shiny::reactive({
      memberships <- memberships %>% 
        dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>% 
        dplyr::filter(year %in% input$num) %>% 
        dplyr::filter(agr_type %in% input$agr_type) %>% 
        dplyr::filter(stateID %in% input$country) %>%
      migraph::as_tidygraph()
    })
    filteredData4 <- shiny::reactive({
      memberships <- memberships %>% 
        dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>% 
        dplyr::filter(year %in% input$num) %>% 
        dplyr::filter(agr_type %in% input$agr_type) %>% 
        dplyr::filter(category %in% input$category) %>%
        dplyr::filter(stateID %in% input$country) %>%
      migraph::as_tidygraph()
    })
    
    coords1 <- reactive({
      ggdata1 <- ggplot2::ggplot_build(migraph::autographr(filteredData()))$data[[1]]
    })
    coords2 <- reactive({
      ggdata2 <- ggplot2::ggplot_build(migraph::autographr(filteredData2()))$data[[1]]
    })
    coords3 <- reactive({
      ggdata3 <- ggplot2::ggplot_build(migraph::autographr(filteredData3()))$data[[1]]
    })
    coords4 <- reactive({
      ggdata4 <- ggplot2::ggplot_build(migraph::autographr(filteredData4()))$data[[1]]
    })
    
    output$distPlot <- renderPlot({
      if(is.null(input$country) & is.null(input$category)){
        migraph::autographr(filteredData())
        
      }
      else if(is.null(input$country) & !is.null(input$category)){
        migraph::autographr(filteredData2())
        
      }
      else if(!is.null(input$country) & is.null(input$category)){
        migraph::autographr(filteredData3())
      }
      else if(!is.null(input$country) & !is.null(input$category)){
        migraph::autographr(filteredData4())
      }
    })
    
    output$click_info <- renderText({
      if(is.null(input$country) & is.null(input$category)){
        point <- nearPoints(coords1(), input$plot_click, 
                            addDist = TRUE)
        title <- as.character(titles[titles$manyID %in% point$label, 2])
        if(title == "character(0)"){
          print("Please click on a node representing a treaty to display its title.")
        }
        else if(!(title == "character(0)")){
          print(title)
        }
      }
      else if(is.null(input$country) & !is.null(input$category)){
        point <- nearPoints(coords2(), input$plot_click, 
                            addDist = TRUE)
        title <- as.character(titles[titles$manyID %in% point$label, 2])
        if(title == "character(0)"){
          print("Please click on a node representing a treaty to display its title.")
        }
        else if(!(title == "character(0)")){
          print(title)
        }
      }
      
      else if(!is.null(input$country) & is.null(input$category)){
        point <- nearPoints(coords3(), input$plot_click, 
                            addDist = TRUE)
        title <- as.character(titles[titles$manyID %in% point$label, 2])
        if(title == "character(0)"){
          print("Please click on a node representing a treaty to display its title.")
        }
        else if(!(title == "character(0)")){
          print(title)
        }
      }
      else if(!is.null(input$country) & !is.null(input$category)){
        point <- nearPoints(coords4(), input$plot_click, 
                            addDist = TRUE)
        title <- as.character(titles[titles$manyID %in% point$label, 2])
        if(title == "character(0)"){
          print("Please click on a node representing a treaty to display its title.")
        }
        else if(!(title == "character(0)")){
          print(title)
        }
      }
    })
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
