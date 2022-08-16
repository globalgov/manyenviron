# library(shiny)
# library(shinydashboard)
# library(dplyr)

# Prepare memberships dataset
memberships <- manyenviron::memberships$IEADB_MEM[,c(1,2,3)]
memberships$type <- manypkgs::code_type(memberships$Title)
memberships$agr_type <- ifelse(stringr::str_detect(memberships$type, "A"), "Agreement",
                               (ifelse(stringr::str_detect(memberships$type, "P"), "Protocol",
                                       (ifelse(stringr::str_detect(memberships$type, "E"), "Amendment",
                                               (ifelse(stringr::str_detect(memberships$type, "N"), "Notes",
                                                       (ifelse(stringr::str_detect(memberships$type, "S"), "Strategy",
                                                               (ifelse(stringr::str_detect(memberships$type, "R"), "Resolution",NA)))))))))))

# Define UI 
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Type of Environmental Treaties", titleWidth = "400"),
  shinydashboard::dashboardSidebar(
        width = 350,
        shinydashboard::sidebarMenu(
          shiny::checkboxGroupInput("agr_type", 
                               "Select treaty type:",
                               choices = c("Agreement", "Protocol",
                                           "Amendment", "Notes", "Strategy", "Resolution"),
                               selected = "Agreement"),
          shinydashboard::menuItem(shiny::sliderInput("num", "Dates", value = 1960, min = 1950, max = 2020, width = 350))
        )),
  shinydashboard::dashboardBody(
        shiny::plotOutput("distPlot", height = "550px")
    )
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
    output$distPlot <- shiny::renderPlot({
        migraph::autographr(filteredData())
    })
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
