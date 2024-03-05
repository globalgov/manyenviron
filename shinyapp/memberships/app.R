# library(shiny)
# library(shinydashboard)
# library(dplyr)
# library(ggplot2)
# library(tidygraph)

# Prepare memberships dataset
memberships <- manyenviron::memberships$IEADB_MEM %>%
  dplyr::select("manyID", "stateID", "Title")
memberships$type <- manypkgs::code_type(memberships$Title)
memberships$agr_type <- ifelse(stringr::str_detect(memberships$type, "A"), "Agreement",
                               (ifelse(stringr::str_detect(memberships$type, "P"), "Protocol",
                                       (ifelse(stringr::str_detect(memberships$type, "E"), "Amendment",
                                               (ifelse(stringr::str_detect(memberships$type, "N"), "Notes",
                                                       (ifelse(stringr::str_detect(memberships$type, "S"), "Strategy",
                                                               (ifelse(stringr::str_detect(memberships$type, "R"), "Resolution",NA)))))))))))
# Determine whether treaties are bilateral or multilateral, or other (most
# likely accessions)
memberships <- memberships %>%
  dplyr::mutate(category = NA)
i <- 0
manyID <- NA
for(i in seq_len(nrow(memberships))){
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
                               choices = c("Agreement", "Protocol", "Amendment",
                                           "Notes", "Strategy", "Resolution"),
                               selected = "Agreement"),
          selectInput("category",
                      "Select category:",
                      choices = c("choose" = "", "Bilateral", "Multilateral", "Other"),
                      selected = "choose",
                      multiple = T),
          shinydashboard::menuItem(shiny::sliderInput("range", "Dates", value = c(1951, 1952),
                                                      min = 1950, max = 2020, width = 350, sep = "")),
          checkboxInput("treatylabel", "Display treaty node labels", TRUE),
          checkboxInput("countrylabel", "Display country node labels", TRUE)),
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
  # Save a data frame of titles of agreements
  titles <- memberships %>%
    dplyr::distinct(manyID, .keep_all = TRUE) %>%
    dplyr::select(manyID, Title)
  # filteredData for each combination of inputs
  # Four possibilities for each combination of inputs due to possible
  # combinations of display labels checkboxes
  # Preserve memberships dataframe for later use with coordinates
    filteredData <- shiny::reactive({
      if(input$treatylabel == TRUE & input$countrylabel == TRUE){
        memberships1 <- memberships %>% 
            dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>%
            dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>%
            dplyr::filter(agr_type %in% input$agr_type) %>%
            manynet::as_tidygraph() %>%
            dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
            dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
            dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square"))
      }
      else if(input$treatylabel == FALSE & input$countrylabel == FALSE){
        memberships1 <- memberships %>% 
          dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>%
          dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>%
          dplyr::filter(agr_type %in% input$agr_type) %>%
          manynet::as_tidygraph() %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square")) %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ "",
                                                TRUE ~ ""))
      }
      else if(input$treatylabel == TRUE & input$countrylabel == FALSE){
        memberships1 <- memberships %>% 
          dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>%
          dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>%
          dplyr::filter(agr_type %in% input$agr_type) %>%
          manynet::as_tidygraph() %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square")) %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ name,
                                                TRUE ~ ""))
      }
      else if(input$treatylabel == FALSE & input$countrylabel == TRUE){
        memberships1 <- memberships %>% 
          dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>%
          dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>%
          dplyr::filter(agr_type %in% input$agr_type) %>%
          manynet::as_tidygraph() %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square")) %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ "",
                                                TRUE ~ name))
      }
})
    filteredData2 <- shiny::reactive({
      if(input$treatylabel == TRUE & input$countrylabel == TRUE){
        memberships1 <- memberships %>% 
          dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>% 
          dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>% 
          dplyr::filter(agr_type %in% input$agr_type) %>% 
          dplyr::filter(category %in% input$category) %>%
          manynet::as_tidygraph() %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square"))
      }
      else if (input$treatylabel == FALSE & input$countrylabel == FALSE){
        memberships1 <- memberships %>% 
          dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>% 
          dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>% 
          dplyr::filter(agr_type %in% input$agr_type) %>% 
          dplyr::filter(category %in% input$category) %>%
          manynet::as_tidygraph() %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square")) %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ "",
                                                TRUE ~ ""))
      }
      else if (input$treatylabel == TRUE & input$countrylabel == FALSE){
        memberships1 <- memberships %>% 
          dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>% 
          dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>% 
          dplyr::filter(agr_type %in% input$agr_type) %>% 
          dplyr::filter(category %in% input$category) %>%
          manynet::as_tidygraph() %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square")) %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ name,
                                                TRUE ~ ""))
      }
      else if (input$treatylabel == FALSE & input$countrylabel == TRUE){
        memberships1 <- memberships %>% 
          dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>% 
          dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>% 
          dplyr::filter(agr_type %in% input$agr_type) %>% 
          dplyr::filter(category %in% input$category) %>%
          manynet::as_tidygraph() %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square")) %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ "",
                                                TRUE ~ name))
      }
    })
    filteredData3 <- shiny::reactive({
      if (input$treatylabel == TRUE & input$countrylabel == TRUE){
        memberships1 <- memberships %>% 
          dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>% 
          dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>% 
          dplyr::filter(agr_type %in% input$agr_type) %>% 
          dplyr::filter(stateID %in% input$country) %>%
          manynet::as_tidygraph() %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square"))
      }
      else if (input$treatylabel == FALSE & input$countrylabel == FALSE){
      memberships1 <- memberships %>% 
        dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>% 
        dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>% 
        dplyr::filter(agr_type %in% input$agr_type) %>% 
        dplyr::filter(stateID %in% input$country) %>%
        manynet::as_tidygraph() %>%
        dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                               TRUE ~ "black")) %>%
        dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                              TRUE ~ 1.5)) %>%
        dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                               TRUE ~ "square")) %>%
        dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ "",
                                                TRUE ~ ""))
      }
      else if (input$treatylabel == TRUE & input$countrylabel == FALSE){
        memberships1 <- memberships %>% 
          dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>% 
          dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>% 
          dplyr::filter(agr_type %in% input$agr_type) %>% 
          dplyr::filter(stateID %in% input$country) %>%
          manynet::as_tidygraph() %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square")) %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ name,
                                                TRUE ~ ""))
      }
      else if (input$treatylabel == FALSE & input$countrylabel == TRUE){
        memberships1 <- memberships %>% 
          dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>% 
          dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>% 
          dplyr::filter(agr_type %in% input$agr_type) %>% 
          dplyr::filter(stateID %in% input$country) %>%
          manynet::as_tidygraph() %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square")) %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ "",
                                                TRUE ~ name))
      }
    })
    filteredData4 <- shiny::reactive({
      if(input$treatylabel == TRUE & input$countrylabel == TRUE){
        memberships1 <- memberships %>%
          dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>%
          dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>%
          dplyr::filter(agr_type %in% input$agr_type) %>%
          dplyr::filter(category %in% input$category) %>%
          dplyr::filter(stateID %in% input$country) %>%
          manynet::as_tidygraph() %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square"))
      }
      else if(input$treatylabel == FALSE & input$countrylabel == FALSE){
        memberships1 <- memberships %>%
          dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>%
          dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>%
          dplyr::filter(agr_type %in% input$agr_type) %>%
          dplyr::filter(category %in% input$category) %>%
          dplyr::filter(stateID %in% input$country) %>%
          manynet::as_tidygraph() %>%
          
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square"))  %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ "",
                                                TRUE ~ ""))
      }
      else if(input$treatylabel == TRUE & input$countrylabel == FALSE){
        memberships1 <- memberships %>%
          dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>%
          dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>%
          dplyr::filter(agr_type %in% input$agr_type) %>%
          dplyr::filter(category %in% input$category) %>%
          dplyr::filter(stateID %in% input$country) %>%
          manynet::as_tidygraph() %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square"))  %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ name,
                                                TRUE ~ ""))
      }
      else if(input$treatylabel == FALSE & input$countrylabel == TRUE){
        memberships1 <- memberships %>%
          dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>%
          dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>%
          dplyr::filter(agr_type %in% input$agr_type) %>%
          dplyr::filter(category %in% input$category) %>%
          dplyr::filter(stateID %in% input$country) %>%
          manynet::as_tidygraph() %>%
          dplyr::mutate(color = dplyr::case_when(grepl("[0-9]", name) ~ "red",
                                                 TRUE ~ "black")) %>%
          dplyr::mutate(size = dplyr::case_when(grepl("[0-9]", name) ~ 2,
                                                TRUE ~ 1.5)) %>%
          dplyr::mutate(shape = dplyr::case_when(grepl("[0-9]", name) ~ "circle",
                                                 TRUE ~ "square"))  %>%
          dplyr::mutate(name = dplyr::case_when(grepl("[0-9]", name) ~ "",
                                                TRUE ~ name))
      }
    })
    # Create a plot object, whose coordinates will serve as reference
    # for the click interactivity on the plot that will be rendered.
    # Using the original memberships data frame instead of the tidygraph object
    # created after filtering data allows to display titles of agreements even
    # if their labels are not rendered on the actual plot.
    coords1 <- reactive({
      ggdata1 <- memberships %>%
        dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>%
        dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>%
        dplyr::filter(agr_type %in% input$agr_type) %>%
        manynet::as_tidygraph() %>%
        manynet::autographr()
      ggdata1 <- ggplot2::ggplot_build(ggdata1)$data[[1]]
    })
    coords2 <- reactive({
      ggdata2 <- memberships %>% 
        dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>% 
        dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>% 
        dplyr::filter(agr_type %in% input$agr_type) %>% 
        dplyr::filter(category %in% input$category) %>%
        manynet::as_tidygraph() %>%
        manynet::autographr()
      ggdata2 <- ggplot2::ggplot_build(ggdata2)$data[[1]]
        
    })
    coords3 <- reactive({
      ggdata3 <- memberships %>% 
        dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>% 
        dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>% 
        dplyr::filter(agr_type %in% input$agr_type) %>% 
        dplyr::filter(stateID %in% input$country) %>%
        manynet::as_tidygraph() %>%
        manynet::autographr()
      ggdata3 <- ggplot2::ggplot_build(ggdata3)$data[[1]]
    })
    coords4 <- reactive({
      ggdata4 <- memberships %>% 
        dplyr::mutate(year = stringr::str_extract(manyID, "[:digit:]{4}")) %>% 
        dplyr::filter(year >= input$range[1] & year <= input$range[2]) %>% 
        dplyr::filter(agr_type %in% input$agr_type) %>% 
        dplyr::filter(category %in% input$category) %>%
        dplyr::filter(stateID %in% input$country) %>%
        manynet::as_tidygraph() %>%
        manynet::autographr()
      ggdata4 <- ggplot2::ggplot_build(ggdata4)$data[[1]]
    })
    
    output$distPlot <- renderPlot({
      if(is.null(input$country) & is.null(input$category)){
        manynet::autographr(filteredData(), node_color = "color",
                            node_size = "size", node_shape = "shape") +
          ggplot2::scale_color_manual(values = c("black" = "black",
                                                 "red" = "red"))
        
      }
      else if(is.null(input$country) & !is.null(input$category)){
        manynet::autographr(filteredData2(), node_color = "color",
                            node_size = "size", node_shape = "shape") +
          ggplot2::scale_color_manual(values = c("black" = "black",
                                                 "red" = "red"))
        
      }
      else if(!is.null(input$country) & is.null(input$category)){
        manynet::autographr(filteredData3(), node_color = "color",
                            node_size = "size", node_shape = "shape") +
          ggplot2::scale_color_manual(values = c("black" = "black",
                                                 "red" = "red"))
      }
      else if(!is.null(input$country) & !is.null(input$category)){
        manynet::autographr(filteredData4(), node_color = "color",
                            node_size = "size", node_shape = "shape") +
          ggplot2::scale_color_manual(values = c("black" = "black",
                                                 "red" = "red"))
      }
    })
    # Add interactivity to display the titles of agreements the user
    # clicks on, according to possible input combinations.
    output$click_info <- renderText({
      if(is.null(input$country) & is.null(input$category)){
        point <- nearPoints(coords1(), input$plot_click, 
                            addDist = TRUE)
        title <- as.character(titles[titles$manyID %in% point$label, 2])
        if(title == "character(0)"){
          print("Please click on a node representing a treaty (red circle) to display its title.")
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
          print("Please click on a node representing a treaty (red circle) to display its title.")
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
          print("Please click on a node representing a treaty (red circle) to display its title.")
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
          print("Please click on a node representing a treaty (red circle) to display its title.")
        }
        else if(!(title == "character(0)")){
          print(title)
        }
      }
    })
}

# Run the application 
shiny::shinyApp(ui = ui, server = server)
