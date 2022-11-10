
#' UI Elements
#'
#' @param buttonWidth button width
#' @param sideBarWidth (numeric) width of sidebar
#' @param ... UI elements for the box
#'
#' @export
#' @rdname uiElements
box <- function(...){
  shinydashboard::box(...)
}


#' @export
#' @rdname uiElements
panelTitle <- function(sideBarWidth) {
  dashboardHeader(
    title = "Online Retail Analytics",
    titleWidth = sideBarWidth
  )
}

#' @export
#' @rdname uiElements
panelSelectInput <- function(buttonWidth) {
  wellPanel(
    selectInput("inputType", "Upload a file",
                choices = c("File", "Sample Data"),
                selected = "File"),
    conditionalPanel(
      condition = "input.inputType == 'File'",
      fileInput(inputId = "file1", label = "File to be uploaded"),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ",")
    ),
    conditionalPanel(
      condition = "input.inputType == 'Sample Data'"
    )
    ,
    style = "color:black"
  )
}


### Shop Level Analytics Elements ###
#' @export
#' @rdname uiElements
shopLevelTimeAnalysis <- function() {
  column(width = 12,
         h4("Order volume over time"),
         box(width = 4, wellPanel(
           selectInput("timeVar", label = "Time Axis", selected = "Month",
                       choices = c("Hour", "Weekday", "Month")))),
         box(width = 8, plotOutput("time"))
  )
  
}

#' @export
#' @rdname uiElements
shopLevelTrendAnalysis <- function() {
  column(
    width = 12,
    h4("Sales/Quantity Analysis Over time"),
    box(
      width = 4,
      wellPanel(
        selectInput("trendVar", label = "Variable",
                    selected = "Sales", choices = c("Sales", "Quantity")),
        dateRangeInput("trendSpanVar", label = "Time Span",
                       start = "2011-01-01", end = "2011-12-31")
      )
    ),
    box(width = 8, plotOutput("trend"))
  )
}

### Individual Level Analysis Elements ###
#' @export
#' @rdname uiElements
customerIdSelection <- function() {
  wellPanel(
    box(
      width = 12,
      selectInput("customerId", "Select Customer ID", choices = "")
    ),
    style = "color:black"
  )
}
#' @export
#' @rdname uiElements
iLevelKpis <- function() {
  tagList(
    h4("Overall Statistics"),
    box(
      width =  12,
      infoBoxOutput("revenueKpiI", width = 4),
      infoBoxOutput("numProductsKpiI", width = 4),
      infoBoxOutput("quantileKpiI", width = 4)
    )
  )
}

#' @export
#' @rdname uiElements
iLevelProductRanking <- function() {
  column(
    width =12,
    h4("Product Frequency"),
    box(width = 3.5, wellPanel(
      selectInput("numProductsI", label = "Top", selected = 5,
                  choices = c(3, 5, 10)),
      dateRangeInput("productsSpanVarI", label = "Time Span",
                     start = "2010-12-01", end = "2011-12-31"))),
    box(width = 6, title = "Highest Bought Products",
        tableOutput("topProductsI")),
    box(width = 6, title = "Lowest Bought Products",
        tableOutput("lowProductsI"))
  )
}

#' @export
#' @rdname uiElements
Kmeans_ <- function(){
  column(
    width = 12,
    h5('k-means clustering'),
    box(width = 3.5, wellPanel(numericInput('clusters', 'Cluster count', 3, min = 1, max = 9))),
    box(width = 6, plotOutput("plot2")),
    box(width = 6, plotOutput("plot1"))
  )
}


library("ecomAnalytics")
library("shiny")
library("shinydashboard")

buttonWidth <- 220
sideBarWidth <- 250

dashboardPage(
  skin = "black",
  header = panelTitle(sideBarWidth),
  
  sidebar = dashboardSidebar(
    width = sideBarWidth,
    shinyjs::useShinyjs(),
    panelSelectInput(buttonWidth)
  ),
  body = dashboardBody(
    tags$head(
      tags$link(
        # rel = "stylesheet",
        #  type = "text/css",
        # href = "styleDefinitions.css"
      )),
    div(class = "span", tabsetPanel(
      id = "Reiter",
      tabPanel("Raw Data", value = "tab1", DT::DTOutput("rawDataOverview")),
      tabPanel(
        "Product Analysis", value = "tab2",
        fluidRow(
          shopLevelTimeAnalysis()
        ),
        fluidRow(
          shopLevelTrendAnalysis()
        )
      ),
      tabPanel(
        "Customer Analysis", value = "tab3",
        fluidRow(customerIdSelection()),
        fluidRow(iLevelKpis()),
        fluidRow(
          iLevelProductRanking()        
        )
      ),
      tabPanel("K-Means", value = "tab4",Kmeans_())
    ))))





