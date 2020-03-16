library(shiny)
library(shinyjs)
source("./utils.R")

shinyServer(function(input, output, session) {

ready <- reactiveValues(ok = FALSE)
  
fetchMemoData <- function() {
    if (!user_input$authenticated) {
      return(NULL)
    } else {
      memo_getPrioritizationTable()
    }
    
  }

memo_data <- reactive({
    fetchMemoData()
  })
  
user_input <- reactiveValues(authenticated = FALSE, status = "")
  
observeEvent(input$login_button, {
    is_logged_in <- FALSE
    user_input$authenticated <-
      DHISLogin(input$server, input$user_name, input$password)
    flog.info(paste0("User ", input$user_name, " logged in."), name = "datapack")
  })
  
output$memo_table <- DT::renderDataTable({
    
  d <- memo_data()
    
    if (!inherits(d, "error") & !is.null(d)) {
      
      DT::datatable(d,options = list(pageLength = 50, 
                                     columnDefs = list(list(className = 'dt-right', 
                                                            targets = 3:8)))) %>% 
        formatCurrency(3:8, '',digits =0)

    } else
    {
      NULL
    }
  })

output$uiLogin <- renderUI({
  wellPanel(fluidRow(
    img(src = 'pepfar.png', align = "center"),
    h4(
      "Welcome to the  COP20 DataPack Validation App. Please login with your DATIM credentials:"
    )
  ),
  fluidRow(
    textInput("user_name", "Username: ", width = "600px"),
    passwordInput("password", "Password:", width = "600px"),
    actionButton("login_button", "Log in!")
  ))
})

output$ui <- renderUI({
  if (user_input$authenticated == FALSE) {
    ##### UI code for login page
    fluidPage(fluidRow(
      column(
        width = 2,
        offset = 5,
        br(),
        br(),
        br(),
        br(),
        uiOutput("uiLogin"),
        uiOutput("pass")
      )
    ))
  } else {
    wiki_url <- a("Datapack Wiki",
                  href = "https://github.com/pepfar-datim/Data-Pack-Feedback/wiki",
                  target = "_blank")
    
    fluidPage(tags$head(
      tags$style(
        ".shiny-notification {
        position: fixed;
        top: 10%;
        left: 33%;
        right: 33%;}"
      )
      ),
      sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          id = "side-panel",
          tagList(wiki_url)
        ),
        mainPanel(tabsetPanel(
          id = "main-panel",
          type = "tabs",
          tabPanel("Prioritization", dataTableOutput("memo_table"))
        ))
      ))
}
  
})

})