pacman::p_load(shiny, shinyjs, shinyWidgets, magrittr, knitr, flextable,officer,
               kableExtra, gdtools, futile.logger, glue, dplyr, tibble, jsonlite, httr, tidyr, stringr, DT,
               datapackr, datimutils, purrr, rpivotTable,parallel,magrittr)

options(scipen = 999)

logger <- flog.logger()

if (!file.exists(Sys.getenv("LOG_PATH"))) {
  file.create(Sys.getenv("LOG_PATH"))
}

flog.appender(appender.console(), name = "cop-memo")


shinyServer(function(input, output, session) {
  ready <- reactiveValues(ok = FALSE)
  
  fetchMemoData <- function() {
    shinyjs::disable("downloadReport")
    if (!user_input$authenticated | !ready$ok)  {
      return(NULL)
    } else {
      shinyWidgets::sendSweetAlert(
        session,
        title = "Retreiving data.",
        text = "This process may take a few minutes. Please wait.",
        btn_labels = NA
      )
      d <- list()
      d$info$operating_unit <- input$ou
      d$info$datapack_name <- input$ou
      #Get a list of country UIDSs
      d$info$country_uids <- datapack_config() %>%
        dplyr::filter(`datapack_name` == d$info$operating_unit) %>%
        dplyr::pull(country_uids) %>%
        unlist()
      #Get a list of PSNUs
      d$info$psnus <- datapackr::valid_PSNUs %>%
        dplyr::filter(country_uid %in% d$info$country_uids) %>%
        dplyr::filter(!is.na(psnu_type)) %>%
        dplyr::select(ou, country_name, snu1, psnu, psnu_uid)
      
      
      d$info$cop_year <- as.numeric(user_input$cop_year)
      d <- datapackr::prepareMemoData(d,memo_type ="datim",
                                      include_no_prio = input$include_no_prio,
                                      d2_session = user_input$d2_session)

      
      if (NROW(d$memo$datim$analytics) == 0) {
        shinyjs::disable("downloadXLSX")
        shinyjs::disable("downloadDOCX")
        closeSweetAlert(session)
        sendSweetAlert(
          session,
          title = "Oops!",
          text = "Sorry, I could not find any data for you!"
        )
        return(d)
      }
      
      shinyjs::enable("fetch")
      shinyjs::enable("downloadXLSX")
      shinyjs::enable("downloadDOCX")
      closeSweetAlert(session)
      
      return(d)
    }
  }
  memo_data <- reactive({
    fetchMemoData()
  })
  user_input <- reactiveValues(authenticated = FALSE,
                               status = "",
                               base_url = getBaseURL(),
                               d2_session = NULL)
  observeEvent(input$login_button, {
    is_authorized <- FALSE
    tryCatch({
      datimutils::loginToDATIM(base_url = user_input$base_url,
                               username = input$user_name,
                               password = input$password)
      # Need to check the user is a member of the PRIME Data Systems Group, COP Memo group, or a super user
      is_authorized <- grepl("VDEqY8YeCEk|ezh8nmc4JbX",
                             d2_default_session$me$userGroups) | grepl("jtzbVV4ZmdP",
                                                                       d2_default_session$me$userCredentials$userRoles)
    },
    #This function throws an error if the login is not successful
    error = function(e) {
      sendSweetAlert(
        session,
        title = "Login failed",
        text = "Please check your username/password!",
        type = "error")
      flog.info(paste0("User ", input$user_name, " login failed."), name = "cop_memo")
    })
    
    if (exists("d2_default_session") & is_authorized) {
      
      futile.logger::flog.info(paste0("User ",
                                      d2_default_session$me$userCredentials$username,
                                      " logged in to ", d2_default_session$base_url), name = "cop_memo")
      user_input$authenticated <- TRUE
      user_input$d2_session <- d2_default_session$clone()
      
    } else {
      sendSweetAlert(
        session,
        title = "Not authorized",
        text = "This app is for specific DATIM users. Please contact DATIM support for more information.",
        type = "error")
      user_authenticated <- FALSE
      rm(d2_default_session)
    }
    
  })
  
  output$prio_table <- DT::renderDataTable({
    
    d <- memo_data() %>% 
      purrr::pluck("memo") %>% 
      purrr::pluck("datim") %>% 
      purrr::pluck("by_prio")
    
    if (!inherits(d, "error") & !is.null(d)) {
      DT::datatable(d, options = list(pageLength = 50,
                                      columnDefs = list(list(className = "dt-right",
                                                             targets = 3:dim(d)[2])))) %>%
        formatCurrency(3:dim(d)[2], "", digits = 0)
      
    } else {
      NULL
    }
  })
  
  output$partners_table <- DT::renderDataTable({
    
    d <- memo_data() %>% 
      purrr::pluck("memo") %>% 
      purrr::pluck("datim") %>% 
      purrr::pluck("by_partner")
    
    if (!inherits(d, "error") & !is.null(d)) {
      
      DT::datatable(d, options = list(pageLength = 50,
                                      columnDefs = list(list(className = "dt-right",
                                                             targets = 3:dim(d)[2])))) %>%
        formatCurrency(4:dim(d)[2], "", digits = 0)
      
    } else {
      NULL
    }
  })
  
  observeEvent(input$fetch, {
    shinyjs::disable("fetch")
    ready$ok <- TRUE
    
  })
  
  output$agency_table <- DT::renderDataTable({
    
    d <- memo_data() %>% 
      purrr::pluck("memo") %>% 
      purrr::pluck("datim") %>% 
      purrr::pluck("by_agency")
    
    if (!inherits(d, "error") & !is.null(d)) {
      
      DT::datatable(d, options = list(pageLength = 50,
                                      columnDefs = list(list(className = "dt-right",
                                                             targets = 3:dim(d)[2])))) %>%
        formatCurrency(3:dim(d)[2], "", digits = 0)
      
    } else {
      NULL
    }
  })
  
  observeEvent(input$fetch, {
    shinyjs::disable("fetch")
    ready$ok <- TRUE
    
  })
  
  observeEvent(input$logout, {
    flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
    ready$ok <- FALSE
    user_input$authenticated <- FALSE
    user_input$d2_session <- NULL
    gc()
    
  })
  
  observeEvent(input$cop_year, {
    
    user_input$cop_year <- input$cop_year
    
  })
  
  output$uiLogin <- renderUI({
    wellPanel(fluidRow(
      img(src = "pepfar.png", align = "center"),
      h4(
        "Welcome to the COP Memo App. Please login with your DATIM credentials:"
      )
    ),
    fluidRow(
      textInput("user_name", "Username: ", width = "600px"),
      passwordInput("password", "Password:", width = "600px"),
      actionButton("login_button", "Log in!")
    ),
    tags$hr(),
    fluidRow(HTML(getVersionInfo())))
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
          uiOutput("base_url"),
          uiOutput("uiLogin"),
          uiOutput("pass")
        )
      ))
    } else {
      
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
          selectInput("cop_year", "COP Year", c(2020, 2021,2022), selected = 2022),
          tags$hr(),
          selectInput("ou", "Operating Unit", datapack_config()$datapack_name),
          tags$hr(),
          checkboxInput("include_no_prio", "Show No Prioritization", value = TRUE),
          tags$hr(),
          actionButton("fetch", "Get data"),
          tags$hr(),
          h4("Download report:"),
          div(style = "display: inline-block; vertical-align:top; width: 60 px; font-size:80%'",
              disabled(downloadButton("downloadXLSX", "XLSX"))),
          div(style = "display: inline-block; vertical-align:top; width: 60 px;font-size:80%'",
              disabled(downloadButton("downloadDOCX", "DOCX"))),
          tags$hr(),
          actionButton("logout", "Log out"),
          width = 3
        ),
        mainPanel(tabsetPanel(
          id = "main-panel",
          type = "tabs",
          tabPanel("By Prioritization", DT::dataTableOutput("prio_table")),
          tabPanel("By Partners/Mechanism", DT::dataTableOutput("partners_table")),
          tabPanel("By Agency", DT::dataTableOutput("agency_table")),
          tabPanel("Pivot", rpivotTable::rpivotTableOutput({"pivot"}) ) # nolint
        ))
      ))
    }
  })
  
  
  output$downloadDOCX <- downloadHandler(
    filename = function() {
      
      prefix <- "cop_approval_memo_"
      date <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0(paste(prefix, date, sep = "_"), ".docx")
    },
    
    content = function(file) {
      
      d <- memo_data()
      
      doc <- datapackr::generateApprovalMemo(d,
                                             memo_type = "datim",
                                             draft_memo = FALSE,
                                             d2_session = user_input$d2_session)
      
      print(doc, target = file, draft=FALSE)
    }
  )
  
  output$downloadXLSX <- downloadHandler(
    filename = function() {
      
      
      prefix <- "cop_approval_memo_"
      date <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0(paste(prefix, date, sep = "_"), ".xlsx")
      
    },
    content = function(file) {
      
      d <- memo_data()
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "By Prioritization")
      openxlsx::writeDataTable(wb = wb,
                               sheet = "By Prioritization", x = d$memo$datim$by_prio)
      openxlsx::addWorksheet(wb, "By Partner")
      openxlsx::writeDataTable(wb = wb,
                               sheet = "By Partner", x = d$memo$datim$by_partner)
      openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
      openxlsx::addWorksheet(wb, "By Agency")
      openxlsx::writeDataTable(wb = wb,
                               sheet = "By Agency", x = d$memo$datim$by_agency)
      
      openxlsx::addWorksheet(wb, "By PSNUxIM")
      openxlsx::writeDataTable(wb = wb,
                               sheet = "By PSNUxIM", x = d$memo$datim$by_psnu)
      openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
    }
  )
  
  output$pivot <- renderRpivotTable({
    d <- memo_data()
    
    if (!inherits(d, "error") & !is.null(d)) {
      
      if (is.null(d$memo$datim$by_psnu)) {
        return(NULL)
      }
      
      PSNUxIM_pivot(d)
      
    } else {
      NULL
    }
  })
  
})
