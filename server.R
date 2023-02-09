pacman::p_load(shiny, shinyjs, shinyWidgets, magrittr, knitr, flextable,officer,
               kableExtra, gdtools, futile.logger, glue, dplyr, tibble, jsonlite, httr, tidyr, stringr, DT,
               datapackr, datimutils, purrr, rpivotTable,parallel,magrittr)

#Prevents scientific notation
options(scipen = 999)

logger <- flog.logger()

if (!file.exists(Sys.getenv("LOG_PATH"))) {
  file.create(Sys.getenv("LOG_PATH"))
}

flog.appender(appender.console(), name = "cop-memo")

################ OAuth Client information #####################################
if (interactive()) {
  # testing url
  options(shiny.port = 3123)
  APP_URL <- "http://127.0.0.1:3123/"# This will be your local host path
} else {
  # deployed URL
  APP_URL <- Sys.getenv("APP_URL") #This will be your shiny server path
}

oauth_app <- httr::oauth_app(Sys.getenv("OAUTH_APPNAME"),
                             key = Sys.getenv("OAUTH_KEYNAME"),        # dhis2 = Client ID
                             secret = Sys.getenv("OAUTH_SECRET"), #dhis2 = Client Secret
                             redirect_uri = APP_URL)


oauth_api <- httr::oauth_endpoint(base_url = paste0(getBaseURL(), "uaa/oauth"),
                                  request = NULL, # Documentation says to leave this NULL for OAuth2
                                  authorize = "authorize",
                                  access = "token")

oauth_scope <- "ALL"


has_auth_code <- function(params) {

  return(!is.null(params$code))
}


shinyServer(function(input, output, session) {
  # Trigger variable to allow download. Changes with authentication
  ready <- reactiveValues(ok = FALSE)
  # Function to retrieve the memo data and check if the user is authenticated
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
      d$info$cop_year <- as.numeric(user_input$cop_year)

      #Get a list of country UIDSs
      d$info$country_uids <- datapack_config() %>%
        dplyr::filter(`datapack_name` == d$info$operating_unit) %>%
        dplyr::pull(country_uids) %>%
        unlist()
      #Get a list of PSNUs
      # d$info$psnus <- datapackr::getValidOrgUnits(d$info$cop_year) %>%
      #   dplyr::filter(country_uid %in% d$info$country_uids) %>%
      #   dplyr::filter(is.na(`DREAMS`)) %>%
      #   dplyr::select(ou, country_name, snu1, psnu = name, psnu_uid = uid)

      d$info$tool <- "memo"


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
  # Reactive variable that captures the retun of the above function.
  memo_data <- reactive({
    fetchMemoData()
  })

  # Reactive variable list that assists with authentication
  user_input <- reactiveValues(authenticated = FALSE,
                               status = "",
                               base_url = getBaseURL(),
                               d2_session = NULL)

  output$ui_redirect <- renderUI({
    #print(input$login_button_oauth) useful for debugging
    if (!is.null(input$login_button_oauth)) {
      if (input$login_button_oauth > 0) {
        url <-
          httr::oauth2.0_authorize_url(oauth_api, oauth_app, scope = oauth_scope)
        redirect <- sprintf("location.replace(\"%s\");", url)
        tags$script(HTML(redirect))
      } else  {
        NULL
      }
    } else  {
      NULL
    }
  })

  ### Login Button oauth Checks
  observeEvent(input$login_button_oauth > 0, {

    #Grabs the code from the url
    params <- parseQueryString(session$clientData$url_search)
    #Wait until the auth code actually exists
    req(has_auth_code(params))

    #Manually create a token
    token <- httr::oauth2.0_token(
      app = oauth_app,
      endpoint = oauth_api,
      scope = oauth_scope,
      use_basic_auth = TRUE,
      oob_value = APP_URL,
      cache = FALSE,
      credentials = httr::oauth2.0_access_token(endpoint = oauth_api,
                                                app = oauth_app,
                                                code = params$code,
                                                use_basic_auth = TRUE)
    )

    loginAttempt <- tryCatch({

      datimutils::loginToDATIMOAuth(base_url =  getBaseURL(),
                                    token = token,
                                    app = oauth_app,
                                    api = oauth_api,
                                    redirect_uri = APP_URL,
                                    scope = oauth_scope,
                                    d2_session_envir = parent.env(environment()))

       },
      # This function throws an error if the login is not successful
      error = function(e) {
        flog.info(paste0("User ", input$user_name, " login failed. ", e$message), name = "datapack")
      }
    )

    # Need to check the user is a member of the PRIME Data Systems Group,
    # COP Memo group, or a super user
    is_authorized <- grepl("VDEqY8YeCEk|ezh8nmc4JbX", d2_default_session$me$userGroups) |
      grepl("jtzbVV4ZmdP",d2_default_session$me$userCredentials$userRoles)

    print(is_authorized)
    if (exists("d2_default_session") && is_authorized) {

      user_input$authenticated  <-  TRUE
      user_input$d2_session  <-  d2_default_session$clone()
      d2_default_session <- NULL

      # Need to check the user is a member of the
      # PRIME Data Systems Group, COP Memo group, or a super user
      user_input$memo_authorized  <-
        grepl("VDEqY8YeCEk|ezh8nmc4JbX",
              user_input$d2_session$me$userGroups) |
        grepl(
          "jtzbVV4ZmdP",
          user_input$d2_session$me$userCredentials$userRoles
        )
      flog.info(
        paste0(
          "User ",
          user_input$d2_session$me$userCredentials$username,
          " logged in."
        ),
        name = "datapack"
      )
    } else {
      if(exists("d2_default_session") & !(is_authorized)){# Display custom
        # message to the user in regards to NOT being a member of the
        # PRIME Data Systems Group, COP Memo group, or a super user

        # Might be a better way to trigger this if statement
        sendSweetAlert(
          session,
          title = "Not authorized",
          text = "This app is for specific DATIM users. Please contact DATIM support for more information.",
          type = "error")

      } else { #Display datimutils errors message to the user

        sendSweetAlert(
          session,
          title = "Login failed",
          text = substr(loginAttempt,# full error message printed to console
                        regexpr("User",loginAttempt)[1],# Where to start extract
                        nchar(loginAttempt)),# Where to end extract
          type = "error"
        )
      }
      user_authenticated <- FALSE
      rm(d2_default_session)
    }
  })


  # DataTable 1 Controls
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

  # DataTable 2 Controls
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
    req(input$logout)
    # Gets you back to the login without the authorization code at top
    updateQueryString("?", mode = "replace", session = session)
    flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
    ready$ok <- FALSE
    user_input$authenticated <- FALSE
    user_input$user_name <- ""
    user_input$authorized <- FALSE
    user_input$d2_session <- NULL
    d2_default_session <- NULL
    gc()
    session$reload()
  })

  observeEvent(input$cop_year, {

    user_input$cop_year <- input$cop_year

  })

  output$uiLogin <- renderUI({
    wellPanel(fluidRow(
      #img(src = "pepfar.png", align = "center"),
      tags$div(HTML('<center><img src="pepfar.png"></center>')),
      h4(
        "Welcome to the COP Memo App. You will be redirected to DATIM to authenticate."
      )
    ),
    fluidRow(
      actionButton("login_button_oauth", "Log in with DATIM"),
      uiOutput("ui_hasauth"),
      uiOutput("ui_redirect")
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

      prefix <- "cop_approval_memo"
      date <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0(paste(memo_data()$info$datapack_name,prefix, date, sep = "_"), ".docx")
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


      prefix <- "cop_approval_memo"
      date <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0(paste(memo_data()$info$datapack_name,prefix, date, sep = "_"), ".xlsx")

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
