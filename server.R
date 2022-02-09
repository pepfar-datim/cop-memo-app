pacman::p_load(shiny, shinyjs, shinyWidgets, magrittr, knitr,
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
      d$ou <- input$ou
      d$cop_year <- as.numeric(user_input$cop_year)
      d$inds <- datapackr::getMemoIndicators(cop_year =  d$cop_year, d2_session = user_input$d2_session)
      d$partners_agencies <- getAgencyPartnersMechsView(user_input$d2_session)
      d <- getPSUxIMData(d, d2_session = user_input$d2_session)

      if (NROW(d$data$datim_export) == 0) {
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

      d <- getMechanismTable(d, d2_session = user_input$d2_session)
      d <- getPrioritizationTable(d, d2_session = user_input$d2_session, include_no_prio = input$include_no_prio)
      d <- getPartnersTable(d)
      d <- getAgencyTable(d)
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

    d <- memo_data() %>% purrr::pluck("prio")

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

    d <- memo_data() %>% purrr::pluck("partner_table")

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

    d <- memo_data() %>% purrr::pluck("agency_table")

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

      library(flextable)
      library(officer)

      d <- memo_data()

      ou_name <- d$ou

      #Transform all zeros to dashes
      d$prio %<>%
        dplyr::mutate_if(is.numeric,
                         function(x) ifelse(x == 0, "-", formatC(x, format = "f", big.mark = ",", digits = 0)))

      style_para_prio <- fp_par(text.align = "right",
                              padding.right = 0.04,
                              padding.bottom = 0,
                              padding.top = 0,
                              line_spacing = 1)

      style_header_prio <- fp_par(text.align = "center",
                              padding.right = 0,
                              padding.bottom = 0,
                              padding.top = 0,
                              line_spacing = 1)


      header_old <- names(d$prio)
      header_new <- c(ou_name, ou_name, header_old[3:dim(d$prio)[2]])

      prio_table <- flextable(d$prio) %>%
        merge_v(., j = "Indicator") %>%
        delete_part(., part = "header") %>%
        add_header_row(., values = header_new) %>%
        add_header_row(., values = c(ou_name, ou_name, rep("SNU Prioritizations", (dim(d$prio)[2] - 2)))) %>%
        merge_h(., part = "header") %>%
        merge_v(., part = "header") %>%
        bg(., bg = "#CCC0D9", part = "header") %>%
        bg(., i = ~ Age == "Total", bg = "#E4DFEC", part = "body") %>% #Highlight total rows
        bold(., i = ~ Age == "Total", bold = TRUE, part = "body")  %>%
        bg(., j = "Indicator", bg = "#FFFFFF", part = "body") %>%
        bold(., j = "Indicator", bold = FALSE) %>%
        bold(., bold = TRUE, part = "header") %>%
        fontsize(., size = 7, part = "all") %>%
        style(., pr_p = style_header_prio, part = "header") %>%
        style(., pr_p = style_para_prio, part = "body") %>%
        align(., j = 1:2, align = "center") %>%  #Align first two columns center
        flextable::add_footer_lines(., values = paste("* Totals may be greater than the sum of categories due",
         "to activities outside of the SNU prioritization areas outlined above"))

      fontname <- "Arial"
      if (gdtools::font_family_exists(fontname)) {
        prio_table <- font(prio_table, fontname = fontname, part = "header")
      }

      doc <- read_docx()
      doc <- body_add_flextable(doc, value = prio_table)
      doc <- body_add_break(doc, pos = "after")

      #Partners tables
      d$partner_table %<>%
        dplyr::mutate_if(is.numeric,
                         function(x) ifelse(x == 0, "-", formatC(x, format = "f", big.mark = ",", digits = 0)))

      sub_heading <- names(d$partner_table)[4:length(d$partner_table)] %>%
        stringr::str_split(., " ") %>%
        purrr::map(purrr::pluck(2)) %>%
        unlist() %>%
        c("Agency", "Partner", "Mechanism", .)

      group_heading <- names(d$partner_table)[4:length(d$partner_table)] %>%
        stringr::str_split(., " ") %>%
        purrr::map(purrr::pluck(1)) %>%
        unlist() %>%
        c("Agency", "Partner", "Mechanism", .)

      chunks <- list(c(1:15), c(1:3, 16:26), c(1:3, 27:35), c(1:3, 36:44))

      renderPartnerTable <- function(chunk, d_table, group_heading) {

       partner_table <- flextable(d_table[, chunk]) %>%
          bg(., i = ~ Partner == "", bg = "#D3D3D3", part = "body") %>%
          bold(., i = ~ Partner == "", bold = TRUE) %>%
          delete_part(., part = "header") %>%
          add_header_row(., values = sub_heading[chunk]) %>%
          add_header_row(., top = TRUE, values = group_heading[chunk]) %>%
          merge_h(., part = "header") %>%
          merge_v(., part = "header")  %>%
          fontsize(., size = 7, part = "all") %>%
          style(., pr_p = style_para_prio, part = "body") %>%
          style(., pr_p = style_header_prio, part = "header") %>%
          width(., j = 1:3, 0.75) %>%
          width(., j = 4:(length(chunk)), 0.4)

        fontname <- "Arial"
        if (gdtools::font_family_exists(fontname)) {
          partner_table <- font(partner_table, fontname = fontname, part = "all")
        }

        partner_table
      }

      for (i in seq_along(chunks)) {
        chunk <- chunks[[i]]
        partner_table_ft <- renderPartnerTable(chunk = chunk, d_table = d$partner_table, group_heading = group_heading)
        doc <- body_add_flextable(doc, partner_table_ft)
        doc <- body_add_break(doc, pos = "after")
      }

      ##Agency tables
      d$agency_table %<>%
        dplyr::rename("Deduplicated Total" = "Total") %>%
        dplyr::mutate_if(
          is.numeric,
          function(x) ifelse(x == 0, "-", formatC(x, format = "f", big.mark = ",", digits = 0))
        )
      new_header <- c("Indicator", "Age",
      rep("Duplicated Agency Totals", dim(d$agency_table)[2] - 3),
      "Deduplicated Total")

      agency_table <- flextable(d$agency_table) %>%
        add_header_row(., top = TRUE, values = new_header) %>%
        merge_h(., part = "header") %>%
        merge_v(., part = "header")  %>%
        merge_v(., j = "Indicator") %>%
        bg(., bg = "#CCC0D9", part = "header") %>%
        bg(., i = ~ Age == "Total", bg = "#E4DFEC", part = "body") %>% #Highlight total rows
        bold(., i = ~ Age == "Total", bold = TRUE, part = "body")  %>%
        bg(., j = "Indicator", bg = "#FFFFFF", part = "body") %>%
        bold(., j = "Indicator", bold = FALSE) %>%
        bold(., bold = TRUE, part = "header") %>%
        fontsize(., size = 7, part = "all") %>%
        style(., pr_p = style_header_prio, part = "header") %>%
        style(., pr_p = style_para_prio, part = "body") %>%
        align(., j = 1:2, align = "center") %>%
        flextable::add_footer_lines(., values =  paste("* Agency totals cannot be presented as deduplicated totals.",
        "The deduplicated total may not be equal to the sum of the rows",
        "of the data presented due to deduplication adjustments."))

      doc <- body_add_flextable(doc, value = agency_table)

      print(doc, target = file)
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
      openxlsx::addWorksheet(wb, "Prioritization")
      openxlsx::writeDataTable(wb = wb,
                               sheet = "Prioritization", x = d$prio)
      openxlsx::addWorksheet(wb, "By Partner")
      openxlsx::writeDataTable(wb = wb,
                               sheet = "By Partner", x = d$partner_table)
      openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
      openxlsx::addWorksheet(wb, "By Agency")
      openxlsx::writeDataTable(wb = wb,
                               sheet = "By Agency", x = d$agency_table)
      openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
    }
  )

  output$pivot <- renderRpivotTable({
    d <- memo_data()

    if (!inherits(d, "error") & !is.null(d)) {

      if (is.null(d$data$by_psnuim)) {
        return(NULL)
        }

      PSNUxIM_pivot(d)

    } else {
      NULL
    }
  })

})
