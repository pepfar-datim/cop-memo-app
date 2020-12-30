library(shiny)
library(shinyjs)
require(shinyWidgets)
require(magrittr)
require(knitr)
require(kableExtra)
require(gdtools)
source("./utils.R")

shinyServer(function(input, output, session) {
  
  ready <- reactiveValues(ok = FALSE)
  
  fetchMemoData <- function() {
    
    shinyjs::disable("downloadReport")
    if (!user_input$authenticated | !ready$ok)  {
      return(NULL)
    } else {
      
      sendSweetAlert(
        session,
        title = "Retreiving data.",
        text = "This process may take a few minutes. Please wait.",
        btn_labels= NA
      )
      
      prio<-memo_getPrioritizationTable(input$ou, d2_session = user_input$d2_session)
      partners<-memo_getPartnersTable(input$ou, d2_session = user_input$d2_session)
      shinyjs::enable("fetch")
      shinyjs::enable("downloadXLSX")
      shinyjs::enable("downloadPDF")
      shinyjs::enable("downloadDOCX")
      closeSweetAlert(session)
      my_data<-list(prio=prio,partners=partners)
      if (is.null(my_data$prio) & is.null(my_data$partners)) {
        sendSweetAlert(
          session,
          title = "Oops!",
          text = "Sorry, I could not find any data for you!"
        )
      }
      return(my_data)
    }
    
  }
  
  memo_data <- reactive({
    fetchMemoData()
  })
  
  user_input <- reactiveValues(authenticated = FALSE, 
                               status = "",
                               base_url = "https://www.datim.org/",
                               d2_session = NULL)
  
  observeEvent(input$login_button, {
    
    tryCatch(  {  datimutils::loginToDATIM(base_url = input$base_url,
                                           username = input$user_name,
                                           password = input$password) },
               #This function throws an error if the login is not successful
               error=function(e) {
                 sendSweetAlert(
                   session,
                   title = "Login failed",
                   text = "Please check your username/password!",
                   type = "error")
                 flog.info(paste0("User ", input$user_name, " login failed."), name = "cop_memo")
               } )
    
     if ( exists("d2_default_session"))  {
       
       flog.info(paste0("User ", d2_default_session$me$userCredentials$username, " logged in to ", d2_default_session$base_url), name = "cop_memo")
       user_input$authenticated<-TRUE
       user_input$baseurl<- input$base_url
       user_input$d2_session<-d2_default_session$clone()

       }
    
  })
  
  output$prio_table <- DT::renderDataTable({
    
    d <- memo_data() %>% purrr::pluck("prio")
    
    if (!inherits(d, "error") & !is.null(d)) {
      print(NROW(d))
      DT::datatable(d,options = list(pageLength = 50, 
                                     columnDefs = list(list(className = 'dt-right', 
                                                            targets = 3:8)))) %>% 
        formatCurrency(3:8, '',digits =0)
      
    } else
    {
      NULL
    }
  })
  
  output$partners_table <- DT::renderDataTable({
    
    d <- memo_data() %>% purrr::pluck("partners")
    
    if (!inherits(d, "error") & !is.null(d)) {
      
      DT::datatable(d,options = list(pageLength = 50, 
                                     columnDefs = list(list(className = 'dt-right', 
                                                            targets = 3:dim(d)[2])))) %>% 
        formatCurrency(3:dim(d)[2], '',digits =0)
      
    } else
    {
      NULL
    }
  })
  
  observeEvent(input$fetch, {
    shinyjs::disable("fetch")
    ready$ok <- TRUE

  })
  
  observeEvent(input$logout,{
    flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
    ready$ok <- FALSE
    user_input$authenticated<-FALSE
    user_input$d2_session<-NULL
    gc()
    
  } )
  
  
  output$uiLogin <- renderUI({
    wellPanel(fluidRow(
      img(src = 'pepfar.png', align = "center"),
      h4(
        "Welcome to the COP20 Memo App. Please login with your DATIM credentials:"
      )
    ),
    fluidRow(
      selectInput(inputId = "base_url", label = "Server", 
      choices = c("https://www.datim.org/","https://cop-test.datim.org/"), 
      multiple = FALSE), 
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
          uiOutput("base_url"),
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
          tagList(wiki_url),
          tags$hr(),
          selectInput("ou", "Operating Unit",getUserOperatingUnits(user_input$d2_session$user_orgunit)),
          tags$hr(),
          actionButton("fetch","Get Data"),
          tags$hr(),
          disabled(downloadButton("downloadXLSX", "Download XLSX")),
          tags$hr(),
          disabled(downloadButton("downloadPDF", "Download PDF")),
          tags$hr(),
          disabled(downloadButton("downloadDOCX", "Download DOCX")),
          tags$hr(),
          actionButton("logout","Log out"),
          width = 2
        ),
        mainPanel(tabsetPanel(
          id = "main-panel",
          type = "tabs",
          tabPanel("Prioritization", DT::dataTableOutput("prio_table")),
          tabPanel("Partners/Agencies", DT::dataTableOutput("partners_table"))
        ))
      ))
    }
    
  })
  
  
  output$downloadPDF <- downloadHandler(
    filename = function() {
      
      prefix <-"cop_20_approval_memo_"
      date<-format(Sys.time(),"%Y%m%d_%H%M%S")
      paste0(paste(prefix,date,sep="_"),".pdf")
    },
    
    content = function(file) {
      
      src <- normalizePath('approval_memo_template.Rmd')
      img <- normalizePath('pepfar.png')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      file.copy(img, 'pepfar.png', overwrite = TRUE)
      
      library(rmarkdown)
      out <- rmarkdown::render('report.Rmd', pdf_document(latex_engine = "xelatex"))
      file.rename(out, file)
    }
  )
  
  
  output$downloadDOCX <- downloadHandler(
    filename = function() {
      
      prefix <-"cop_20_approval_memo_"
      date<-format(Sys.time(),"%Y%m%d_%H%M%S")
      paste0(paste(prefix,date,sep="_"),".docx")
    },
    
    content = function(file) {
      
      library(flextable)
      library(officer)
      
      d <- memo_data()
      
      ou_name<-getOrgtunitNamefromUID(input$ou,user_input$d2_session)

      #Transform all zeros to dashes
      d$prio %<>% 
        dplyr::mutate_if(is.numeric, 
                         function(x) ifelse(x == 0 ,"-",formatC(x, format="f", big.mark=",",digits = 0))) 
      
      style_para_prio<-fp_par(text.align = "right",
                              padding.right = 0.04,
                              padding.bottom = 0,
                              padding.top = 0,
                              line_spacing = 1)
      
      style_header_prio<-fp_par(text.align = "center",
                              padding.right = 0,
                              padding.bottom = 0,
                              padding.top = 0,
                              line_spacing = 1)
      
      
      header_old<-names(d$prio)
      header_new<-c(ou_name,ou_name,header_old[3:9])
      
      prio_table<-flextable(d$prio) %>% 

        merge_v(.,j="Indicator") %>% 
        delete_part(.,part = "header") %>% 
        add_header_row(.,values = header_new) %>% 
        add_header_row(., values = c(ou_name, ou_name,rep("SNU Prioritizations",7))) %>% 
        merge_h(., part = "header") %>% 
        merge_v(.,part="header") %>% 
        bg(.,bg = "#CCC0D9", part = "header") %>% 
        bg(., i = ~ Age == "Total", bg = "#E4DFEC", part = "body") %>% #Highlight total rows
        bold(., i = ~ Age == "Total", bold = TRUE, part = "body")  %>% 
        bg(.,j= "Indicator", bg = "#FFFFFF" , part="body") %>% 
        bold(., j = "Indicator", bold = FALSE) %>% 
        bold(.,bold = TRUE,part = "header") %>% 
        fontsize(., size = 7, part = "all") %>%
        style(.,pr_p = style_header_prio,part="header") %>% 
        style(.,pr_p = style_para_prio,part = "body") %>%
        align(.,j=1:2,align = "center") %>%  #Align first two columns center
        flextable::add_footer_lines(.,values="* Totals may be greater than the sum of categories due to activities outside of the SNU prioritization areas outlined above")
      
      fontname<-"Arial"
      if ( gdtools::font_family_exists(fontname) ) {
        prio_table <- font(prio_table,fontname = fontname,part = "header") 
      } 
        
      doc <- read_docx()
      doc<-body_add_flextable(doc,value=prio_table)
      doc<-body_add_break(doc,pos="after")
      
      #Partners tables
      sub_heading<-names(d$partners)[3:length(d$partners)] %>% 
        stringr::str_split(.," ") %>% 
        purrr::map(purrr::pluck(2)) %>%
        unlist() %>% 
        c("Funding Agency","Partner",.)
      
      group_heading<-names(d$partners)[3:length(d$partners)] %>% 
        stringr::str_split(.," ") %>% 
        purrr::map(purrr::pluck(1)) %>% 
        unlist() %>% 
        c("Funding Agency","Partner",.)
      
      chunks<-list(c(1:14),c(1:2,15:25),c(1:2,26:34),c(1:2,35:43))
      
      renderPartnerTable<-function(chunk) {
        
       partner_table<- flextable(d$partners[,chunk]) %>% 
          bg(., i = ~ Partner == "", bg = "#D3D3D3", part = "body") %>% 
          bold(.,i = ~ Partner == "", bold=TRUE) %>% 
          delete_part(.,part = "header") %>% 
          add_header_row(.,values=sub_heading[chunk]) %>% 
          add_header_row(.,top = TRUE,values = group_heading[chunk] ) %>% 
          merge_h(.,part="header") %>% 
          merge_v(.,part = "header")  %>% 
          fontsize(., size = 7, part = "all") %>% 
          style(.,pr_p = style_para_prio,part = "body") %>% 
          width(.,j=1:2,0.75) %>% 
          width(.,j=3:(length(chunk)-2),0.4)
        
        fontname<-"Arial"
        if ( gdtools::font_family_exists(fontname) ) {
          partner_table <- font(partner_table,fontname = fontname, part = "all") 
        } 
        
        partner_table
      }

      for (i in 1:length(chunks)) {
        chunk<-chunks[[i]]
        partner_table_ft<-renderPartnerTable(chunk = chunk)
        doc<-body_add_flextable(doc,partner_table_ft)
        doc<-body_add_break(doc,pos="after")
      }
      
      print(doc,target=file)
    }
  )
  
  
  output$downloadXLSX <- downloadHandler(
    filename = function() {
      
      
      prefix <-"cop_20_approval_memo_"
      date<-format(Sys.time(),"%Y%m%d_%H%M%S")
      paste0(paste(prefix,date,sep="_"),".xlsx")
      
    },
    content = function(file) {
      
      d <- memo_data()
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb,"Prioritization")
      openxlsx::writeDataTable(wb = wb,
                               sheet = "Prioritization",x = d$prio)
      
      
      openxlsx::addWorksheet(wb,"Partners_Agencies")
      openxlsx::writeDataTable(wb = wb,
                               sheet = "Partners_Agencies",x = d$partners)
      openxlsx::saveWorkbook(wb,file=file,overwrite = TRUE)
    }
  )
  
})


