#' Title getBaseURL()
#'
#' @return A base URL to be used throughout the application. If the BASE_URL 
#' environment variable is not set, this function will return https://www.datim.org, 
#' otherwise, the value of the environment variable. 
#' @export
#'
#' @examples
getBaseURL<-function() {
  if ( Sys.getenv("BASE_URL") !=  "" )  {
    return(Sys.getenv("BASE_URL")) } else {
      flog.warn("No BASE_URL environment variable found. Using www.datim.org")
      "https://www.datim.org/"
    }
  
}

getVersionInfo<-function() {
  
  
  currDCF <- read.dcf("DESCRIPTION")
  currVersion <- currDCF[1,"Version"]
  
  #paste0("Version: ",gert::git_branch(),"@",substr(gert::git_log(max=1)$commit,0,10)) %>% 
  paste0("Version: ",currVersion) %>% 
    paste('<div style="font-size:small;text-align: center;"><p>',.) %>% 
    paste(.,"</p></div>")
}


datapack_config<-function() {

  datapackr::COP21_datapacks_countries %>% 
    dplyr::arrange(datapack_name)
}


d2_analyticsResponse <- function(url,remapCols=TRUE,d2_session) {
  d <- jsonlite::fromJSON(content(GET(url, handle = d2_session$handle), "text"))
  if ( NROW(d$rows) > 0 ) {
    metadata <- do.call(rbind,
                        lapply(d$metaData$items,
                               data.frame, stringsAsFactors = FALSE)) %>% mutate(., from = row.names(.))
    remapMeta <-
      function(x) {
        plyr::mapvalues(x, metadata$from, metadata$name, warn_missing = FALSE)
      }
    
    d<-tibble::as_tibble(d$rows) %>% `names<-`(., d$headers$column)
    if(remapCols == TRUE) {
      d<-plyr::colwise(remapMeta)(d)
    }
    return(d) } else {
      return(NULL)
    }
}

indicatorOrder<-function(cop_year="2020") {
  
  if ( !(cop_year %in% c("2020","2021")) ) {
    stop("Unsupported COP year!")
  }
  
  if (cop_year == "2020") {
    
    inds<-tibble::tribble(
      ~ind,~options, ~in_partner_table,
      "HTS_INDEX","<15",TRUE,
      "HTS_INDEX","15+",TRUE,
      "HTS_INDEX","Total",FALSE,
      "HTS_TST","<15",TRUE,
      "HTS_TST","15+",TRUE,
      "HTS_TST","Total",FALSE,
      "HTS_TST_POS","<15",TRUE,
      "HTS_TST_POS","15+",TRUE,
      "HTS_TST_POS","Total",FALSE,
      "TX_NEW","<15",TRUE,
      "TX_NEW","15+",TRUE,
      "TX_NEW","Total",FALSE,
      "TX_CURR","<15",TRUE,
      "TX_CURR","15+",TRUE,
      "TX_CURR","Total",FALSE,
      "TX_PVLS","<15",TRUE,
      "TX_PVLS","15+",TRUE,
      "TX_PVLS","Total",FALSE,
      "CXCA_SCRN","Total",TRUE,
      "OVC_SERV","<18",TRUE,
      "OVC_SERV","18+",TRUE,
      "OVC_SERV","Total",FALSE,
      "OVC_HIVSTAT", "Total",TRUE,
      "PMTCT_STAT","<15",TRUE,
      "PMTCT_STAT","15+",TRUE,
      "PMTCT_STAT","Total",FALSE,
      "PMTCT_STAT_POS","<15",TRUE,
      "PMTCT_STAT_POS","15+",TRUE,
      "PMTCT_STAT_POS","Total",FALSE,
      "PMTCT_ART","<15",TRUE,
      "PMTCT_ART","15+",TRUE,
      "PMTCT_ART","Total",FALSE,
      "PMTCT_EID","Total",TRUE,
      "PP_PREV","<15",TRUE,
      "PP_PREV","15+",TRUE,
      "PP_PREV","Total",FALSE,
      "KP_PREV","Total",TRUE,
      "KP_MAT","Total",TRUE,
      "VMMC_CIRC","Total",TRUE,
      "HTS_SELF","<15",TRUE,
      "HTS_SELF","15+",TRUE,
      "HTS_SELF","Total",FALSE,
      "PrEP_NEW","Total",TRUE,
      "PrEP_CURR","Total",TRUE,
      "TB_STAT","<15",TRUE,
      "TB_STAT","15+",TRUE,
      "TB_STAT","Total",FALSE,
      "TB_ART","<15",TRUE,
      "TB_ART","15+",TRUE,
      "TB_ART","Total",FALSE,
      "TB_PREV","<15",TRUE,
      "TB_PREV","15+",TRUE,
      "TB_PREV","Total",FALSE,
      "TX_TB","<15",TRUE,
      "TX_TB","15+",TRUE,
      "TX_TB","Total",FALSE,
      "GEND_GBV","Total",TRUE)  
  }
  
  if (cop_year == "2021") {
    inds<-tibble::tribble(
      ~ind,~options, ~in_partner_table,
      "HTS_INDEX","<15",TRUE,
      "HTS_INDEX","15+",TRUE,
      "HTS_INDEX","Total",FALSE,
      "HTS_TST","<15",TRUE,
      "HTS_TST","15+",TRUE,
      "HTS_TST","Total",FALSE,
      "HTS_TST_POS","<15",TRUE,
      "HTS_TST_POS","15+",TRUE,
      "HTS_TST_POS","Total",FALSE,
      "TX_NEW","<15",TRUE,
      "TX_NEW","15+",TRUE,
      "TX_NEW","Total",FALSE,
      "TX_CURR","<15",TRUE,
      "TX_CURR","15+",TRUE,
      "TX_CURR","Total",FALSE,
      "TX_PVLS","<15",TRUE,
      "TX_PVLS","15+",TRUE,
      "TX_PVLS","Total",FALSE,
      "CXCA_SCRN","Total",TRUE,
      "OVC_SERV","<18",TRUE,
      "OVC_SERV","18+",TRUE,
      "OVC_SERV","Total",FALSE,
      "OVC_HIVSTAT", "Total",TRUE,
      "PMTCT_STAT","<15",TRUE,
      "PMTCT_STAT","15+",TRUE,
      "PMTCT_STAT","Total",FALSE,
      "PMTCT_STAT_POS","<15",TRUE,
      "PMTCT_STAT_POS","15+",TRUE,
      "PMTCT_STAT_POS","Total",FALSE,
      "PMTCT_ART","<15",TRUE,
      "PMTCT_ART","15+",TRUE,
      "PMTCT_ART","Total",FALSE,
      "PMTCT_EID","Total",TRUE,
      "PP_PREV","<15",TRUE,
      "PP_PREV","15+",TRUE,
      "PP_PREV","Total",FALSE,
      "KP_PREV","Total",TRUE,
      "KP_MAT","Total",TRUE,
      "VMMC_CIRC","Total",TRUE,
      "HTS_SELF","<15",TRUE,
      "HTS_SELF","15+",TRUE,
      "HTS_SELF","Total",FALSE,
      "PrEP_NEW","Total",TRUE,
      "PrEP_CURR","Total",TRUE,
      "TB_STAT","<15",TRUE,
      "TB_STAT","15+",TRUE,
      "TB_STAT","Total",FALSE,
      "TB_ART","<15",TRUE,
      "TB_ART","15+",TRUE,
      "TB_ART","Total",FALSE,
      "TB_PREV","<15",TRUE,
      "TB_PREV","15+",TRUE,
      "TB_PREV","Total",FALSE,
      "TX_TB","<15",TRUE,
      "TX_TB","15+",TRUE,
      "TX_TB","Total",FALSE,
      "GEND_GBV","Total",TRUE,
      "AGYW_PREV","Total",FALSE)  
  }
  
  inds
}

getIndicatorGroups<-function(cop_year = "2020") {
  if (cop_year == "2020") {
    "wWi08ToZ2gR"
  } else if (cop_year == "2021") {
    #TODO: Fix this once the COP21 indicator group has been finalized
    "TslxbFe3VUZ"
  }
}

getIndicatorMetadata<-function(cop_year,d2_session) {
  #Fetch indicators from the COP21 memo group
  #TODO: Make this work for both COP years.!
  
  if (cop_year == 2020) {
    ind_group <-"wWi08ToZ2gR"
  } else if (cop_year == 2021) {
    #TODO: Fix this with the real indicator group once it has been deployed to prod
    ind_group <-"TslxbFe3VUZ"
  } else {
    flog.info("Indicator group was not found")
    return(NULL)
  }
  inds <-
    datimutils::getIndicatorGroups(ind_group, 
                                   d2_session = d2_session, 
                                   fields = "indicators[id,name,numerator,denominator]") 
  
  
  if (class(inds) != "data.frame") { stop("No indicator metadata  was returned from DATIM") }
  
  inds
}

evaluateIndicators<-function(combis,values,inds) {
  
  indicators_empty<-data.frame("Indicator" = character(),
                               "N_OR_D" = character(),
                               "Age" = character(),
                               id = character(),
                               numerator = numeric(),
                               denominator = numeric(),
                               value  = numeric())
  
  this.des <-
    vapply(combis, function(x) {
      unlist(strsplit(x, "\\."))[[1]]
    }, FUN.VALUE = character(1))
  
  totals_df<-data.frame(exp = this.des,values=values,stringsAsFactors = FALSE) %>% 
    dplyr::group_by(exp) %>% 
    dplyr::summarise(values = sum(values)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(exp=paste0(exp,"}"))
  
  matches_indicator <- function(x) {
    agrepl(x, inds$numerator) |
      agrepl(x, inds$denominator)
  }
  
  
  matches <- this.des %>% 
    unique(.) %>% 
    purrr::map(.,matches_indicator) %>% 
    Reduce("|",.) %>% 
    dplyr::filter(inds,.)
  
  #Return something empty here if we have no indicator matches
  
  if (nrow(matches) == 0) {return(indicators_empty)}
  
  
  replaceCombisWithValues<-function(x,combis.this=combis,values.this=values) {
    stringi::stri_replace_all_fixed(x,
                                    combis.this, values.this, vectorize_all =
                                      FALSE)
  }
  
  replaceTotalsWithValues<-function(x) replaceCombisWithValues(x,combis=totals_df$exp,values=totals_df$values)
  
  replaceExpressionsWithZeros<-function(x) {
    expression.pattern<-"#\\{[a-zA-Z][a-zA-Z0-9]{10}(\\.[a-zA-Z][a-zA-Z0-9]{10})?\\}"
    gsub(expression.pattern, "0", x)
  }
  
  evaluateExpression<-function(exp) {
    vapply(exp,function(x) {eval(parse(text=x))},FUN.VALUE=double(1))
  }
  
  
  matches %>% purrr::modify_at(.,c("numerator","denominator"),replaceCombisWithValues) %>% 
    purrr::modify_at(.,c("numerator","denominator"),replaceTotalsWithValues) %>% 
    purrr::modify_at(.,c("numerator","denominator"),replaceExpressionsWithZeros) %>% 
    purrr::modify_at(.,c("numerator","denominator"),evaluateExpression) %>% 
    dplyr::mutate(value = numerator / denominator)
  
  
  
}
getMemoIndicators<-function(cop_year,d2_session) {
  
  ind_group<-getIndicatorGroups(cop_year)
  
  inds <- 
    datimutils::getIndicatorGroups(ind_group, 
                                   d2_session = d2_session, 
                                   fields = "indicators[id,shortName]") %>% 
    dplyr::rename(indicator_name = shortName) %>% 
    dplyr::mutate(indicator_name = stringr::str_replace_all(indicator_name,"COP2[01] ","")) %>%
    dplyr::mutate(indicator_name = stringr::str_replace_all(indicator_name,"Targets ","")) %>%
    dplyr::mutate(indicator_name = stringr::str_trim(indicator_name)) %>% 
    tidyr::separate("indicator_name",into=c("Indicator","Numerator","Age"),sep=" ") %>% 
    dplyr::mutate(Age = case_when(Age == "15-" ~ "<15",
                                  Age == "15+" ~ "15+",
                                  Age == "18-" ~"<18",
                                  Age == "18+" ~ "18+",
                                  TRUE ~ "Total")) %>% 
    dplyr::mutate( Age = case_when( Indicator %in% c("CXCA_SCRN","OVC_HIVSTAT","KP_PREV","PMTCT_EID","KP_MAT","VMMC_CIRC","PrEP_NEW","PrEP_CURR","GEND_GBV")  ~ "Total",
                                    TRUE ~ Age)) %>% 
    dplyr::select(-Numerator) %>% 
    suppressWarnings()
  
  if (class(inds) != "data.frame") { stop("No indicator metadata  was returned from DATIM")}
  
  if ( !all(unique(inds$Indicator) %in%  ( indicatorOrder(cop_year) %>% dplyr::pull(ind) %>% unique(.) ) ) ) {
    stop("Unknown indicators found in metadata response.")
  }
  
  inds
}

getExistingPrioritization<-function(psnus,cop_year,d2_session) {
    
    period<- paste0( cop_year,"Oct") 
     dx <-"r4zbW3owX9n"
      ous<-paste(psnus,sep="",collapse=";")
      
        prios<-datimutils::getAnalytics(dx="r4zbW3owX9n",pe_f =period, ou = ous,d2_session = d2_session ) 
        
          if (is.null(prios)) {
              return(data.frame("psnu_uid" = psnus,"prioritization" = "No Prioritization"))
          }
        
        
         prios %>% 
              dplyr::select(-Data) %>% 
              dplyr::rename("psnu_uid" = "Organisation unit",
                            "value" = "Value") %>% 
              dplyr::left_join(datapackr::prioritization_dict()) %>% 
              dplyr::select(psnu_uid,"prioritization" = "name") %>% 
              dplyr::mutate(prioritization = dplyr::case_when(is.na(prioritization) ~ 'No Prioritization',
                                                              TRUE ~ prioritization))
         
}

getPrioritizationTable <- function(d,d2_session, include_no_prio = TRUE) {


  inds <- d$inds
  
  df_cols<-tibble::tribble(
    ~id,~shortName,~col_name,
    "ATX2xv8PsrX","PPG Attained","Attained",
    "IzmZerN7tDN","PPG Scale-Up: Saturation", "Scale-Up: Saturation",
    "AHMMjoPYta6","PPG Scale Up: Aggressive", "Scale-Up: Aggressive",
    "b1X6pxMHgs6","PPG Sustained","Sustained",
    "pibJV72pMyW","PPG Centrally Supported","Centrally Supported",
    "CJYtvFbjeG2", "PPG No Prioritization","No Prioritization",
    "p0JrTY2hLii","PPG Not PEPFAR Supported","Not PEPFAR Supported"
  )
  
  df_rows<-indicatorOrder(d$cop_year) %>% dplyr::select(ind,options)
  
  df_base<-tidyr::crossing(df_rows,dplyr::select(df_cols,col_name)) %>% 
    dplyr::arrange(ind,options,col_name) %>% 
    dplyr::mutate(Value = 0) %>% 
    dplyr::rename("Indicator" = ind,
                  Age = options)
  
  country_uids<-datapack_config() %>% 
    dplyr::filter( `datapack_name` == d$ou) %>% 
    dplyr::pull(country_uids) %>% 
    unlist()
  
    psnus<-d$psnus
    
   df <- d$data$by_psnuim %>%  
         dplyr::group_by(`Indicator`,`Age`,`prioritization`) %>% 
         dplyr::summarise(Value = sum(value)) %>% 
         dplyr::ungroup() %>% 
         dplyr::rename("col_name" = "prioritization") %>% 
         dplyr::mutate(col_name = stringr::str_replace(col_name,"Scale-up","Scale-Up")) 

  df_totals<-df %>%
    dplyr::filter(Age != 'Total') %>% 
    group_by(Indicator,col_name) %>% 
    dplyr::summarise(Value = sum(Value)) %>% 
    dplyr::mutate(Age = "Total") %>% 
    dplyr::ungroup() %>% 
    dplyr::select(names(df))
  
    df_final<-dplyr::bind_rows(df,df_totals) %>% 
    dplyr::group_by(Indicator,Age,col_name) %>% 
    dplyr::summarise(Value = sum(Value)) %>% 
    dplyr::distinct() %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(col_name = factor(col_name,levels = df_cols$col_name)) %>% 
    dplyr::mutate(Indicator = factor(Indicator,levels = unique(df_rows$ind))) %>% 
    dplyr::arrange(Indicator,col_name) %>% 
    tidyr::pivot_wider(names_from = col_name ,values_from = "Value", values_fill = 0) %>% 
    suppressWarnings()
    
    # #Remove Not PEPFAR supported if its only zeros, otherwise, show this, since its potentially problematic
    # if (df_final %>%  dplyr::select("Not PEPFAR Supported") %>% sum(.,na.rm = TRUE) == 0) {
    #   df_final<-df_final %>%  select(-`Not PEPFAR Supported`)
    # }
    
    
    df_final %<>% 
    dplyr::mutate("Total" = rowSums(dplyr::across(where(is.numeric)),na.rm = TRUE)) %>% 
    dplyr::select("Indicator","Age",3:dim(.)[2])
    
    if (!include_no_prio & any("No Prioritization" %in% names(df_final))) {
      df_final  %<>% dplyr::select(-`No Prioritization`)
    }
  
   d$prio <- df_final
   
   return(d)
   
 }

getAgencyPartnersMechsView<-function(d2_session) {
  
  agencies_partners_cached_file<- "agencies_partners_mechs.rds"
  can_read_file<-file.access(agencies_partners_cached_file,mode=4) == 0
  
  if (can_read_file) {
    
    #Set a reasonable default here
    if (is.null(d2_session$max_cache_age)) {
      max_cache_age <- "1 day"
    } else {
      max_cache_age <- d2_session$max_cache_age
    }
    
    is_fresh <-
      lubridate::as.duration(lubridate::interval(Sys.time(), file.info(agencies_partners_cached_file)$mtime)) < lubridate::duration(max_cache_age)
    if (is_fresh) {
      flog.info(paste0("Using cached support file at ", agencies_partners_cached_file))
      partners_agencies <- readRDS(agencies_partners_cached_file)
    }
  }

 if (!exists("partners_agencies")) {
   
   dedupe_mechs<-tibble::tribble(
     ~mechuid,~mech_code,~Partner,~Agency,
     "xEzelmtHWPn","00000","Dedupe","Dedupe",
     "OM58NubPbx1","00001","Crosswalk dedupe","Crosswalk dedupe"
   )
   
   partners_agencies<-glue::glue("{d2_session$base_url}api/sqlViews/IMg2pQJRHCr/data.csv") %>% 
     httr::GET(., handle = d2_session$handle )  %>% 
     httr::content(.,"text") %>% 
     readr::read_csv(.) %>% 
     dplyr::select('Funding Mechanism' = mechname,
                   'Agency' = agencyname,
                   'Partner' = partnername,
                   mechuid) %>% 
     dplyr::mutate(mech_code = ( stringr::str_split(`Funding Mechanism`,"-") 
                                 %>% purrr::map(.,purrr::pluck(1)) 
                                 %>% unlist() 
                                 %>%  stringr::str_trim())) %>% 
     dplyr::select(mechuid,mech_code,`Partner`,'Agency') %>% 
     dplyr::bind_rows(.,dedupe_mechs)
   flog.info(paste0("Overwriting stale mechanisms view to ", agencies_partners_cached_file))
   saveRDS(partners_agencies, file = agencies_partners_cached_file)
 }

  partners_agencies

}

getDataByMechanism<-function(d,d2_session) {

  inds <- d$inds
  
  country_uids<-datapack_config() %>% 
    dplyr::filter( `datapack_name` == d$ou) %>% 
    dplyr::pull(country_uids) %>% 
    unlist()
  
  df<-datimutils::getAnalytics( "dimension=SH885jaRe0o",
                                dx=inds$id,
                                ou = country_uids,
                                pe_f = paste0(d$cop_year,"Oct"),
                                d2_session = d2_session
  ) 
  
  if (is.null(df) | NROW(df) == 0) {return(d)}
  
  
 df   %>% 
    dplyr::mutate(Value = as.numeric(Value)) %>% 
    dplyr::inner_join(inds,by=c(`Data` = "id")) %>% 
    dplyr::select(-Data) %>% 
    dplyr::inner_join(d$partners_agencies,by=c(`Funding Mechanism` = "mechuid")) %>% 
    dplyr::rename("Mechanism" = mech_code) %>% 
    dplyr::select("Indicator","Age","Mechanism","Partner","Agency","Value")
  
 d$d_mechs<-df
 
 return(d)
}

getPSUxIMData<-function(d,d2_session) {
  country_uids<-datapack_config() %>% 
    dplyr::filter( `datapack_name` == d$ou) %>% 
    dplyr::pull(country_uids) %>% 
    unlist()
  
  d$country_uids<-country_uids
  

  d$data$datim_export<-datapackr::getCOPDataFromDATIM(country_uids,
                                 d$cop_year,
                                 streams="mer_targets",
                                 d2_session = d2_session) 

  return(d)
}


#Prepares an memo indicator table from raw data elements/catcombos
getMechanismTable<-function(d,d2_session) {
  
  inds<-getIndicatorMetadata(d$cop_year,d2_session )
  
  d$psnus<-dplyr::bind_rows(datapackr::valid_PSNUs) %>% 
    dplyr::filter(country_uid %in% d$country_uids) %>% 
    dplyr::filter(!is.na(psnu_type)) %>% 
    dplyr::select(ou,country_name,snu1,psnu, psnu_uid)
  
  #Break up into 2048 character URLS (approximately)
  n_requests<-ceiling(nchar(paste(d$psnus$psnu_uid,sep="",collapse=";"))/2048)
  n_groups<-split(sample(d$psnus$psnu_uid),1:n_requests)
  
  prios<-n_groups %>% 
    purrr::map_dfr(function(x) getExistingPrioritization(x,d$cop_year,d2_session))
  
  
  d$data$by_psnuim <- d$data$datim_export %>%
    dplyr::select(dataElement,period,orgUnit,categoryOptionCombo,attributeOptionCombo,value) %>% 
    dplyr::mutate(combi =paste0("#{",dataElement,".", categoryOptionCombo,"}")) %>% 
    plyr::ddply(., plyr::.(orgUnit,attributeOptionCombo),
                function(x)
                  evaluateIndicators(x$combi, x$value,inds)) %>% 
    adornIndicators(.) %>% 
    dplyr::left_join(prios,by =c("orgUnit" = "psnu_uid" )) %>% 
    dplyr::mutate(prioritization = dplyr::case_when(is.na(prioritization) ~ "No Prioritization",
                                                    TRUE ~ prioritization)) %>% 
    dplyr::rename("Mechanism" = attributeOptionCombo) %>% 
    dplyr::inner_join(d$partners_agencies,by=c("Mechanism" = "mech_code")) %>% 
    dplyr::inner_join(d$psnus,by=c("orgUnit" = "psnu_uid" ))
  
  return(d)
}

adornIndicators<-function(x) {
   x %>% dplyr::mutate(name =  stringr::str_replace_all(name,"^COP2[01] Targets ","")) %>% 
    dplyr::mutate(name = stringr::str_trim(name)) %>% 
    tidyr::separate("name",into=c("Indicator","N_OR_D","Age"),sep=" ") %>%
    dplyr::mutate(Indicator = dplyr::case_when(Indicator == "GEND_GBV" & N_OR_D == "Physical" ~ "GEND_GBV Physical and Emotional Violence",
                                        Indicator == "GEND_GBV" & N_OR_D == "Sexual" ~ "GEND_GBV Sexual Violence",
                                        TRUE ~ Indicator)) %>% 
    dplyr::select(-"N_OR_D") %>% 
    dplyr::mutate(Age = dplyr::case_when(Age == "15-" ~ "<15",
                                  Age == "15+" ~ "15+",
                                  Age == "18-" ~"<18",
                                  Age == "18+" ~ "18+",
                                  TRUE ~ "Total")) %>% 
    dplyr::mutate( Age = dplyr::case_when( Indicator %in% c("CXCA_SCRN","OVC_HIVSTAT","KP_PREV","PMTCT_EID","KP_MAT","VMMC_CIRC","PrEP_NEW","PrEP_CURR","GEND_GBV")  ~ "Total",
                                    TRUE ~ Age))
}

getPartnersTable<-function(d,d2_session) {
  

  inds <- d$inds
  
  country_uids<-d$country_uids
  
  df<-d$data$by_psnuim
  
  if (is.null(df) | NROW(df) == 0) {return(d)}
  

  d_partners <- df   %>% 
    dplyr::group_by(Indicator,Age,Agency,Partner,Mechanism) %>% 
    dplyr::summarise(Value = sum(value)) %>% 
    dplyr::ungroup()

  #We need to pad for zeros
  df_rows<-indicatorOrder(d$cop_year) %>% 
    dplyr::filter(in_partner_table) %>% 
    dplyr::select(ind,options)
  
   d_base<-tidyr::crossing(df_rows,dplyr::distinct(unique(d_partners[,c("Agency","Partner","Mechanism")]))) %>% 
     dplyr::mutate(Value = 0) %>% 
     dplyr::rename("Indicator" = ind,
                   Age = options)
  
   #Calculate totals
   
  d_totals<-dplyr::bind_rows(d_base,d_partners) %>% 
    dplyr::group_by(`Indicator`,`Age`) %>% 
    dplyr::summarise(`Value` = sum(`Value`)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(`Partner` = 'Total',`Mechanism` = 'Total', Agency="Total")
  
  #Remove dedupe 
  d_partners %<>% dplyr::filter(!(`Mechanism` %in% c("00001","00000")))
  
  d_indicators<- indicatorOrder(d$cop_year) %>% 
    dplyr::filter(in_partner_table) %>%
    dplyr::select(ind,options) %>% 
    dplyr::mutate(indicator_name = factor(paste(ind, options)))

  #Put totals at the bottom of the table
  partner_levels <- c(sort(unique(d_partners$Partner)),"Total")
  agency_levels<-c(sort(unique(d_partners$Agency)),"Total")
  
  #Return the final data frame 
  d$partner_table<-dplyr::bind_rows(d_totals,d_partners) %>% 
    dplyr::mutate(indicator_name = paste(`Indicator`, `Age`)) %>% 
    #dplyr::mutate(indicator_name = factor(indicator_name,levels=unique(d_indicators$indicator_name))) %>% 
    dplyr::mutate(`Label` = indicator_name) %>% 
    dplyr::select(`Agency`,`Partner`,`Mechanism`,`Label`,`Value`) %>% 
    tidyr::pivot_wider(names_from = `Label`, values_from = `Value`, values_fill = 0) %>% 
    dplyr::select(`Agency`,`Partner`,`Mechanism`,d_indicators$indicator_name) %>% 
    dplyr::mutate(`Partner` = factor(Partner,levels = partner_levels),
                  `Agency` = factor(Agency,levels=agency_levels)) %>% 
    dplyr::arrange(`Agency`,`Partner`,`Mechanism`)
   
  
  return(d)
 
}

getAgencyTable<-function(d,d2_session) {
  
  
  inds <- d$inds
  
  country_uids<-d$country_uids
  
  df<-d$data$by_psnuim
  
  if (is.null(df) | NROW(df) == 0) {return(d)}
  
  df_rows<-indicatorOrder(d$cop_year) %>% dplyr::select(ind,options) 
  
  d_agency <- df   %>% 
    dplyr::group_by(Indicator,Age,Agency) %>% 
    dplyr::summarise(Value = sum(value)) %>% 
    dplyr::ungroup()
  
  agency_totals<-d_agency %>%
    group_by(Indicator,Age) %>% 
    dplyr::summarise(Value = sum(Value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate("Agency" = "Total") %>% 
    dplyr::select(names(d_agency))
  
  age_totals<-dplyr::bind_rows(d_agency,agency_totals) %>%
    dplyr::filter(Age != 'Total') %>% 
    group_by(Indicator,Agency) %>% 
    dplyr::summarise(Value = sum(Value)) %>% 
    dplyr::mutate(Age = "Total") %>% 
    dplyr::ungroup() %>% 
    dplyr::select(names(d_agency))
  
  #Remove dedupe
  d_agency %<>% dplyr::filter(stringr::str_detect(`Agency`,"[Dd]edupe",negate=TRUE))
  
  agency_levels <- c(sort(unique(d_agency$Agency)),"Total")
  
  #Return the final data frame 
  d$agency_table<-dplyr::bind_rows(d_agency, agency_totals,age_totals) %>%
    dplyr::mutate(Indicator = factor(Indicator,levels = unique(df_rows$ind))) %>% 
    dplyr::arrange(Indicator,Age) %>% 
    tidyr::pivot_wider(names_from = `Agency`, values_from = `Value`, values_fill = 0) %>% 
    dplyr::select(`Indicator`,Age,all_of(agency_levels)) %>% 
    dplyr::mutate(Indicator = factor(Indicator,levels = unique(df_rows$ind))) %>% 
    dplyr::arrange(`Indicator`,Age)
  
  return(d)
  
}

getOrgtunitNamefromUID<-function(uid, d2_session) {
    
    glue(d2_session$base_url,"api/organisationUnits/{uid}?fields=name") %>% 
      httr::GET(., handle = d2_session$handle) %>% 
      httr::content(.,"text") %>% 
      jsonlite::fromJSON(.) %>% 
      purrr::pluck("name")
}

PSNUxIM_pivot<-function(d){
  
  pivot<- d  %>%
    purrr::pluck("data") %>%
    purrr::pluck("by_psnuim") %>% 
    dplyr::select("Agency","Partner","Mechanism",
                  "Organisation unit"= "ou",
                  "Country"= "country_name",
                  "SNU1" ="snu1",
                  "PSNU" = "psnu",
                  "Indicator",
                  "Age",
                  "value")
  
  rpivotTable(data =  pivot  ,  rows = c( "Indicator", "Age"),
              vals = "value", aggregatorName = "Integer Sum", rendererName = "Table"
              , width="70%", height="700px")
}