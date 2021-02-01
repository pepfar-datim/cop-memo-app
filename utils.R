require(config)
require(futile.logger)
require(glue)
require(dplyr)
require(tibble)
require(jsonlite)
require(httr)
require(tidyr)
require(stringr)
require(DT)
require(datapackr)
require(datimutils)


config <- config::get()


if ( file.access(config$log_path,2) == 1 ) {
  flog.appender(appender.file(config$log_path), name="cop_memo")
} else {
  flog.appender(appender.console(), name = "cop_memo")
}

getUserOperatingUnits<-function(uid) {
  
  ous<-datapackr::configFile %>% 
    dplyr::select(DataPack_name,model_uid,countryName,countryUID) %>% 
    dplyr::filter(!stringr::str_detect(countryName,"_Military")) %>% 
    dplyr::distinct() %>% 
    dplyr::arrange(countryName)
  
  if ( is.null(uid) ) {return("")} 
  
  if ( uid != "ybg3MO3hcf4") {
    ous %<>% 
      dplyr::filter(model_uid == uid) 
  }
  setNames(ous$countryUID,ous$countryName)
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


indicatorOrder<-function(cop_year="2020Oct") {
  
  if (cop_year == "2020Oct") {
    tibble::tribble(
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
  
}

  

memo_getPrioritizationTable <- function(ou_uid="cDGPF739ZZr", d2_session, cop_year = "2020Oct") {
  
  base_url<-d2_session$base_url
  
  if (cop_year == "2020Oct") {
    url<-glue::glue("{base_url}api/33/analytics?dimension=riR005xJPsS:IzmZerN7tDN;AHMMjoPYta6;b1X6pxMHgs6;
                  pibJV72pMyW;ATX2xv8PsrX;CJYtvFbjeG2;p0JrTY2hLii&dimension=dx:pyD3q4hsocw;
                  mpoYh9odYG5;DJF6GKEa9Jw;uzrCoPjSHAM;LejpyPTzSop;yoaC47zCSML;gVjB3hNi3r6;
                  o8zSyUaIPRR;TadWkOKgCYt;dIhPb5PaNak;niYlMjiztpL;egV0AFr0hcJ;fUeLws683gU;
                  tYNTb7iXfB5;kCfFLyrsr63;baC8xbo39Ih;H9jkgrFTECK;FcWaUSDQyaK;dBZCfaRJHpl;
                  wpBQqYCcUvl;BnLh5JaCvH9;yse3LYDict6;Z4B96JB9FPp;EPZB5449dks;mQrwwNQ61nF;
                  mFD2sZFAABk;rpayStjaa1a;AggcL3yaPE6;CwKwrnJIo6r;zdR0UbSXAvP;gg20xBdjq7V;
                  jHwvOp0wwkk;luxafh3nWng;sUwQqFDiuzq;dYKVOITB5ju;RrBIFT7aQDh;egyFeGZVxGf;
                  A4emI2AABjd;OxiC4DAZNxh;YgCYwt8Jshb;LdiiIrW3GAg&filter=ou:{ou_uid}
                  &filter=pe:2020Oct&displayProperty=SHORTNAME&skipData=false
                  &includeMetadataDetails=false") %>% 
      stringr::str_replace_all( "[\r\n]" , "") %>% 
      URLencode(.)     
  } else
  {
    stop("Fiscal year not implemented yet!")
  }

  
  
  df_cols<-tibble::tribble(
    ~id,~shortName,~col_name,
    "ATX2xv8PsrX","PPG Attained","Attained",
    "IzmZerN7tDN","PPG Scale-Up: Saturation", "Scale-Up: Saturation",
    "AHMMjoPYta6","PPG Scale Up: Aggressive", "Scale-Up: Aggressive",
    "b1X6pxMHgs6","PPG Sustained","Sustained",
    "pibJV72pMyW","PPG Centrally Supported","Centrally Supported",
    "CJYtvFbjeG2", "PPG No Prioritization","No Prioritization"
  )
  
  df_rows<-indicatorOrder() %>% dplyr::select(ind,options)
  
  df_base<-tidyr::crossing(df_rows,dplyr::select(df_cols,col_name)) %>% 
    dplyr::arrange(ind,options,col_name) %>% 
    dplyr::mutate(Value = 0) %>% 
    dplyr::rename("Indicator" = ind,
                  Age = options)
  
  df <- d2_analyticsResponse(url, d2_session = d2_session) 
  
  if (is.null(df)) {return(NULL)}
  
  df %<>% 
    dplyr::mutate(Value = as.numeric(Value)) %>% 
    dplyr::mutate(Data = stringr::str_replace_all(Data,"COP20 Targets ","")) %>% 
    dplyr::mutate(Data = stringr::str_trim(Data)) %>% 
    tidyr::separate("Data",into=c("Indicator","Numerator","Age"),sep=" ") %>% 
    dplyr::mutate(Age = case_when(Age == "15-" ~ "<15",
                                  Age == "15+" ~ "15+",
                                  Age == "18-" ~"<18",
                                  Age == "18+" ~ "18+",
                                  TRUE ~ "Total")) %>% 
    dplyr::mutate( Age = case_when( Indicator %in% c("CXCA_SCRN","OVC_HIVSTAT","KP_PREV","PMTCT_EID","KP_MAT","VMMC_CIRC","PrEP_NEW","PrEP_CURR","GEND_GBV")  ~ "Total",
                                    TRUE ~ Age)) %>% 
    dplyr::select(-Numerator) %>% 
    dplyr::rename("col_name" = `Planning Prioritization Set` ) %>% 
    dplyr::mutate(col_name = plyr::mapvalues(col_name,from=df_cols$shortName,to=df_cols$col_name))
  
  
  df_totals<-df %>% 
    group_by(Indicator,col_name) %>% 
    dplyr::summarise(Value = sum(Value)) %>% 
    dplyr::mutate(Age = "Total") %>% 
    dplyr::ungroup() %>% 
    dplyr::select(names(df))
  
  dplyr::bind_rows(df,df_totals,df_base) %>% 
    dplyr::group_by(Indicator,Age,col_name) %>% 
    dplyr::summarise(Value = sum(Value)) %>% 
    dplyr::distinct() %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(col_name = factor(col_name,levels = df_cols$col_name)) %>% 
    dplyr::mutate(Indicator = factor(Indicator,levels = unique(df_rows$ind))) %>% 
    dplyr::arrange(Indicator,col_name) %>% 
    tidyr::pivot_wider(names_from = col_name ,values_from = "Value") %>% 
    dplyr::mutate("Total *" = rowSums(.[3:7]) ) 
  
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
   partners_agencies<-glue::glue("{d2_session$base_url}api/sqlViews/IMg2pQJRHCr/data.csv") %>% 
     httr::GET(., handle = d2_session$handle )  %>% 
     httr::content(.,"text") %>% 
     readr::read_csv(.) %>% 
     dplyr::select('Funding Mechanism' = mechname,
                   'Partner' = partnername) %>% 
     dplyr::mutate(mech_code = ( stringr::str_split(`Funding Mechanism`,"-") 
                                 %>% purrr::map(.,purrr::pluck(1)) 
                                 %>% unlist() 
                                 %>%  stringr::str_trim())) %>% 
     dplyr::select(mech_code,`Partner`)
   flog.info(paste0("Overwriting stale mechanisms view to ", agencies_partners_cached_file))
   saveRDS(partners_agencies, file = agencies_partners_cached_file)
 }

  partners_agencies

}

memo_getPartnersTable<-function(ou_uid="cDGPF739ZZr", d2_session, cop_year = "2020Oct") {
  

  
  if ( cop_year == "2020Oct") {
    url<-glue::glue("{d2_session$base_url}api/33/analytics.json?dimension=dx:pyD3q4hsocw;mpoYh9odYG5;
DJF6GKEa9Jw;uzrCoPjSHAM;LejpyPTzSop;yoaC47zCSML;gVjB3hNi3r6;
o8zSyUaIPRR;TadWkOKgCYt;dIhPb5PaNak;niYlMjiztpL;egV0AFr0hcJ;fUeLws683gU;
tYNTb7iXfB5;kCfFLyrsr63;baC8xbo39Ih;H9jkgrFTECK;FcWaUSDQyaK;dBZCfaRJHpl;
wpBQqYCcUvl;BnLh5JaCvH9;yse3LYDict6;Z4B96JB9FPp;EPZB5449dks;mQrwwNQ61nF;
mFD2sZFAABk;rpayStjaa1a;AggcL3yaPE6;CwKwrnJIo6r;zdR0UbSXAvP;gg20xBdjq7V;jHwvOp0wwkk;
luxafh3nWng;sUwQqFDiuzq;dYKVOITB5ju;RrBIFT7aQDh;egyFeGZVxGf;A4emI2AABjd;OxiC4DAZNxh;YgCYwt8Jshb;
LdiiIrW3GAg&dimension=bw8KHXzxd9i:OO5qyDIwoMk;FPUgmtt8HRi;RGC9tURSc3W;cL6cHd6QJ5B;a7p2WOqhhzQ;PpCZbJvQyjL;r3bmih0XRCe;NLV6dy7BE2O
&dimension=SH885jaRe0o&filter=ou:{ou_uid}&filter=pe:2020Oct&displayProperty=SHORTNAME&skipData=false&includeMetadataDetails=false") %>% 
      stringr::str_replace_all( "[\r\n]" , "") %>% 
      URLencode(.)     
  } else {
    stop("Fiscal year not supported yet!")
  }

  
  df <- d2_analyticsResponse(url, d2_session = d2_session ) 
  
  if (is.null(df)) { return(NULL)}
  
  #Agencies/Partners view
  
  partners_agencies<-getAgencyPartnersMechsView(d2_session)
  
  d_partners<-df %>% 
    dplyr::mutate(Value = as.numeric(Value)) %>% 
    dplyr::mutate(Data = stringr::str_replace_all(Data,"COP20 Targets ","")) %>% 
    dplyr::mutate(Data = stringr::str_trim(Data)) %>% 
    tidyr::separate("Data",into=c("Indicator","Numerator","Age"),sep=" ") %>% 
    dplyr::mutate(Age = case_when(Age == "15-" ~ "<15",
                                  Age == "15+" ~ "15+",
                                  Age == "18-" ~"<18",
                                  Age == "18+" ~ "18+",
                                  TRUE ~ "Total")) %>% 
    dplyr::mutate( Age = case_when( Indicator %in% c("CXCA_SCRN","OVC_HIVSTAT","KP_PREV",
                                                     "PMTCT_EID","KP_MAT","VMMC_CIRC","PrEP_NEW","PrEP_CURR","GEND_GBV")  ~ "Total",
                                    TRUE ~ Age)) %>% 
    dplyr::mutate(mech_code  =  ( stringr::str_split(df$`Funding Mechanism`,"-") 
                                  %>% purrr::map(.,purrr::pluck(2)) 
                                  %>% unlist() 
                                  %>%  stringr::str_trim()) ) %>% 
    dplyr::inner_join(partners_agencies,by='mech_code') %>% 
    dplyr::select(`Funding Agency`,`Partner`,`Indicator`,`Age`,`Value`)

  #We need to pad for zeros
  df_rows<-indicatorOrder() %>% 
    dplyr::filter(in_partner_table) %>% 
    dplyr::select(ind,options)
  
  d_base<-tidyr::crossing(df_rows,dplyr::distinct(unique(d_partners[,1:2]))) %>% 
    dplyr::mutate(Value = 0) %>% 
    dplyr::rename("Indicator" = ind,
                  Age = options)
  
  d_totals<-dplyr::bind_rows(d_base,d_partners) %>% 
    dplyr::group_by(`Funding Agency`,`Indicator`,`Age`) %>% 
    dplyr::summarise(`Value` = sum(`Value`)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(`Partner` = '')
  
  d_partners <- d_partners %>% 
    dplyr::group_by(`Funding Agency`,`Partner`,`Indicator`,`Age`) %>% 
    dplyr::summarise(`Value` = sum(`Value`)) %>% 
    dplyr::ungroup()
  
  d_indicators<- indicatorOrder() %>% 
    dplyr::filter(in_partner_table) %>%
    dplyr::select(ind,options) %>% 
    dplyr::mutate(indicator_name = factor(paste(ind, options)))
  
  #Return the final data frame 
  dplyr::bind_rows(d_totals,d_partners) %>% 
    dplyr::mutate(indicator_name = paste(`Indicator`, `Age`)) %>% 
    dplyr::mutate(indicator_name = factor(indicator_name,levels=unique(d_indicators$indicator_name))) %>% 
    dplyr::mutate(`Label` = indicator_name) %>% 
  dplyr::arrange(`Funding Agency`,`Partner`,indicator_name) %>% 
    dplyr::select(`Funding Agency`,`Partner`,`Label`,`Value`) %>% 
    tidyr::pivot_wider(names_from = `Label`, values_from = `Value`, values_fill = 0) 
  
}
 
 getOrgtunitNamefromUID<-function(uid, d2_session) {
    
    glue(d2_session$base_url,"api/organisationUnits/{uid}?fields=name") %>% 
      httr::GET(., handle = d2_session$handle) %>% 
      httr::content(.,"text") %>% 
      jsonlite::fromJSON(.) %>% 
      purrr::pluck("name")
  }