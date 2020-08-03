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


config <- config::get()
options("baseurl" = config$baseurl)
flog.appender(appender.file(config$log_path), name="cop_memo")

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

DHISLogin <- function(baseurl, username, password) {
  httr::set_config(httr::config(http_version = 0))
  url <- URLencode(URL = paste0(config$baseurl, "api/me"))
  #Logging in here will give us a cookie to reuse
  r <- httr::GET(url,
                 httr::authenticate(username, password),
                 httr::timeout(60))
  if (r$status != 200L) {
    return(FALSE)
  } else {
    me <- jsonlite::fromJSON(httr::content(r, as = "text"))
    options("organisationUnit" = me$organisationUnits$id)
    return(TRUE)
  }
}

d2_analyticsResponse <- function(url,remapCols=TRUE) {
  d <- jsonlite::fromJSON(content(GET(url), "text"))
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

memo_getPrioritizationTable <- function(ou_uid="cDGPF739ZZr") {
  
  base_url<-config$baseurl
  
  url<-glue::glue("{base_url}api/29/analytics?dimension=riR005xJPsS:IzmZerN7tDN;AHMMjoPYta6;b1X6pxMHgs6;
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
  
  
  df_cols<-tibble::tribble(
    ~id,~shortName,~col_name,
    "ATX2xv8PsrX","PPG Attained","Attained",
    "IzmZerN7tDN","PPG Scale-Up: Saturation", "Scale-Up: Saturation",
    "AHMMjoPYta6","PPG Scale Up: Aggressive", "Scale-Up: Aggressive",
    "b1X6pxMHgs6","PPG Sustained","Sustained",
    "pibJV72pMyW","PPG Centrally Supported","Centrally Supported",
    "CJYtvFbjeG2", "PPG No Prioritization","No Prioritization"
  )
  
  df_rows<-tibble::tribble(
    ~ind,~options,
    "HTS_INDEX","<15",
    "HTS_INDEX","15+",
    "HTS_INDEX","Total",
    "HTS_TST","<15",
    "HTS_TST","15+",
    "HTS_TST","Total",
    "HTS_TST_POS","<15",
    "HTS_TST_POS","15+",
    "HTS_TST_POS","Total",
    "TX_NEW","<15",
    "TX_NEW","15+",
    "TX_NEW","Total",
    "TX_CURR","<15",
    "TX_CURR","15+",
    "TX_CURR","Total",
    "TX_PVLS","<15",
    "TX_PVLS","15+",
    "TX_PVLS","Total",
    "CXCA_SCRN","Total",
    "OVC_SERV","<18",
    "OVC_SERV","18+",
    "OVC_SERV","18+",
    "OVC_SERV","Total",
    "OVC_HIVSTAT", "Total",
    "PMTCT_STAT","<15",
    "PMTCT_STAT","15+",
    "PMTCT_STAT","Total",
    "PMTCT_STAT_POS","<15",
    "PMTCT_STAT_POS","15+",
    "PMTCT_STAT_POS","Total",
    "PMTCT_ART","<15",
    "PMTCT_ART","15+",
    "PMTCT_ART","Total",
    "PMTCT_EID","Total",
    "PP_PREV","<15",
    "PP_PREV","15+",
    "PP_PREV","Total",
    "KP_PREV","Total",
    "KP_MAT","Total",
    "VMMC_CIRC","Total",
    "HTS_SELF","<15",
    "HTS_SELF","15+",
    "HTS_SELF","Total",
    "PrEP_NEW","Total",
    "PrEP_CURR","Total",
    "TB_STAT","<15",
    "TB_STAT","15+",
    "TB_STAT","Total",
    "TB_ART","<15",
    "TB_ART","15+",
    "TB_ART","Total",
    "TB_PREV","<15",
    "TB_PREV","15+",
    "TB_PREV","Total",
    "TX_TB","<15",
    "TX_TB","15+",
    "TX_TB","Total",
    "GEND_GBV","Total")
  
  df_base<-tidyr::crossing(df_rows,dplyr::select(df_cols,col_name)) %>% 
    dplyr::arrange(ind,options,col_name) %>% 
    dplyr::mutate(Value = 0) %>% 
    dplyr::rename("Indicator" = ind,
                  Age = options)
  
  df <- d2_analyticsResponse(url) 
  
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

memo_getPartnersTable<-function(ou_uid="cDGPF739ZZr") {
  
  base_url<-config$baseurl
  
  url<-glue::glue("{base_url}api/29/analytics.json?dimension=dx:pyD3q4hsocw;mpoYh9odYG5;
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
  
  df <- d2_analyticsResponse(url) 
  
  if (is.null(df)) { return(NULL)}
  
  df %>% 
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
    tidyr::pivot_wider(names_from = c("Indicator", "Age") ,values_from = "Value")
  
  
}
