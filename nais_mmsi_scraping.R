#' Scrapes call sign and mmsi from USCG NAIS website using list of US Official numbers
#' from the CPS permit data I collected earlier.
#'

library(tidyverse)
library(rvest)
library(curl)

id <- cps.permits$CG_num

cps.permits$mmsi <- "NA"

for(i in 1:length(id)) {
  url <- str_c("https://www.navcen.uscg.gov/aisSearch/dbo_aisVessels_list.php?q=(US%20Official%20No~contains~", id[i], ")&f=all#skipdata")
  url
  
  html.table <- read_html(url) %>%
    html_node("table") %>%
    html_table(fill = T) %>%
    slice(13:14)
  
  names(html.table) <- html.table[1,]
  html.table <- html.table[-1,]
  if(is.null(html.table$`MMSI:`) == T) {
    cps.permits[cps.permits$CG_num == id[i],]$mmsi <- "NA"
  }
  else {
    cps.permits[cps.permits$CG_num == id[i],]$mmsi <- html.table$'MMSI:'
  }
}

cps.permits$mmsi <- as.integer(cps.permits$mmsi)
