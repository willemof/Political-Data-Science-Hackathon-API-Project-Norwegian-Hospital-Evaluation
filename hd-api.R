#I have found out though RMarkdown is nice, R-scripts are easier to read when 
#the code is the focus. Not to difficult to make the adjustment into RMarkdown
#further down the line.

#But without further ado, I will right now try to replicate the code that Anton
#made, in the hopes to get some meaningful data out of the HealthDirectorateAPI

###### Health Directorate API replication ######

## Some case-specific info

#We got to use old-fashioned `httr`. 
# https://www.helsedirektoratet.no/om-oss/apne-data-api/hvordan-finne-frem-i-innholdet) 
# ^- gives some indication of the arguments for the endpoint. 
#To register for api - https://www.helsedirektoratet.no/om-oss/apne-data-api/fa-tilgang-til-helsedirektoratets-api-tjeneste-hapi

#Since the API requires authorization, you have to add the "headers" argument as well. 
library(httr)
library(tinytex)
library(tidyverse)
library(janitor)
library(kableExtra)
library(rjstat)
url <- "https://api.helsedirektoratet.no/innhold/nki/kvalitetsindikatorer/"
key <- "1c50d76931ba48f69d177c18eaf3c6a8"

ds<-GET(url, 
        add_headers("Ocp-Apim-Subscription-Key" = key),
        add_headers("Cache-Control"= "no-cache")) %>% 
  content(as = "text") %>% # extracting the data
  jsonlite::fromJSON(flatten = TRUE) # parsing to dataframe

test <- do.call(rbind, ds$attachments)
test <- test %>% 
  filter(fileType == "application/json")

for(i in 1:nrow(test)){
  
  tmp <- GET(test$fileUri[i],
      add_headers("Ocp-Apim-Subscription-Key" = key),
      add_headers("Cache-Control"= "no-cache")) %>% 
    content(as = "text") %>% 
    jsonlite::fromJSON(flatten = TRUE) # parsing to dataframe
  
  write_csv(tmp$AttachmentDataRows, 
            file = paste0("./data/quality_indicators/",
                          str_extract(test$fileName[i], "^[0-9]+"),
                          ".csv"))
 
  
  Sys.sleep(2+abs(rnorm(1)))
}

qi_files <- list.files("./data/quality_indicators/",
                       full.names = TRUE)

qi <- lapply(qi_files, read_csv, show_col_types = FALSE)

qi <- bind_rows(qi)

clean_qi<- clean_names(qi)

filter_qi <- clean_qi %>%
  filter(period_type=="Årlig") %>%
  filter(quality_indicator_name=="Reinnleggelse blant eldre pasienter 30 dager etter utskrivning fra sykehus") %>%
  filter(parent_name == "Hele landet") %>% 
  filter(location_name == "Helse Sør-Øst RHF")

dataset_name <- tibble(datasett = unique(filter_qi$quality_indicator_name))

write.csv(dataset_name, file = "./datasett_name.csv")

clean_qi$period_type

url2 <- "https://api.helsedirektoratet.no/innhold/nki/kvalitetsindikatorer/0003-0010-5/data?contentType=application/json"

dg<-GET(url2, 
        add_headers("Ocp-Apim-Subscription-Key" = key),
        add_headers("Cache-Control"= "no-cache")) %>% 
  content(as = "text") %>% # extracting the data
  jsonlite::fromJSON(flatten = TRUE)

dge<-dg$AttachmentDataRows 

dge <-   clean_names(dge)

dge_on <- dge %>%
  filter(parent_name=="Hele landet") %>%
  filter(period_type=="Årlig") %>%
  filter(qualiter_indicator_name=="Sykehusopphold – ventetid")

group_mean <- aggregate(location_id ~ location_name, data = dge_only_norway, mean)
group_mean <- aggregate(dge_only_norway$location_id ~ dge_only_norway$location_name, data = dge_only_norway, mean)


