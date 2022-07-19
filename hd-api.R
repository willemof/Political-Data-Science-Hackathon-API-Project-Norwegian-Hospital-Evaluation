#Health Directorate API/Data file
library(httr)
library(tinytex)
library(tidyverse)
library(janitor)
library(kableExtra)
library(rjstat)
library(dplyr)
library(stingr)


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

#for Loop that retrieves all quality indicators and saves them to data folder
#It's a 10 minute download.
httr_options("UTF-8")
for(i in 1:nrow(test)){
  
  tmp <- GET(test$fileUri[i],
             add_headers("Ocp-Apim-Subscription-Key" = key),
             add_headers("Cache-Control"= "no-cache")) %>% 
    content(as = "text") %>% 
    jsonlite::fromJSON(flatten = TRUE) # parsing to dataframe
  
  file.tmp <- paste0("./data/quality_indicators/",
                                str_extract(test$fileName[i], "^[0-9]+"),
                                ".csv")
  
  if(file.exists(file.tmp)){
    print(noquote(c(file.tmp, noquote("was skipped"))))
    next}
      write_csv(tmp$AttachmentDataRows, file.tmp)
      print(noquote(c(file.tmp, noquote("was not skipped"))))
      Sys.sleep(2+abs(rnorm(1)))
    
  }
#End of for loop

#makes qi list out of csv files
qi_files <- list.files("./data/quality_indicators/",
                       full.names = TRUE)
#combines qis into a single dataset
qi <- lapply(qi_files, read_csv, show_col_types = FALSE)
qi <- bind_rows(qi)

#cleaning
clean_qi<- clean_names(qi)

#filtering
filter_qi <- clean_qi %>%
  filter(period_type=="Årlig") %>%
  filter(quality_indicator_name=="Reinnleggelse blant eldre pasienter 30 dager etter utskrivning fra sykehus") %>%
  filter(parent_name == "Hele landet")

health_region_qi <- filter_qi %>%
filter(location_name == "Helse Sør-Øst RHF")

#generating a list of unique qi indicators
qi_name_list <- tibble(datasett = unique(filter_qi$quality_indicator_name))
write.csv(qi_name_list, file = "./qi_name_list.csv")


