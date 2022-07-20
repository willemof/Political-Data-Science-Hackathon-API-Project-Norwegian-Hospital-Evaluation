#Health Directorate API/Data file
library(httr)
library(tinytex)
library(tidyverse)
library(janitor)
library(kableExtra)
library(rjstat)
library(dplyr)
library(stringr)


url <- "https://api.helsedirektoratet.no/innhold/nki/kvalitetsindikatorer/"
key <- "1c50d76931ba48f69d177c18eaf3c6a8"

#Meta-dataset
ds<-GET(url, 
        add_headers("Ocp-Apim-Subscription-Key" = key),
        add_headers("Cache-Control"= "no-cache")) %>% 
  content(as = "text") %>% # extracting the data
  jsonlite::fromJSON(flatten = TRUE) # parsing to dataframe

#Retrieving attachments connected to ds
api.call <- do.call(rbind, ds$attachments)
api.call <- api.call %>% 
  filter(fileType == "application/json")

#for-Loop that retrieves all quality indicators and saves them to data folder
#It's a 10 minute download.
for(i in 1:nrow(api.call)){
  
  file.tmp <- paste0("./data/quality_indicators/",
                     str_extract(api.call$fileName[i], "^[0-9]+"),
                     ".csv")
  
  if(file.exists(file.tmp)){
    print(noquote(c(file.tmp, noquote("was skipped."))))
    next
    }
  
  tmp <- GET(api.call$fileUri[i],
             add_headers("Ocp-Apim-Subscription-Key" = key),
             add_headers("Cache-Control"= "no-cache")) %>% 
    content(as = "text") %>% 
    jsonlite::fromJSON(flatten = TRUE) # parsing to dataframe
  
      write_csv(tmp$AttachmentDataRows, file.tmp)
      print(noquote(c(file.tmp, noquote("was saved."))))
      Sys.sleep(2+abs(rnorm(1)))
    
  }
#End of for-loop

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

#generating a list of unique qi
qi_name_list <- tibble(datasett = unique(clean_qi$quality_indicator_name))
write.csv(qi_name_list, file = "./qi_name_list.csv")


write_csv(filter_qi, file = "./data/merged_datasets/filter_qi.csv")


ventetid_qi <- clean_qi %>%
  filter(quality_indicator_name=="Oppdaterte ventetider i Velg behandlingssted") %>%
  filter(parent_name == "Hele landet")


#create a list
#include indices
#create a for-loop
#fit linear regression for each index
#look at the hundreds of linear regressions and then look at what has the best fit (highest r2) 
#or AIC - akaike information criterion
#BIC- bayesian information criterion
# find out number of variables in optimal model
#endogeneity problem
