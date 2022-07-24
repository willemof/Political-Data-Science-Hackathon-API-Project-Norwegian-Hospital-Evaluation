#This file is without a doubt the most definitive way of accessing SSB Data.
{#load settings, libraries
  options(encoding="UTF-8")
  library(httr)
  library(ggplot2)
  library(janitor)
  library(tidyverse)
  library(rjstat)
  library(tidyr)
  library(plotly)
  library(janitor)
  library(PxWebApiData)
  library(stringr)
  library(rlang)
  library(kableExtra)
  library(dplyr)
  
  
}
#Get SSB Data
#New method.. Simply enter the url(s) and let the code take you away!
#make urls
url_list <- c("https://data.ssb.no/api/v0/en/table/06464/",
              "https://data.ssb.no/api/v0/en/table/06922/",
              "https://data.ssb.no/api/v0/en/table/09548/")
ds_tables <- c()
for(i in 1:NROW(url_list)){
  ds_tables[i] <-  str_extract(url_list[i], "/+[0-9]+/") %>%
    str_remove_all("/")
}

#derives json queries from url_list using PxWebApiData package
query_list <- c()
for (i in 1:NROW(url_list)){
  query_list[i] <- ApiData(url_list[i], TRUE, returnApiQuery = TRUE, defaultJSONquery = c(1, -2, -1))
  t_query_list <- tibble(query_list)
}

file.list <- paste0("./data/individual_datasets/",
                    str_extract(ds_tables, "^[0-9]+"),".csv")
#for-loop generating merged ssb dataset
ssb_ds<- tibble()
for(i in 1:NROW(url_list)){
  if(file.exists(file.list[i])){
    print(noquote(c(file.list[i],noquote("was retrieved from the project folder. No download has been done,"),  
                    noquote("because the file already exists."))))
    d.tmp.list<-read_csv(file.list[i], col_names = TRUE)
    if(i==1) {
      ssb_ds <- d.tmp.list
    }
    if(i>1) {ssb_ds<-full_join(ssb_ds, d.tmp.list)
    }
    next
  }
  d.tmp.list <- c(c())
  d.tmp.list[[i]] <- POST(url_list[i],body=query_list[i],encode="json",verbose())
  d.tmp.list <- do.call(rbind, d.tmp.list)
  d.tmp.list <- POST(url_list[i], body= query_list[i],encode="json",verbose())
  d.tmp.list <- fromJSONstat(content(d.tmp.list, "text"))
  d.tmp.list <- do.call(rbind, d.tmp.list)
  write_csv(d.tmp.list, file.list[i])
  print(noquote(c(file.list[i], noquote("was saved."))))
  if(i==1) {
    ssb_ds <- d.tmp.list
  }
  if(i>1) {
    ssb_ds<-full_join(ssb_ds, d.tmp.list)
  }
  if(i==NROW(url_list)){
    write_csv(ssb_ds, file = "./data/merged_datasets/ssb_ds.csv")
  }
  Sys.sleep(0.1+abs(rnorm(1)))
}


#Health Directorate API/Data file



url <- "https://api.helsedirektoratet.no/innhold/kvalitetsindikatorer?spraak=true"
key <- "80ad2d7c471e4b8fad3b000d21b6ef41"

#Meta-dataset
#if statement that jumps to line 128 if files have already been generated.
if(file.exists("./data/merged_datasets/hd_qi.csv")){
  next
ds<-GET(url, 
        add_headers("Ocp-Apim-Subscription-Key" = key),
        add_headers("Cache-Control"= "no-cache")) %>% 
  content(as = "text") %>% # extracting the data
  jsonlite::fromJSON(flatten = TRUE) # parsing to dataframe

#Retrieving attachments connected to tmp
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
} #made an if statement (line 86) which skips all downloads if files have already been generated
#combines qis into a single dataset
if(file.exists("./data/merged_datasets/hd_qi.csv")){
  hd_qi <- read_csv("./data/merged_datasets/hd_qi.csv", col_names = TRUE)
  next
} else{
hd_qi <- lapply(qi_files, read_csv, show_col_types = FALSE)
hd_qi <- bind_rows(hd_qi)
write_csv(hd_qi, "./data/merged_datasets/hd_qi.csv")
}



# Apply janitor::clean_names
ssb_ds <- ssb_ds %>% 
  clean_names()
hd_qi <- hd_qi %>%
  clean_names()

#Need to merge data sets... language translation needed. but first, filter for yearly.
hd_qi <- hd_qi %>%
  filter(period_type=="Årlig")

hd_qi <- hd_qi %>%
  select.list(contains("RHF"))

unique_location_hd <- tibble(unique(hd_qi$parent_name))
unique_location_ssb <- tibble(unique(ssb_ds$health_region))

unique_location <- full_join(unique_location_hd, unique_location_ssb)

# Noticing that there is unique ids for older systems of classification.
# Considering that the presentation explicitly mentions 4 RHF, and 20 HF. That is what
# we initially will try form


hd_qi <- hd_qi %>%
  select(contains("RHF"))

hd_qi <- filter(hd_qi, parent_name==contains("RHF"))


merged_ds <- full_join(ssb_ds, clean_qi)
