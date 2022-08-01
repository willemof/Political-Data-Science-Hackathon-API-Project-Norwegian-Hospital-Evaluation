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
  library(stringr)
  library(furrr)
  
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

  query1<-  ApiData(url_list[1], HelseReg = TRUE, 
                    HelseRegnKost = TRUE,
                    HelseRegnFunk = TRUE,
                    Tid= c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12),
                    ContentsCode = TRUE,
                    returnApiQuery = TRUE )
  
  query2<-  ApiData(url_list[2], HelseReg = TRUE, 
                    HelseTjenomr = TRUE,
                    Tid= c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12),
                    ContentsCode = TRUE,
                    returnApiQuery = TRUE )
  
  query3<-  ApiData(url_list[3], HelseReg = TRUE, 
                    HelseTjenomr = TRUE,
                    Yrke = TRUE,
                    Tid= c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12),
                    ContentsCode = TRUE,
                    returnApiQuery = TRUE )
  

  
  query_list <- c(query1, query2, query3)

file.list <- paste0("./data/individual_datasets/",
                    str_extract(ds_tables, "^[0-9]+"),".csv")
#for-loop generating merged ssb dataset
ssb_ds<- tibble()

{  
  if(file.exists("./data/merged_datasets/ssb_ds.csv")){
  ssb_ds<- read_csv("./data/merged_datasets/ssb_ds.csv")
} 
  if(file.exists("./data/merged_datasets/ssb_ds.csv") == FALSE){ 
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
    if(i==NROW(url_list)){ssb_ds<-clean_names(ssb_ds)
    write_csv(ssb_ds, file = "./data/merged_datasets/ssb_ds.csv")
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
    
    ssb_ds<-clean_names(ssb_ds)
    
    write_csv(ssb_ds, file = "./data/merged_datasets/ssb_ds.csv")
  }
  Sys.sleep(0.1+abs(rnorm(1)))
}
}
}
 #end of if statement that generates ssb_ds
#Health Directorate API/Data file



url <- "https://api.helsedirektoratet.no/innhold/kvalitetsindikatorer"
key <- "80ad2d7c471e4b8fad3b000d21b6ef41"

#Meta-dataset
#if statement that jumps to line 136 if files have already been generated.
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
}
if(file.exists("./data/merged_datasets/hd_qi.csv")==FALSE){
hd_qi <- lapply(qi_files, read_csv, show_col_types = FALSE)
hd_qi <- bind_rows(hd_qi)

hd_qi <- hd_qi %>% 
  clean_names()  # used to filter for årlig, but removed since it needs to include fristbrudd_region and which does not have data on årlig
hd_qi <- hd_qi %>%
  mutate(rhf = parent_name)
hd_qi <- hd_qi %>%
  filter(grepl('RHF', rhf))
hd_qi <- hd_qi %>%
  mutate(hf = location_name)
hd_qi <- hd_qi %>%
  filter(grepl('HF', hf))

hd_qi <- hd_qi %>% 
  rename(year = time_from) %>%
  select(-(time_to))
hd_qi$year <- format(as.Date(hd_qi$year), "%Y")
hd_qi <- hd_qi %>%
  filter(year>=2010)
write_csv(hd_qi, "./data/merged_datasets/hd_qi.csv")
}
###################end of if chain for hd-qi 
#removing objects that wont be used after data aquisition
remove(ds_tables, file.list, key, query_list, query1, query2, query3, url,url_list, i)
##treating ssb_ds
#using _tmp to add in a variable for when the location name contains RHF
#
#
#
#
#
# Filtering and datawrangling starts here
#
#
#
#
#
#
#
#
#
ssb_ds_tmp <- ssb_ds
ssb_ds_tmp <- ssb_ds_tmp %>%
  mutate(rhf = health_region) %>%
  filter(str_extract("RHF"))
  filter(grepl("RHF" & "Helse", rhf))
ssb_ds <- full_join(ssb_ds_tmp, ssb_ds)

str_extract(ssb_ds$health_region[240], "[^0-9]+")
str_extract(ssb_ds$health_region[240],"+RHF$")


#if we use the same function for HF the function will include RHF since HF is contained
#within RHF. So we use an if statement that jumps over every RHF.
ssb_ds_tmp <- ssb_ds_tmp %>%
mutate(hf = NA)
ssb_ds <- full_join(ssb_ds_tmp, ssb_ds)
for(i in 1:NROW(ssb_ds)){
  if (grepl("RHF", ssb_ds$rhf[i]) == TRUE) {
    }  else {
      if (grepl("HF", ssb_ds$health_region[i]) == TRUE) {
      ssb_ds$hf[i] = ssb_ds$health_region[i]
      }
  }
}

#The SSB data uses all caps for the older classification of RHF, today there is only 4.
ssb_ds_tmp <- ssb_ds %>%
mutate(rhf_caps = NA)
ssb_ds <- full_join(ssb_ds_tmp, ssb_ds)
ssb_ds_tmp <- ssb_ds %>%
  mutate(rhf_allcaps = C(""))
ssb_ds <- full_join(ssb_ds_tmp, ssb_ds)

#if location data is all caps, put it under var allcaps, if it isnt rhf_caps
for(i in 1:NROW(ssb_ds)){
if((ssb_ds$health_region[i]) == str_to_upper(ssb_ds$health_region[i])) {
  ssb_ds$rhf_allcaps[i] = ssb_ds$health_region[i]
  } else {ssb_ds$rhf_caps[i] = ssb_ds$health_region[i]
  }
}

#making new column that is only for curewnt RHF - rhf_caps_4cat
ssb_ds_tmp <- ssb_ds %>%
  mutate(rhf_caps_4cat = NA)
ssb_ds <- full_join(ssb_ds_tmp, ssb_ds)



for(i in 1:NROW(ssb_ds)){
  if (ssb_ds$rhf_allcaps[i] == ssb_ds$health_region[i]) {
  } else {
    ssb_ds$rhf_caps_4cat[i] = ssb_ds$health_region[i]
    
  }
}

for(i in 1:NROW(ssb_ds)){
  if (is.na(ssb_ds$rhf_caps[i])) {
  } else {
  if ((ssb_ds$rhf_caps[i]) == (ssb_ds$health_region[i])) {
  } else {
    if (grepl("RHF", ssb_ds$health_region[i]) == TRUE) {
      ssb_ds$rhf_caps_4cat[i] = ssb_ds$health_region[i]
    }
  }
  }
}

for(i in 1:NROW(ssb_ds)){
  if(is.na(ssb_ds$rhf_caps[i])){
    ssb_ds$rhf_caps[i]=c("")
  } else {
    ssb_ds$rhf_caps[i]=NA
  }
}

#quick for-loop to turn a white space character into NA, and NA to white space.
for(i in 1:NROW(ssb_ds)){
  if(is.na(ssb_ds$rhf[i])){
    ssb_ds$rhf[i]=c("")
  } else {
    ssb_ds$rhf[i]=NA
  }
}

unique_location_ssb_3 <- tibble(unique(ssb_ds$rhf_caps_4cat))



ssb_ds<- filter(ssb_ds$rhf, grepl("RHF", health_region))

ssb_ds <- ssb_ds %>%
  mutate(hf = health_region)


for(i in 1:NROW(ssb_ds)){
  if(str_length(str_extract(ssb_ds$health_region[i],"[A-Z]"))>5) {
    rhf_caps[i] = str_extract(ssb_ds$health_region[i],"[A-Z]")
  }
}


ssb_ds <- ssb_ds %>%
  mutate(hf = health_region) 
ssb_ds$hf <- ssb_ds$hf %>%
  filter(grepl('HF', hf))

  filter(grepl("RHF", hf, fixed = TRUE))

unique_parent_hd <- tibble(unique(hd_qi$parent_name))
unique_location_hd <- tibble(unique(hd_qi$location_name))
unique_location_ssb <- tibble(unique(ssb_ds$health_region))
unique_location_ssb_2 <- tibble(unique(ssb_ds$rhf))

unique_location <- full_join(unique_location_ssb, unique_location_ssb_2)

# Noticing that there is unique ids for older systems of classification.
# Considering that the presentation explicitly mentions 4 RHF, and 20 HF. That is what
# we initially will try form


hd_qi_f <- hd_qi %>%
  filter(grepl('HF', location_name))
unique_location_hd_f <- tibble(unique(hd_qi_f$location_name))


ssb_ds_f <- ssb_ds %>%
  filter(grepl('HF', health_region))
unique_location_ssb_f <- tibble(unique(ssb_ds_f$health_region))

hd_qi <- hd_qi %>%
  mutate(is_from_hd = "is_from_health-directorate")
ssb_ds <- ssb_ds %>%
  mutate(is_from_ssb = "is_from_statistics_norway")

merged_ds <- full_join(ssb_ds, hd_qi)
