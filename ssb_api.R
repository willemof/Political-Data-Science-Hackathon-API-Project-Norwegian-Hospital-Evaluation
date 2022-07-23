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
}
#Get SSB Data
#New method.. Simply enter the url(s) and let the code take you away!
#make urls
url_list <- c("https://data.ssb.no/api/v0/en/table/06464/",
              "https://data.ssb.no/api/v0/en/table/06922/",
              "https://data.ssb.no/api/v0/en/table/09548/")
  pre_variable_names <- c()
  for(i in 1:NROW(url_list)){
    pre_variable_names[i] <-  str_extract(url_list[i], "/+[0-9]+/") %>%
      str_remove_all("/")
  link<- url_list[i]
  }
 onetonrow<-1:NROW(url_list)

#derives json queries from url_list using PxWebApiData package
query_list <- c()
for (i in 1:NROW(url_list)){
query_list[i] <- ApiData(url_list[i], TRUE, returnApiQuery = TRUE, defaultJSONquery = c(1, -2, -1))
t_query_list <- tibble(query_list)
}

 file.list <- paste0("./data/individual_datasets/",
                    str_extract(pre_variable_names, "^[0-9]+"),".csv")
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

# Apply janitor::clean_names
ssb_ds <- ssb_ds %>% 
  clean_names()
