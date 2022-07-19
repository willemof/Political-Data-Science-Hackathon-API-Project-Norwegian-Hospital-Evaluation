#Health directorate API replication/reapplication

#libraries
library(httr)
library(tinytex)
library(tidyverse)
library(janitor)
library(kableExtra)
library(rjstat)


url <- "https://api-qa.helsedirektoratet.no/innhold/intern-import/kvalitetsindikatorer/"
key <- "5f01971200da4be3a5deb2a5f20fc29e"

ds<-GET(url, 
        add_headers("Ocp-Apim-Subscription-Key" = key),
        add_headers("Cache-Control"= "no-cache")) %>% 
  content(as = "text") %>% # extracting the data
  jsonlite::fromJSON(flatten = TRUE) # parsing to dataframe

