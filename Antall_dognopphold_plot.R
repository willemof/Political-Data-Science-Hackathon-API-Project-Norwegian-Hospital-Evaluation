options(encoding="UTF-8")
library(httr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(janitor)
library(ggplot2)



# henter rjstat bibliotek for behandling av JSON-stat
library(rjstat)
url <- "https://data.ssb.no/api/v0/no/table/10261/"



# spørring fra konsoll - kan være på en linje
data <- '
{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "vs:Helseregion330b",
        "values": [
          "H12",
          "H03",
          "H04",
          "H05"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "AntDognopphold"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'
d.tmp <- POST(url , body = data, encode = "json", verbose())



# Henter ut innholdet fra d.tmp som tekst deretter bearbeides av fromJSONstat
antall_dognopphold <- fromJSONstat(content(d.tmp, "text"))



# Viser datasettet
antall_dognopphold

#cleaned the main tables names 
antall_dognopphold <- antall_dognopphold %>% 
  clean_names()

#I removed the column "statistikk variable" because it was not relevant anymore 
antall_dognopphold <- antall_dognopphold %>% 
  select(region, ar, value)

# changing the numbers so it's measured in thousands 
antall_dognopphold <- antall_dognopphold %>% 
  mutate(value = value/1000) 

#Creation a line plot 
antall_dognopphold_plot <- ggplot(antall_dognopphold, aes(x = ar, y = value, group = region)) + 
  geom_line() 
















