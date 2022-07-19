options(encoding="UTF-8")
library(httr)
library(dplyr)
library(tidyverse)



# henter rjstat bibliotek for behandling av JSON-stat
library(rjstat)
url <- "https://data.ssb.no/api/v0/no/table/10261"



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
          "AntDagbehandlinger"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019",
          "2020"
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
antall_dagbehandlinger <- fromJSONstat(content(d.tmp, "text"))



# Viser datasettet
antall_dagbehandlinger

options(encoding="UTF-8")
library(httr)



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

options(encoding="UTF-8")
library(httr)



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
          "AntPoliklinKonsult"
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
polikliniske_konsultasjoner <- fromJSONstat(content(d.tmp, "text"))



# Viser datasettet
polikliniske_konsultasjoner

#Cleaning the names as always for all data frames
antall_dagbehandlinger <- antall_dagbehandlinger %>% 
  clean_names()

antall_dognopphold <- antall_dognopphold %>% 
  clean_names()

polikliniske_konsultasjoner <- polikliniske_konsultasjoner %>% 
  clean_names()

# Only leaving relevant columns 
antall_dognopphold <- antall_dognopphold %>% 
  select(region, ar, value)

antall_dagbehandlinger <- antall_dagbehandlinger %>% 
  select(region, ar, value)

polikliniske_konsultasjoner <- polikliniske_konsultasjoner %>% 
  select(region, ar, value)

#Deviding values by 1000, so its measured in 1000s 
antall_dognopphold <- antall_dognopphold %>% 
  mutate(value = value/1000)

antall_dagbehandlinger <- antall_dagbehandlinger %>% 
  mutate(value = value/1000)

polikliniske_konsultasjoner <- polikliniske_konsultasjoner %>% 
  mutate(value = value/1000)

#First, merging antall_dagbehandlinger and antall_dognopphold 
dagbehandlinger_dognopphold <- merge(antall_dagbehandlinger, antall_dognopphold, by = c("ar", "region"), suffixes = c("_dagb", "_dognopp"))

#Then, merging it to polikliniske konsultasjoner 
dagb_dognopp_konsul <- merge(dagbehandlinger_dognopphold, polikliniske_konsultasjoner,  by = c("ar", "region"))

# Line plot of the merged data 
ggplot(dagb_dognopp_konsul, aes(x = ar, y = value_dagb, group = region)) + 
  geom_line(col = "blue") + 
  geom_line(aes(y = value_dognopp), col = "red") + 
  geom_line(aes(y = value), col = "green")

#Look at the summary statistics (value column measured in thousands)
summary(antall_dagbehandlinger)
summary(antall_dognopphold)
summary(polikliniske_konsultasjoner)

# We can see that the biggest numbers are from Sør-Øst and the smallest numbers are from Nord
# Helse Sør-Øst has the highest population - around 3.1 million people (source: https://helse-sorost.no/om-oss/vart-oppdrag/hva-har-vi-gjort/nokkeltall)
# Helse Nord has population of around 0.5 millim people (source: https://helse-nord.no/om-oss/hva-gjor-helse-nord-rhf/nokkeltall)
# Helse Vest has population of around 1.1 million people (source: https://helse-vest.no/om-oss/nokkeltal)
# Helse Midt-Norge has population of around 0.687 million people (source: https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwip9b_p6fr4AhUrYPEDHXHGCAQQFnoECAQQAw&url=https%3A%2F%2Fwww.mercell.com%2Fm%2Ffile%2FGetFile.ashx%3Fid%3D51872281%26version%3D1&usg=AOvVaw0K5fgVoMt9zlsf3_v-XV7A)

# Standard deviations 
antall_dagbehandlinger %>% 
  summarise(sd = sd(antall_dagbehandlinger$value))

antall_dognopphold %>% 
  summarise(sd = sd(antall_dognopphold$value))

polikliniske_konsultasjoner %>% 
  summarise(sd = sd(polikliniske_konsultasjoner$value))



