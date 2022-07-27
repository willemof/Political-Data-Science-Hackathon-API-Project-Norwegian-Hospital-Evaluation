options(encoding="UTF-8")
library(httr)
library(tidyverse)
library(httr)
library(tinytex)
library(tidyverse)
library(janitor)
library(kableExtra)
library(rjstat)
library(dplyr)
library(stringr)

# henter rjstat bibliotek for behandling av JSON-stat
library(rjstat)
url <- "https://data.ssb.no/api/v0/no/table/06922/"



# spørring fra konsoll - kan være på en linje
data <- '
{
  "query": [
    {
      "code": "HelseReg",
      "selection": {
        "filter": "vs:HelseRegion4",
        "values": [
          "H12_F",
          "H01_F",
          "983971652",
          "983971636",
          "983971660",
          "983971679",
          "983971687",
          "983971695",
          "983971709",
          "993467049",
          "983971717",
          "983971725",
          "983971733",
          "983971741",
          "983971680",
          "983971700",
          "983971768",
          "883971752",
          "983971776",
          "983971784",
          "894166762",
          "H01_P",
          "H02_F",
          "983975208",
          "883975162",
          "983975178",
          "983975305",
          "983975224",
          "983975186",
          "987399708",
          "983975348",
          "883975332",
          "983975216",
          "983975283",
          "983975267",
          "983975259",
          "983975200",
          "983975240",
          "H02_P",
          "H12_P",
          "H12_AV",
          "H03_F",
          "983974724",
          "983974694",
          "983974732",
          "983974678",
          "H03_P",
          "H03_AV",
          "H04_F",
          "983974791",
          "983974767",
          "983974759",
          "983974856",
          "983974872",
          "997005562",
          "986523065",
          "883974832",
          "H04_P",
          "H04_AV",
          "H05_F",
          "983974929",
          "983974880",
          "983974902",
          "983974910",
          "983974899",
          "H05_P",
          "H05_AV"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Dognplass",
          "Liggedag",
          "Polikliniske",
          "Dag"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019",
          "2020",
          "2021"
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
ssb_hospitals <- fromJSONstat(content(d.tmp, "text"))

# Cleaning the names 
ssb_hospitals <- ssb_hospitals %>% 
  clean_names()
# Filtering so that we don't have 0 values 
ssb_hospitals <- ssb_hospitals %>% 
  filter(value != 0)
# From long to wide 
ssb_hospitals <- spread(ssb_hospitals, key = statistikkvariabel, value = value)
# Cleaning the names again 
ssb_hospitals <- ssb_hospitals %>% 
  clean_names()

options(encoding="UTF-8")
library(httr)



# henter rjstat bibliotek for behandling av JSON-stat
library(rjstat)
url <- "https://data.ssb.no/api/v0/no/table/09548/"



# spørring fra konsoll - kan være på en linje
data <- '
{
  "query": [
    {
      "code": "HelseReg",
      "selection": {
        "filter": "vs:HelseReg5",
        "values": [
          "H12_F",
          "991324968",
          "993467049",
          "983971652",
          "983971636",
          "983971680",
          "983971700",
          "983971768",
          "883971752",
          "983971784",
          "894166762",
          "883975162",
          "983975305",
          "987399708",
          "983975348",
          "883975332",
          "983975267",
          "983975259",
          "983975200",
          "H12_P",
          "H03_F",
          "983658725",
          "983974724",
          "983974694",
          "983974732",
          "983974678",
          "H03_P",
          "H04_F",
          "983658776",
          "983974791",
          "983974767",
          "983974759",
          "986523065",
          "883974832",
          "997005562",
          "998308615",
          "H04_P",
          "H05_F",
          "883658752",
          "983974929",
          "983974880",
          "983974910",
          "983974899",
          "H05_P"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "ArsvEksFrav"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019",
          "2020",
          "2021"
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
avtalte_arsverk_hospitals <- fromJSONstat(content(d.tmp, "text"))



# Viser datasettet
avtalte_arsverk_hospitals


avtalte_arsverk_hospitals <- avtalte_arsverk_hospitals %>% 
  clean_names()
avtalte_arsverk_hospitals <- avtalte_arsverk_hospitals %>% 
  rename(value_arsverk = value)
avtalte_arsverk_hospitals <- avtalte_arsverk_hospitals %>% 
  select(region, ar, value_arsverk)

# Merging 
ssb_hospitals <- full_join(ssb_hospitals, avtalte_arsverk_hospitals, by = c("region", "ar"))

options(encoding="UTF-8")
library(httr)



# henter rjstat bibliotek for behandling av JSON-stat
library(rjstat)
url <- "https://data.ssb.no/api/v0/no/table/06464/"



# spørring fra konsoll - kan være på en linje
data <- '
{
  "query": [
    {
      "code": "HelseReg",
      "selection": {
        "filter": "vs:HelseForRegn2",
        "values": [
          "H12_R",
          "H01_R",
          "883971752",
          "983971652",
          "983971636",
          "983971680",
          "983971700",
          "983971768",
          "983971784",
          "894166762",
          "993467049",
          "H02_R",
          "883975162",
          "883975332",
          "987399708",
          "983975200",
          "983975259",
          "983975267",
          "983975305",
          "983975348",
          "983971687",
          "983971695",
          "H03_R",
          "983974678",
          "983974694",
          "983974724",
          "983974732",
          "H04_R",
          "883974832",
          "983974759",
          "983974767",
          "983974791",
          "998308615",
          "986523065",
          "997005562",
          "H05_R",
          "983974880",
          "983974899",
          "983974902",
          "983974910",
          "983974929"
        ]
      }
    },
    {
      "code": "HelseRegnKost",
      "selection": {
        "filter": "item",
        "values": [
          "000"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "LopendeKr"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019",
          "2020",
          "2021"
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
driftskostnader_hospitals <- fromJSONstat(content(d.tmp, "text"))



# Viser datasettet
driftskostnader_hospitals

driftskostnader_hospitals <- driftskostnader_hospitals %>% 
  clean_names()
driftskostnader_hospitals <- driftskostnader_hospitals %>%
  rename(value_driftskostnader = value)
driftskostnader_hospitals <- driftskostnader_hospitals %>% 
  select(region, ar, value_driftskostnader)
ssb_hospitals <- full_join(ssb_hospitals, driftskostnader_hospitals, by = c("region", "ar"))






