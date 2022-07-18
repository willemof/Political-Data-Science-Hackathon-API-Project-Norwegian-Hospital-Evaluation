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
        "filter": "vs:Helsereg3",
        "values": [
          "H03",
          "H04",
          "H05"
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
          "Per10000"
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
kostnader_per_10000 <- fromJSONstat(content(d.tmp, "text"))



# Viser datasettet
kostnader_per_10000



# med versjon 1 av json-stat vil sbtabell være en liste. Slik hentes kun datasettet fra sbtabell
#ds <- sbtabell[[1]]
#ds