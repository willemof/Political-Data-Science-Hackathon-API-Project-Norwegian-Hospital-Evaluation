options(encoding="UTF-8")
library(httr)



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




