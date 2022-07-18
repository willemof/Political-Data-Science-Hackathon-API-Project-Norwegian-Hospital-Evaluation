options(encoding="UTF-8")
library(httr)
library(rjstat) #For JSON
url <- "https://data.ssb.no/api/v0/no/table/10811"
# 
data.tmp <- '
{
  "query": [
    {
      "code": "FinansKilde2",
      "selection": {
        "filter": "item",
        "values": [
          "HF.1-HF.4",
          "HF.1",
          "HF.1.1",
          "HF.1.2",
          "HF.2-4"
        ]
      }
    },
    {
      "code": "MedisinKost",
      "selection": {
        "filter": "item",
        "values": [
          "HC.1-HC.7",
          "HC.1",
          "HC.1.1",
          "HK.1",
          "HKR.4"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Belop"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2008",
          "2009",
          "2010",
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
d.tmp <- POST(url , body = data.tmp, encode = "json", verbose())
# Henter ut innholdet fra d.tmp som tekst deretter bearbeides av fromJSONstat
ssb.df.expenses <- fromJSONstat(content(d.tmp, "text"))
# Viser datasettet


