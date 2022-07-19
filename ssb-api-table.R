options(encoding="UTF-8")
library(httr)
library(rjstat) #For JSON
url <- "https://data.ssb.no/api/v0/no/table/10261"
# 
data.tmp <- '
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
          "Heldognsopphold",
          "Dagbehandlinger"
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
ssb.df.dagbehandling <- fromJSONstat(content(d.tmp, "text"))
# Viser datasettet
