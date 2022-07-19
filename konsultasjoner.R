
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



# med versjon 1 av json-stat vil sbtabell være en liste. Slik hentes kun datasettet fra sbtabell
#ds <- sbtabell[[1]]
#ds