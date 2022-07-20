options(encoding="UTF-8")
library(httr)
library(ggplot2)
library(janitor)
library(tidyverse)
library(rjstat)
library(tidyr)
library(plotly)

# henter rjstat bibliotek for behandling av JSON-stat
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
          "H12",
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
driftskostander <- fromJSONstat(content(d.tmp, "text"))



# Viser datasettet
# Kostnader (mill. kr per 10 000 innbyggere) 
driftskostander 

url <- "https://data.ssb.no/api/v0/no/table/06922/"



# spørring fra konsoll - kan være på en linje
data <- '
{
  "query": [
    {
      "code": "HelseReg",
      "selection": {
        "filter": "vs:HelseRegion3",
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
          "Dognplass"
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



# Henter ut innholdet fra d.tmp som tekst deretter bearbeides av fromJSONstatdø <- fromJSONstat(content(d.tmp, "text"))
dognplasser <- fromJSONstat(content(d.tmp, "text"))

url <- "https://data.ssb.no/api/v0/no/table/09548/"



# spørring fra konsoll - kan være på en linje
data <- '
{
  "query": [
    {
      "code": "HelseReg",
      "selection": {
        "filter": "vs:HelseReg6",
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
avtalte_arsverk <- fromJSONstat(content(d.tmp, "text"))

options(encoding="UTF-8")

# henter rjstat bibliotek for behandling av JSON-stat
url <- "https://data.ssb.no/api/v0/no/table/06922/"



# spørring fra konsoll - kan være på en linje
data <- '
{
  "query": [
    {
      "code": "HelseReg",
      "selection": {
        "filter": "vs:HelseRegion3",
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
          "Liggedager",
          "Poliklinisk3",
          "Dagbehandling2"
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
ssb_data <- fromJSONstat(content(d.tmp, "text"))

# Viser datasettet
ssb_data

# Cleaned the names 
ssb_data <- ssb_data %>% 
  clean_names()

# From long data to wide 
ssb_data <- spread(ssb_data, key = statistikkvariabel, value = value)

# Clean the column names 
avtalte_arsverk <- avtalte_arsverk %>% 
  clean_names()
dognplasser <- dognplasser %>% 
  clean_names()
driftskostander <- driftskostander %>% 
  clean_names()

# Choosing the relevant columns 
avtalte_arsverk <- avtalte_arsverk %>% 
  select(region, ar, value)
dognplasser <- dognplasser %>% 
  select(region, ar, value)
driftskostander <- driftskostander %>% 
  select(region, ar, value)



# Population per health region 
sor_ost_pop <- 3.1 
nord_pop <- 0.5 
vest_pop <- 1.1 
midt_pop <- 0.687 

# Deviding the man-years by population 
avtalte_arsverk <- avtalte_arsverk %>% 
  mutate(value = ifelse(region == "Helseregion Sør-Øst (2007-)", value/sor_ost_pop, value))

avtalte_arsverk <- avtalte_arsverk %>% 
  mutate(value = ifelse(region == "Helseregion Vest", value/vest_pop, value))

avtalte_arsverk <- avtalte_arsverk %>% 
  mutate(value = ifelse(region == "Helseregion Midt-Norge", value/midt_pop, value))

avtalte_arsverk <- avtalte_arsverk %>% 
  mutate(value = ifelse(region == "Helseregion Nord", value/nord_pop, value))


# Merge the data 
merged_data <- merge(avtalte_arsverk, dognplasser, by = c("ar", "region"), suffixes = c("_arsverk", "_dognplasser"))
merged_data <- merge(merged_data, driftskostander, by = c("ar", "region"))
merged_data <- merge(merged_data, ssb_data, by = c("ar", "region"))

# Clean the columns of the merged data 
merged_data <- merged_data %>% 
  clean_names()

# Rename the long names 
merged_data <- merged_data %>% 
  rename(konsultasjoner = polikliniske_konsultasjoner_per_1_000_innbyggere)
merged_data <- merged_data %>% 
  rename(oppholdsdogn = liggedager_oppholdsdogn_per_1_000_innbyggere)
merged_data <- merged_data %>% 
  rename(dagbehandlinger = dagbehandling_oppholdsdager_per_1_000_innbyggere)

# Deviding by 1000 because the initial data is measured per 1000 people 
merged_data <- merged_data %>% 
  mutate(konsultasjoner = konsultasjoner/1000)
merged_data <- merged_data %>% 
  mutate(value_arsverk = value_arsverk/1000000)
merged_data <- merged_data %>% 
  mutate(oppholdsdogn = oppholdsdogn/1000)
merged_data <- merged_data %>% 
  mutate(dagbehandlinger = dagbehandlinger/1000)

merged_data <- merged_data %>% 
  mutate(value_dognplasser = ifelse(region == "Helseregion Sør-Øst (2007-)", value_dognplasser/sor_ost_pop, value_dognplasser))

merged_data <- merged_data %>% 
  mutate(value_dognplasser = ifelse(region == "Helseregion Vest", value_dognplasser/vest_pop, value_dognplasser))

merged_data <- merged_data %>% 
  mutate(value_dognplasser = ifelse(region == "Helseregion Midt-Norge", value_dognplasser/midt_pop, value_dognplasser))

merged_data <- merged_data %>% 
  mutate(value_dognplasser = ifelse(region == "Helseregion Nord", value_dognplasser/nord_pop, value_dognplasser))

# Man years vs consultations 
ggplot(merged_data, aes(x = value_arsverk, y = konsultasjoner, group = region)) + 
  geom_point() + 
  facet_grid(vars(region))

# Man-years vs day treatments 
ggplot(merged_data, aes(x = value_arsverk, y = dagbehandlinger, group = region)) + 
  geom_point() + 
  facet_grid(vars(region))

#Man-years vs 24-hours stays 
ggplot(merged_data, aes(x = value_arsverk, y = oppholdsdogn, group = region)) + 
  geom_point() + 
  facet_grid(vars(region))

#Doing the same with plotly (without facet_grid) 
merged_data %>% 
  plot_ly(x = ~value_arsverk, y = ~oppholdsdogn, type = "scatter", color = ~region)

# Here you can see that Helse Nord produces around the same amounts of consultations 
# while using more man-years per one person 
merged_data %>% 
  plot_ly(x = ~value_arsverk, y = ~konsultasjoner, type = "scatter", color = ~region, text = ~ar)

# Helse Sør-Øst has the highest population - around 3.1 million people (source: https://helse-sorost.no/om-oss/vart-oppdrag/hva-har-vi-gjort/nokkeltall)
# Helse Nord has population of around 0.5 million people (source: https://helse-nord.no/om-oss/hva-gjor-helse-nord-rhf/nokkeltall)
# Helse Vest has population of around 1.1 million people (source: https://helse-vest.no/om-oss/nokkeltal)
# Helse Midt-Norge has population of around 0.687 million people 

# Ideas: devide the man-years by population given above: conditional mutation (done)
# Interactive visualisations with plotly 
# merge data from Helsedirektoratet 

# Filtering for year 2020 
merged_data_2020 <- merged_data %>% 
  filter(ar == 2020)

# Number of 24 hours stays vs number of man-years (not really related)
merged_data_2020 %>% 
  plot_ly(x = ~value_arsverk, y = ~oppholdsdogn, type = "scatter", color = ~region)

# Number of consultations vs number of man-years 
merged_data_2020 %>% 
  plot_ly(x = ~value_arsverk, y = ~konsultasjoner, type = "scatter", color = ~region)

# Number of 24 hour stays vs beds 
merged_data_2020 %>% 
  plot_ly(x = ~value_dognplasser, y = ~oppholdsdogn, type = "scatter", color = ~region)

# Filtering for health regions 
sor_ost_data <- merged_data %>% 
  filter(region == "Helseregion Sør-Øst (2007-)")
midt_data <- merged_data %>% 
  filter(region == "Helseregion Midt-Norge")

# Visualising health regions data 
sor_ost_data %>% 
  plot_ly(x = ~value_arsverk, y = ~konsultasjoner, type = "scatter", color = ~region, text = ~ar)

midt_data %>% 
  plot_ly(x = ~value_arsverk, y = ~konsultasjoner, type = "scatter", color = ~region, text = ~ar)





