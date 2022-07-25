#Health Directorate API/Data file
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








url <- "https://api.helsedirektoratet.no/innhold/nki/kvalitetsindikatorer/"
key <- "1c50d76931ba48f69d177c18eaf3c6a8"

#Meta-dataset
ds<-GET(url, 
        add_headers("Ocp-Apim-Subscription-Key" = key),
        add_headers("Cache-Control"= "no-cache")) %>% 
  content(as = "text") %>% # extracting the data
  jsonlite::fromJSON(flatten = TRUE) # parsing to dataframe

#Retrieving attachments connected to ds
api.call <- do.call(rbind, ds$attachments)
api.call <- api.call %>% 
  filter(fileType == "application/json")

#for-Loop that retrieves all quality indicators and saves them to data folder
#It's a 10 minute download.
for(i in 1:nrow(api.call)){
  
  file.tmp <- paste0("./data/quality_indicators/",
                     str_extract(api.call$fileName[i], "^[0-9]+"),
                     ".csv")
  
  if(file.exists(file.tmp)){
    print(noquote(c(file.tmp, noquote("was skipped."))))
    next
  }
  
  tmp <- GET(api.call$fileUri[i],
             add_headers("Ocp-Apim-Subscription-Key" = key),
             add_headers("Cache-Control"= "no-cache")) %>% 
    content(as = "text") %>% 
    jsonlite::fromJSON(flatten = TRUE) # parsing to dataframe
  
  write_csv(tmp$AttachmentDataRows, file.tmp)
  print(noquote(c(file.tmp, noquote("was saved."))))
  Sys.sleep(2+abs(rnorm(1)))
  
}
#End of for-loop

#makes qi list out of csv files
qi_files <- list.files("./data/quality_indicators/",
                       full.names = TRUE)
#combines qis into a single dataset
qi <- lapply(qi_files, read_csv, show_col_types = FALSE)
qi <- bind_rows(qi)

#cleaning
clean_qi<- clean_names(qi)

#filtering
# Reinnleggelse blant eldre pasienter 30 dager etter utskrivning fra sykehus (health regions)
reinleggelse_regions  <- clean_qi %>%
  filter(period_type=="Årlig") %>%
  filter(quality_indicator_name=="Reinnleggelse blant eldre pasienter 30 dager etter utskrivning fra sykehus") %>%
  filter(parent_name == "Hele landet") 
reinleggelse_regions$time_from <- format(as.Date(reinleggelse_regions$time_from), "%Y")
# Reinnleggelse blant eldre pasienter 30 dager etter utskrivning fra sykehus (hospitals)
reinleggelse_hospitals  <- clean_qi %>%
  filter(period_type=="Årlig") %>%
  filter(quality_indicator_name=="Reinnleggelse blant eldre pasienter 30 dager etter utskrivning fra sykehus") %>%
  filter(parent_name %in% c("Helse Sør-Øst RHF", "Helse Midt-Norge RHF", "Helse Nord RHF", "Helse Vest RHF")) 
reinleggelse_hospitals$time_from <- format(as.Date(reinleggelse_hospitals$time_from), "%Y")

# Overlevelse 30 dager etter innleggelse på sykehus (health regions)
overlevelse_regions  <- clean_qi %>%
  filter(period_type=="Årlig") %>%
  filter(measure_name=="30-dagers overlevelse etter innleggelse på sykehus") %>%
  filter(parent_name == "Hele landet") 
overlevelse_regions$time_from <- format(as.Date(overlevelse_regions$time_from), "%Y")
# Overlevelse 30 dager etter innleggelse på sykehus (hospitals)
overlevelse_hospitals <- clean_qi %>% 
  filter(period_type=="Årlig") %>%
  filter(measure_name=="30-dagers overlevelse etter innleggelse på sykehus") %>%
  filter(parent_name %in% c("Helse Sør-Øst RHF", "Helse Midt-Norge RHF", "Helse Nord RHF", "Helse Vest RHF")) 
overlevelse_hospitals$time_from <- format(as.Date(overlevelse_hospitals$time_from), "%Y")

# Utsettelse av planlagte operasjoner (health regions)
utsettelse_regions <- clean_qi %>%
  filter(period_type=="Årlig") %>%
  filter(measure_name=="Andel utsettelser") %>%
  filter(parent_name == "Hele landet") 
utsettelse_regions$time_from <- format(as.Date(utsettelse_regions$time_from), "%Y")
# Utsettelse av planlagte operasjoner (hospitals)
utsettelse_hospitals <- clean_qi %>% 
  filter(period_type=="Årlig") %>%
  filter(measure_name=="Andel utsettelser") %>%
  filter(parent_name %in% c("Helse Sør-Øst RHF", "Helse Midt-Norge RHF", "Helse Nord RHF", "Helse Vest RHF"))
utsettelse_hospitals$time_from <- format(as.Date(utsettelse_hospitals$time_from), "%Y")

# Andel pasienter plassert på korridor (health regions)
korridor_regions <- clean_qi %>%
  filter(period_type=="Årlig") %>%
  filter(measure_name=="Andel pasienter plassert på korridor") %>%
  filter(parent_name == "Hele landet") 
korridor_regions$time_from <- format(as.Date(korridor_regions$time_from), "%Y")
# Andel pasienter plassert på korridor (hospitals)
korridor_hospitals <- clean_qi %>% 
  filter(period_type=="Årlig") %>%
  filter(measure_name=="Andel pasienter plassert på korridor") %>%
  filter(parent_name %in% c("Helse Sør-Øst RHF", "Helse Midt-Norge RHF", "Helse Nord RHF", "Helse Vest RHF")) 
korridor_hospitals$time_from <- format(as.Date(korridor_hospitals$time_from), "%Y")

# Medvirkning i egen behandling - involvering i behandlingsplan (health regions)
medvirkning_regions <- clean_qi %>%
  filter(period_type=="Årlig") %>%
  filter(measure_name=="Andel av pasienter i pakkeforløp som sammen med behandler har utarbeidet en behandlingsplan") %>%
  filter(parent_name == "Hele landet") 
medvirkning_regions$time_from <- format(as.Date(medvirkning_regions$time_from), "%Y")
# Medvirkning i egen behandling - involvering i behandlingsplan (hospitals)
medvirkning_hospitals <- clean_qi %>% 
  filter(period_type=="Årlig") %>%
  filter(measure_name=="Andel av pasienter i pakkeforløp som sammen med behandler har utarbeidet en behandlingsplan") %>%
  filter(parent_name %in% c("Helse Sør-Øst RHF", "Helse Midt-Norge RHF", "Helse Nord RHF", "Helse Vest RHF")) 
medvirkning_hospitals$time_from <- format(as.Date(medvirkning_hospitals$time_from), "%Y")
# Sykehusopphold - fristbrudd for pasienter på venteliste (health regions)
fristbrudd_regions <- clean_qi %>%
  filter(measure_name=="Andel fristbrudd for pasienter som står på venteliste i somatisk helsetjeneste") %>% 
  filter(parent_name == "Hele landet") 

fristbrudd_regions$time_from <- format(as.Date(fristbrudd_regions$time_from), "%Y")
fristbrudd_regions <- fristbrudd_regions %>% 
  filter(period_type == "Tertialvis")
fristbrudd_regions<- fristbrudd_regions %>% 
  group_by(location_name, time_from) %>%
  filter(row_number() == 1)

# Sykehusopphold - fristbrudd for pasienter på venteliste (hospitals)
fristbrudd_hospitals <- clean_qi %>% 
  filter(measure_name=="Andel fristbrudd for pasienter som står på venteliste i somatisk helsetjeneste") %>%
  filter(parent_name %in% c("Helse Sør-Øst RHF", "Helse Midt-Norge RHF", "Helse Nord RHF", "Helse Vest RHF"))   
fristbrudd_hospitals$time_from <- format(as.Date(fristbrudd_hospitals$time_from), "%Y")
# There were too few yearly observations so I removed the yearly filter and use the "tertialvis" period type and only chose 
# the observations that are first in the year 
fristbrudd_hospitals <- fristbrudd_hospitals %>% 
  group_by(location_name, time_from) %>%
  filter(row_number() == 1)

# Sykehusopphold - pasienterfaringer (health regions)
pasient_erfaringer_regions <- clean_qi %>%
  filter(period_type=="Årlig") %>%
  filter(quality_indicator_name=="Pasienterfaringer med somatiske sykehus") %>%
  filter(parent_name == "Hele landet") %>% 
  filter(measure_name== "Informasjon, gjennomsnittlig poengsum (0-100)")
pasient_erfaringer_regions$time_from <- format(as.Date(pasient_erfaringer_regions$time_from), "%Y")
# Sykehusopphold - pasienterfaringer (hospitals)
pasient_erfaringer_hospitals <- clean_qi %>% 
  filter(period_type=="Årlig") %>%
  filter(quality_indicator_name=="Pasienterfaringer med somatiske sykehus") %>%
  filter(parent_name %in% c("Helse Sør-Øst RHF", "Helse Midt-Norge RHF", "Helse Nord RHF", "Helse Vest RHF")) %>% 
  filter(measure_name== "Informasjon, gjennomsnittlig poengsum (0-100)")
pasient_erfaringer_hospitals$time_from <- format(as.Date(pasient_erfaringer_hospitals$time_from), "%Y")


# Psykisk helse for voksne - fristbrudd for voksne på venteliste (health regions)
fristbrudd_psykisk_regions <- clean_qi %>%
  filter(parent_name == "Hele landet") %>% 
  filter(measure_name== "Andel fristbrudd for pasienter som står på venteliste i PHV")
fristbrudd_psykisk_regions$time_from <- format(as.Date(fristbrudd_psykisk_regions$time_from), "%Y")
fristbrudd_psykisk_regions <- fristbrudd_psykisk_regions %>% 
  group_by(location_name, time_from) %>%
  filter(row_number() == 1)
# Psykisk helse for voksne - fristbrudd for voksne på venteliste (hospitals)
fristbrudd_psykisk_hospitals <- clean_qi %>% 
  filter(parent_name %in% c("Helse Sør-Øst RHF", "Helse Midt-Norge RHF", "Helse Nord RHF", "Helse Vest RHF")) %>% 
  filter(measure_name== "Andel fristbrudd for pasienter som står på venteliste i PHV") 
fristbrudd_psykisk_hospitals$time_from <- format(as.Date(fristbrudd_psykisk_hospitals$time_from), "%Y")

fristbrudd_psykisk_hospitals <- fristbrudd_psykisk_hospitals %>% 
  group_by(location_name, time_from) %>%
  filter(row_number() == 1)




qi_name_list <- tibble(datasett = unique(clean_qi$quality_indicator_name))
# qi_name_list <- tibble(datasett = unique(clean_qi$quality_indicator_name))
qi_measure_list <- tibble(datasett = unique(clean_qi$measure_name))

# Choosing only the columns we need: 
reinleggelse_regions <- reinleggelse_regions %>% 
  select(location_name, value, time_from)
reinleggelse_hospitals <- reinleggelse_hospitals %>% 
  select(location_name, value, time_from)
overlevelse_regions <- overlevelse_regions %>% 
  select(location_name, value, time_from)
overlevelse_hospitals <- overlevelse_hospitals %>% 
  select(location_name, value, time_from)
utsettelse_regions <- utsettelse_regions %>% 
  select(location_name, value, time_from)
utsettelse_hospitals <- utsettelse_hospitals %>% 
  select(location_name, value, time_from)
korridor_regions <- korridor_regions %>% 
  select(location_name, value, time_from)
korridor_hospitals <- korridor_hospitals %>% 
  select(location_name, value, time_from)
medvirkning_regions <- medvirkning_regions %>% 
  select(location_name, value, time_from)
medvirkning_hospitals <- medvirkning_hospitals %>% 
  select(location_name, value, time_from)
fristbrudd_regions <- fristbrudd_regions %>% 
  select(location_name, value, time_from)
fristbrudd_hospitals <- fristbrudd_hospitals %>% 
  select(location_name, value, time_from)
pasient_erfaringer_regions <- pasient_erfaringer_regions %>% 
  select(location_name, value, time_from)
pasient_erfaringer_hospitals <- pasient_erfaringer_hospitals %>% 
  select(location_name, value, time_from)
fristbrudd_psykisk_regions <- fristbrudd_psykisk_regions %>% 
  select(location_name, value, time_from)
fristbrudd_psykisk_hospitals <- fristbrudd_psykisk_hospitals %>% 
  select(location_name, value, time_from)

# Merging the data 
merged_hd_regions <- full_join(reinleggelse_regions ,overlevelse_regions, by = c("location_name", "time_from"), suffix = c("_reinleggelse", "_overlevelse"))
merged_hd_regions <- full_join(merged_hd_regions , utsettelse_regions, by = c("location_name", "time_from"))
merged_hd_regions <- merged_hd_regions %>% 
  rename(value_utsettelse = value)
merged_hd_regions <- full_join(merged_hd_regions , korridor_regions, by = c("location_name", "time_from"))
merged_hd_regions <- merged_hd_regions %>% 
  rename(value_korridor = value)
merged_hd_regions <- full_join(merged_hd_regions , medvirkning_regions, by = c("location_name", "time_from"))
merged_hd_regions <- merged_hd_regions %>% 
  rename(value_medvirkning = value)
merged_hd_regions <- full_join(merged_hd_regions , fristbrudd_regions, by = c("location_name", "time_from"))
merged_hd_regions <- merged_hd_regions %>% 
  rename(value_fristbrudd = value)
merged_hd_regions <- full_join(merged_hd_regions , pasient_erfaringer_regions, by = c("location_name", "time_from"))
merged_hd_regions <- merged_hd_regions %>% 
  rename(value_erfaringer = value)
merged_hd_regions <- full_join(merged_hd_regions , fristbrudd_psykisk_regions, by = c("location_name", "time_from"))
merged_hd_regions <- merged_hd_regions %>% 
  rename(value_fristbrudd_psykisk = value)

merged_hd_hospitals <- full_join(reinleggelse_hospitals, overlevelse_hospitals, by = c("location_name", "time_from"), suffix = c("_reinleggelse", "_overlevelse"))
merged_hd_hospitals <- full_join(merged_hd_hospitals , utsettelse_hospitals, by = c("location_name", "time_from"))
merged_hd_hospitals <- merged_hd_hospitals %>% 
  rename(value_utsettelse = value)
merged_hd_hospitals <- full_join(merged_hd_hospitals , korridor_hospitals, by = c("location_name", "time_from"))
merged_hd_hospitals <- merged_hd_hospitals %>% 
  rename(value_korridor = value)
merged_hd_hospitals <- full_join(merged_hd_hospitals , medvirkning_hospitals, by = c("location_name", "time_from"))
merged_hd_hospitals <- merged_hd_hospitals %>% 
  rename(value_medvirkning = value)
merged_hd_hospitals <- full_join(merged_hd_hospitals , fristbrudd_hospitals, by = c("location_name", "time_from"))
merged_hd_hospitals <- merged_hd_hospitals%>% 
  rename(value_fristbrudd = value)
merged_hd_hospitals <- full_join(merged_hd_hospitals , pasient_erfaringer_hospitals, by = c("location_name", "time_from"))
merged_hd_hospitals <- merged_hd_hospitals %>% 
  rename(value_erfaringer = value)
merged_hd_hospitals <- full_join(merged_hd_hospitals , fristbrudd_psykisk_hospitals, by = c("location_name", "time_from"))
merged_hd_hospitals <- merged_hd_hospitals %>% 
  rename(value_fristbrudd_psykisk = value)

merged_hd_hospitals<-  rename(merged_hd_hospitals, ar=time_from)
<<<<<<< HEAD
super_merge <- inner_join(merged_hd_hospitals, ssb_hospitals)
reg_data <- tibble()
for(i:NROW(super_merge)){
  # Y = B + B1(x1)+B2(x2) + E
  reg_data[i] <- lm(qi_index ~ expenses + man_hours+ etc, data =super_merge)
}
super_merge_2019 <- super_merge %>%
  filter(ar==2019)
lm_2019 <- lm(value_erfaringer ~ value_driftskostnader, data = super_merge_2019)
lm
  
=======
# super_merge <- inner_join(merged_hd_hospitals, ssb_hospitals)
ssb_hospitals <- ssb_hospitals %>% 
  rename(location_name = region)
super_merge <- full_join(merged_hd_hospitals, ssb_hospitals, by = c("location_name", "ar"))

##### SSB data on health regions 

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
driftskostnader <- fromJSONstat(content(d.tmp, "text"))



# Viser datasettet
# Kostnader (mill. kr per 10 000 innbyggere) 
driftskostnader 

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
driftskostnader <- driftskostnader %>% 
  clean_names()

# Choosing the relevant columns 
avtalte_arsverk <- avtalte_arsverk %>% 
  select(region, ar, value)
dognplasser <- dognplasser %>% 
  select(region, ar, value)
driftskostnader <- driftskostnader %>% 
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
merged_data <- merge(merged_data, driftskostnader, by = c("ar", "region"))
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

merged_data <- merged_data %>% 
  rename(value_driftskostnader = value)
merged_data <- merged_data %>% 
  rename(location_name = region)
merged_hd_regions <- merged_hd_regions %>% 
  rename(ar = time_from)
super_merge_regions <- full_join(merged_data, merged_hd_regions, by = c("location_name", "ar"))






>>>>>>> 271834ba6b6909312c63f1ffc170bd0aa35bd168
