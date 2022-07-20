#Health Directorate API/Data file
library(httr)
library(tinytex)
library(tidyverse)
library(janitor)
library(kableExtra)
library(rjstat)
library(dplyr)
library(stringr)


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


merged_hd_hospitals <- full_join(reinleggelse_hospitals ,overlevelse_hospitals, by = c("location_name", "time_from"), suffix = c("_reinleggelse", "_overlevelse"))
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




