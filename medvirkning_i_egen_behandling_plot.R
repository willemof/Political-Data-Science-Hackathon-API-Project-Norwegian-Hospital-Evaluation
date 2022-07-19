
# Exporting the excel file 
library(readxl)
medvirkning_i_egen_behandling <- read_excel("~/Desktop/Data Science Hackathon/medvirkning_i_egen_behandling.xlsx")
View(medvirkning_i_egen_behandling) 

# Clean the names 
medvirkning_i_egen_behandling <- medvirkning_i_egen_behandling %>% 
  clean_names()

plot_medvirkning_i_egen_behandling <- ggplot(data = medvirkning_i_egen_behandling, aes(x = tidsperiode, y = verdi, group = lokasjon)) + 
  geom_line() + 
  geom_label_repel(aes(label = lokasjon))

print(plot_medvirkning_i_egen_behandling)






