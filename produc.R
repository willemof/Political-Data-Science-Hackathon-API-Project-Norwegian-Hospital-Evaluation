
test1 <- lm(polikliniske_konsultasjoner  ~ value_arsverk+ value_driftskostnader, super_merge)
summary(test1)

library(ggplot2)
library(dplyr)
figure1 <- ggplot(super_merge, aes(y=polikliniske_konsultasjoner,x=value_arsverk))+ 
  geom_point(na.rm = TRUE)
figure1       

figure2 <- ggplot(super_merge, aes(x=ar, y=value_arsverk))+geom_point()
figure2

#creating a productivity feature called p1 
super_merge <- super_merge %>% 
  mutate(p1 = polikliniske_konsultasjoner/value_arsverk)

#showing the productivity changes for Akershus sykehus throughout the years
plot1 <- ggplot(super_merge %>% 
                  filter(location_name == "Akershus universitetssykehus HF"), 
                aes(x = ar, y= p1, color = location_name)) + geom_point() + theme_bw()  
plot1

#creating two other productivity features because we are not sure which one is the optimal
super_merge <- super_merge %>% 
  mutate(p2 = liggedager_oppholdsdogn/value_driftskostnader)

super_merge <- super_merge %>% 
  mutate(p3 = dagbehandlinger_oppholdsdager/value_arsverk)

super_merge2 <- super_merge %>%
  gather(p1, p2, p3,
         key = "indikator", value = "verdi")

#plotting the productivity p1 accross all sykehus in the data throughtout the years           
plot2 <- ggplot(super_merge2, 
                aes(x = ar, y= verdi, color = indikator)) + 
  geom_point(na.rm = TRUE) + theme_bw()+
  facet_wrap(vars(location_name))+
  theme(legend.position = "none")

plot2