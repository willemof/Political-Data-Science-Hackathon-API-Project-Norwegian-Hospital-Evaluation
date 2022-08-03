library(tidyverse)
library(plotly)
library(ggplot2)
library(scales)

#Cleaning the names so that they will show up nicely in the plots

super_merge <- super_merge %>% 
  mutate(p3 = dagbehandlinger_oppholdsdager/value_arsverk, 
         Year = ar, Management_costs = value_driftskostnader, 
         Hospital = location_name)


#Creating different measures for relative productivity. The idea behind p1, p2, p3 is to divide output with input to measure productivity.
super_merge <- super_merge %>% 
  mutate(p1 = polikliniske_konsultasjoner/value_arsverk)

super_merge <- super_merge %>% 
  mutate(p2 = liggedager_oppholdsdogn/value_driftskostnader)

super_merge <- super_merge %>% 
  mutate(p3 = dagbehandlinger_oppholdsdager/value_arsverk)


#Creating an object grouping hospitals and measuring change in productivity using p1, p2, p3
#Using plotly to create interactive plots to show change in productivity using p1, p2, p3
#Using p1
a1 <- super_merge %>% 
  group_by(Hospital) %>% 
  mutate(Productivity1= p1-lag(p1))

p1.plot <- a1 %>% 
  ggplot(aes(Year, Productivity1))+ 
  geom_point(aes(colour=Hospital))+
  labs(subtitle="Change in productivity compared to the previous year.", y = "Delta P in Consultations/Man-years")+
  ggtitle("Change in productivity using p1")+
  theme_dark()
ggplotly(p1.plot)


#Using p2
a1 <- super_merge %>% 
  group_by(Hospital) %>% 
  mutate(Productivity2 = p2-lag(p2))

p2.plot <- a1 %>% 
  ggplot(aes(Year, Productivity2))+ 
  geom_point(aes(colour=Hospital))+
  labs(subtitle="Change in productivity compared to the previous year.", y = "Delta P in 24-hour stays/Management costs")+
  ggtitle("Change in productivity using p2")+
  theme_dark()
ggplotly(p2.plot)


#Using p3
a1 <- super_merge %>% 
  group_by(Hospital) %>% 
  mutate(Productivity3 = p3-lag(p3))


p3.plot <- a1 %>% 
  ggplot(aes(Year, Productivity3))+ 
  geom_point(aes(colour=Hospital))+
  labs(subtitle="Change in productivity compared to the previous year.", y = "Delta P in Day-treatment stays/Man-years")+
  ggtitle("Change in productivity using p3")+
  theme_dark()
ggplotly(p3.plot)


#Showing problems with using p1, p2, p3 
#showing the productivity changes for Akershus sykehus throughout the years
plot1 <- ggplot(super_merge %>% 
                  filter(location_name == "Akershus universitetssykehus HF"), 
                aes(x = ar, y= p1, color = location_name)) + geom_point() + theme_bw()  
plot1


plot2 <- ggplot(super_merge %>% 
                  filter(location_name == "Akershus universitetssykehus HF"), 
                aes(x = ar, y= p2, color = location_name)) + geom_point() + theme_bw()  
plot2


plot3 <- ggplot(super_merge %>% 
                  filter(location_name == "Akershus universitetssykehus HF"), 
                aes(x = ar, y= p3, color = location_name)) + geom_point() + theme_bw()  
plot3

#The three plots above show an overall increasing trend in relative productivity for Akershus universitetssykehus using p1, while the trend is decreasing when using p2. 
#We therefore concluded that we cannot use p1, p2, p3 as reliable measurement of productivity for each hospital. Though, p1, p2, p3 can still be useful as measurements of comparisons when comparing individual hospitals. 
#Especially for hospitals that specialize in the output (variable in the numerator place). 


#Interactive plot showing each hospital's management costs (driftskostnader)

drkostnader.plot <- a1 %>% 
  ggplot2::ggplot(aes(Year, Management_costs))+
  geom_point(aes(colour=Hospital))+
  labs(subtitle="Each hospital's yearly management costs.", y = "Management costs in 1000NOK")+
  theme_dark()
ggplotly(drkostnader.plot)




#We try some other plots to see if there is a correlation between various variables.

#This first graph shows outliers when plotting delays (utsettelse) and survival rate (overlevelse)
super_merge %>% 
  ggplot(aes(value_utsettelse, value_overlevelse, 
             colour = ar) )+ 
  geom_point()+
theme_dark()


#To find the outliers 
super_merge %>% 
  arrange(desc(value_utsettelse))


#To filter out the outliers (two hospitals from 2016)
super_merge %>% 
  filter(!location_name %in% c("Haraldsplass Diakonale Sykehus AS", "Sunnaas sykehus HF")) %>% 
  ggplot(aes(value_utsettelse, value_overlevelse, 
             colour = ar) )+ 
  geom_point()+
theme_dark()

#Second plot. Similarly, there are outliers in the data
super_merge %>% 
  ggplot(aes(dognplasser, value_utsettelse))+ 
  geom_point(aes(size = value_utsettelse, colour = ar))
geom_label (aes(label = location_name))
theme_bw()

#to find the outliers 
super_merge %>%  
  group_by(location_name, ar) %>% 
  summarise(sum = value_utsettelse) %>%
  arrange(desc(sum))

#Filtered out the two outliers (same two hospitals in 2016)

super_merge %>% 
  filter(!location_name %in% c("Haraldsplass Diakonale Sykehus AS", "Sunnaas sykehus HF")) %>% 
  ggplot(aes(dognplasser, value_utsettelse) )+ 
  geom_point(aes(size = value_utsettelse, colour = ar))
geom_label (aes(label = location_name))
theme_bw()


#These plots don't appear to show correlation between the plotted variables either
#Third plot
super_merge %>% 
  ggplot(aes(value_driftskostnader, value_reinleggelse, 
             colour = ar) )+ 
  geom_point()
theme_bw()


#Fourth plot
super_merge %>% 
  ggplot(aes(dagbehandlinger_oppholdsdager, value_reinleggelse) )+ 
  geom_point(aes(colour = ar))+
  theme(legend.position = "top")
