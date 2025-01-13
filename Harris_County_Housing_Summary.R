require(tidyverse)
require(stringr)
require(ggplot2)
require(dplyr)
#filter data for housing class types
Housing_2023 <- read.csv('C:\\urban_developement\\Portfolio\\HCAD\\real_acct_2023.txt', sep="\t", header=T)  
  
Housing_2022 <- read.csv('C:\\urban_developement\\Portfolio\\HCAD\\real_acct_2022.txt', sep="\t", header=T) 
  
Housing_2021 <- read.csv('C:\\urban_developement\\Portfolio\\HCAD\\real_acct_2021.txt', sep="\t", header=T) 
  
Housing_2020 <- read.csv('C:\\urban_developement\\Portfolio\\HCAD\\real_acct_2020.txt', sep="\t", header=T)  

Housing_2024 <- read.csv('C:\\urban_developement\\Portfolio\\HCAD\\real_acct_2024.txt', sep="\t", header=T) %>% 
  filter(str_detect(state_class,'^A|B')) %>% 
  filter(land_ar>500) %>%
  filter(tot_mkt_val>1000) %>%
  filter(!str_detect(site_addr_1,"0"))
  mutate(land_ar_mi=land_ar*3.58701e-8, tot_mkt_val=na_if(tot_mkt_val,0)) 
  

                                                                                          

#Number of properties for each housing class

Housing_2024 %>% ggplot(aes(x=state_class))
  + geom_histogram(stat="count")
  + labs(x= 'Housing Type Class Code', 
         y= 'Number of properties', 
         title = 'Housing Class Property Counts')

#Area of each housing class     

Housing_2024 %>% group_by(state_class) %>% 
  summarize(area=sum(land_ar_mi, na.rm=TRUE)) %>% 
  ggplot(aes(x=state_class)) + 
  geom_col(aes(y=area)) + 
  labs(x='Housing Type Class Code', 
       y='Square Miles', 
       title='Housing Class Property Counts')

#Market value of each housing class

Housing_2024 %>% group_by(state_class) %>% 
  summarize(area=sum(tot_mkt_val, na.rm=TRUE)) %>% 
  ggplot(aes(x=state_class)) + geom_col(aes(y=area)) + 
  labs(x='Housing Type Class Code', 
       y= 'Total Market Value', 
       title='Total Market Values by Housing Class')


HarrisCo_housing %>%
  filter(state_class == 'A1') %>%
  group_by(yr) %>%
  summarise(count = n(), .groups = 'drop')

class(HarrisCo_housing$site_addr_3)

  