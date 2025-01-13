require(tidyverse)
require(stringr)
require(ggplot2)
require(dplyr)

HarrisCo_housing <- rbind(Housing_2023,Housing_2022,Housing_2021,Housing_2020) %>% 
  select(-starts_with('mail'),
         -starts_with('Market'), 
         -starts_with('str'), 
         -starts_with('lgl')) %>%
  filter(str_detect(state_class,'^A|B')) %>%
  filter(!str_detect(site_addr_1,"0")) %>%
  filter(tot_mkt_val>1000) %>% 
  filter(land_ar>500) %>%
  mutate(land_ar_mi=land_ar*3.58701e-8, tot_mkt_val=na_if(tot_mkt_val,0))


#Harris County Single-family housing counts 2020-2023. County data shows significant increases in single family housing structures 2020-2023. Increase over this time period likely do to high demand for housing and reopening of supply chains. 
HarrisCo_housing %>% 
  group_by(yr) %>% 
  filter(state_class=='A1') %>% 
  tally() %>% 
  mutate(
    yr=as.numeric(yr)) %>%  
  ggplot(aes(x=yr,y= n)) + 
    geom_point() + 
    geom_line() +
  labs(x='Year',
       y='Count',
       title='Harris County Single-Family Homes') +
  theme_minimal()

#Harris County Multi-family housing counts 2020-2023. The county data reveals a notable decline in multifamily structures from 2020-2022, possibly due to Covid-19 policy effecting data.
HarrisCo_housing %>%
  group_by(yr) %>%
  filter(state_class=='B1',na.rm=TRUE) %>%
  tally() %>%
  mutate(yr=as.numeric(yr)) %>%
  ggplot(aes(x=yr,y=n)) +
    geom_point() +
    geom_line() +
  labs(x='YEAR',
       y='COUNT',
       title='Harris County Multi-Family Homes') +
  theme_minimal()


#Multifamily counts for zip codes 77018, 77008, 77009, 77022 from 2020-2023. 
#Data reveals significantly greater amount of multifamily homes in 77008, and 77009, likely due to lack of parking minimums in these zip codes allowing for greater density. 
HarrisCo_housing %>% 
  filter(site_addr_3 %in% c(77018,77022,77008,77009)) %>%
  filter(str_detect(state_class, 'B1')) %>%
  group_by(yr,site_addr_3) %>% 
  tally(name = 'n') %>% 
  mutate(yr=as.numeric(yr)) %>%  
  ggplot(aes(x=yr,y= n, color=factor(site_addr_3))) + 
    geom_point() + 
    geom_line() +
  labs(x='YEAR',
       y='COUNT',
       title='Multi-Family Homes by Zipcode') +
  theme_minimal()

#Singlefamily counts for zip codes 77018, 77008, 77009, 77022 from 2020-2023. 
#Notably missing are the sharp increases seen in the county overall.
HarrisCo_housing %>% 
  filter(site_addr_3 %in% c(77018,77022,77008,77009)) %>%
  filter(str_detect(state_class, 'A1')) %>%
  group_by(yr,site_addr_3) %>% 
  tally(name = 'n') %>% 
  mutate(yr=as.numeric(yr)) %>%  
  ggplot(aes(x=yr,y= n, color=factor(site_addr_3))) + 
  geom_point() + 
  geom_line() +
  labs(x='YEAR',
       y='COUNT',
       title='Single-Family Homes in zips 77018 and 77022') +
  theme_minimal()

#Median SF market values by zip code. 
#Median value growth for 77008 diverges from other zips in 2023 with noticeably greater growth rate. 
HarrisCo_housing %>%
  filter(site_addr_3 %in% c(77018,77022,77008,77009)) %>%
  filter(tot_mkt_val>10) %>%
  filter(state_class=='A1') %>%
  group_by(yr, site_addr_3) %>%
  mutate(tot_mkt_val=median(tot_mkt_val), na.rm=TRUE) %>%
  ggplot(aes(x=yr, y=tot_mkt_val, color=factor(site_addr_3) )) +
  geom_point() +
  geom_line() +
  labs(x='YEAR',
       y='MEDIAN MARKET VALUE',
       title='Single Family Median Values by Zipcode')


#Median SF market value per square foot by zip code.
#77008 growth in value per sqft seems to show even greater outpacing of the selected zip codes.
#Notable 77009 surpassing 77018 in 2023
HarrisCo_housing %>%
  filter(site_addr_3 %in% c(77018,77022,77008,77009)) %>%
  filter(tot_mkt_val>10) %>%
  filter(str_detect(state_class,'^A')) %>%
  group_by(yr,site_addr_3) %>%
  mutate(tot_mkt_val=median(tot_mkt_val/land_ar),na.rm=TRUE) %>%
  ggplot(aes(x=yr, 
             y=tot_mkt_val,
             color=factor(site_addr_3))) +
  geom_point() +
  geom_line() +
  labs(x='YEAR',
       y='VALUE / SQFT',
       title = 'Single-Family Median Market Value per Square Foot by Zipcode') +
  theme_minimal()



