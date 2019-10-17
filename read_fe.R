#### Risk of being killed by police in the U.S. by age, race/ethnicity, and sex
#### Frank Edwards, Hedwig Lee, Michael Esposito
#### read in FE
library(tidyverse)
library(lubridate)
library(fuzzyjoin)

set.seed(1)
######## read in police mort data
# ... attach and configure mortality file
fe <- read_csv("fe_10_16.csv") %>% 
  filter(!(is.na(`Unique ID`)))

# .... make names more user-friendly
names(fe)<-c("id", "name", "age", "sex", "race", "URL", "death_date", 
                 "loc_address", "loc_city", "loc_state", "loc_zip", "loc_county", 
                 "loc_full_address", "Latitude", "Longitude", "agency", 
                 "cause_of_death","cause_description", "official_disposition", 
                 "news_url", "mental_illness", "video", "dateanddesc", 
                 "id_formula", "id2", "year")


### comp cities: ft worth,austin, jacksonville, columbus, 
### SF, charlotte
### indianapolis, seattle, denver, el paso, nashville, OKC,
### washington dc
### this is all cities b/w 700-1000k pop for 2018 ACS
### removed jacksonville, because they are policed by sherriff

### Agency list: 

includes<-data.frame(
  agency = 
    c("Fort Worth Police Department",
      "Austin Police Department",
      "Columbus Division of Police", 
      "Columbus Police Department",
      "San Francisco Police Department",
      "Charlotte-Mecklenburg Police Department",
      "Indianapolis Metropolitan Police Department",
      "Seattle Police Department",
      "Denver Police Department",
      "Washington Metropolitan Police Department"),
  loc_state = 
    c("TX", "TX", "OH", "OH", "CA",
      "NC", "IN", "WA", "CO", "DC"),
  stringsAsFactors = FALSE)

includes$match<-TRUE

## remove suicides

fe<-fe %>% 
  select(name,agency,
         loc_city, loc_state, year,
         cause_of_death, official_disposition) %>% 
  mutate(official_disposition = tolower(official_disposition)) %>% 
  filter(!(str_detect(official_disposition,
                      pattern = "suicide"))) %>% 
  filter(cause_of_death=="Gunshot")

fe_index<-fe %>% 
  select(agency, loc_state)

fe_temp<-fuzzy_left_join(fe_index, includes,
                         match_fun = str_detect)

index<-which(fe_temp$match==TRUE)

fe_cities<-fe[index,]

## add Atatiana Jefferson to data
fe_cities<-rbind(fe_cities,
                 c("Atatiana Jefferson",
                   "Forth Worth Police Department",
                   "Fort Worth",
                   "TX",
                   2019,
                   "Gunshot",
                   "unreported"))
### summarize counts by city,year

### some PD killings out of city - merge them back

fe_cities<-fe_cities %>% 
  mutate(loc_city = case_when(
    loc_state=="CO" ~ "Denver",
    loc_state=="OH" ~ "Columbus",
    T ~ loc_city
  ))

fe_cities<-fe_cities %>% 
  group_by(year, loc_city, loc_state) %>% 
  summarise(shooting_deaths = n()) %>% 
  filter(year>2013)

### from wikipedia us city pop
### for 2018 ACS
### https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population
pop<-data.frame(
  loc_city = c("Austin",
               "Fort Worth",
               "Columbus",
               "San Francisco",
               "Charlotte",
               "Indianapolis",
               "Seattle",
               "Denver",
               "Washington"),
  pop = c(964254,
          895008,
          892533,
          883305,
          872498,
          867125,
          744955,
          716492,
          702455),
  stringsAsFactors = F)

fe_cities<-fe_cities %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(pop) %>% 
  mutate(deaths_pc = shooting_deaths / pop * 1e5) %>% 
  mutate(deaths_pc = ifelse(
    year==2019,
    deaths_pc + deaths_pc * (1-284/365), # adjust for partial year
    deaths_pc)) %>% 
  ungroup() 

### add a zero for SF in 2019
fe_cities<-bind_rows(fe_cities,
                     fe_cities %>% 
                       filter(year==2018, loc_city=="San Francisco") %>% 
                       mutate(shooting_deaths = 0,
                              deaths_pc = 0,
                              year = 2019))

### US Total
pop<-327.2*1e6
fe_nat<-fe %>% 
  group_by(year) %>% 
  filter(year>2013) %>% 
  summarise(shooting_deaths = n()) %>% 
  mutate(pop = pop,
         deaths_pc = shooting_deaths / pop * 1e5,
         deaths_pc = ifelse(
           year==2019,
           deaths_pc + deaths_pc * (1-284/365), # adjust for partial year
           deaths_pc)) %>% 
  ungroup() %>% 
  summarise(deaths_pc = mean(deaths_pc))
  
ggplot(fe_cities,
       aes(x = year, y = deaths_pc)) + 
  geom_line() + 
  geom_abline(intercept = fe_nat$deaths_pc, slope= 0, lty=2) +
  facet_wrap(~loc_city) +
  theme_minimal() +
  ylab("Police shooting deaths per 100,000 residents") +
  xlab("Year") + 
  labs(subtitle = "Dashed line indicates 2014-2019 US average police shooting death rate (0.32)") +
  ggsave("deaths_per100k.pdf")
  

ggplot(fe_cities %>% 
         filter(loc_city!="US Total"),
       aes(x = year, y = shooting_deaths)) + 
  geom_line() + 
  facet_wrap(~loc_city) + 
  ylim(0,10)+
  theme_minimal() +
  ylab("Police shooting deaths (count)") +
  xlab("Year") + 
  ggsave("deaths_count.pdf")

write_csv(fe_cities, "fe_cities.csv")

