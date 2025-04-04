library(tidyverse)
library(tidycensus)

#Detailed information about tidycensus:
#https://walker-data.com/census-r/the-united-states-census-and-the-r-programming-language.html

load_variables(2022, "acs5", cache = TRUE) %>%
  filter(str_detect(name,"B11016")) %>% print(n=20)

#Below, I was using a different variable from the CDC study for household size
#No variable for Family household with 1-person
house_vars<-c(
  two_person_Fam="B11016_003",
  three_person_Fam="B11016_004",
  four_person_Fam="B11016_005",
  five_person_Fam="B11016_006",
  six_person_Fam="B11016_007",
  seven_plus_Fam="B11016_008",
  one_person_nonFam="B11016_010",
  two_person_nonFam="B11016_011",
  three_person_nonFam="B11016_012",
  four_person_nonFam="B11016_013",
  five_person_nonFam="B11016_014",
  six_person_nonFam="B11016_015",
  seven_plus_nonFam="B11016_016"
)

#Extracting data used by the CDC to get household distributions
household_census <- get_acs(
  geography = "zcta",
  #  state = "TX",
  variables = house_vars,
  #  summary_var = "B11016_002",
  year = 2022
)


## Age distribution in population

load_variables(2022, "acs5", cache = TRUE) %>%
  filter(str_detect(name,"B01001")) %>% filter(str_detect(name,"B01001_")) %>% print(n=300)

vars <- c(
  "B01001_003",  # Male Under 5 years
  "B01001_004",  # Male 5 to 9 years
  "B01001_005",  # Male 10 to 14 years
  "B01001_006",  # Male 15 to 17 years  
  "B01001_007",  # Male 18-19 years
  "B01001_008",  # Male 20 years
  "B01001_009",  # Male 21 years
  "B01001_010",  # Male 22-24 years
  "B01001_011",  # Male 25-30 years
  "B01001_012",  # Male 30-34 years
  "B01001_013",  # Male 35-39 years
  "B01001_014",  # Male 40-44 years
  "B01001_015",  # Male 45-49 years
  "B01001_016",  # Male 50-54 years
  "B01001_017",  # Male 55-59 years
  "B01001_018",  # Male 60-61 years
  "B01001_019",  # Male 62-64 years
  "B01001_020",  # Male 65-66 years
  "B01001_021",  # Male 67-69 years
  "B01001_022",  # Male 70-74 years
  "B01001_023",  # Male 75-79 years
  "B01001_024",  # Male 80-84 years
  "B01001_025",  # Male 85+ years
  
  "B01001_027",  # Female Under 5 years
  "B01001_028",  # Female 5 to 9 years
  "B01001_029",  # Female 10 to 14 years
  "B01001_030",  # Female 15 to 17 years
  "B01001_031",  # Female 18-19 years
  "B01001_032",  # Female 20 years
  "B01001_033",  # Female 21 years
  "B01001_034",  # Female 22-24 years
  "B01001_035",  # Female 25-29 years
  "B01001_036",  # Female 30-34 years
  "B01001_037",  # Female 35-39 years
  "B01001_038",  # Female 40-44 years
  "B01001_039",  # Female 45-49 years
  "B01001_040",  # Female 50-54 years
  "B01001_041",  # Female 55-59 years
  "B01001_042",  # Female 60-61 years
  "B01001_043",  # Female 62-64 years
  "B01001_044",  # Female 65-66 years
  "B01001_045",  # Female 67-69 years
  "B01001_046",  # Female 70-74 years
  "B01001_047",  # Female 75-79 years
  "B01001_048",  # Female 80-84 years
  "B01001_049"   # Female 85+ years
)

# Get census data by ZIP code
census_age_data <- get_acs(
  geography = "zcta", # ZIP Code Tabulation Area
  variables = vars,
  year = 2022,
  survey = "acs5"
)

#Age groups according to groups relevant to measles
age_groups_by_zipcode<-census_age_data %>%
  mutate(age_group = case_when(
  variable == "B01001_003" ~ "0-4",
  variable <= "B01001_006" ~ "5-17",
  variable <= "B01001_025" ~ "18+",
  variable == "B01001_027" ~ "0-4",
  variable <= "B01001_030" ~ "5-17",
  variable >= "B01001_031" ~ "18+"
))  %>% 
  select(GEOID,age_group,estimate) %>% group_by(GEOID,age_group) %>% summarise_each(sum)

age_groups_by_zipcode

age_distribution_all_US <- age_groups_by_zipcode %>% ungroup() %>% select(-GEOID) %>%
  group_by(age_group) %>% summarise_each(sum) %>% mutate(total=sum(estimate),proporAges=estimate/total) %>% slice(1,3,2)

age_distribution_all_US %>% 
  mutate(age_group=age_group %>% fct_relevel(c("0-4","5-17","18+"))) %>%
  ggplot(aes(x=age_group,y=proporAges)) +
  geom_col()

#Number of households with number of inhabitants
household_census

total_number_homes<-household_census %>% select(type=variable,totalHomes=estimate) %>%
  mutate(type=type %>% str_remove_all("_nonFam")) %>%
  mutate(type=type %>% str_remove_all("_Fam")) %>%
  group_by(type) %>% summarise_each(sum) %>% 
  mutate(probHomes=totalHomes/sum(totalHomes))

total_number_homes %>%
  mutate(type=type %>% str_replace_all(c("_"=" "))) %>%
  mutate(type=type %>% 
           fct_relevel(c("one person","two person","three person",
                         "four person","five person","six person","seven plus"))) %>%
  ggplot(aes(x=type,y=probHomes)) + theme_bw() +
  geom_col(position = position_dodge()) + ylim(0,0.4) +
  xlab("Number of people in household") + ylab("Probability of household") 

#Now, we want to analyze Texas in more detail, in the context of measles.
library(readxl)
library(sf)

cases<-read_excel("~/Documents/GitHub/Measles_in_Texas/Data/measles_TX_02_28_2025.xlsx",sheet = 2)

#total population in counties with geometry for maps
population_county_US <- get_acs(
  geography = "county",  # ZIP Code Tabulation Area
  variables = "B01003_001",  # Total Population
  year = 2022,
  survey = "acs5",
  geometry = T
)

population_county_TX<-population_county_US %>% separate(NAME,c("County","State"),sep=", ") %>%
  mutate(County=County %>% str_remove_all(.," County")) %>%
  filter(State=="Texas") 

population_county_TX %>% as_tibble() %>% filter(County %in% cases$County) %>%
  select(County,population=estimate) %>% arrange(desc(population))

#To be able to add names in counties with cases
counties_with_cases<-population_county_TX %>% filter(County %in% cases$County) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2])

ggplot() + theme_void() +
  geom_sf(data=population_county_TX) +
  geom_sf(data = counties_with_cases,fill="red") +
  geom_text(data = counties_with_cases, aes(x = lon, y = lat, label = County),
            color = "yellow", fontface = "bold", size = 3) +
  ggtitle("Counties with reported measles cases as of March 25, 2025")

ggsave(last_plot(),file="Figures/county_cases_texas.png",bg="white",width = 27,height = 13)

#Household distribution in affected counties

#Extracting data used by the CDC to get household distributions
households_counties <- get_acs(
  geography = "county",
  state = "TX",
  variables = house_vars,
  #  summary_var = "B11016_002",
  year = 2022
)

household_size_dist_Texas<-households_counties %>% 
  select(County=NAME,type=variable,totalHomes=estimate) %>%
  mutate(County=County %>% str_remove_all(" County, Texas")) %>%
  mutate(type=type %>% str_remove_all("_nonFam")) %>%
  mutate(type=type %>% str_remove_all("_Fam")) %>%
  group_by(County,type) %>% summarise_each(sum) %>% 
  mutate(probHomes=totalHomes/sum(totalHomes))

household_size_dist_Texas %>% filter(County %in% cases$County) %>%
  mutate(type=type %>% 
           fct_relevel(c("one_person","two_person","three_person","four_person","five_person","six_person","seven_plus"))) %>%
  ggplot(aes(x=type,y=probHomes,fill=County))+
  geom_col(position = position_dodge()) #+ facet_wrap(~County,scales = "free_y")

#Population by age group

# load_variables(2022, "acs5", cache = TRUE) %>%
#   filter(str_detect(name,"B01001")) %>% filter(str_detect(name,"B01001_")) %>% print(n=300)

# Get census data by ZIP code
population_by_age_Texas <- get_acs(
  geography = "county", # ZIP Code Tabulation Area
  state="TX",
  variables = vars,
  year = 2022,
  survey = "acs5"
)

age_groups_by_county_Texas<-population_by_age_Texas %>% mutate(age_group = case_when(
  variable == "B01001_003" ~ "0-4",
  variable <= "B01001_006" ~ "5-17",
  variable <= "B01001_025" ~ "18+",
  variable == "B01001_027" ~ "0-4",
  variable <= "B01001_030" ~ "5-17",
  variable >= "B01001_031" ~ "18+")) %>% #print(n=50)
  select(County=NAME,age_group,estimate) %>% mutate(County=County %>% str_remove_all(" County, Texas")) %>%
  group_by(County,age_group) %>% summarise_each(sum)

age_groups_by_county_Texas %>% filter(County %in% cases$County) %>%
  mutate(total_popu=sum(estimate)) %>% mutate(prop_in_group=estimate/total_popu) %>%
  mutate(age_group=age_group %>% fct_relevel(c("0-4","5-17","18+"))) %>%
  ggplot(aes(x=age_group,y=prop_in_group,fill=County)) +
  geom_col(position = position_dodge()) 

# Since transmission is mostly happening in children (0-17) years of age,
# I will see which counties have the highest proportion of people in these
# age groups.

# Aggregate ages from 0-17
prop_of_children_in_county<-age_groups_by_county_Texas %>% 
  mutate(age_group1=ifelse(age_group=="18+","Adult","Children")) %>%
  select(-age_group) %>% group_by(County,age_group1) %>% summarise_each(sum) %>% group_by(County) %>% 
  mutate(total_popu=sum(estimate),prop_of_children=estimate/total_popu) %>%
  ungroup() %>% filter(age_group1 == "Children") %>% arrange(desc(prop_of_children))

prop_of_children_in_county %>% filter(County %in% cases$County)

population_county_TX %>% 
  left_join(prop_of_children_in_county %>% select(County,prop_of_children)) %>%
  ggplot(aes(fill=prop_of_children)) + geom_sf() + theme_void() +
  scale_fill_gradient(low="lightyellow", high="slateblue4") 

children_with_threshold<-population_county_TX %>% 
  left_join(prop_of_children_in_county %>% select(County,prop_of_children)) %>%
  mutate(more_than_30=ifelse(prop_of_children>0.25,"Yes","No"))

ggplot() + theme_void() +
  geom_sf(data=children_with_threshold,aes(fill=more_than_30)) +
  geom_sf(data = counties_with_cases,color="red",fill=NA) +
  geom_text(data = counties_with_cases, aes(x = lon, y = lat, label = County),
            color = "black", fontface = "bold", size = 2.5)+
  scale_fill_manual(values=c("#0072B2","lightyellow")) +
  theme(legend.title = element_blank(),legend.position = c(0.15,0.7),
        text=element_text(size=18))

ggsave(last_plot(),file="map_prop_children.png",width=25,height = 18)

