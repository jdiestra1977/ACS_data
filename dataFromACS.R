library(tidycensus)
library(tidyverse)
library(stringr)
library(sf)

## To generate data for highways in the map of Austin ------
library(cowplot) # plot side by side
library(heavy) # heavyLm
library(rgeos)

# Define API key each time unless you save
census_api_key("3deb7c3e77d1747cf53071c077e276d05aa31407", install = TRUE, overwrite = TRUE) # Spencer Woody's API KEY
library(rmapzen) # needed for mz_ functions to get roads
mz_set_tile_host_nextzen(key="hxNDKuWbRgetjkLAf_7MUQ")


# Change this function to just be make a map of TX
make_zip_hosp_map = function(){
  
  # Get list of ZIPs in MSA
  zips_msa = read_csv("input_data/zips_in_austin_rr_msa.csv")
  
  # get zcta pop for all US with geometry # really we just want the geometry
  if(file.exists('produced_data/zip_geometries.rda' )){
    load('produced_data/zip_geometries.rda')
  } else{
    zip_pop_us = get_acs(geography="zcta", variables=c("B01001_001"), geometry = TRUE, year=2019) %>%
      rename(ZIP=GEOID)
    zip_geom = subset(zip_pop_us, ZIP %in% zips_msa$ZIP) %>% # don't need the pop, just want the polygons for plotting
      select(ZIP)
    save(zip_geom, file = 'produced_data/zip_geometries.rda')
  }
  
  # Hospitals and ZIP code points
  hosp_loc_df = read_csv("input_data/hospital_info_location.csv") %>% # hospital locations and staffed beds
    filter(!(name_code %in% c("DCMC", "StDavidsSurgicalHospital")) ) %>% # remove chilren's hospital and surgical hosp
    select(short_name, long_x, lat_y, staffed_beds_AHA) # get just the location of each hospital
  
  zip_cent = read_csv("input_data/pop_weight_zip_centroids_from_cbg.csv") %>% 
    rename(ZIP = ZCTA5CE10) %>%
    mutate(ZIP = as.character(ZIP)) %>%
    filter(ZIP %in% zips_msa$ZIP)
  
  
  # Get drive times from ZIP to Hosp
  zip_geom_dt =  read_csv(paste0(res_dir, "/zip_hosp_drive_time_min.csv")) %>%
    rename(ZIP = org) %>%
    mutate(ZIP = as.character(ZIP)) %>%
    group_by(ZIP) %>%
    summarise(mean_dt = round(mean(drive_time), 0) ) %>%
    ungroup() %>%
    right_join(zip_geom, by="ZIP")
  
  # GET ROADS MSA
  get_vector_tiles <- function(bbox){
    mz_box=mz_rect(bbox$xmin,bbox$ymin,bbox$xmax,bbox$ymax)
    mz_vector_tiles(mz_box)
  }
  zcta_geom <- st_union(zip_geom$geometry)
  zcta_bbox <- st_bbox(zcta_geom)
  zcta_vector_tiles <- get_vector_tiles(zcta_bbox)
  zcta_roads <- as_sf(zcta_vector_tiles$roads) 
  
  I35<- zcta_roads %>%
    mutate(st_transform(geometry, st_crs(zip_geom))) %>% 
    filter(ref== "I 35" | ref=="I 35;US 190" | ref=="I 35;US 290" | ref=="I 35;US 77") %>% # | ref=="I 35-H Business") %>%
    pull(geometry) %>%
    st_union() %>%
    st_transform(st_crs(zcta_geom)) %>%
    st_intersection(zcta_geom)
  
  US183 = zcta_roads %>%
    mutate(st_transform(geometry, st_crs(zip_geom))) %>% 
    # filter(grepl('183', ref)) %>% 
    filter(ref=="US 183" | ref=="US 183;FM 20" | ref=="US 183;TX 80" | ref=="US 90;US 183" | ref== "US 183;US 190;US 281"
           | ref=="US 183;US 190;US 281;FM 580" | ref=="US 183;US 190;US 281;Truck" | ref=="US 183;US 190" | id=="2c2066c3b6fafe2574463154f5cc877d" ) %>%
    pull(geometry) %>%
    st_union() %>%
    st_transform(st_crs(zcta_geom)) %>%
    st_intersection(zcta_geom)
  
  
  ggplot()+
    geom_sf(data=zip_geom_dt, mapping=aes(geometry=geometry, fill=mean_dt), #fill="white",
            size = 0.05, color="black")+ # , show.legend = FALSE
    #geom_sf(data=travis_outline, col = "black", size=0.15, fill=NA)+
    scale_fill_gradient(low="lightyellow", high="slateblue4")+ #, 
    #breaks=seq(min(zip_geom_dt$mean_dt), max(zip_geom_dt$mean_dt), 10))+
    geom_sf(data = I35, col = "black", size=0.15)+
    geom_sf(data = US183, col = "black", size=0.15)+
    # geom_point(data = hosp_loc_df, aes(x=long_x, y=lat_y, size=staffed_beds_AHA), 
    #            shape=1, )+
    geom_point(data=zip_cent, aes(x=ZCTA_CENT_LONG, y=ZCTA_CENT_LAT), shape=17 )+
    #geom_sf(data = missing_183, col = "black", size=0.15)+
    annotate(geom="text", x=-97.5, y=30.87, label="I-35", size=4)+
    annotate(geom="text", x=-98.05, y=30.87, label="US 183", size=4)+
    guides(size=guide_legend(title="Staffed Beds"),
           fill=guide_legend(title="Mean Drive Time (min)"))+
    #labs(fill = "Cumulative\nInfections (%)")+
    theme_void()+ # base_size = 10
    theme(
      legend.position = "right",
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 15),
      legend.text.align = 1,
      legend.key.size = unit(0.5, "lines"),
      plot.margin=unit(c(0, 0, 0, 0),"cm")
    )
  
  
}

mz_set_tile_host_nextzen(key="hxNDKuWbRgetjkLAf_7MUQ")

zip_pop_us = get_acs(geography="zcta", variables=c("B01001_001"), geometry = TRUE, year=2019) %>%
  rename(ZIP=GEOID)
#load("~/Projects/VulnerabilityMobility/Data/MovementZipToZip/forMapsWithSVI.RData")
zips_msa <- forMapsWithSVI %>% filter(County=="Travis") %>% rename_at("Zip",~"ZIP")

zip_geom = subset(zip_pop_us, ZIP %in% zips_msa$ZIP) %>% # don't need the pop, just want the polygons for plotting
  select(ZIP)

# GET ROADS MSA
get_vector_tiles <- function(bbox){
  mz_box=mz_rect(bbox$xmin,bbox$ymin,bbox$xmax,bbox$ymax)
  mz_vector_tiles(mz_box)
}

zcta_geom <- st_union(zip_geom$geometry)
zcta_bbox <- st_bbox(zcta_geom)
zcta_vector_tiles <- get_vector_tiles(zcta_bbox)
zcta_roads <- as_sf(zcta_vector_tiles$roads) 

I35<- zcta_roads %>%
  mutate(st_transform(geometry, st_crs(zip_geom))) %>% 
  filter(ref== "I 35" | ref=="I 35;US 190" | ref=="I 35;US 290" | ref=="I 35;US 77") %>% # | ref=="I 35-H Business") %>%
  pull(geometry) %>%
  st_union() %>%
  st_transform(st_crs(zcta_geom)) %>%
  st_intersection(zcta_geom)

US183 = zcta_roads %>%
  mutate(st_transform(geometry, st_crs(zip_geom))) %>% 
  # filter(grepl('183', ref)) %>% 
  filter(ref=="US 183" | ref=="US 183;FM 20" | ref=="US 183;TX 80" | ref=="US 90;US 183" | ref== "US 183;US 190;US 281"
         | ref=="US 183;US 190;US 281;FM 580" | ref=="US 183;US 190;US 281;Truck" | ref=="US 183;US 190" | id=="2c2066c3b6fafe2574463154f5cc877d" ) %>%
  pull(geometry) %>%
  st_union() %>%
  st_transform(st_crs(zcta_geom)) %>%
  st_intersection(zcta_geom)

ggplot()+
  geom_sf(data=zips_msa, mapping=aes(geometry=geometry, fill=SVI), #fill="white",
          color="gray")+ # , show.legend = FALSE
  scale_fill_gradient(low="lightyellow", high="slateblue4")+ #, 
  geom_sf(data = I35, col = "black", size=0.5)+
  geom_sf(data = US183, col = "black", size=0.5)+
  annotate(geom="text", x=-97.65, y=30.5, label="I-35", size=5)+
  annotate(geom="text", x=-97.84, y=30.5, label="US 183", size=5)+
  theme_void()+ # base_size = 10
  theme(
    legend.position = "right",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.text.align = 1,
    legend.key.size = unit(0.5, "lines"),
    plot.margin=unit(c(0, 0, 0, 0),"cm")
  )

#######

age_table <- get_acs(
  geography = "zcta",
  state="TX",
#  county="Travis",
  table = "B01001",
  year = 2019
)

varNames<-load_variables(2018, "acs5", cache = TRUE) %>%
  filter(str_detect(name,"B01001_")) %>% select(name,label) %>% 
  filter(!name %in% c("B01001_001","B01001_002","B01001_026")) %>%
  mutate(label=str_remove_all(label,"Estimate!!Total!!")) %>%
  mutate(label=label %>% str_replace_all(c("!!"="_"," "="_"))) %>%
  rename_at(c("name","label"),~c("variable","age_group"))

estPopByAgeGroup<-age_table %>% filter(str_detect(variable,"B01001_")) %>%
  left_join(varNames) %>% drop_na() %>% select(-moe,-NAME)

write_csv(estPopByAgeGroup,file="~/Projects/AustinGranularModel/DataJL/estPopByAgeGroup.csv")

estPopByAgeGroup %>% mutate(age_group=str_remove_all(age_group,"Female_")) %>%
  mutate(age_group=str_remove_all(age_group,"Male_")) %>%
  select(-variable) %>% group_by(GEOID,age_group) %>%
  summarise_each(sum) %>% pull(age_group) %>% unique()

write_csv(estPopByAgeGroup,file="~/Projects/AustinGranularModel/DataJL/estPopByAgeGroup1.csv")

ageGroupsTexasZCTA<-estPopByAgeGroup %>% mutate(age_group=str_remove_all(age_group,"Female_")) %>%
  mutate(age_group=str_remove_all(age_group,"Male_")) %>%
  select(-variable) %>% group_by(GEOID,age_group) %>%
  summarise(across(estimate,~mean(.x))) %>% rename_at("GEOID",~"ZCTA")

zipsTravis<-c("78739","78730","78733","78703","78736","78726","78746","78749","78756","78732","78759",
              "78734","78738","78731","78727","78701","78751","78704","78669","78750","78757","78645",
              "78722","78748","78652","78660","78735","78745","78705","78728","78747","78758","78653",
              "78723","78725","78754","78741","78753","78702","78752","78617","78719","78721","78744",
              "78724","78742")

#load("~/Projects/VulnerabilityMobility//Data/MovementZipToZip/forMapsWithSVI.RData")

forMapsWithSVI %>% glimpse()

load("~/Projects/SVITexas/sviForEachCountyAtZipLevel.RData")
dataMap<-sviForEachCountyAtZipLevel %>% filter(County=="Travis") %>% arrange(SVI) %>%
  mutate(SVIGroup_local=cut(SVI,quantile(SVI),include.lowest=TRUE,labels=FALSE) %>% as.factor()) %>%
  merge(forMapsWithSVI,by="Zip") %>%rename_at(c("SVI.x","SVI.y"),~c("SVI_local","SVI_Texas")) %>%
  select(-contains("county"),-HospPer100k,-CumInf95Crl) %>% as_tibble() %>% arrange(SVI_Texas) %>%
  mutate(SVIGroup_Texas=cut(SVI_Texas,quantile(SVI_Texas),include.lowest=TRUE,labels=FALSE) %>% as.factor()) %>%
  mutate(SVIGroup_Texas1=c(rep("1",11),rep("2",12),rep("3",11),rep("4",12))) %>%
  mutate(Region_Texas1=c(rep("East",23),rep("West",23))) %>%
  mutate(Region_local=ifelse(SVI_local>=0.5,"West","East")) %>%
  mutate(Region_Texas=ifelse(SVI_Texas>=0.5,"West","East")) %>% as_tibble()


Local<-ggplot()+ theme_void() +
  geom_sf(data=dataMap,aes(geometry=geometry,fill=Region_local))+ 
  geom_sf(data = I35, col = "black", size=0.5)+
  ggtitle("Using only zip codes in Austin")
#  geom_sf(data = US183, col = "black", size=0.5)
  
Texas<-ggplot()+ theme_void() +
  geom_sf(data=dataMap,aes(geometry=geometry,fill=Region_Texas))+ 
  geom_sf(data = I35, col = "black", size=0.5)+
  ggtitle("Using zip codes in the state")

Texas1<-ggplot()+ theme_void() +
  geom_sf(data=dataMap,aes(geometry=geometry,fill=Region_Texas1))+ 
  geom_sf(data = I35, col = "black", size=0.5)+
  ggtitle("Using zip codes in the state")

sviMap<-ggplot()+
  geom_sf(data=zips_msa, mapping=aes(geometry=geometry, fill=SVI), #fill="white",
          color="gray")+ # , show.legend = FALSE
  scale_fill_gradient(low="lightyellow", high="slateblue4")+ #, 
  geom_sf(data = I35, col = "black", size=1.5)+
#  geom_sf(data = US183, col = "black", size=0.5)+
  annotate(geom="text", x=-97.65, y=30.5, label="I-35", size=5)+
#  annotate(geom="text", x=-97.84, y=30.5, label="US 183", size=5)+
  theme_void()+ # base_size = 10
  theme(
    legend.position = "right",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.text.align = 1,
    legend.key.size = unit(0.5, "lines"),
    plot.margin=unit(c(0, 0, 0, 0),"cm")
  )

localGroup<-ggplot()+ theme_void() +
  geom_sf(data=dataMap,aes(geometry=geometry,fill=SVIGroup_local))+ 
  geom_sf(data = I35, col = "black", size=0.5)+
  ggtitle("Using only zip codes in Austin")

texasGroup<-ggplot()+ theme_void() +
  geom_sf(data=dataMap,aes(geometry=geometry,fill=SVIGroup_Texas))+ 
  geom_sf(data = I35, col = "black", size=0.5)+
  ggtitle("Using zip codes in the state")

texasGroup1<-ggplot()+ theme_void() +
  geom_sf(data=dataMap,aes(geometry=geometry,fill=SVIGroup_Texas1))+ 
  geom_sf(data = I35, col = "black", size=0.5)+
  ggtitle("Using zip codes in the state")

plot_grid(sviMap,Local,Texas,localGroup,texasGroup)  
plot_grid(Local,Texas1,localGroup,texasGroup1)  
plot_grid(Local,localGroup)  

ggsave(last_plot(),file="~/Projects/H5N1/AustinMaps1.png",width=15,height = 6)

zipsInRegions<-dataMap %>% as_tibble() %>% select(Zip,Region_local)

popuInAgeGroupByRegion<-ageGroupsTexasZCTA %>% merge(zipsInRegions,by.x="ZCTA",by.y="Zip") %>%
  select(-ZCTA) %>% group_by(age_group,Region_local) %>%
  summarise(across(estimate,~sum(.x)))

write_csv(popuInAgeGroupByRegion,file="~/Projects/H5N1/popuInAgeGroupByRegion.csv")




