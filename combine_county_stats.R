# Manolis Chatzikonstantinou
# January 2025
# this script uses the intermediate files prepared in the previous steps - combines the data in a single file - 
# prepares additional variables used in the regressions.
# Variables, needed for the regressions: 
# ai_intensity; Share of AI jobs; AI_mentions/N_Job_Posts,
# share_bac= Pop. with bachelors/Population,
# share_black= Black Population/Population,
# share_poverty= Population in poverty/Population
# logpop= Log(Population)
# logincome= Log(Median Household Income),
# tightness= Number of Job Posts/Number of Unemployed,
# pat_intensity = Number of Job Posts/Number of Unemployed
#patai_intensity
#degshare
#stemshare
#large_firms
#information_intensity
#manuf_intensity
#TurnOvrS
# hpi

hpi=readxl::read_excel("~/Documents/hpi_at_bdl_county.xlsx",skip = 6) %>% select(3:6) %>% mutate_all(as.numeric) %>% rename(county=1)



# qcew
# Download from QCEW all files at the annual level
# {year}_annual_singlefile.zip

qcew_make <- function(year) {
  qcew2017=data.table::fread(glue::glue("~/Downloads/{year}_annual_singlefile.zip"))
  
  # 31 missing %>%  filter(disclosure_code=="N")
  qcew2017 %>% filter(own_code==5,agglvl_code==71) %>% select(area_fips,year,disclosure_code,annual_avg_estabs:avg_annual_pay) %>%  
    mutate(state = floor(as.numeric(area_fips)/1000),
           county =  as.numeric(area_fips) %% 1000  ) %>%
    write_csv(glue::glue("~/Downloads/qcew_{year}.csv"))

  
  
  qcew2017 %>% filter(own_code==5,agglvl_code==74,industry_code=="31-33") %>% select(area_fips,year,disclosure_code,annual_avg_estabs:avg_annual_pay) %>%  
    mutate(state = floor(as.numeric(area_fips)/1000),
           county =  as.numeric(area_fips) %% 1000  )%>%
    rename_with(~ paste0("manuf_", .), 
                .cols = c("annual_avg_estabs":"avg_annual_pay")) %>% select(-area_fips)  %>% 
    rename(Year=year)%>% write_csv(glue::glue("~/Downloads/qcew_{year}_manuf.csv"))

  
  
  qcew2017 %>% filter(own_code==5,agglvl_code==74,industry_code=="51") %>% select(area_fips,year,disclosure_code,annual_avg_estabs:avg_annual_pay) %>%  
    mutate(state = floor(as.numeric(area_fips)/1000),
           county =  as.numeric(area_fips) %% 1000  )%>%
    rename_with(~ paste0("information_", .), 
                .cols = c("annual_avg_estabs":"avg_annual_pay")) %>% select(-area_fips)  %>% 
    rename(Year=year)%>% write_csv(glue::glue("~/Downloads/qcew_{year}_information.csv"))
  
  qcew2017 %>% filter(own_code==5,agglvl_code==74,industry_code=="54") %>% select(area_fips,year,disclosure_code,annual_avg_estabs:avg_annual_pay) %>%  
    mutate(state = floor(as.numeric(area_fips)/1000),
           county =  as.numeric(area_fips) %% 1000  )%>%
    rename_with(~ paste0("research_", .), 
                .cols = c("annual_avg_estabs":"avg_annual_pay")) %>% select(-area_fips)  %>% 
    rename(Year=year)%>% write_csv(glue::glue("~/Downloads/qcew_{year}_research.csv"))
  
  
  qcew2017 %>% filter(own_code==5,agglvl_code==74,industry_code=="54") %>% select(area_fips,year,disclosure_code,annual_avg_estabs:avg_annual_pay) %>%  
    mutate(state = floor(as.numeric(area_fips)/1000),
           county =  as.numeric(area_fips) %% 1000  )%>%
    rename_with(~ paste0("management_", .), 
                .cols = c("annual_avg_estabs":"avg_annual_pay")) %>% select(-area_fips)  %>% 
    rename(Year=year) %>% write_csv(glue::glue("~/Downloads/qcew_{year}_management.csv"))
  rm(qcew2017)
  gc()
}
qcew_make(2010)
qcew_make(2011)
qcew_make(2012)
qcew_make(2013)
qcew_make(2014)
qcew_make(2015)
qcew_make(2016)
qcew_make(2017)
qcew_make(2018)
qcew_make(2019)
qcew_make(2020)
qcew_make(2021)
qcew_make(2022)
qcew_make(2023)

qcew_management=purrr::map_df(2010:2023,.f = function(year) read_csv(glue::glue("~/Downloads/qcew_{year}_management.csv")))
qcew_manuf=purrr::map_df(2010:2023,.f = function(year) read_csv(glue::glue("~/Downloads/qcew_{year}_manuf.csv")))
qcew_information=purrr::map_df(2010:2023,.f = function(year) read_csv(glue::glue("~/Downloads/qcew_{year}_information.csv")))
qcew_research=purrr::map_df(2010:2023,.f = function(year) read_csv(glue::glue("~/Downloads/qcew_{year}_research.csv")))
qcew=purrr::map_df(2010:2023,.f = function(year) read_csv(glue::glue("~/Downloads/qcew_{year}.csv")))

# laus
# Download all LAUS data from BLS
laus=bind_rows("/Volumes/Untitled/segments/glfactors/laucnty01.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty02.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty03.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty04.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty05.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty06.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty07.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty08.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty09.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty10.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty11.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty12.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty13.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty14.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty15.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty16.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty17.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty18.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty19.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty20.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty21.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty22.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7),
               "/Volumes/Untitled/segments/glfactors/laucnty23.xlsx" %>% readxl::read_excel(skip=4) %>% select(2,3,5,7,8,9,10) %>% drop_na() %>% rename(state=1,county=2,unrate=7))

laus %>% write_csv("laus.csv")

laus <-read_csv("laus.csv")

# state 9 y=2022 fixme
# state 51, 48 constantly wrong fix me

# Download county demographics data from ACS
# We relied on https://mcdc.missouri.edu and ACS 5 year summaries at the county level to create a summary of demographics for each year 
# found in this folder.
county_stats=read_csv("laus.csv") %>% mutate(state=as.numeric(state),county=as.numeric(county)) %>% 
  left_join(read_csv("demographics.csv") %>% rename(Year=year) %>% mutate(state = floor(GEOID/1000),county =  GEOID %% 1000  )) %>% drop_na(GEOID)

# We relied on data from USPTO (with detailed location information from PatentsView) and classification of patents as AI 
# to create counts of patents and inventors, either aggregate or AI specific.

county_stats=county_stats %>% left_join(read_csv("county_patents.csv") %>% rename(state=2,county=3)%>% mutate(Year=as.numeric(year)) %>% select(-year)) %>%
  mutate( n_inventors=replace_na(n_inventors,0), n_patents=replace_na(n_patents,0))%>%
  left_join(read_csv("county_patents_ai.csv") %>% rename(state=2,county=3)%>% mutate(Year=as.numeric(year))%>% select(-year)) %>%
  mutate( ai_inventors=replace_na(ai_inventors,0), ai_patents=replace_na(ai_patents,0))
# can not match very small set of patents: state and county
#    78     10
#    66     10
#    69    110
#    66     NA
#    33     NA
#    78     30
#    41     NA
#    12     NA

county_stats=county_stats %>% left_join(read_csv("healthcare_researchers.csv")  %>% mutate(state=STATE %>% as.numeric,county=COUNTY %>% as.numeric) %>% 
  rename(health_researchers=n_researchers))%>%
  mutate( health_researchers=replace_na(health_researchers,0)) %>% 
  left_join(read_csv("education_researchers.csv")  %>% mutate(state=STATE %>% as.numeric,county=COUNTY %>% as.numeric) %>% 
                                                                         rename(education_researchers=n_researchers))%>%
  mutate( education_researchers=replace_na(education_researchers,0)) %>% 
  left_join(read_csv("company_researchers.csv")  %>% mutate(state=STATE %>% as.numeric,county=COUNTY %>% as.numeric) %>% 
              rename(company_researchers=n_researchers))%>%
  mutate( company_researchers=replace_na(company_researchers,0)) %>% 
  left_join(read_csv("government_researchers.csv")  %>% mutate(state=STATE %>% as.numeric,county=COUNTY %>% as.numeric) %>% 
              rename(government_researchers=n_researchers))%>%
  mutate( government_researchers=replace_na(government_researchers,0)) %>% 
  left_join(read_csv("county_researchers.csv")  %>% mutate(state=STATE %>% as.numeric,county=COUNTY %>% as.numeric) %>% 
              rename(n_researchers=n_researchers))%>%
  mutate( n_researchers=replace_na(n_researchers,0)) 

#read_csv("county_researchers.csv")%>% mutate(state=STATE %>% as.numeric,county=COUNTY %>% as.numeric) %>%
#  mutate( n_researchers=replace_na(n_researchers,0)) %>% anti_join(county_stats) %>% filter(Year>2010)
# 2022 connecticut because they changed fips and 46113
 



county_stats=county_stats %>% left_join(read_csv("information_gdp.csv")%>% 
    mutate(state = floor(as.numeric(GeoFIPS)/1000),county =  as.numeric(GeoFIPS) %% 1000  ) %>% select(-GeoFIPS) %>% 
    pivot_longer(-c(state,county)) %>% rename(information_gdp=value,Year=name) %>% 
    mutate(Year=as.numeric(Year))) %>% 
  left_join(read_csv("business_gdp.csv")%>% 
                  mutate(state = floor(as.numeric(GeoFIPS)/1000),county =  as.numeric(GeoFIPS) %% 1000  ) %>% select(-GeoFIPS) %>% 
                  pivot_longer(-c(state,county)) %>% rename(business_gdp=value,Year=name) %>% 
                  mutate(Year=as.numeric(Year)))%>% 
  left_join(read_csv("management_gdp.csv")%>% 
              mutate(state = floor(as.numeric(GeoFIPS)/1000),county =  as.numeric(GeoFIPS) %% 1000  ) %>% select(-GeoFIPS) %>% 
              pivot_longer(-c(state,county)) %>% rename(management_gdp=value,Year=name) %>% 
              mutate(Year=as.numeric(Year)))%>% 
  left_join(read_csv("manuf_gdp.csv")%>% 
              mutate(state = floor(as.numeric(GeoFIPS)/1000),county =  as.numeric(GeoFIPS) %% 1000  ) %>% select(-GeoFIPS) %>% 
              pivot_longer(-c(state,county)) %>% rename(manuf_gdp=value,Year=name) %>% 
              mutate(Year=as.numeric(Year)))%>% 
  left_join(read_csv("research_gdp.csv")%>% 
              mutate(state = floor(as.numeric(GeoFIPS)/1000),county =  as.numeric(GeoFIPS) %% 1000  ) %>% select(-GeoFIPS) %>% 
              pivot_longer(-c(state,county)) %>% rename(research_gdp=value,Year=name) %>% 
              mutate(Year=as.numeric(Year)))%>% 
  left_join(read_csv("private_gdp.csv")%>% 
              mutate(state = floor(as.numeric(GeoFIPS)/1000),county =  as.numeric(GeoFIPS) %% 1000  ) %>% select(-GeoFIPS) %>% 
              pivot_longer(-c(state,county)) %>% rename(private_gdp=value,Year=name) %>% 
              mutate(Year=as.numeric(Year)))%>% 
  left_join(read_csv("total_gdp.csv")%>% 
              mutate(state = floor(as.numeric(GeoFIPS)/1000),county =  as.numeric(GeoFIPS) %% 1000  ) %>% select(-GeoFIPS) %>% 
              pivot_longer(-c(state,county)) %>% rename(total_gdp=value,Year=name) %>% 
              mutate(Year=as.numeric(Year)))
# state 2 and 51 are missing due to gdp data construction
county_stats=county_stats %>% left_join(read_csv("qwi_totals.csv")%>% 
                                          mutate(state = floor(as.numeric(geography)/1000),
                                                 county =  as.numeric(geography) %% 1000  ) %>% select(-geography)  %>% 
                                          rename(Year=year))  %>% 
  left_join(read_csv("qwi_information.csv")%>%  mutate(state = floor(as.numeric(geography)/1000),
                                                 county =  as.numeric(geography) %% 1000  )%>%
  rename_with(~ paste0("information_", .), 
              .cols = c("Emp", "EmpS", "EarnS", "TurnOvrS", "newhirerate", 
                        "stableseprate", "Sjobcreationrate", "jobcreationrate", "stablejobs")) %>% select(-geography)  %>% 
    rename(Year=year))%>% 
  left_join(read_csv("qwi_management.csv")%>%  mutate(state = floor(as.numeric(geography)/1000),
                                                  county =  as.numeric(geography) %% 1000  )%>%
              rename_with(~ paste0("management_", .), 
                          .cols = c("Emp", "EmpS", "EarnS", "TurnOvrS", "newhirerate", 
                                    "stableseprate", "Sjobcreationrate", "jobcreationrate", "stablejobs")) %>% select(-geography)  %>% 
              rename(Year=year))%>% 
  left_join(read_csv("qwi_business.csv")%>%  mutate(state = floor(as.numeric(geography)/1000),
                                                      county =  as.numeric(geography) %% 1000  )%>%
              rename_with(~ paste0("business_", .), 
                          .cols = c("Emp", "EmpS", "EarnS", "TurnOvrS", "newhirerate", 
                                    "stableseprate", "Sjobcreationrate", "jobcreationrate", "stablejobs")) %>% select(-geography)  %>% 
              rename(Year=year))%>% 
  left_join(read_csv("qwi_manuf.csv")%>%  mutate(state = floor(as.numeric(geography)/1000),
                                                      county =  as.numeric(geography) %% 1000  )%>%
              rename_with(~ paste0("manuf_", .), 
                          .cols = c("Emp", "EmpS", "EarnS", "TurnOvrS", "newhirerate", 
                                    "stableseprate", "Sjobcreationrate", "jobcreationrate", "stablejobs")) %>% select(-geography)  %>% 
              rename(Year=year))%>% 
  left_join(read_csv("qwi_research.csv")%>%  mutate(state = floor(as.numeric(geography)/1000),
                                                 county =  as.numeric(geography) %% 1000  )%>%
              rename_with(~ paste0("research_", .), 
                          .cols = c("Emp", "EmpS", "EarnS", "TurnOvrS", "newhirerate", 
                                    "stableseprate", "Sjobcreationrate", "jobcreationrate", "stablejobs")) %>% select(-geography)  %>% 
              rename(Year=year))
# state 9 can not be matched
# + a few states missing from qwi

county_stats=county_stats %>% left_join(read_csv("cbp_totals.csv")%>% mutate(Year=as.numeric(paste0(20,year)))%>% 
                                          rename(state = 2,county = 3 ) %>% select(-year))  %>% 
  left_join(read_csv("cbp_research.csv")%>% mutate(Year=as.numeric(paste0(20,year)))%>% 
              rename(state = 2,county = 3 ) %>% select(-year)%>%
              rename_with(~ paste0("research_", .), 
                          .cols = c("emp", "qp1", "ap", "est", "small", 
                                    "medium") ))%>% 
  left_join(read_csv("cbp_information.csv")%>% mutate(Year=as.numeric(paste0(20,year)))%>% 
              rename(state = 2,county = 3 ) %>% select(-year)%>%
              rename_with(~ paste0("information_", .), 
                          .cols = c("emp", "qp1", "ap", "est", "small", 
                                    "medium") ))%>% 
  left_join(read_csv("cbp_management.csv")%>% mutate(Year=as.numeric(paste0(20,year)))%>% 
              rename(state = 2,county = 3 ) %>% select(-year)%>%
              rename_with(~ paste0("management_", .), 
                          .cols = c("emp", "qp1", "ap", "est", "small", 
                                    "medium") ))%>% 
  left_join(read_csv("cbp_manuf.csv")%>% mutate(Year=as.numeric(paste0(20,year)))%>% 
              rename(state = 2,county = 3 ) %>% select(-year)%>%
              rename_with(~ paste0("manuf_", .), 
                          .cols = c("emp", "qp1", "ap", "est", "small", 
                                    "medium") ))%>% 
  left_join(read_csv("cbp_business.csv")%>% mutate(Year=as.numeric(paste0(20,year)))%>% 
              rename(state = 2,county = 3 ) %>% select(-year)%>%
              rename_with(~ paste0("business_", .), 
                          .cols = c("emp", "qp1", "ap", "est", "small", 
                                    "medium") ))
# FIXME add the counties that we are missing information
# STEM degrees

ipeds=bind_rows(read_csv("ipeds2011.csv"),
          read_csv("ipeds2012.csv"),
          read_csv("ipeds2013.csv"),
          read_csv("ipeds2014.csv"),
          read_csv("ipeds2015.csv"),
          read_csv("ipeds2016.csv"),
          read_csv("ipeds2017.csv"),
          read_csv("ipeds2018.csv"),
          read_csv("ipeds2019.csv"),
          read_csv("ipeds2020.csv"),
          read_csv("ipeds2021.csv"),
          read_csv("ipeds2022.csv"))%>%  mutate(state = floor(as.numeric(countycd)/1000),
                                                county =  as.numeric(countycd) %% 1000  )
          


county_stats=county_stats %>% left_join(ipeds %>% rename(Year=year)) %>% 
  mutate( udeg =replace_na(udeg,0),
          mdeg =replace_na(mdeg,0),
          ustemdeg =replace_na(ustemdeg,0),
          mstemdeg =replace_na(mstemdeg,0)) 

county_stats %>% left_join(qcew %>% select(-area_fips) %>% rename(tdisclosure_code=disclosure_code,Year=year)) %>% 
  left_join(qcew_manuf%>% rename(disclosure_code_manuf=disclosure_code)) %>% 
  left_join(qcew_management%>% rename(disclosure_code_management=disclosure_code)) %>% 
  left_join(qcew_information%>% rename(disclosure_code_information=disclosure_code)) %>% 
  left_join(qcew_research%>% rename(disclosure_code_research=disclosure_code))%>% write_csv("county_controls_prelim.csv")

 