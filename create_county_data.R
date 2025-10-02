library(tidyverse)
# Total_Educ_Total_Exp
# Total_Revenue
CAGDP9="/Users/ec1269/Desktop/localAI/CAGDP9/CAGDP9__ALL_AREAS_2001_2023.csv" %>% read_csv %>%
  filter(GeoFIPS %>% str_sub(5,5)!=0) 


# Aggregates

CAGDP9 %>% filter(LineCode==2) %>% select(1,9:31) %>% write_csv("private_gdp.csv")
CAGDP9 %>% filter(LineCode==1) %>% select(1,9:31) %>% write_csv("total_gdp.csv")

# Industry Specific

# Manufacturing


CAGDP9 %>% filter(LineCode==12) %>% select(1,9:31) %>% write_csv("manuf_gdp.csv")
CAGDP9 %>% filter(LineCode==45) %>% select(1,9:31) %>% write_csv("information_gdp.csv") # information
CAGDP9 %>% filter(LineCode==59) %>% select(1,9:31) %>% write_csv("business_gdp.csv") # Professional and business services 
CAGDP9 %>% filter(LineCode==64) %>% select(1,9:31) %>% write_csv("management_gdp.csv") # Management
CAGDP9 %>% filter(LineCode==60) %>% select(1,9:31) %>% write_csv("research_gdp.csv") # Management

# 1 ...                    All industry total                                                      
# 2 ...                    Private industries                                                      
# 3 11                     Agriculture, forestry, fishing and hunting                              
# 4 21                     Mining, quarrying, and oil and gas extraction                           
# 5 22                     Utilities                                                               
# 6 23                     Construction                                                            
# 7 31-33                  Manufacturing                                                           
# 8 321,327-339            Durable goods manufacturing                                             
# 9 311-316,322-326        Nondurable goods manufacturing                                          
# 10 42                     Wholesale trade                                                         
# 11 44-45                  Retail trade                                                            
# 12 48-49                  Transportation and warehousing                                          
# 13 51                     Information                                                             
# 14 52,53                  Finance, insurance, real estate, rental, and leasing                    
# 15 52                     Finance and insurance                                                   
# 16 53                     Real estate and rental and leasing                                      
# 17 54,55,56               Professional and business services                                      
# 18 54                     Professional, scientific, and technical services                        
# 19 55                     Management of companies and enterprises                                 
# 20 56                     Administrative and support and waste management and remediation services
# 21 61,62                  Educational services, health care, and social assistance                
# 22 61                     Educational services                                                    
# 23 62                     Health care and social assistance                                       
# 24 71,72                  Arts, entertainment, recreation, accommodation, and food services       
# 25 71                     Arts, entertainment, and recreation                                     
# 26 72                     Accommodation and food services                                         
# 27 81                     Other services (except government and government enterprises)           
# 28 92                     Government and government enterprises                                   
# 29 11,21                  Natural resources and mining                                            
# 30 42,44-45               Trade                                                                   
# 31 22,48-49               Transportation and utilities                                            
# 32 31-33,51               Manufacturing and information                                           
# 33 ...                    Private goods-producing industries 2/                                   
#   34 ...                    Private services-providing industries 3/                                
#   35 NA                     NA       




 
# Dirst download all the CBP county data and combine them 
# first recover aggregate (all industries combined) cbp data by filtering the codes that

# then recover all two digit naics by filtering the codes that
# have only 2-digits (3 character is "-" while the first character is a digit and save

cbpco=bind_rows("/Volumes/Extreme SSD/cbpco_old_ag.parquet" %>% arrow::read_parquet()%>% filter(naics %>% str_sub(1,1)=="-"),
                "/Volumes/Extreme SSD/cbpco_new_ag.parquet"  %>% arrow::read_parquet()%>% filter(naics %>% str_sub(1,1)=="-") %>% rename(n1_4=12))

cbpco_naics2=bind_rows("/Volumes/Extreme SSD/cbpco_old_ag.parquet" %>% arrow::read_parquet()%>% filter(naics %>% str_sub(3,3)=="-")%>% filter(naics %>% str_sub(1,1)%in% c(0:9)),
                       "/Volumes/Extreme SSD/cbpco_new_ag.parquet"  %>% arrow::read_parquet()%>% filter(naics %>% str_sub(3,3)=="-")%>% filter(naics %>% str_sub(1,1)%in% c(0:9)) %>% rename(n1_4=12))

cbpco %>% write_csv("cbpco.csv")
cbpco_naics2%>% write_csv("cbpco_naics2.csv")


# download all county level QWI files from the ftp of the census
# and save them in a folder named qwifolder
qwifolder=""
pathfiles=paste0(qwifolder,list.files(path = qwifolder,pattern = "[a-z][a-z]_all_E0_ns.parquet"))

# calculate qwi averages for each year for the main variables since qwi are at quarterly frequency
qwi_average = purrr::map_df(pathfiles,.f = function(dataset) dataset %>% arrow::read_parquet() %>% mutate(geography=as.numeric(geography)) %>% 
                              filter(year>2009) %>% select(geography,industry,year,Emp,EmpS,HirN,HirR,Sep,SepS,TurnOvrS,EarnS,Payroll,EarnHirNS,FrmJbGn,FrmJbC,FrmJbLs,FrmJbCS)|>
                              mutate(newhirerate=HirN/EmpS,stableseprate=SepS/EmpS,Sjobcreationrate=FrmJbCS/EmpS,jobcreationrate=FrmJbC/Emp,stabejobs=EmpS/Emp) %>% group_by( geography,industry,year) %>% summarise_all(mean) %>%
                              select(  geography ,industry ,year ,Emp,EmpS,TurnOvrS,EarnS,newhirerate:stabejobs))

# save for aggregated industries
qwi_average %>% group_by(geography,year) %>% summarise(Emp =sum(Emp,na.rm = T), EmpS =sum(EmpS,na.rm = T),EarnS=mean(EarnS,na.rm = T),
                                                       TurnOvrS=mean(TurnOvrS,na.rm = T),
                                                       newhirerate=mean(newhirerate,na.rm = T),
                                                       stableseprate=mean(stableseprate,na.rm = T),
                                                       Sjobcreationrate=mean(Sjobcreationrate,na.rm = T),
                                                       jobcreationrate=mean(jobcreationrate,na.rm = T),
                                                       stablejobs=mean(stabejobs,na.rm = T)) %>% 
  write_csv("qwi_totals.csv")


qwi_average %>% filter(industry %>% str_sub(1,2)=="54"|industry %>% str_sub(1,2)=="55"|industry %>% str_sub(1,2)=="56") %>% group_by(geography,year) %>% summarise(Emp =sum(Emp,na.rm = T), EmpS =sum(EmpS,na.rm = T),EarnS=mean(EarnS,na.rm = T),
                                                                                                                                                                   TurnOvrS=mean(TurnOvrS,na.rm = T),
                                                                                                                                                                   newhirerate=mean(newhirerate,na.rm = T),
                                                                                                                                                                   stableseprate=mean(stableseprate,na.rm = T),
                                                                                                                                                                   Sjobcreationrate=mean(Sjobcreationrate,na.rm = T),
                                                                                                                                                                   jobcreationrate=mean(jobcreationrate,na.rm = T),
                                                                                                                                                                   stablejobs=mean(stabejobs,na.rm = T)) %>% 
  write_csv("qwi_business.csv")

qwi_average %>% filter(industry %>% str_sub(1,2)=="51") %>% group_by(geography,year) %>% summarise(Emp =sum(Emp,na.rm = T), EmpS =sum(EmpS,na.rm = T),EarnS=mean(EarnS,na.rm = T),
                                                                                                   TurnOvrS=mean(TurnOvrS,na.rm = T),
                                                                                                   newhirerate=mean(newhirerate,na.rm = T),
                                                                                                   stableseprate=mean(stableseprate,na.rm = T),
                                                                                                   Sjobcreationrate=mean(Sjobcreationrate,na.rm = T),
                                                                                                   jobcreationrate=mean(jobcreationrate,na.rm = T),
                                                                                                   stablejobs=mean(stabejobs,na.rm = T)) %>% 
  write_csv("qwi_information.csv")

qwi_average %>% filter(industry %>% str_sub(1,2)=="54") %>% group_by(geography,year) %>% summarise(Emp =sum(Emp,na.rm = T), EmpS =sum(EmpS,na.rm = T),EarnS=mean(EarnS,na.rm = T),
                                                                                                   TurnOvrS=mean(TurnOvrS,na.rm = T),
                                                                                                   newhirerate=mean(newhirerate,na.rm = T),
                                                                                                   stableseprate=mean(stableseprate,na.rm = T),
                                                                                                   Sjobcreationrate=mean(Sjobcreationrate,na.rm = T),
                                                                                                   jobcreationrate=mean(jobcreationrate,na.rm = T),
                                                                                                   stablejobs=mean(stabejobs,na.rm = T)) %>% 
  write_csv("qwi_research.csv")

qwi_average %>% filter(industry %>% str_sub(1,2)=="55") %>% group_by(geography,year) %>% summarise(Emp =sum(Emp,na.rm = T), EmpS =sum(EmpS,na.rm = T),EarnS=mean(EarnS,na.rm = T),
                                                                                                   TurnOvrS=mean(TurnOvrS,na.rm = T),
                                                                                                   newhirerate=mean(newhirerate,na.rm = T),
                                                                                                   stableseprate=mean(stableseprate,na.rm = T),
                                                                                                   Sjobcreationrate=mean(Sjobcreationrate,na.rm = T),
                                                                                                   jobcreationrate=mean(jobcreationrate,na.rm = T),
                                                                                                   stablejobs=mean(stabejobs,na.rm = T)) %>% 
  write_csv("qwi_management.csv")

qwi_average %>% filter(industry %>% str_sub(1,1)=="3") %>% group_by(geography,year) %>% summarise(Emp =sum(Emp,na.rm = T), EmpS =sum(EmpS,na.rm = T),EarnS=mean(EarnS,na.rm = T),
                                                                                                  TurnOvrS=mean(TurnOvrS,na.rm = T),
                                                                                                  newhirerate=mean(newhirerate,na.rm = T),
                                                                                                  stableseprate=mean(stableseprate,na.rm = T),
                                                                                                  Sjobcreationrate=mean(Sjobcreationrate,na.rm = T),
                                                                                                  jobcreationrate=mean(jobcreationrate,na.rm = T),
                                                                                                  stablejobs=mean(stabejobs,na.rm = T)) %>% 
  write_csv("qwi_manuf.csv")


read_csv("cbpco.csv") %>% mutate(small=as.numeric(n1_4)+as.numeric(n5_9),
                                 medium= as.numeric(n10_19)+as.numeric(n20_49)) %>% select(year,1,2,emp,qp1,ap,est,small,medium) %>% 
  write_csv("cbp_totals.csv")
read_csv("cbpco_naics2.csv") %>% filter(naics %>% str_sub(1,2)=="31")%>% mutate(small=as.numeric(n1_4)+as.numeric(n5_9),
                                                                                medium= as.numeric(n10_19)+as.numeric(n20_49)) %>% select(year,1,2,emp,qp1,ap,est,small,medium) %>% 
  write_csv("cbp_manuf.csv")
gc()
read_csv("cbpco_naics2.csv") %>% filter(naics %>% str_sub(1,2)=="51")%>% mutate(small=as.numeric(n1_4)+as.numeric(n5_9),
                                                                                medium= as.numeric(n10_19)+as.numeric(n20_49)) %>% select(year,1,2,emp,qp1,ap,est,small,medium) %>% 
  write_csv("cbp_information.csv")
gc()
read_csv("cbpco_naics2.csv") %>% filter(naics %>% str_sub(1,2)=="55")%>% mutate(small=as.numeric(n1_4)+as.numeric(n5_9),
                                                                                medium= as.numeric(n10_19)+as.numeric(n20_49)) %>% select(year,1,2,emp,qp1,ap,est,small,medium) %>% 
  write_csv("cbp_management.csv")
gc()
read_csv("cbpco_naics2.csv") %>% filter(naics %>% str_sub(1,2)=="54")%>% mutate(small=as.numeric(n1_4)+as.numeric(n5_9),
                                                                                medium= as.numeric(n10_19)+as.numeric(n20_49)) %>% select(year,1,2,emp,qp1,ap,est,small,medium) %>% 
  write_csv("cbp_research.csv")
read_csv("cbpco_naics2.csv") %>% filter(naics %>% str_sub(1,2)=="54"|naics %>% str_sub(1,2)=="55"|naics %>% str_sub(1,2)=="56")%>% mutate(small=as.numeric(n1_4)+as.numeric(n5_9),
                                                                                                                                          medium= as.numeric(n10_19)+as.numeric(n20_49)) %>% select(year,1,2,emp,qp1,ap,est,small,medium) %>% 
  group_by(year,fipstate,fipscty) %>% summarise_all(sum) %>% 
  write_csv("cbp_business.csv")


# Create IPEDS data at the county level to create the STEM share variables

options("ipeds.download.dir" = "~/Library/CloudStorage/GoogleDrive-ec1269@georgetown.edu/.shortcut-targets-by-id/1_A02AjcXugwRw245d9TPn3fe8RiOXqmQ/stem/raw/")

ipeds::available_ipeds()

ipeds::download_ipeds(2009)
ipeds::download_ipeds(2010)
ipeds::download_ipeds(2011)
ipeds::download_ipeds(2012)
ipeds::download_ipeds(2013)
ipeds::download_ipeds(2014)
ipeds::download_ipeds(2015)
ipeds::download_ipeds(2016)
ipeds::download_ipeds(2017)
ipeds::download_ipeds(2018)
ipeds::download_ipeds(2019)
ipeds::download_ipeds(2020)
ipeds::download_ipeds(2021)
ipeds::download_ipeds(2022)

load("~/Library/CloudStorage/GoogleDrive-ec1269@georgetown.edu/.shortcut-targets-by-id/1_A02AjcXugwRw245d9TPn3fe8RiOXqmQ/stem/raw/IPEDS2021-22.Rda")
load("~/Library/CloudStorage/GoogleDrive-ec1269@georgetown.edu/.shortcut-targets-by-id/1_A02AjcXugwRw245d9TPn3fe8RiOXqmQ/stem/raw/IPEDS2020-21.Rda")


# Create IPEDS data
create_ipeds <- function(db,yy) {
  
  counties=db[[26]] %>% select(1,COUNTYCD)
  names(counties) <- tolower(names(counties))
  
  
  # Use the enrollment survey data.
  
  completions <- db[[45]]
  names(completions) <- tolower(names(completions))
  completions <- merge(completions, counties, by = "unitid", all.x = TRUE, sort = FALSE)
  ucompletions=completions %>% filter(majornum==1,awlevel==5) 
  mcompletions=completions %>% filter(majornum==1,awlevel==7) 
  
  
  
  ucompletions %>% mutate(cipcode=cipcode %>% as.character()) %>% select(countycd,ctotalt) %>% 
    group_by(countycd) %>% summarise(udeg=sum(ctotalt)) %>% full_join(mcompletions %>% mutate(cipcode=cipcode %>% as.character()) %>% select(countycd,ctotalt) %>% 
                                                                        group_by(countycd) %>% summarise(mdeg=sum(ctotalt))) %>% left_join(
                                                                          read_csv("cipstem.csv",col_types = "cc") %>% 
                                                                            inner_join(ucompletions %>% mutate(cipcode=cipcode %>% as.character())) %>% select(countycd,ctotalt) %>% 
                                                                            group_by(countycd) %>% summarise(ustemdeg=sum(ctotalt))) %>% left_join(
                                                                              read_csv("cipstem.csv",col_types = "cc") %>% inner_join(mcompletions %>% mutate(cipcode=cipcode %>% as.character())) %>% select(countycd,ctotalt) %>% 
                                                                                group_by(countycd) %>% summarise(mstemdeg=sum(ctotalt))) %>% filter(countycd>0) %>% mutate(year=yy) %>% 
    write_csv(glue::glue("ipeds{yy}.csv"))
  
}
