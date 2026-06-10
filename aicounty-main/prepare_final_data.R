# Column1: Demographics
library(pander)
library(fixest)
library(tidyverse)

county_controls_prelim=read_csv("county_controls_prelim.csv")
updated_lightcast_county="~/Dropbox/Rotemberg/counties/updated_lightcast_county.csv" %>% read_csv


data_ai=updated_lightcast_county %>% rename(FIPS=1,year=4,ai=7,da=6,nads=8)%>% select(1,year,ai,nads)

data_ai=data_ai %>% mutate(GEOID=as.numeric(FIPS))%>% inner_join(county_controls_prelim %>% rename(year=Year) %>% mutate(year=year+1))

# prepare demographics
# right hand side variables are demographics: unemployment rate:unrate, log median household income, share_bac, share_black, share_poverty, log HPI, log population, log immigration.
data_ai=data_ai %>% mutate(logincome=log(medhhincome),loghpi=log(hpi),logemp=log(pop_above18),logim=log(immigration))
data_ai=data_ai %>% mutate(gincome=logincome-lag(logincome,1),ghpi=loghpi-lag(loghpi,1),
                           gemp=logemp-lag(logemp,1),gim=logim-lag(logim,1))
data_ai=data_ai %>% mutate(dunrate=unrate-lag(unrate,1),dshare_bac=share_bac-lag(share_bac,1),
                           dshare_black=share_black-lag(share_black,1),
                           dmedage=medage-lag(medage,1))

data_ai=data_ai %>% mutate(large_firms=1-(small+medium)/est,management_intensity=management_emp/emp,
                           information_intensity=information_emp/emp,
                           information_intensity=manuf_emp/emp)


# research_intensity+patai_intensity+pat_intensity
# log number of patents, log number of inventors, log AI patents, log AI inventors, log number of researchers.
data_ai=data_ai %>% mutate(logemp=log(Employed))%>% mutate(logpop=log(pop))
data_ai=data_ai %>% mutate(logn_patents=log(1+n_patents),logn_inventors=log(1+n_inventors),
                           logn_researchers=log(1+n_researchers),
                           logai_patents=log(1+ai_patents),logai_inventors=log(1+ai_inventors))%>%
  mutate(pat_intensity=n_inventors/Employed,patai_intensity=ai_patents/n_patents,
         research_intensity=n_researchers/Employed) %>% mutate(patai_intensity=replace_na(patai_intensity,0))
data_ai=data_ai %>% mutate(pat_intensity1=lag(pat_intensity,9),patai_intensity1=lag(patai_intensity,9),
                           research_intensity1=lag(research_intensity,9))

data_ai=data_ai %>% mutate(pat_intensity1=lag(pat_intensity,9),patai_intensity1=lag(patai_intensity,9),
                           research_intensity1=lag(research_intensity,9))
data_ai= data_ai%>% mutate(small_firms=small/est,large_firms=1-(small+medium)/est,management_intensity=management_emp/emp,
                           information_intensity=information_emp/emp,
                           business_intensity=business_emp/emp,
                           manuf_intensity=manuf_emp/emp)

data_ai=data_ai%>% mutate(large_manuf_est=manuf_est-manuf_small-manuf_medium) %>% 
  mutate(large_information_est=information_est-information_small-information_medium) %>% 
  mutate(large_manuf_est0=replace_na(large_manuf_est,0))%>% 
  mutate(large_information_est0=replace_na(large_information_est,0))%>% 
  mutate(large_manuf_share=large_manuf_est/est,large_information_share=large_information_est/est)

data_ai= data_ai %>% mutate(pat_intensity=n_inventors/Employed,patai_intensity=ai_inventors/n_inventors,
                            research_intensity=education_researchers/Employed)
data_ai= data_ai %>% mutate(logrent=log(median_rent))
data_ai= data_ai %>%  mutate(information_intensity=replace_na(information_intensity,0))
data_ai= data_ai %>%  mutate(business_intensity=replace_na(business_intensity,0))
data_ai= data_ai %>%  mutate(patai_intensity=replace_na(patai_intensity,0))

data_ai= data_ai %>%  mutate(est_size=emp/est)

data_ai= data_ai %>%  mutate(information_intensity_est=information_est/est,business_intensity_est=business_est/est,
                             manuf_intensity_est=manuf_est/est,information_intensity_est=replace_na(information_intensity_est,0))

data_ai= data_ai %>%  mutate(loggdp=log(private_gdp))


data_ai= data_ai %>% group_by(FIPS) %>%  mutate(EarnSgr=log(EarnS)/lag(log(EarnS),1))

data_ai= data_ai %>% group_by(FIPS) %>%  mutate(annual_paygr=log(avg_annual_pay)/lag(log(avg_annual_pay),1))
data_ai= data_ai %>% group_by(FIPS) %>%  mutate(annual_paygr=log(avg_annual_pay)/lag(log(avg_annual_pay),1))

data_ai= data_ai %>% ungroup() %>%  mutate(manufest_intensity=manuf_annual_avg_estabs/annual_avg_estabs)

data_ai= data_ai %>% ungroup() %>%  mutate(informationest_intensity=information_annual_avg_estabs/annual_avg_estabs)

data_ai= data_ai %>% ungroup() %>%  mutate(information_intensity_qcew=information_annual_avg_emplvl/annual_avg_emplvl)

data_ai= data_ai %>% ungroup() %>%  mutate(degshare=(udeg+mdeg)/Employed,stemshare=(ustemdeg+mstemdeg)/(udeg+mdeg),
                                           stemshare2=(ustemdeg)/(udeg))
data_ai= data_ai %>%  mutate(stemshare=replace_na(stemshare,0),stemshare2=replace_na(stemshare2,0))

data_ai= data_ai %>%  mutate(tightness=nads/Unemployed)
data_ai= data_ai %>%  mutate(hpi_ch=hpi_ch/100)

subset_select=c("state","year","FIPS","ai","nads","share_poverty","TurnOvrS","medhhincome","hpi","hpi_ch","pop_above18",
                "unrate","share_bac","share_black","medage","small","medium","est","management_emp","manuf_emp","information_emp",
                "emp","Employed","pop","n_patents","n_inventors","n_researchers","ai_patents","ai_inventors","education_researchers",
                "median_rent","information_est","manuf_est","private_gdp","udeg","mdeg","ustemdeg","mstemdeg","Unemployed")

data_ai %>% select(subset_select)%>% write_csv("data.csv")





 

