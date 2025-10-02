# Column1: Demographics
library(pander)
library(fixest)
library(tidyverse)

data_ai=read_csv("data.csv")%>% mutate(state_year=paste0(state,Year)) %>% 
  mutate(ai_intensity=ai/nads) 

# prepare demographics
# right hand side variables are demographics: unemployment rate:unrate, log median household income, share_bac, share_black, share_poverty, log HPI, log population, log immigration.
data_ai=data_ai %>% mutate(logincome=log(medhhincome),loghpi=log(hpi),logemp=log(pop_above18))

data_ai=data_ai %>% mutate(gincome=logincome-lag(logincome,9))
data_ai=data_ai %>% mutate(dunrate=unrate-lag(unrate,9),dshare_bac=share_bac-lag(share_bac,9),
                           dshare_black=share_black-lag(share_black,9),
                           dmedage14=medage-lag(medage,9))
data_ai=data_ai %>% mutate(lads=log(1+nads))
data_ai=data_ai %>% mutate(lads0=log(nads))
data_ai=data_ai %>% mutate(large_firms=1-(small+medium)/est,management_intensity=management_emp/emp,
                           information_intensity=information_emp/emp,
                           information_intensity=manuf_emp/emp)


# log number of patents, log number of inventors, log AI patents, log AI inventors, log number of researchers.
data_ai=data_ai %>% mutate(logemp=log(Employed))%>% mutate(logpop=log(pop))
data_ai=data_ai %>% mutate(logn_patents=log(1+n_patents),logn_inventors=log(1+n_inventors),
                           logai_patents=log(1+ai_patents),logai_inventors=log(1+ai_inventors))%>%
  mutate(pat_intensity=n_inventors/Employed,patai_intensity=ai_patents/n_patents) %>% mutate(patai_intensity=replace_na(patai_intensity,0))

data_ai= data_ai%>% mutate(small_firms=small/est,large_firms=1-(small+medium)/est,management_intensity=management_emp/emp,
                           information_intensity=information_emp/emp,
                           manuf_intensity=manuf_emp/emp)


data_ai= data_ai %>% mutate(pat_intensity=n_inventors/Employed,patai_intensity=ai_inventors/n_inventors)
data_ai= data_ai %>% mutate(logrent=log(median_rent))
data_ai= data_ai %>%  mutate(information_intensity=replace_na(information_intensity,0))
data_ai= data_ai %>%  mutate(patai_intensity=replace_na(patai_intensity,0))

data_ai= data_ai %>%  mutate(est_size=emp/est)


data_ai= data_ai %>%  mutate(loggdp=log(private_gdp))


 

data_ai= data_ai %>% ungroup() %>%  mutate(degshare=(udeg+mdeg)/Employed,stemshare=(ustemdeg+mstemdeg)/(udeg+mdeg),
                                           stemshare2=(ustemdeg)/(udeg))
data_ai= data_ai %>%  mutate(stemshare=replace_na(stemshare,0),stemshare2=replace_na(stemshare2,0))

data_ai= data_ai %>%  mutate(tightness=nads/Unemployed)
data_ai= data_ai %>%  mutate(hpi_ch=hpi_ch/100)


# zscores

data_ai_z <- data_ai %>% filter(emp!=0)%>%
  mutate(
    share_bac = scale(share_bac),
    share_black = scale(share_black),
    share_poverty = scale(share_poverty),
    logpop = scale(logpop),
    hpi_ch = scale(hpi_ch),
    logincome = scale(logincome),
    tightness = scale(tightness),
    unrate = scale(unrate),
    pat_intensity = scale(pat_intensity),
    patai_intensity = scale(patai_intensity),
    degshare = scale(degshare),
    stemshare = scale(stemshare),
    large_firms = scale(large_firms),
    information_intensity = scale(information_intensity),
    manuf_intensity = scale(manuf_intensity),
    TurnOvrS = scale(TurnOvrS)
  ) %>% mutate(ai_intensity=ai_intensity*100)




est_demog_no=fixest::feols("ai_intensity ~share_bac+share_black+share_poverty+logpop+hpi_ch+logincome+tightness|Year+COUNTY_FIPS" %>% as.formula(),
                           data_ai_z %>% drop_na(share_bac,share_black,share_poverty,logpop,hpi_ch,logincome,
                                                 tightness,pat_intensity,patai_intensity,degshare,stemshare,
                                                 large_firms,information_intensity,manuf_intensity,TurnOvrS),cluster = "COUNTY_FIPS",weights = ~lads) 
est_innovation_no=fixest::feols("ai_intensity ~pat_intensity+patai_intensity+degshare+stemshare|Year+COUNTY_FIPS" %>% as.formula(),
                                data_ai_z %>% drop_na(share_bac,share_black,share_poverty,logpop,hpi_ch,logincome,
                                                      tightness,pat_intensity,patai_intensity,degshare,stemshare,
                                                      large_firms,information_intensity,manuf_intensity,TurnOvrS),cluster = "COUNTY_FIPS",weights = ~lads) 
est_industry_no=fixest::feols("ai_intensity ~large_firms+information_intensity+manuf_intensity+TurnOvrS|Year+COUNTY_FIPS" %>% as.formula(),
                              data_ai_z %>% drop_na(share_bac,share_black,share_poverty,logpop,hpi_ch,logincome,
                                                    tightness,pat_intensity,patai_intensity,degshare,stemshare,
                                                    large_firms,information_intensity,manuf_intensity,TurnOvrS),cluster = "COUNTY_FIPS",weights = ~lads) 
#est_all_no=fixest::feols("ai_intensity ~share_bac+share_black+share_poverty+logpop+logrent+logincome+unrate+logn_inventors+logai_inventors+large_firms+information_intensity+manuf_intensity" %>% as.formula(),
#                      data_ai,cluster = "COUNTY_FIPS",weights = ~lads)
#est_all=fixest::feols("ai_intensity ~share_bac+share_black+share_poverty+logpop+logrent+logincome+tightness+logn_inventors+logai_inventors+large_firms+information_intensity+manuf_intensity|state_year+COUNTY_FIPS" %>% as.formula(),
#                      data_ai,vcov = "twoway",weights = ~lads)

est_all=fixest::feols("ai_intensity ~share_bac+share_black+share_poverty+logpop+hpi_ch+logincome+tightness+pat_intensity+patai_intensity+degshare+stemshare+large_firms+information_intensity+manuf_intensity+TurnOvrS|Year+COUNTY_FIPS" %>% as.formula(),
                      data_ai_z,cluster = "COUNTY_FIPS",weights = ~lads)
est_all_large=fixest::feols("ai_intensity ~share_bac+share_black+share_poverty+logpop+hpi_ch+logincome+tightness+pat_intensity+patai_intensity+degshare+stemshare+large_firms+information_intensity+manuf_intensity+TurnOvrS|state_year+COUNTY_FIPS" %>% as.formula(),
                            data_ai_z %>% filter(pop>9999),vcov = "twoway",weights = ~lads)
est_all_state_year=fixest::feols("ai_intensity ~share_bac+share_black+share_poverty+logpop+hpi_ch+logincome+tightness+pat_intensity+patai_intensity+degshare+stemshare+large_firms+information_intensity+manuf_intensity+TurnOvrS|state_year+COUNTY_FIPS" %>% as.formula(),
                                 data_ai_z,vcov = "twoway",weights = ~lads)


setFixest_dict(c(share_bac ="Bachelors' share, z-score",share_black= "Black pop, z-score",share_poverty= "Poverty share, z-score",
                 logpop= "log(Population), z-score",hpi_ch= "House Price Growth, z-score", unrate= "Unemployment Rate, z-score",
                 tightness= "Labor Market Tightness, z-score",logincome= "log(Median Income), z-score",
                 pat_intensity= "Patents per employee, z-score",patai_intensity= "AI patents' share, z-score",
                 lai_patents= "log(AI Patents), z-score",degshare= "Degrees awarded per capita, z-score",stemshare= "Stem Degrees' share, z-score",
                 logai_inventors= "log(AI Inventors), z-score",
                 TurnOvrS= "Turnover Rate, z-score",information_intensity= "ICT sector Intensity, z-score",
                 manuf_intensity= "Manufacturing Intensity, z-score",large_firms= "Large Establishments, z-score"))

etable(est_demog_no,est_innovation_no,est_industry_no,est_all,est_all_state_year,tex = TRUE, postprocess.df = function(x) round(x, 2), style.df = style.df(depvar.title = "", fixef.title = "", 
                                                                                                                                                           fixef.suffix = " fixed effect", yesNo = "yes")) %>% 
  write_lines("aisharezscore.tex")


data_ai_2017_2022=read_csv("~/Dropbox/Rotemberg/counties/data.csv") %>% filter(Year==2017|Year==2018|Year==2022|Year==2023) %>% 
  mutate(state_year=paste0(state,Year)) %>% mutate(new=1*(Year>2020)) %>% group_by(new,COUNTY_FIPS) %>% mutate(ai=sum(ai),nads=sum(nads))%>% 
  mutate(ai_intensity=ai/nads)  %>% filter(Year==2017|Year==2022) %>% 
  group_by(COUNTY_FIPS) %>% mutate(dai_intensity=ai_intensity-lag(ai_intensity),
                                   dai_intensity9=ai_intensity-lag(ai_intensity,1))
# prepare demographics
# right hand side variables are demographics: unemployment rate:unrate, log median household income, share_bac, share_black, share_poverty, log HPI, log population, log immigration.
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(logincome=log(medhhincome),loghpi=log(hpi),logemp=log(pop_above18),logim=log(immigration))
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(unrate14=lag(unrate,1),logincome14=lag(logincome,1),loghpi14=lag(loghpi,1),
                                               share_bac14=lag(share_bac,1),share_black14=lag(share_black,1),share_poverty14=lag(share_poverty,1),
                                               logemp14=lag(logemp,1),logim14=lag(logim,1),medage14=lag(medage,1),
                                               median_rent14=lag(median_rent,1))
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(gincome=logincome-lag(logincome,1),ghpi=loghpi-lag(loghpi,1),
                                               gemp=logemp-lag(logemp,1),gim=logim-lag(logim,1))
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(dunrate=unrate-lag(unrate,1),dshare_bac=share_bac-lag(share_bac,1),
                                               dshare_black=share_black-lag(share_black,1),
                                               dmedage14=medage-lag(medage,1))
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(lads=log(1+nads))
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(lads0=log(nads))
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(large_firms=1-(small+medium)/est,management_intensity=management_emp/emp,
                                               information_intensity=information_emp/emp,
                                               information_intensity=manuf_emp/emp)


# log number of patents, log number of inventors, log AI patents, log AI inventors, log number of researchers.
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(logemp=log(Employed))%>% mutate(logpop=log(pop))
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(logn_patents=log(1+n_patents),logn_inventors=log(1+n_inventors),
                                               logai_patents=log(1+ai_patents),logai_inventors=log(1+ai_inventors))%>%
  mutate(pat_intensity=n_inventors/Employed,patai_intensity=ai_patents/n_patents)
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(pat_intensity14=lag(pat_intensity,1),patai_intensity14=lag(patai_intensity,1))

data_ai_2017_2022=data_ai_2017_2022 %>% mutate(pat_intensity14=lag(pat_intensity,1),patai_intensity14=lag(patai_intensity,1))
data_ai_2017_2022= data_ai_2017_2022%>% mutate(small_firms=small/est,large_firms=1-(small+medium)/est,management_intensity=management_emp/emp,
                                               information_intensity=information_emp/emp,
                                               manuf_intensity=manuf_emp/emp)

data_ai_2017_2022= data_ai_2017_2022 %>% mutate(pat_intensity=n_inventors/Employed,patai_intensity=ai_patents/n_patents)
data_ai_2017_2022= data_ai_2017_2022 %>% mutate(patai_intensity=replace_na(patai_intensity,0))
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(pat_intensity14=lag(pat_intensity,1),patai_intensity14=lag(patai_intensity,1),
                                               lads14=lag(lads,1))


data_ai_2017_2022=data_ai_2017_2022 %>% mutate(logn_inventors14=lag(logn_inventors,1),
                                               logai_inventors14=lag(logai_inventors,1),
                                               lads14=lag(lads,1))


data_ai_2017_2022= data_ai_2017_2022 %>% ungroup() %>%  mutate(degshare=(udeg+mdeg)/Employed,stemshare=(ustemdeg+mstemdeg)/(udeg+mdeg))
data_ai_2017_2022= data_ai_2017_2022 %>%  mutate(stemshare=replace_na(stemshare,0))

data_ai_2017_2022=data_ai_2017_2022 %>% mutate(stemshare14=lag(stemshare,1))
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(degshare14=lag(degshare,1))


data_ai_2017_2022= data_ai_2017_2022 %>%  mutate(tightness=nads/Unemployed)
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(tightness14=lag(tightness,1))
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(hpi_ch14=lag(hpi_ch,1))
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(logpop14=lag(logpop,1))
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(logincome14=lag(logincome,1))
data_ai_2017_2022=data_ai_2017_2022 %>% mutate(gpop=(logpop-lag(logpop,1))/5)

data_ai_2017_2022= data_ai_2017_2022 %>%  mutate(hpi_ch=hpi_ch/100)
data_ai_2017_2022= data_ai_2017_2022 %>%  mutate(hpi_ch14=hpi_ch14/100)

# TurnOvrS is missing, hpi is missing from many- maybe log rent

data_ai_2017_2022=data_ai_2017_2022 %>% mutate(large_firms14=lag(large_firms,1),information_intensity14=lag(information_intensity,1),
                                               manuf_intensity14=lag(manuf_intensity,1),
                                               TurnOvrS14=lag(TurnOvrS,1))

setFixest_dict(c(share_bac14 ="Bachelors,% in 2017",share_black14= "Black, % in 2017",share_poverty14= "Poverty, % in 2017",gpop= "Pop. Growth",
                 hpi_ch14= "House Price Growth in 2017",
                 unrate14= "Unemployment Rate, in 2017",
                 tightness14= "Tightness, in 2017",logincome14= "Income, Log in 2017",
                 pat_intensity14= "Patents per employee  in 2017",
                 patai_intensity14= "AI Patents' Share in 2017",
                 degshare14= "Degrees awarded per capita, in 2017",stemshare14= "Stem Degrees' share, in 2017",
                 TurnOvrS14= "Turnover Rate, %  in 2017",
                 manuf_intensity14= "Manufacturing Intensity, %  in 2017",
                 information_intensity14= "ICT sector Intensity, %  in 2017",
                 large_firms14= "Large Establishments, %  in 2017"))



# Zscores


data_ai_l_z <- data_ai_2017_2022 %>% filter(emp!=0) %>% drop_na(share_bac14)%>%
  mutate(
    share_bac14 = scale(share_bac14),
    share_black14 = scale(share_black14),
    share_poverty14 = scale(share_poverty14),
    logpop14 = scale(logpop14),
    hpi_ch14 = scale(hpi_ch14),
    logincome14 = scale(logincome14),
    tightness14 = scale(tightness14),
    pat_intensity14 = scale(pat_intensity14),
    patai_intensity14 = scale(patai_intensity14),
    degshare14 = scale(degshare14),
    stemshare14 = scale(stemshare14),
    large_firms14 = scale(large_firms14),
    information_intensity14 = scale(information_intensity14),
    manuf_intensity14 = scale(manuf_intensity14),
    TurnOvrS14 = scale(TurnOvrS14),unrate14 = scale(unrate14)
  ) %>% mutate(dai_intensity9=dai_intensity9*100)



est_demog_no=fixest::feols("dai_intensity9 ~share_bac14+share_black14+share_poverty14+gpop+hpi_ch14+logincome14+tightness14" %>% as.formula(),
                           data_ai_l_z %>% filter(dai_intensity9>-5,dai_intensity9<10)%>% drop_na(share_bac14,share_black14,share_poverty14,logpop14,hpi_ch14,logincome14,
                                                                                                  tightness14,pat_intensity14,patai_intensity14,degshare14,stemshare14,
                                                                                                  large_firms14,information_intensity14,manuf_intensity14,TurnOvrS14),weights = ~lads14) 
est_innovation_no=fixest::feols("dai_intensity9 ~pat_intensity14+patai_intensity14+degshare14+stemshare14" %>% as.formula(),
                                data_ai_l_z %>% filter(dai_intensity9>-5,dai_intensity9<10)%>% drop_na(share_bac14,share_black14,share_poverty14,logpop14,hpi_ch14,logincome14,
                                                                                                       tightness14,pat_intensity14,patai_intensity14,degshare14,stemshare14,
                                                                                                       large_firms14,information_intensity14,manuf_intensity14,TurnOvrS14),weights = ~lads14) 
est_industry_no=fixest::feols("dai_intensity9 ~large_firms14+information_intensity14+manuf_intensity14+TurnOvrS14" %>% as.formula(),
                              data_ai_l_z%>% filter(dai_intensity9>-5,dai_intensity9<10) %>% drop_na(share_bac14,share_black14,share_poverty14,logpop14,hpi_ch14,logincome14,
                                                                                                     tightness14,pat_intensity14,patai_intensity14,degshare14,stemshare14,
                                                                                                     large_firms14,information_intensity14,manuf_intensity14,TurnOvrS14),weights = ~lads14)

est_all=fixest::feols("dai_intensity9 ~share_bac14+share_black14+share_poverty14+gpop+hpi_ch14+logincome14+tightness14+pat_intensity14+patai_intensity14+degshare14+stemshare14+large_firms14+information_intensity14+manuf_intensity14+TurnOvrS14" %>% as.formula(),
                      data_ai_l_z%>% filter(dai_intensity9>-5,dai_intensity9<10) %>% drop_na(share_bac14,share_black14,share_poverty14,logpop14,hpi_ch14,logincome14,
                                                                                             tightness14,pat_intensity14,patai_intensity14,degshare14,stemshare14,
                                                                                             large_firms14,information_intensity14,manuf_intensity14,TurnOvrS14),weights = ~lads14)

est_all_state=fixest::feols("dai_intensity9 ~share_bac14+share_black14+share_poverty14+gpop+hpi_ch14+logincome14+tightness14+pat_intensity14+patai_intensity14+degshare14+stemshare14+large_firms14+information_intensity14+manuf_intensity14+TurnOvrS14|state" %>% as.formula(),
                            data_ai_l_z%>% filter(dai_intensity9>-5,dai_intensity9<10) %>% drop_na(share_bac14,share_black14,share_poverty14,logpop14,hpi_ch14,logincome14,
                                                                                                   tightness14,pat_intensity14,patai_intensity14,degshare14,stemshare14,
                                                                                                   large_firms14,information_intensity14,manuf_intensity14,TurnOvrS14),weights = ~lads14)
est_all_large=fixest::feols("dai_intensity9 ~share_bac14+share_black14+share_poverty14+gpop+hpi_ch14+logincome14+tightness14+pat_intensity14+patai_intensity14+degshare14+stemshare14+large_firms14+information_intensity14+manuf_intensity14+TurnOvrS14|state" %>% as.formula(),
                            data_ai_l_z %>% filter(dai_intensity9>-5,dai_intensity9<10) %>% filter(pop>9999) %>% drop_na(share_bac14,share_black14,share_poverty14,logpop14,hpi_ch14,logincome14,
                                                                                                                         tightness14,pat_intensity14,patai_intensity14,degshare14,stemshare14,
                                                                                                                         large_firms14,information_intensity14,manuf_intensity14,TurnOvrS14),weights = ~lads14)



setFixest_dict(c(share_bac14 ="Bachelors,% z-score in 2017",share_black14= "Black, % z-score in 2017",share_poverty14= "Poverty, % z-score in 2017",gpop= "Pop. Growth",
                 hpi_ch14= "House Price Growth z-score in 2017",
                 unrate14= "Unemployment Rate, z-score in 2017",
                 tightness14= "Tightness, z-score in 2017",logincome14= "Income, Log z-score in 2017",
                 pat_intensity14= "Patents per employee  z-score in 2017",
                 patai_intensity14= "AI Patents' Share z-score in 2017",
                 degshare14= "Degrees awarded per capita, z-score in 2017",stemshare14= "Stem Degrees' share, z-score in 2017",
                 TurnOvrS14= "Turnover Rate, %  z-score in 2017",
                 manuf_intensity14= "Manufacturing Intensity, %  z-score in 2017",
                 information_intensity14= "ICT sector Intensity, %  z-score in 2017",
                 large_firms14= "Large Establishments, %  z-score in 2017"))
etable(est_demog_no,est_innovation_no,est_industry_no,est_all,est_all_state,tex = TRUE, postprocess.df = function(x) round(x, 2), style.df = style.df(depvar.title = "", fixef.title = "", 
                                                                                                                                                      fixef.suffix = " fixed effect", yesNo = "yes")) %>% 
  write_lines("aisharelongchangezscore.tex")



