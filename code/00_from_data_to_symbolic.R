# from data to symbolic data
#  library(tidycensus)
# 
  ny_pums <- get_pums(
    variables = c("PWGTP","SEX", "AGEP", "SCHL","SFR",
                  "SOCP","WKHP",
                  "HHT","COW","NAICSP","OIP",
                  "PERNP","PINCP","POVPIP","WAGP",
                  "OCCP","FWKHP","WKWN"),
    state = "NY",
    survey = "acs5",
    year = 2022
  )
  
  ca_pums <- get_pums(
    variables = c("PWGTP","SEX", "AGEP", "SCHL","SFR",
                  "SOCP","WKHP",
                  "HHT","COW","NAICSP","OIP",
                  "PERNP","PINCP","POVPIP","WAGP",
                  "OCCP","FWKHP","WKWN"),
    state = "CA",
    survey = "acs5",
    year = 2022
  )
  
# #census_api_key("d76c948b7c36024ed72f6bd5f3bdde8a0ced9bc3",install = TRUE)
# save(ny_pums,file="Pums.RData")
# load("Pums.RData")

pums_vars<-pums_variables %>% filter(survey=="acs5",year==2022)

ny_pums<-ny_pums %>% mutate(Pay_per_week=WAGP/WKWN)
ca_pums<-ca_pums %>% mutate(Pay_per_week=WAGP/WKWN)
tmp2<-ny_pums %>% filter(WKWN>30) %>% group_by(OCCP,SEX) %>% 
  summarize(n=n(),mwa=mean(WAGP,na.rm=T),
            mwk_p=mean(Pay_per_week,na.rm=T),
            med=quantile(Pay_per_week,probs = 0.50,na.rm = T),
            q10=quantile(Pay_per_week,probs = 0.10,na.rm = T),
            q90=quantile(Pay_per_week,probs = 0.90,na.rm = T)
            ) %>% filter(n>50) %>% 
  arrange(OCCP) %>% print(n=30)

tmp3<-ny_pums %>% filter(WKWN>30) %>% group_by(OCCP,SEX) %>% 
  summarize(n=n(),mwa=mean(WAGP,na.rm=T),
            mwk_p=mean(Pay_per_week,na.rm=T),
            med=quantile(Pay_per_week,probs = 0.50,na.rm = T),
            q10=quantile(Pay_per_week,probs = 0.10,na.rm = T),
            q90=quantile(Pay_per_week,probs = 0.90,na.rm = T)
  ) %>% filter(n>50) %>% 
  arrange(OCCP) %>% print(n=30)


ggplot(ny_pums %>% filter(OCCP=="0205",WKWN>30))+geom_density(aes(x=log(Pay_per_week),fill=SEX),alpha=0.5)
ggplot(ny_pums %>% filter(OCCP=="0010",WKWN>30))+
  geom_density(aes(x=Pay_per_week,fill=SEX,weight=PWGTP),alpha=0.5)+scale_x_log10()

ggplot(ny_pums %>% filter(OCCP=="0010",WKWN>30))+
  geom_density(aes(x=Pay_per_week,fill=SEX),alpha=0.5)+scale_x_log10()

mini_vars<-pums_vars %>% group_by(survey,year,var_code) %>% summarise(n=n(),desc=last(var_label),type=last(data_type),level=last(level)) %>% arrange(level)

save.image(file="Pums2.RData")
