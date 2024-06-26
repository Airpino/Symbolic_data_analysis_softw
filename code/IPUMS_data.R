#ipums time use
library(ipumsr)
# key=<your API KEY>
# set_ipums_api_key(key, save = TRUE)

# Sarah M. Flood, Liana C. Sayer, Daniel Backman, and Annie Chen. American Time Use Survey Data Extract Builder: Version 3.2 [dataset]. College Park, MD: University of Maryland and Minneapolis, MN: IPUMS, 2023.
# https://doi.org/10.18128/D060.V3.2

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
library(readr)
library(tidyverse)
library(HistDAWass)
atus_00002 <- read_csv("Data/atus_00002.csv", 
                           col_types = cols(CASEID = col_character(), 
                                                      STRATA = col_character(), AGE_CPS8 = col_character(), 
                                                       EDUC = col_character(), OCC2_CPS8 = col_character(), 
                                                       IND2_CPS8 = col_character()))

data<-atus_00002
data<- data %>%mutate(age_cl=cut(AGE,breaks = c(0,25,35,50,65,100)),
                      EDUC=case_when(
                        EDUC %in% c("10", "11","12","13","14",
                                    "15","16","17") ~ "Less Diploma",
                        EDUC %in% c("20", "21") ~ "HS diploma",
                        EDUC %in% c("30", "31","32") ~ "Some college",
                        EDUC %in% c("40", "41","42","43") ~ "College or more",
                        EDUC %in% c("999") ~ "NOT IN UNI"
                      ),
                      OCCU_sect=case_when(
                        OCC2_CPS8 %in% c("110", "111") ~ "Man. Buss. Fin. occ.",
                        OCC2_CPS8 %in% c("120", "121","122",
                                         "123","124","125",
                                         "126","127") ~ "Professionals",
                        OCC2_CPS8 %in% c("130", "131","132",
                                         "133","134") ~ "Services occ.",
                        OCC2_CPS8 %in% c("140") ~ "Sales",
                        OCC2_CPS8 %in% c("150") ~ "Office and adm.",
                        OCC2_CPS8 %in% c("160") ~ "Farming",
                        OCC2_CPS8 %in% c("170") ~ "Contructions",
                        OCC2_CPS8 %in% c("180") ~ "Install. and repair",
                        OCC2_CPS8 %in% c("190") ~ "Production",
                        OCC2_CPS8 %in% c("200") ~ "Transportation",
                        OCC2_CPS8 %in% c("210") ~ "Armed forces",
                        OCC2_CPS8 %in% c("9999") ~ "NOT IN UNI"
                      ),
                      SEX=case_when(
                        SEX==1 ~ "Male",
                        SEX==2 ~ "Female"
                      ),
                      key_G=paste0(YEAR,"_",age_cl,"_",SEX,"_",OCCU_sect),
                      key_G2=paste0(YEAR,"_",SEX,"_",OCCU_sect)
                      )
  
data<-data %>% rowwise() %>% mutate(ST = sum(c_across(ACT_CAREHH:ACT_WORK)))
<<<<<<< HEAD
data_rid<-data %>% filter(ST>1439) %>% mutate(WST=if_else(ACT_WORK>10,"Worker","No Work")) %>% 
  filter(WST=="Worker") %>% mutate(ACT_ALL_THE_REST=ST-(ACT_FOOD+ACT_PCARE+ACT_SOCIAL+ACT_TRAVEL+ ACT_WORK)) %>% 
  select(key_G2,ACT_FOOD,ACT_PCARE,ACT_SOCIAL,ACT_TRAVEL, ACT_WORK,ACT_ALL_THE_REST)
=======
data_rid<-data %>% filter(ST>1439) %>% mutate(WST=if_else(ACT_WORK>0,"Worker","No Work")) %>% filter(WST=="Worker")
>>>>>>> 29a0b31018c6f61740c2f9775720b4f3d5da83f6

# data2<-data_rid %>% filter(YEAR==2022) %>% group_by(OCCU_sect,SEX,age_cl) %>% #summarize(n=n(),.groups="keep") %>% filter(n>50) %>%  
#   group_split()

<<<<<<< HEAD
data3<-data_rid %>% group_by(key_G2) %>% #summarize(n=n(),.groups="keep") %>% filter(n>50) %>%  
=======
data3<-data_rid %>% group_by(YEAR,OCCU_sect,SEX) %>% #summarize(n=n(),.groups="keep") %>% filter(n>50) %>%  
>>>>>>> 29a0b31018c6f61740c2f9775720b4f3d5da83f6
  group_split()



# uno<-sapply(data2,FUN=function(data){round(sapply(data %>% select(ACT_CAREHH:ACT_WORK), FUN=function(x){sum(x==0)/length(x)})*100,3)})
# 
# due<-sapply(data2,FUN=function(data){nrow(data)})
# tre<-t(uno[,due>50])

<<<<<<< HEAD
uno<-sapply(data3,FUN=function(data){round(sapply(data %>% select(ACT_FOOD:ACT_ALL_THE_REST), FUN=function(x){sum(x==0)/length(x)})*100,3)})

due<-sapply(data3,FUN=function(data){nrow(data)})
min_n<-30
tre<-t(uno[,due>min_n])
labs<-sapply(data3,FUN=function(x) {x$key_G2[1]})[due>min_n]
tre<-as.data.frame(tre) %>% mutate(labs=labs)

HMAT<-MatH(nrows=length(labs),ncols = 6,rownames = labs,varnames = colnames(tre)[1:6])

for(i in 1:length(labs)){
  for (j in 1:6){
    tmpd<-data_rid %>% filter(key_G2==labs[i]) %>% select(j+1) %>% unlist() %>% unname()
     tmp<-data2hist(tmpd,algo = "FixedQuantiles",qua = 20)
     HMAT@M[i,j][[1]]<-tmp
  }
  
}

resu<-WH_hclust(HMAT)
plot(resu)

resuKM<-list()
for(KK in 2:10){
resuKM[[KK-1]]<-WH_kmeans(HMAT, k=KK, rep=50, qua=20)
}
resuWKM<-WH_adaptive.kmeans(HMAT,k=5,schema=2,rep = 100)
=======
uno<-sapply(data3,FUN=function(data){round(sapply(data %>% select(ACT_CAREHH:ACT_WORK), FUN=function(x){sum(x==0)/length(x)})*100,3)})

due<-sapply(data3,FUN=function(data){nrow(data)})
tre<-t(uno[,due>50])
labs<-sapply(data3,FUN=function(x) {x$key_G2[1]})[due>50]
tre<-as.data.frame(tre) %>% mutate(labs=labs)
>>>>>>> 29a0b31018c6f61740c2f9775720b4f3d5da83f6
