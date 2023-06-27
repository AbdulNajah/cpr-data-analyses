
##merging dyn with tcpd
#======================


library(dplyr)
library(data.table)
tcpd = read_csv("/Users/najah/academics/d3s/d3s_r/data/TCPD_GE_All_States.csv")
names(tcpd) = tolower(names(tcpd))

#filtering to 19089-2019 with the first oikk
tcpd = tcpd %>% filter(year >=1989 & year<=2019  & vote_share_percentage >= 5 & poll_no == 0)

dyn_ai = read.csv("./data/df_dynast.csv") %>% select(state_name, year, constituency_no, 
       position,Caste_Rec,dyn, source)
#merging top 5percent with the all India dynasty data



tcpd_dyn = left_join(dyn_ai, tcpd, by= c("state_name", "year","constituency_no", "position"))

write.csv(tcpd_dyn, "./data/tcpd_ge_dyn_23.csv")


## cleaning college data
#===================

college<- fread("/Users/najah/work/cpr/data/college/colleges_pc.csv") %>% 
  rename_all(tolower) 



## creating slabs for election years


college <- college %>% filter(year_estd>=1989  &!is.na(pc_no)) %>% 
  mutate(year_el = case_when(
    year_estd>=1989 & year_estd<1991 ~1989,
    year_estd>=1991 & year_estd<1996 ~1991,
    year_estd>=1996 & year_estd<1998 ~1996,
    year_estd>=1998 & year_estd<1999 ~1998,
    year_estd>=1999 & year_estd<2004 ~1999,
    year_estd>=2004 & year_estd<2009 ~2004,
    year_estd>=2009 & year_estd<2014 ~2009,
    year_estd>=2014 & year_estd<2019 ~2014,
    year_estd>=2019 & year_estd<2022 ~2019,
    TRUE ~0
  )) %>% 
  mutate(state = tolower(state),
         state= str_trim(state)) %>% 
  filter(state!= "andhra pradesh" & state!= "telengana" & management!= "University")
### ,merging with college data
#=====================

college_pc <- college %>% group_by(year_el,state, pc_no) %>% 
  summarise(n_college = n()) %>% ungroup()

write.csv(college_pc, "./data/college_pc_ag.csv")









