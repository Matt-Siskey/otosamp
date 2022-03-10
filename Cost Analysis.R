###############################################################################################################
######################################### Cost-Revenue Analysis ###############################################
###############################################################################################################
library(ggplot2); library(dplyr); library(plyr); library(purrr); library(reshape2); library(data.table)
rm(list=ls())
################ Cost Analysis ######################
### Sum of nomSS
pathR<-""
method  <-c("ototow_otos","ototow_tows") #
stock   <-"GOA"
samp_scenario <-c("1 Boat","2 Boat","3 Boat","4 Boat","5 Boat")
species <-c("30152","30060","21740") #
spec_boots <-200
exp_boots <-100

tmp_scen_nomSS<-list();tmp_meth_nomSS<-list();tmp_spec_nomSS<-list()

for(s in 1:length(species)){
  if(species[s]=="30152"){
    species_common <-"Dusky Rockfish"
    nyrs<-16
    yrs<-c("1984","1987","1990","1993","1996","1999","2001","2003","2005","2007","2009","2011","2013","2015","2017","2019")
  }
  if(species[s]=="30060"){
    species_common <-"Pacific Ocean Perch"
    nyrs<-13
    yrs<-c("1990","1993","1996","1999","2003","2005","2007","2009","2011","2013","2015","2017","2019")
  }
  if(species[s]=="21740"){
    species_common <-"Walleye Pollock"
    nyrs<-14
    yrs<-c("1990","1993","1996","1999","2001","2003","2005","2007","2009","2011","2013","2015","2017","2019")
  }
  for(m in 1:length(method)){
    if(method[m]=="ototow_otos"){
      nomSS_list<-list()
      load(paste(pathR,"SzAC Results_ototow_otos/pipeline_nomSS_",species[s],"_ototow_otos",sep=""))
      nomSS_list<-nomSS_alt}
    if(method[m]=="ototow_tows"){
      nomSS_list<-list()
      load(paste(pathR,"SzAC Results_ototow_tows/pipeline_nomSS_",species[s],"_ototow_tows",sep=""))
      nomSS_list<-nomSS_alt}
    
    for(alt in 1:length(samp_scenario)){
      
      if(alt==1){nomSS_top<-nomSS_list[[1]]}
      if(alt==2){nomSS_top<-nomSS_list[[2]]}
      if(alt==3){nomSS_top<-nomSS_list[[3]]}
      if(alt==4){nomSS_top<-nomSS_list[[4]]}
      if(alt==5){nomSS_top<-nomSS_list[[5]]}
      
      nomSS <-matrix(NA,ncol=7,nrow=spec_boots*exp_boots)
      
      for(i in 1:spec_boots){
        
        if(i==1){counter<-1}
        if(i>1){counter<-counter+1}
        
        for(ii in 1:exp_boots){
          
          if(ii>1){counter<-counter+1}
          # nomSS[counter:(counter+(nyrs-1)),1] <-yrs
          nomSS[counter,1] <-sum(as.numeric(nomSS_top[[i]][[ii]][1:nyrs]))/nyrs
          nomSS[counter,2] <-samp_scenario[alt]
          nomSS[counter,3] <-species_common
          nomSS[counter,4] <-"SUMnomSS"
          nomSS[counter,5] <-i
          nomSS[counter,6] <-ii
          nomSS[counter,7] <-method[m]
        }
      }
      tmp_scen_nomSS[[alt]]<-nomSS
    }
    names(tmp_scen_nomSS) <-samp_scenario
    tmp_meth_nomSS[[m]]   <-tmp_scen_nomSS
  }
  names(tmp_meth_nomSS) <-method
  tmp_spec_nomSS[[s]]   <-tmp_meth_nomSS
}

names(tmp_spec_nomSS)<-species

nomSS_dr_otos  <-map_df(tmp_spec_nomSS[[1]][[1]],~as.data.frame(.))
nomSS_pop_otos <-map_df(tmp_spec_nomSS[[2]][[1]],~as.data.frame(.))
nomSS_pk_otos  <-map_df(tmp_spec_nomSS[[3]][[1]],~as.data.frame(.))

nomSS_dr_tows  <-map_df(tmp_spec_nomSS[[1]][[2]],~as.data.frame(.))
nomSS_pop_tows <-map_df(tmp_spec_nomSS[[2]][[2]],~as.data.frame(.))
nomSS_pk_tows  <-map_df(tmp_spec_nomSS[[3]][[2]],~as.data.frame(.))

SUMnomSS_otos <-rbind(nomSS_dr_otos,nomSS_pop_otos,nomSS_pk_otos)
colnames(SUMnomSS_otos) <-c("SUMnomSS","Scenario","Species","Metric","Spec_Boot","Exp_Boot","Method")
SUMnomSS_otos$SUMnomSS <-as.numeric(SUMnomSS_otos$SUMnomSS)

SUMnomSS_tows <-rbind(nomSS_dr_tows,nomSS_pop_tows,nomSS_pk_tows)
colnames(SUMnomSS_tows) <-c("SUMnomSS","Scenario","Species","Metric","Spec_Boot","Exp_Boot","Method")
SUMnomSS_tows$SUMnomSS <-as.numeric(SUMnomSS_tows$SUMnomSS)

######################## Per-otolith cost ######################## 
salary_benefits <-(90000*0.35)+90000 # 90k average base salary plus 35% for indirect costs
FTE_per_hr      <-salary_benefits/1920 # 1920 hours work per year
FTE_per_day     <-FTE_per_hr*8 # FTE salary per hour * 8 hour day

# cost ageing per day
dusky_cost_per_day <-FTE_per_day/8.1 # 8.1 dusky otos can be aged per day
pop_cost_per_day   <-FTE_per_day/12.5 # 12.5 pop otos can be aged per day
pk_cost_per_day    <-FTE_per_day/27.2 # 27.2 pollock otos can be aged per day

# otos aged per hour
dusky_oto_per_hr <-8.1/8
pop_oto_per_hr<-12.5/8
pk_oto_per_hr<-27.2/8

######################## METHOD 1 ########################
SUMnomSS_otos <-aggregate(as.numeric(SUMnomSS_otos[,1]),list(SUMnomSS_otos$Scenario,SUMnomSS_otos$Species,SUMnomSS_otos$Method),mean)
colnames(SUMnomSS_otos)<-c("Scenario","Species","Method","SUMnomSS")
SUMnomSS_tows <-aggregate(as.numeric(SUMnomSS_tows[,1]),list(SUMnomSS_tows$Scenario,SUMnomSS_tows$Species,SUMnomSS_tows$Method),mean)
colnames(SUMnomSS_tows)<-c("Scenario","Species","Method","SUMnomSS")

SUMnomSS_otos[which(SUMnomSS_otos$Species=="Dusky Rockfish"),5] <-SUMnomSS_otos[which(SUMnomSS_otos$Species=="Dusky Rockfish"),]$SUMnomSS*dusky_cost_per_day
SUMnomSS_otos[which(SUMnomSS_otos$Species=="Pacific Ocean Perch"),5] <-SUMnomSS_otos[which(SUMnomSS_otos$Species=="Pacific Ocean Perch"),]$SUMnomSS*pop_cost_per_day
SUMnomSS_otos[which(SUMnomSS_otos$Species=="Walleye Pollock"),5] <-SUMnomSS_otos[which(SUMnomSS_otos$Species=="Walleye Pollock"),]$SUMnomSS*pk_cost_per_day
# SUMnomSS_otos[,5] <-rep("Otoliths Changed",length(SUMnomSS_otos$Scenario))
colnames(SUMnomSS_otos) <-c("Scenario","Species","Method","NomSS","Cost")

SUMnomSS_tows[which(SUMnomSS_tows$Species=="Dusky Rockfish"),5] <-SUMnomSS_tows[which(SUMnomSS_tows$Species=="Dusky Rockfish"),]$SUMnomSS*dusky_cost_per_day
SUMnomSS_tows[which(SUMnomSS_tows$Species=="Pacific Ocean Perch"),5] <-SUMnomSS_tows[which(SUMnomSS_tows$Species=="Pacific Ocean Perch"),]$SUMnomSS*pop_cost_per_day
SUMnomSS_tows[which(SUMnomSS_tows$Species=="Walleye Pollock"),5] <-SUMnomSS_tows[which(SUMnomSS_tows$Species=="Walleye Pollock"),]$SUMnomSS*pk_cost_per_day
# SUMnomSS_tows[,5] <-rep("Tows Changed",length(SUMnomSS_tows$Scenario))
colnames(SUMnomSS_tows) <-c("Scenario","Species","Method","NomSS","Cost")

SUMnomSS_cost <-rbind(SUMnomSS_otos,SUMnomSS_tows)
write.csv(SUMnomSS_cost,"SUMnomSS_cost.csv")



