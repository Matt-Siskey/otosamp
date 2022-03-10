###############################################################################################################
################################# NomSS, Tows, and Input SS ###################################################
###############################################################################################################
library(ggplot2); library(dplyr); library(plyr); library(purrr); library(reshape2); library(mgcv);
library(Rcpp); library(TMB); library(glmmTMB);library(tidyr); library(RODBC); library(data.table)
rm(list=ls())
pathR         <-""
pathOUT       <-"/Output/"
method        <-c("ototow_otos","ototow_tows") #
stock         <-"GOA"
samp_scenario <-c("1 Boat","2 Boat","3 Boat","4 Boat","5 Boat")
species       <-c("30152","30060","21740") #
spec_boots    <-200
exp_boots     <-100
setwd(pathR)

################################################################################
### NomSS
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

      nomSS <-matrix(NA,ncol=8,nrow=(spec_boots*exp_boots)*nyrs)

      for(i in 1:spec_boots){

        if(i==1){counter<-1}
        if(i>1){counter<-counter+nyrs}

        for(ii in 1:exp_boots){

          if(ii>1){counter<-counter+nyrs}
          nomSS[counter:(counter+(nyrs-1)),1] <-yrs
          nomSS[counter:(counter+(nyrs-1)),2] <-as.numeric(nomSS_top[[i]][[ii]][1:nyrs])
          nomSS[counter:(counter+(nyrs-1)),3] <-rep(samp_scenario[alt],nyrs)
          nomSS[counter:(counter+(nyrs-1)),4] <-rep(i,nyrs)
          nomSS[counter:(counter+(nyrs-1)),5] <-rep(species_common,nyrs)
          nomSS[counter:(counter+(nyrs-1)),6] <-rep("NomSS",nyrs)
          nomSS[counter:(counter+(nyrs-1)),7] <-rep(ii,nyrs)
          nomSS[counter:(counter+(nyrs-1)),8] <-rep(paste(method[m]),nyrs)
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

nomSS <-rbind(nomSS_dr_otos,nomSS_dr_tows,nomSS_pop_otos,nomSS_pop_tows,nomSS_pk_otos,nomSS_pk_tows)
colnames(nomSS) <-c("Year","Value","Scenario","Spec_Boot","Species","Metric","Exp_Boot","Method")
nomSS$Value<-as.numeric(nomSS$Value)
nomSS$Exp_Boot<-as.numeric(nomSS$Exp_Boot)
nomSS$Spec_Boot<-as.numeric(nomSS$Spec_Boot)

nomSS$Scenario <-revalue(nomSS$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
nomSS$Scenario <-factor(nomSS$Scenario,levels=c("-67%","-33%","0%","+33%","+67%"))
nomSS$Method   <-revalue(nomSS$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))

write.csv(nomSS,paste0(pathOUT,"nomSS.csv"),row.names=FALSE)

################################################################################
### Tows
tmp_scen_tows<-list();tmp_meth_tows<-list();tmp_spec_tows<-list()

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
      tows_list<-list()
      load(paste(pathR,"SzAC Results_ototow_otos/pipeline_towcount_",species[s],"_ototow_otos",sep=""))
      tows_list<-tow_count_alt}
    if(method[m]=="ototow_tows"){
      tows_list<-list()
      load(paste(pathR,"SzAC Results_ototow_tows/pipeline_towcount_",species[s],"_ototow_tows",sep=""))
      tows_list<-tow_count_alt}

    for(alt in 1:length(samp_scenario)){

      if(alt==1){tows_top<-tows_list[[1]]}
      if(alt==2){tows_top<-tows_list[[2]]}
      if(alt==3){tows_top<-tows_list[[3]]}
      if(alt==4){tows_top<-tows_list[[4]]}
      if(alt==5){tows_top<-tows_list[[5]]}

      tows <-matrix(NA,ncol=8,nrow=(spec_boots*exp_boots)*nyrs)

      for(i in 1:spec_boots){

        if(i==1){counter<-1}
        if(i>1){counter<-counter+nyrs}

        for(ii in 1:exp_boots){

          if(ii>1){counter<-counter+nyrs}
          tows[counter:(counter+(nyrs-1)),1] <-yrs
          tows[counter:(counter+(nyrs-1)),2] <-as.numeric(tows_top[[i]][[ii]][1:nyrs])
          tows[counter:(counter+(nyrs-1)),3] <-rep(samp_scenario[alt],nyrs)
          tows[counter:(counter+(nyrs-1)),4] <-rep(i,nyrs)
          tows[counter:(counter+(nyrs-1)),5] <-rep(species_common,nyrs)
          tows[counter:(counter+(nyrs-1)),6] <-rep("Tows",nyrs)
          tows[counter:(counter+(nyrs-1)),7] <-rep(ii,nyrs)
          tows[counter:(counter+(nyrs-1)),8] <-rep(paste(method[m]),nyrs)
        }
      }

      tmp_scen_tows[[alt]]<-tows

    }
    names(tmp_scen_tows) <-samp_scenario
    tmp_meth_tows[[m]]   <-tmp_scen_tows
  }
  names(tmp_meth_tows) <-method
  tmp_spec_tows[[s]]   <-tmp_meth_tows
}

names(tmp_spec_tows)<-species

tows_dr_otos  <-map_df(tmp_spec_tows[[1]][[1]],~as.data.frame(.))
tows_pop_otos <-map_df(tmp_spec_tows[[2]][[1]],~as.data.frame(.))
tows_pk_otos  <-map_df(tmp_spec_tows[[3]][[1]],~as.data.frame(.))

tows_dr_tows  <-map_df(tmp_spec_tows[[1]][[2]],~as.data.frame(.))
tows_pop_tows <-map_df(tmp_spec_tows[[2]][[2]],~as.data.frame(.))
tows_pk_tows  <-map_df(tmp_spec_tows[[3]][[2]],~as.data.frame(.))

tows <-rbind(tows_dr_otos,tows_dr_tows,tows_pop_otos,tows_pop_tows,tows_pk_otos,tows_pk_tows)
colnames(tows)  <-c("Year","Value","Scenario","Spec_Boot","Species","Metric","Exp_Boot","Method")
tows$Value<-as.numeric(tows$Value)
tows$Exp_Boot<-as.numeric(tows$Exp_Boot)
tows$Spec_Boot<-as.numeric(tows$Spec_Boot)

tows$Scenario <-revalue(tows$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
tows$Scenario <-factor(tows$Scenario,levels=c("-67%","-33%","0%","+33%","+67%"))
tows$Method   <-revalue(tows$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))

write.csv(tows,paste0(pathOUT,"tows.csv"),row.names=FALSE)

################################################################################
### InputSS
tmp_meth_iss<-list();tmp_spec_iss<-list();tmp_scen_iss<-list()

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
    inputSS <-list()
    load(paste(pathR,"SzAC Results_",method[m],"/pipeline_samp_change_alt_",species[s],"_",method[m],sep=""))
    inputSS <-samp_suite_alt
    for(alt in 1:length(samp_scenario)){
      if(alt==1){iss_top<-inputSS[[1]]}
      if(alt==2){iss_top<-inputSS[[2]]}
      if(alt==3){iss_top<-inputSS[[3]]}
      if(alt==4){iss_top<-inputSS[[4]]}
      if(alt==5){iss_top<-inputSS[[5]]}
      iss <-matrix(NA,ncol=7,nrow=(spec_boots)*nyrs)
      for(i in 1:spec_boots){
        if(i==1){counter<-1}
        if(i>1){counter<-counter+nyrs}
        iss[counter:(counter+(nyrs-1)),1] <-yrs
        iss[counter:(counter+(nyrs-1)),2] <-as.numeric(inputSS[[alt]][[i]][1:nyrs])
        iss[counter:(counter+(nyrs-1)),3] <-rep(samp_scenario[alt],nyrs)
        iss[counter:(counter+(nyrs-1)),4] <-rep(species_common,nyrs)
        iss[counter:(counter+(nyrs-1)),5] <-rep("Input SS",nyrs)
        iss[counter:(counter+(nyrs-1)),6] <-rep(i,nyrs)
        iss[counter:(counter+(nyrs-1)),7] <-rep(paste(method[m]),nyrs)
      }
      tmp_scen_iss[[alt]]<-iss
    }
    names(tmp_scen_iss) <-samp_scenario
    tmp_meth_iss[[m]]   <-tmp_scen_iss
  }
  names(tmp_meth_iss) <-method
  tmp_spec_iss[[s]]   <-tmp_meth_iss
}

names(tmp_spec_iss)<-species

iss_dr_otos  <-map_df(tmp_spec_iss[[1]][[1]],~as.data.frame(.))
iss_pop_otos <-map_df(tmp_spec_iss[[2]][[1]],~as.data.frame(.))
iss_pk_otos  <-map_df(tmp_spec_iss[[3]][[1]],~as.data.frame(.))

iss_dr_tows  <-map_df(tmp_spec_iss[[1]][[2]],~as.data.frame(.))
iss_pop_tows <-map_df(tmp_spec_iss[[2]][[2]],~as.data.frame(.))
iss_pk_tows  <-map_df(tmp_spec_iss[[3]][[2]],~as.data.frame(.))

iss <-rbind(iss_dr_otos,iss_dr_tows,iss_pop_otos,iss_pop_tows,iss_pk_otos,iss_pk_tows)
colnames(iss)  <-c("Year","Value","Scenario","Species","Metric","Spec_Boot","Method")

iss$Scenario <-revalue(iss$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
iss$Scenario <-factor(iss$Scenario,levels=c("-67%","-33%","0%","+33%","+67%"))
iss$Method   <-revalue(iss$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))

write.csv(iss,paste0(pathOUT,"iss.csv"),row.names=FALSE)


#######################################################################################
### This code is for calculating means across bootstrap replicates and joining datasets
# # Calc mean/median iss for 0% sampling scenario
# iss_0 <-iss[which(iss$Scenario=="3 Boat"),]
# iss_0_mean<-aggregate(as.numeric(iss_0[,2]),list(iss_0$Year,iss_0$Species,iss_0$Method),mean)
# iss_0_median<-aggregate(as.numeric(iss_0[,2]),list(iss_0$Year,iss_0$Species,iss_0$Method),median)
# # write.csv(iss_0_mean,"iss_0_mean.csv")
# # write.csv(iss_0_median,"iss_0_median.csv")
#
# # Calc otos per tow
# otoPtow <-as.numeric(nomSS$Value)/as.numeric(tows$Value)
# otoPtow <-as.data.frame(cbind(nomSS[,1],as.numeric(otoPtow),nomSS[,3],nomSS[,4],nomSS[,5],rep("Oto/Tow",length(nomSS[,4])),nomSS[,7],nomSS[,8]))
# otoPtow$V2 <-as.numeric(otoPtow$V2)
# colnames(otoPtow) <-c("Year","Value","Scenario","Spec_Boot","Species","Metric","Exp_Boot","Method")
# otoPtow$Scenario <-revalue(otoPtow$Scenario,c("1"="-67%","2"="-33%","3"="0%","4"="+33%","5"="+67%"))
# otoPtow$Scenario <-factor(otoPtow$Scenario,levels=c("-67%","-33%","0%","+33%","+67%"))
#
# # Combine nomSS, tows, iss, and ess data frames
# dat <-rbind(tows,otoPtow,nomSS)
# # dat <-rbind(nomSS,tows,otoPtow)
# dat$Value<-as.numeric(dat$Value)
# dat_means<-aggregate(dat[,2],list(dat$Metric,dat$Year,dat$Scenario,dat$Species,dat$Method),mean)
# colnames(dat_means) <-c("Metric","Year","Scenario","Species","Method","Value")
# 
# iss$Year <-as.character(iss$Year)
# iss$Value <-as.numeric(iss$Value)
# iss$Spec_Boot <-as.numeric(iss$Spec_Boot)
# iss_means<-aggregate(iss[,2],list(iss$Year,iss$Scenario,iss$Species,iss$Method),mean)
# colnames(iss_means) <-c("Year","Scenario","Species","Method","InputSS")
# iss_means$Scenario <-revalue(iss_means$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
# iss_means$Scenario <-factor(iss_means$Scenario,levels=c("-67%","-33%","0%","+33%","+67%"))
# iss_means$Method <-revalue(iss_means$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))
#
# effn$Year <-as.character(effn$Year)
# effn$Value <-as.numeric(effn$Value)
# effn$Boot <-as.numeric(effn$Boot)
# effn_means<-aggregate(effn[,2],list(effn$Year,effn$Scenario,effn$Species,effn$Method),mean)
# colnames(effn_means) <-c("Year","Scenario","Species","Method","effn")
# effn_means$Scenario <-revalue(effn_means$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
# effn_means$Scenario <-factor(effn_means$Scenario,levels=c("-67%","-33%","0%","+33%","+67%"))
# effn_means$Method <-revalue(effn_means$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))
# effn_trtmean<-aggregate(effn[,2],list(effn$Scenario,effn$Species,effn$Method),mean)
# colnames(effn_trtmean) <-c("Scenario","Species","Method","effn")
#
# N_join <-dplyr::full_join(effn_means,iss_means,by=c("Method","Species","Year","Scenario"))
# dat_join<-dplyr::full_join(dat_means,iss_means,by=c("Method","Species","Year","Scenario"))
#
# iss$Value<-as.numeric(iss$Value)
# # iss[which(iss$Value==0),]$Value <-1
# # iss[which(iss$Value=="NaN"),]$Value <-1
# iss$Spec_Boot<-as.numeric(iss$Spec_Boot)
# iss_mean<-aggregate(iss[,2],list(iss$Year,iss$Scenario,iss$Species,iss$Method),mean)
# colnames(iss_mean) <-c("Year","Scenario","Species","Method","InputSS")
# iss_specmean<-aggregate(iss[,2],list(iss$Spec_Boot,iss$Year,iss$Scenario,iss$Species,iss$Method),mean)
# colnames(iss_specmean) <-c("Spec_Boot","Year","Scenario","Species","Method","nomSS")
#
# iss<-iss[which(iss$Value!="NaN"),]
# iss_trtmean<-aggregate(iss[,2],list(iss$Scenario,iss$Species,iss$Method),mean)
# colnames(iss_trtmean) <-c("Scenario","Species","Method","InputSS")
# # write.csv(iss_trtmean,"iss_trtmean.csv")


################################################################################
### OFL
pathAMAK <-"C:/Users/matthew.siskey/Desktop/2022_UW-AFSC Post-Doc/Oto Redist Project/Data & Code/"
tmp_meth_ofl<-list();tmp_spec_ofl<-list()

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
    pathR   <-paste(pathAMAK,"AMAK_saves_",method[m],"_2022Jan23",sep="")
    load(paste(pathR,"/ofl_",species[s],"_",stock,"_",method[m],sep=""))
    AMAK_ofl <-matrix(NA,ncol=6,nrow=spec_boots*length(samp_scenario))

    for(i in 1:length(samp_scenario)){
      if(i==1){counter=0}
      if(i==2){counter=200}
      if(i==3){counter=400}
      if(i==4){counter=600}
      if(i==5){counter=800}

      for(ii in 1:spec_boots){
        counter=counter+1
        AMAK_ofl[counter,1] <-ofl_top[[i]][[ii]][1]
        AMAK_ofl[counter,2] <-paste(samp_scenario[i])
        AMAK_ofl[counter,3] <-paste(species_common)
        AMAK_ofl[counter,4] <-"OFL"
        AMAK_ofl[counter,5] <-ii
        AMAK_ofl[counter,6] <-method[m]
      }
    }
    tmp_meth_ofl[[m]]<-AMAK_ofl
  }
  names(tmp_meth_ofl) <-method
  tmp_spec_ofl[[s]]   <-tmp_meth_ofl
}

names(tmp_spec_ofl)<-species

ofl_dr  <-map_df(tmp_spec_ofl[[1]],~as.data.frame(.))
ofl_pop <-map_df(tmp_spec_ofl[[2]],~as.data.frame(.))
ofl_pk  <-map_df(tmp_spec_ofl[[3]],~as.data.frame(.))

colnames(ofl_dr) <-c("OFL","Scenario","Species","Metric","Boot","Method")
ofl_dr$OFL<-as.numeric(ofl_dr$OFL)
colnames(ofl_pop) <-c("OFL","Scenario","Species","Metric","Boot","Method")
ofl_pop$OFL<-as.numeric(ofl_pop$OFL)
colnames(ofl_pk) <-c("OFL","Scenario","Species","Metric","Boot","Method")
ofl_pk$OFL<-as.numeric(ofl_pk$OFL)

ofl <-rbind(ofl_dr,ofl_pop,ofl_pk) #
colnames(ofl) <-c("Value","Scenario","Species","Metric","Boot","Method")
write.csv(ofl,"ofl.csv")

### Mean of OFL
ofl_means<-aggregate(ofl[,1],list(ofl$Scenario,ofl$Species,ofl$Method),mean)
colnames(ofl_means)<-c("Scenario","Species","Method","Mean")
ofl_means$Scenario <-revalue(ofl_means$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
ofl_means$Scenario <-factor(ofl_means$Scenario,levels=c("-67%","-33%","0%","+33%","+67%"))
write.csv(ofl_means,"ofl_means.csv")


################################################################################
### Francis weights
tmp_meth_wt<-list();tmp_spec_wt<-list()

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
    pathR   <-paste("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/AMAK_saves_",method[m],"_2022Jan23",sep="")
    load(paste(pathR,"/age_mult_",species[s],"_",stock,"_",method[m],sep=""))
    AMAK_wt <-matrix(NA,ncol=6,nrow=spec_boots*length(samp_scenario))

    for(i in 1:length(samp_scenario)){
      if(i==1){counter=0}
      if(i==2){counter=200}
      if(i==3){counter=400}
      if(i==4){counter=600}
      if(i==5){counter=800}

      for(ii in 1:spec_boots){
        counter=counter+1
        AMAK_wt[counter,1] <-as.numeric(age_mult_top[[i]][[ii]][3])
        AMAK_wt[counter,2] <-paste(samp_scenario[i])
        AMAK_wt[counter,3] <-paste(species_common)
        AMAK_wt[counter,4] <-"AgeMult"
        AMAK_wt[counter,5] <-ii
        AMAK_wt[counter,6] <-method[m]
      }
    }
    tmp_meth_wt[[m]]<-AMAK_wt
  }
  names(tmp_meth_wt) <-method
  tmp_spec_wt[[s]]   <-tmp_meth_wt
}

names(tmp_spec_wt)<-species

wt_dr  <-map_df(tmp_spec_wt[[1]],~as.data.frame(.))
wt_pop <-map_df(tmp_spec_wt[[2]],~as.data.frame(.))
wt_pk  <-map_df(tmp_spec_wt[[3]],~as.data.frame(.))

colnames(wt_dr) <-c("AgeMult","Scenario","Species","Metric","Boot","Method")
wt_dr$AgeMult<-as.numeric(wt_dr$AgeMult)
colnames(wt_pop) <-c("AgeMult","Scenario","Species","Metric","Boot","Method")
wt_pop$AgeMult<-as.numeric(wt_pop$AgeMult)
colnames(wt_pk) <-c("AgeMult","Scenario","Species","Metric","Boot","Method")
wt_pk$AgeMult<-as.numeric(wt_pk$AgeMult)

wt <-rbind(wt_dr,wt_pop,wt_pk) #
colnames(wt) <-c("Value","Scenario","Species","Metric","Boot","Method")
# write.csv(wt,"wt.csv")


################################################################################
### Francis N_eff (iss*weights)
tmp_meth_iss<-list();tmp_spec_iss<-list();tmp_scen_iss<-list()

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
    inputSS <-list()
    pathISS<-"C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/otosamp/"
    load(paste(pathISS,"SzAC Results_",method[m],"/pipeline_samp_change_alt_",species[s],"_",method[m],sep=""))
    pathR   <-paste("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/AMAK_saves_",method[m],"_2022Jan23",sep="")
    load(paste(pathR,"/age_mult_",species[s],"_",stock,"_",method[m],sep=""))
    inputSS <-samp_suite_alt
    for(alt in 1:length(samp_scenario)){
      if(alt==1){iss_top<-inputSS[[1]]}
      if(alt==2){iss_top<-inputSS[[2]]}
      if(alt==3){iss_top<-inputSS[[3]]}
      if(alt==4){iss_top<-inputSS[[4]]}
      if(alt==5){iss_top<-inputSS[[5]]}
      iss <-matrix(NA,ncol=7,nrow=(spec_boots)*nyrs)
      for(i in 1:spec_boots){
        if(i==1){counter<-1}
        if(i>1){counter<-counter+nyrs}
        iss[counter:(counter+(nyrs-1)),1] <-yrs
        iss[counter:(counter+(nyrs-1)),2] <-as.numeric(inputSS[[alt]][[i]][1:nyrs])*as.numeric(age_mult_top[[alt]][[i]][3])
        iss[counter:(counter+(nyrs-1)),3] <-rep(samp_scenario[alt],nyrs)
        iss[counter:(counter+(nyrs-1)),4] <-rep(species_common,nyrs)
        iss[counter:(counter+(nyrs-1)),5] <-rep("Input SS",nyrs)
        iss[counter:(counter+(nyrs-1)),6] <-rep(i,nyrs)
        iss[counter:(counter+(nyrs-1)),7] <-rep(paste(method[m]),nyrs)
      }
      tmp_scen_iss[[alt]]<-iss
    }
    names(tmp_scen_iss) <-samp_scenario
    tmp_meth_iss[[m]]   <-tmp_scen_iss
  }
  names(tmp_meth_iss) <-method
  tmp_spec_iss[[s]]   <-tmp_meth_iss
}

names(tmp_spec_iss)<-species

effn_dr_otos  <-map_df(tmp_spec_iss[[1]][[1]],~as.data.frame(.))
effn_pop_otos <-map_df(tmp_spec_iss[[2]][[1]],~as.data.frame(.))
effn_pk_otos  <-map_df(tmp_spec_iss[[3]][[1]],~as.data.frame(.))

effn_dr_tows  <-map_df(tmp_spec_iss[[1]][[2]],~as.data.frame(.))
effn_pop_tows <-map_df(tmp_spec_iss[[2]][[2]],~as.data.frame(.))
effn_pk_tows  <-map_df(tmp_spec_iss[[3]][[2]],~as.data.frame(.))

effn <-rbind(effn_dr_otos,effn_dr_tows,effn_pop_otos,effn_pop_tows,effn_pk_otos,effn_pk_tows)
colnames(effn)  <-c("Year","Value","Scenario","Species","Metric","Spec_Boot","Method")

effn$Value<-as.numeric(effn$Value)
effn[which(effn$Value==0),]$Value <-10
effn[which(effn$Value=="NaN"),]$Value <-10
effn$Spec_Boot<-as.numeric(effn$Spec_Boot)
effn_trtmean<-aggregate(effn[,2],list(effn$Scenario,effn$Species,effn$Method),mean)
colnames(effn_trtmean) <-c("Scenario","Species","Method","effn")
# write.csv(effn_trtmean,"effn_trtmean.csv")

N_join_trtmean <-dplyr::full_join(effn_trtmean,iss_trtmean,by=c("Method","Species","Scenario"))

################################################################################
### Log Theta
pathR   <-"/AMAK_saves_ototow_otos"
species <-c("30152","30060")
tmp_meth_LOGtheta<-list();tmp_spec_LOGtheta<-list()

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
    load(paste(pathR,"/log_theta_",species[s],"_",stock,"_",method[m],sep=""))
    if(species[s]=="30152"){log_theta_top[[1]][[1]]<-20}
    if(species[s]=="30060"){log_theta_top[[1]][[1]]<-0.5}
    AMAK_LOGtheta <-matrix(NA,ncol=5,nrow=500)

    for(i in 1:length(samp_scenario)){
      if(i==1){counter=0}
      if(i==2){counter=100}
      if(i==3){counter=200}
      if(i==4){counter=300}
      if(i==5){counter=400}
      for(ii in 1:100){
        counter=counter+1
        AMAK_LOGtheta[counter,1] <-as.numeric(log_theta_top[[i]][[ii]])
        AMAK_LOGtheta[counter,2] <-paste(samp_scenario[i])
        AMAK_LOGtheta[counter,3] <-paste(species_common)
        AMAK_LOGtheta[counter,4] <-"Log(theta)"
        AMAK_LOGtheta[counter,5] <-ii
      }
    }
    tmp_meth_LOGtheta[[m]]<-AMAK_LOGtheta
  }
  names(tmp_meth_LOGtheta) <-method
  tmp_spec_LOGtheta[[s]]   <-tmp_meth_LOGtheta
}

names(tmp_spec_LOGtheta)<-species

LOGtheta_dr  <-map_df(tmp_spec_LOGtheta[[1]],~as.data.frame(.))
LOGtheta_pop <-map_df(tmp_spec_LOGtheta[[2]],~as.data.frame(.))
LOGtheta_pk  <-map_df(tmp_spec_LOGtheta[[3]],~as.data.frame(.))

LOGtheta <-rbind(LOGtheta_dr,LOGtheta_pop)
LOGtheta$V1<-as.numeric(LOGtheta$V1)
LOGtheta <-LOGtheta[which(LOGtheta$V1<19),]
colnames(LOGtheta) <-c("Value","Scenario","Species","Metric","Boot")


################################################################################
### EffN
tmp_meth_effn<-list();tmp_spec_effn<-list(); tmp_scen_effn<-list();

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
    pathR   <-paste("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/AMAK_saves_",method[m],"_2022Jan23",sep="")
    effss <-list()
    load(paste(pathR,"/effn_out_",species[s],"_",stock,"_",method[m],sep=""))
    effss <-effn_list_top

    for(i in 1:length(samp_scenario)){
      AMAK_effn <-matrix(NA,ncol=7,nrow=spec_boots*nyrs)

      for(ii in 1:spec_boots){
        if(ii==1){counter<-1}
        if(ii>1){counter<-counter+nyrs}
        AMAK_effn[counter:(counter+(nyrs-1)),1] <-yrs
        AMAK_effn[counter:(counter+(nyrs-1)),2] <-as.numeric(effn_list_top[[i]][[ii]][1:nyrs,3])
        AMAK_effn[counter:(counter+(nyrs-1)),3] <-rep(samp_scenario[i],nyrs)
        AMAK_effn[counter:(counter+(nyrs-1)),4] <-rep(species_common,nyrs)
        AMAK_effn[counter:(counter+(nyrs-1)),5] <-rep("EffN",nyrs)
        AMAK_effn[counter:(counter+(nyrs-1)),6] <-rep(ii,nyrs)
        AMAK_effn[counter:(counter+(nyrs-1)),7] <-rep(paste(method[m]),nyrs)
      }
      tmp_scen_effn[[i]]<-AMAK_effn
    }
    names(tmp_scen_effn)<-samp_scenario
    tmp_meth_effn[[m]]<-tmp_scen_effn
  }
  names(tmp_meth_effn) <-method
  tmp_spec_effn[[s]]   <-tmp_meth_effn
}
names(tmp_spec_effn)<-species

effn_dr_otos  <-map_df(tmp_spec_effn[[1]][[1]],~as.data.frame(.))
effn_pop_otos <-map_df(tmp_spec_effn[[2]][[1]],~as.data.frame(.))
effn_pk_otos  <-map_df(tmp_spec_effn[[3]][[1]],~as.data.frame(.))

effn_dr_tows  <-map_df(tmp_spec_effn[[1]][[2]],~as.data.frame(.))
effn_pop_tows <-map_df(tmp_spec_effn[[2]][[2]],~as.data.frame(.))
effn_pk_tows  <-map_df(tmp_spec_effn[[3]][[2]],~as.data.frame(.))

effn <-rbind(effn_dr_otos,effn_pop_otos,effn_pk_otos,effn_dr_tows,effn_pop_tows,effn_pk_tows)
effn$V2<-as.numeric(effn$V2)
colnames(effn) <-c("Year","Value","Scenario","Species","Metric","Boot","Method")

effn$Scenario <-revalue(effn$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
effn$Scenario <-factor(effn$Scenario,levels=c("-67%","-33%","0%","+33%","+67%"))
effn$Method <-revalue(effn$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))

effn$Boot<-as.numeric(effn$Boot)
effn_mean<-aggregate(effn[,2],list(effn$Year,effn$Scenario,effn$Species,effn$Method),mean)
colnames(effn_mean) <-c("Year","Scenario","Species","Method","Neff")
effn_specmean<-aggregate(effn[,2],list(effn$Boot,effn$Year,effn$Scenario,effn$Species,effn$Method),mean)
colnames(effn_specmean) <-c("Spec_Boot","Year","Scenario","Species","Method","Neff")
effn_trtmean<-aggregate(effn[,2],list(effn$Scenario,effn$Species,effn$Method),mean)
colnames(effn_trtmean) <-c("Scenario","Species","Method","Neff")
write.csv(effn_trtmean,"effn_trtmean.csv")


################################################################################
### ISS Retuned
tmp_meth_issout<-list();tmp_spec_issout<-list(); tmp_scen_issout<-list();
species <-c("30152","30060")
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
    pathR   <-paste("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/AMAK_saves_",method[m],"_2022Jan23",sep="")
    issout <-list()
    if(species[s]!="21740"){load(paste(pathR,"/inputN_srv1_",species[s],"_",stock,"_",method[m],sep=""));issout <-inputN_srv1_top}
    if(species[s]=="21740"){load(paste(pathR,"/inputN_srv2_",species[s],"_",stock,"_",method[m],sep=""));issout <-inputN_srv2_top}

    for(i in 1:length(samp_scenario)){
      AMAK_issout <-matrix(NA,ncol=7,nrow=spec_boots*nyrs)

      for(ii in 1:spec_boots){
        if(ii==1){counter<-1}
        if(ii>1){counter<-counter+nyrs}
        AMAK_issout[counter:(counter+(nyrs-1)),1] <-yrs
        if(species[s]!="21740"){AMAK_issout[counter:(counter+(nyrs-1)),2] <-as.numeric(inputN_srv1_top[[i]][[ii]][[3]][1:nyrs,2])} # 3rd iterations iss
        if(species[s]=="21740"){AMAK_issout[counter:(counter+(nyrs-1)),2] <-as.numeric(inputN_srv2_top[[i]][[ii]][[3]][1:nyrs,2])} # 3rd iterations iss
        AMAK_issout[counter:(counter+(nyrs-1)),3] <-rep(samp_scenario[i],nyrs)
        AMAK_issout[counter:(counter+(nyrs-1)),4] <-rep(species_common,nyrs)
        AMAK_issout[counter:(counter+(nyrs-1)),5] <-rep("issOut",nyrs)
        AMAK_issout[counter:(counter+(nyrs-1)),6] <-rep(ii,nyrs)
        AMAK_issout[counter:(counter+(nyrs-1)),7] <-rep(paste(method[m]),nyrs)
      }
      tmp_scen_issout[[i]]<-AMAK_issout
    }
    names(tmp_scen_issout)<-samp_scenario
    tmp_meth_issout[[m]]<-tmp_scen_issout
  }
  names(tmp_meth_issout) <-method
  tmp_spec_issout[[s]]   <-tmp_meth_issout
}
names(tmp_spec_issout)<-species

issout_dr_otos  <-map_df(tmp_spec_issout[[1]][[1]],~as.data.frame(.))
issout_pop_otos <-map_df(tmp_spec_issout[[2]][[1]],~as.data.frame(.))
issout_pk_otos  <-map_df(tmp_spec_issout[[3]][[1]],~as.data.frame(.))

issout_dr_tows  <-map_df(tmp_spec_issout[[1]][[2]],~as.data.frame(.))
issout_pop_tows <-map_df(tmp_spec_issout[[2]][[2]],~as.data.frame(.))
issout_pk_tows  <-map_df(tmp_spec_effn[[3]][[2]],~as.data.frame(.))

issout <-rbind(issout_dr_otos,issout_pop_otos,issout_dr_tows,issout_pop_tows)
issout$V2<-as.numeric(issout$V2)
colnames(issout) <-c("Year","Value","Scenario","Species","Metric","Boot","Method")

issout$Scenario <-revalue(issout$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
issout$Scenario <-factor(issout$Scenario,levels=c("-67%","-33%","0%","+33%","+67%"))
issout$Method <-revalue(issout$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))

issout$Boot<-as.numeric(issout$Boot)
issout_mean<-aggregate(issout[,2],list(issout$Year,issout$Scenario,issout$Species,issout$Method),mean)
colnames(issout_mean) <-c("Year","Scenario","Species","Method","issout")
issout_specmean<-aggregate(issout[,2],list(issout$Boot,issout$Year,issout$Scenario,issout$Species,issout$Method),mean)
colnames(issout_specmean) <-c("Spec_Boot","Year","Scenario","Species","Method","issout")
issout_trtmean<-aggregate(issout[,2],list(issout$Scenario,issout$Species,issout$Method),mean)
colnames(issout_trtmean) <-c("Scenario","Species","Method","issout")
# write.csv(issout_trtmean,"issout_trtmean.csv")


### Figuring out how many model runs were tuned to the threshold
issout_otos <-issout[which(issout$Method=="Otoliths Changed"),]
issout_otos_dr <-issout_otos[which(issout_otos$Species=="Dusky Rockfish"),]
issout_otos_pop <-issout_otos[which(issout_otos$Species=="Pacific Ocean Perch"),]

issout_tows <-issout[which(issout$Method=="Tows Changed"),]
issout_tows_dr <-issout_tows[which(issout_tows$Species=="Dusky Rockfish"),]
issout_tows_pop <-issout_tows[which(issout_tows$Species=="Pacific Ocean Perch"),]

# 5 trt x 16 yrs x 200 reps = 16000 (3200 per trt)
issout_otos_dr %>% dplyr::count(Scenario,Value==3) # iss thresh was 5
issout_tows_dr %>% dplyr::count(Scenario,Value==3) # iss thresh was 10

# 5 trt x 13 yrs x 200 reps = 13000 (2600 per trt)
issout_otos_pop %>% dplyr::count(Scenario,Value>3) # iss thresh was 5
issout_tows_pop %>% dplyr::count(Scenario,Value>3) # iss thresh was 5


################################################################################
### CompDist
species <-c("30152","30060")#,"21740")
tmp_meth_comp<-list();tmp_spec_comp<-list()

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
    pathR <-paste("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/AMAK_saves_",method[m],sep="")
    load(paste(pathR,"/comp_record_",species[s],"_",stock,"_",method[m],sep=""))
    AMAK_compdist <-matrix(NA,ncol=6,nrow=spec_boots*length(samp_scenario))

    for(i in 1:length(samp_scenario)){
      if(i==1){counter=0}
      if(i==2){counter=200}
      if(i==3){counter=400}
      if(i==4){counter=600}
      if(i==5){counter=800}
      for(ii in 1:spec_boots){
        counter=counter+1
        AMAK_compdist[counter,1] <-comp_record_top[[i]][[ii]]
        AMAK_compdist[counter,2] <-paste(samp_scenario[i])
        AMAK_compdist[counter,3] <-paste(species_common)
        AMAK_compdist[counter,4] <-"CompDist"
        AMAK_compdist[counter,5] <-ii
        AMAK_compdist[counter,6] <-method[m]
      }
    }
    tmp_meth_comp[[m]]<-AMAK_compdist
  }
  names(tmp_meth_comp) <-method
  tmp_spec_comp[[s]]   <-tmp_meth_comp
}

names(tmp_spec_comp)<-species

comp_dr  <-map_df(tmp_spec_comp[[1]],~as.data.frame(.))
comp_pop <-map_df(tmp_spec_comp[[2]],~as.data.frame(.))
comp_pk  <-map_df(tmp_spec_comp[[3]],~as.data.frame(.))

comp <-rbind(comp_dr,comp_pop)
comp$V1<-as.numeric(comp$V1)
colnames(comp) <-c("Value","Scenario","Species","Metric","Boot","Method")

comp_multi <-comp[which(comp$Value=="1"),]
comp_sum<-ddply(comp_multi,c("Method","Scenario","Species"),summarize,sum=sum(Value))


###############################################################################################################
############################################## CIs, Uncertainty ###############################################
###############################################################################################################

################################################################################
### OFL CIs & Bootstrapped datasets for Cost-Revenue Analysis
library(boot);library(infer);library(DescTools);library(Rmisc);
rm(list=ls())
pathOUT       <-"/Output/"
ofl   <-read.csv(paste0(pathOUT,"ofl.csv"))
set.seed(4123)
spec <-c("Dusky Rockfish","Pacific Ocean Perch","Walleye Pollock")
meth <-c("ototow_otos","ototow_tows")
scen <-c("1 Boat","2 Boat","3 Boat","4 Boat","5 Boat")

boot_dat<-list();boot_0_mean<-list();boot_0_mean_sc<-list();boot_0_mean_meth<-list();boot_0_mean_sp<-list();
dat<-ofl

### Boots of 0% mean for use in all bootstrapped SD calcs, which are then used to calc CIs of SD OFL and CV OFL
for(sp in 1:length(spec)){
  dat_sp<-dat[which(dat$Species==spec[sp]),]
  for(m in 1:length(meth)){
    dat_m<-dat_sp[which(dat_sp$Method==meth[m]),]
    for(sc in 3:3){
      dat_sc<-dat_m[which(dat_m$Scenario==scen[sc]),]
      for(i in 1:1000){
        boot_dat[[i]]    <-temp<-sample(dat_sc$Value,size=nrow(dat_sc),replace=TRUE)
        boot_0_mean[[i]] <-mean(temp)
      }
      boot_0_mean_sc  <-boot_0_mean
    }
    boot_0_mean_meth[[m]]  <-boot_0_mean_sc
  }
  names(boot_0_mean_meth)<-meth
  boot_0_mean_sp[[sp]]  <-boot_0_mean_meth
}
names(boot_0_mean_sp)<-spec


### Bootstrapped OFLs for use in bootstrapped SD OFL and CV OFL, in turn used to calc CIs of SD OFL and CV OFL
boot_dat <-list(); boot_save_sc <-list(); boot_save_meth <-list(); boot_save_sp <-list();
boot_sd_sc<-list(); boot_sd<-list(); boot_CVSD<-list(); boot_CVSD_sc<-list();boot_sd_meth<-list();  boot_CVSD_meth<-list();
boot_sd_sp<-list(); boot_CVSD_sp<-list();ci_sd_sc<-list(); ci_CVSD_sc<-list(); ci_sd_meth<-list(); ci_CVSD_meth<-list();
boot_log<-list(); boot_sd_log<-list(); boot_log_sc<-list(); boot_sd_log_sc<-list(); ci_sd_sp<-list();  ci_CVSD_sp<-list();
boot_log_meth<-list(); boot_sd_log_meth<-list(); boot_log_sp<-list(); boot_sd_log_sp<-list();

dat <-ofl
N_r <-200

for(sp in 1:length(spec)){
  dat_sp<-dat[which(dat$Species==spec[sp]),]
  for(m in 1:length(meth)){
    dat_m<-dat_sp[which(dat_sp$Method==meth[m]),]
    for(sc in 1:length(scen)){
      dat_sc<-dat_m[which(dat_m$Scenario==scen[sc]),]
      for(i in 1:200){
        boot_dat[[i]]    <-temp<-sample(dat_sc$Value,size=nrow(dat_sc),replace=TRUE)
        boot_sd[[i]]     <-sqrt(1/N_r * sum((temp - as.numeric(boot_0_mean_sp[[sp]][[m]][[i]]))^2))
        boot_CVSD[[i]]   <-sqrt(1/N_r * sum((temp - as.numeric(boot_0_mean_sp[[sp]][[m]][[i]]))^2))/as.numeric(boot_0_mean_sp[[sp]][[m]][[i]])
        boot_sd_log[[i]] <-sqrt(1/N_r * sum((log(temp) - log(as.numeric(boot_0_mean_sp[[sp]][[m]][[i]])))^2))
        # boot_sd[[i]]     <-sd(temp)
        # boot_CVSD[[i]]   <-sd(temp)/mean(temp)
        # boot_sd_log[[i]] <-sd(log(temp))
        boot_log[[i]]    <-log(temp)
      }
      boot_save_sc[[sc]]  <-boot_dat
      boot_sd_sc[[sc]]    <-boot_sd
      boot_CVSD_sc[[sc]]  <-boot_CVSD
      boot_log_sc[[sc]]   <-boot_log
      boot_sd_log_sc[[sc]]<-boot_sd_log
      ci_sd_sc[[sc]]    <-quantile(as.numeric(boot_sd),probs=c(0.025,0.975))
      ci_CVSD_sc[[sc]]  <-quantile(as.numeric(boot_CVSD),probs=c(0.025,0.975))
    }
    names(boot_save_sc)<-scen;names(boot_sd_sc)<-scen;names(boot_CVSD_sc)<-scen;names(boot_log_sc)<-scen;names(boot_sd_log_sc)<-scen;
    names(ci_sd_sc)<-scen;names(ci_CVSD_sc)<-scen;
    boot_save_meth[[m]]  <-boot_save_sc
    boot_sd_meth[[m]]    <-boot_sd_sc
    boot_CVSD_meth[[m]]  <-boot_CVSD_sc
    boot_log_meth[[m]]   <-boot_log_sc
    boot_sd_log_meth[[m]]<-boot_sd_log_sc
    ci_sd_meth[[m]]    <-ci_sd_sc
    ci_CVSD_meth[[m]]  <-ci_CVSD_sc
  }
  names(boot_save_meth)<-meth;names(boot_sd_meth)<-meth;names(boot_CVSD_meth)<-meth;names(boot_log_meth)<-meth;names(boot_sd_log_meth)<-meth;
  boot_save_sp[[sp]]  <-boot_save_meth
  boot_sd_sp[[sp]]    <-boot_sd_meth
  boot_CVSD_sp[[sp]]  <-boot_CVSD_meth
  boot_log_sp[[sp]]   <-boot_log_meth
  boot_sd_log_sp[[sp]]<-boot_sd_log_meth
  names(ci_sd_meth)<-meth;names(ci_CVSD_meth)<-meth;
  ci_sd_sp[[sp]]    <-ci_sd_meth
  ci_CVSD_sp[[sp]]  <-ci_CVSD_meth
}
names(boot_log_sp)<-spec;names(boot_sd_log_sp)<-spec;names(ci_sd_sp)<-spec;names(ci_CVSD_sp)<-spec;names(boot_save_sp)<-spec;names(boot_sd_sp)<-spec;names(boot_CVSD_sp)<-spec;


# Save boots of SD and MAD for bootstrapped revenue analysis
save(boot_sd_sp,file="C:/Users/matthew.siskey/Desktop/ofl_boot_sd_sp_0mean")
save(boot_sd_log_sp,file="C:/Users/matthew.siskey/Desktop/ofl_boot_sd_log_sp_0mean")

### Turn list of CIs for SD and CV into dataframes for plotting below
unc<-list();ci_temp<-list();
unc[[1]] <-ci_sd_sp
unc[[2]] <-ci_CVSD_sp
names(unc) <-c("ci_sd_sp","ci_CVSD_sp")
unc_names  <-c("SD OFL","CV OFL (SD)")
scen_loop <-c("-67%","-67%","-33%","-33%","0%","0%","+33%","+33%","+67%","+67%")

for(u in 1:length(unc)){ci_temp<-unc[[u]]
for(sp in 1:length(spec)){
  for(m in 1:length(meth)){
    ci <-map_df(ci_temp[[sp]][[m]],~as.data.frame(.))
    ci <-cbind(ci,rep(unc_names[u],nrow(ci)),rep(c("Lower","Upper"),5),rep(spec[sp],nrow(ci)),scen_loop,rep(meth[m],nrow(ci)))
    if(m==1){temp_meth <-ci}
    if(m==2){ci_df_meth<-rbind(temp_meth,ci)}
  }
  if(sp==1){temp_sp1<-ci_df_meth}
  if(sp==2){temp_sp2<-rbind(temp_sp1,ci_df_meth)}
  if(sp==3){ci_df_sp<-rbind(temp_sp2,ci_df_meth)}
}
if(u==1){ci1<-ci_df_sp}
if(u==2){ci_df<-rbind(ci1,ci_df_sp)}
}
colnames(ci_df)<-c("CI","Metric","Type","Species","Scenario","Method")
rownames(ci_df)<-NULL

ci_df_upper <-ci_df[which(ci_df$Type=="Upper"),]
ci_df_lower <-ci_df[which(ci_df$Type=="Lower"),]
colnames(ci_df_lower)<-c("Lower CI","Metric","Type","Species","Scenario","Method")
ci_plot <-cbind(ci_df_upper[,1],ci_df_lower[,-3])
colnames(ci_plot)<-c("Upper CI","Lower CI","Metric","Species","Scenario","Method")
ci_plot$Method   <-revalue(ci_plot$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))
# write.csv(ci_plot,"ofl_unc_ci.csv")


### Turn list of bootstrapped SDs and CVs into dataframes for use in Tukey's HSD test
unc<-list();unc_temp<-list();
unc[[1]] <-boot_sd_sp
unc[[2]] <-boot_CVSD_sp
names(unc) <-c("boot_sd_sp","boot_CVSD_sp")
unc_names  <-c("SD OFL","CV OFL (SD)")

for(u in 1:length(unc)){unc_temp<-unc[[u]]
for(sp in 1:length(spec)){
  for(m in 1:length(meth)){
    for(sc in 1:length(scen)){
      bunc <-map_df(unc_temp[[sp]][[m]][[sc]],~as.data.frame(.))
      bunc <-cbind(bunc,rep(unc_names[u],nrow(bunc)),rep(spec[sp],nrow(bunc)),rep(scen[sc],nrow(bunc)),rep(meth[m],nrow(bunc)))
      if(sc==1){temp_sc1 <-bunc}
      if(sc==2){temp_sc2<-rbind(temp_sc1,bunc)}
      if(sc==3){temp_sc3<-rbind(temp_sc2,bunc)}
      if(sc==4){temp_sc4<-rbind(temp_sc3,bunc)}
      if(sc==5){bunc_df_sc<-rbind(temp_sc4,bunc)}
    }
    if(m==1){temp_meth <-bunc_df_sc}
    if(m==2){bunc_df_meth<-rbind(temp_meth,bunc_df_sc)}
  }
  if(sp==1){temp_sp1<-bunc_df_meth}
  if(sp==2){temp_sp2<-rbind(temp_sp1,bunc_df_meth)}
  if(sp==3){bunc_df_sp<-rbind(temp_sp2,bunc_df_meth)}
}
if(u==1){bunc1<-bunc_df_sp}
if(u==2){bunc_df<-rbind(bunc1,bunc_df_sp)}
}
colnames(bunc_df)<-c("Value","Metric","Species","Scenario","Method")
rownames(bunc_df)<-NULL
bunc_df$Method   <-revalue(bunc_df$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))
bunc_df$Scenario   <-revalue(bunc_df$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
write.csv(bunc_df,"C:/Users/matthew.siskey/Desktop/unc_boots.csv")

################################################################################
### Uncertainty of (non-bootstrapped) OFL data & SD log(OFL) calcs for cost-rev
library(boot);library(infer);library(DescTools);library(Rmisc);
rm(list=ls())
setwd("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/otosamp")
spec <-c("Dusky Rockfish","Pacific Ocean Perch","Walleye Pollock")
meth <-c("ototow_otos","ototow_tows")
scen <-c("1 Boat","2 Boat","3 Boat","4 Boat","5 Boat")
method<-c("Otoliths Changed","Tows Changed")
samp_scenario<-c("-67%","-33%","0%","+33%","+67%")
mean_type <-"0_mean" #scen_mean

ofl<-read.csv("ofl.csv",row.names = 1,header=T)
ofl$Method<-revalue(ofl$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))
ofl$Scenario <-revalue(ofl$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))


### SD, MAD, and CV Calculations
if(mean_type=="scen_mean"){
  # ### Mean of OFL
  # ofl_means<-aggregate(ofl[,1],list(ofl$Scenario,ofl$Species,ofl$Method),mean)
  # colnames(ofl_means)<-c("Scenario","Species","Method","Mean")
  # ofl_means$Scenario <-revalue(ofl_means$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
  # ofl_means$Scenario <-factor(ofl_means$Scenario,levels=c("-67%","-33%","0%","+33%","+67%"))
  # write.csv(ofl_means,"ofl_means.csv")
  # # Median of OFL
  # ofl_medians<-aggregate(ofl[,1],list(ofl$Scenario,ofl$Species,ofl$Method),median)
  # colnames(ofl_medians)<-c("Scenario","Species","Method","Median")
  # ofl_medians$Scenario <-revalue(ofl_medians$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
  # ofl_medians$Scenario <-factor(ofl_medians$Scenario,levels=c("-67%","-33%","0%","+33%","+67%"))
  # write.csv(ofl_medians,"ofl_medians.csv")
}

if(mean_type=="0_mean"){
  ofl_0_mean_dr_otos  <-mean(filter(ofl,Species=="Dusky Rockfish" & Method=="Otoliths Changed" & Scenario=="0%")$Value)
  ofl_0_mean_pop_otos <-mean(filter(ofl,Species=="Pacific Ocean Perch" & Method=="Otoliths Changed" & Scenario=="0%")$Value)
  ofl_0_mean_pk_otos  <-mean(filter(ofl,Species=="Walleye Pollock" & Method=="Otoliths Changed" & Scenario=="0%")$Value)
  ofl_0_mean_dr_tows  <-mean(filter(ofl,Species=="Dusky Rockfish" & Method=="Tows Changed" & Scenario=="0%")$Value)
  ofl_0_mean_pop_tows <-mean(filter(ofl,Species=="Pacific Ocean Perch" & Method=="Tows Changed" & Scenario=="0%")$Value)
  ofl_0_mean_pk_tows  <-mean(filter(ofl,Species=="Walleye Pollock" & Method=="Tows Changed" & Scenario=="0%")$Value)
  ofl_0_mean_dr <-list()
  ofl_0_mean_dr[[1]]<-ofl_0_mean_dr_otos
  ofl_0_mean_dr[[2]]<-ofl_0_mean_dr_tows
  names(ofl_0_mean_dr) <-c("Otoliths Changed","Tows Changed")
  ofl_0_mean_pop <-list()
  ofl_0_mean_pop[[1]]<-ofl_0_mean_pop_otos
  ofl_0_mean_pop[[2]]<-ofl_0_mean_pop_tows
  names(ofl_0_mean_pop) <-c("Otoliths Changed","Tows Changed")
  ofl_0_mean_pk <-list()
  ofl_0_mean_pk[[1]]<-ofl_0_mean_pk_otos
  ofl_0_mean_pk[[2]]<-ofl_0_mean_pk_tows
  names(ofl_0_mean_pk) <-c("Otoliths Changed","Tows Changed")
  ofl_0_mean <-list()
  ofl_0_mean[[1]]<-ofl_0_mean_dr
  ofl_0_mean[[2]]<-ofl_0_mean_pop
  ofl_0_mean[[3]]<-ofl_0_mean_pk
  names(ofl_0_mean) <-c("Dusky Rockfish","Pacific Ocean Perch","Walleye Pollock")

  ### Turn 0% means lists into dataframes
  zedmean<-list();
  zedmean <-ofl_0_mean
  scen_loop <-c("0%")

  for(sp in 1:length(spec)){
    for(m in 1:length(meth)){
      zedmean_save <-map_df(zedmean[[sp]][[m]],~as.data.frame(.))
      zedmean_save <-cbind(zedmean_save,spec[sp],scen_loop,meth[m])
      if(m==1){temp_meth <-zedmean_save}
      if(m==2){zedmean_df_meth<-rbind(temp_meth,zedmean_save)}
    }
    if(sp==1){temp_sp1<-zedmean_df_meth}
    if(sp==2){temp_sp2<-rbind(temp_sp1,zedmean_df_meth)}
    if(sp==3){zedmean_df_sp<-rbind(temp_sp2,zedmean_df_meth)}
  }
  colnames(zedmean_df_sp) <-c("Mean","Species","Scenario","Method")
  zedmean_df_sp$Method<-revalue(zedmean_df_sp$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))
}

species_common <-c("Dusky Rockfish","Pacific Ocean Perch","Walleye Pollock") #
SD_list_mean       <-list()
SD_spec_list       <-list()
SD_meth_list       <-list()
###
SD_list_logOFL       <-list()
SD_spec_list_logOFL  <-list()
SD_meth_list_logOFL  <-list()

for(i in 1:length(species_common)){
  if(species_common[i]=="Dusky Rockfish"){ofl_sp <-dplyr::filter(ofl,Species=="Dusky Rockfish"); N_r<-200}
  if(species_common[i]=="Pacific Ocean Perch"){ofl_sp <-dplyr::filter(ofl,Species=="Pacific Ocean Perch"); N_r<-200}
  if(species_common[i]=="Walleye Pollock"){ofl_sp <-dplyr::filter(ofl,Species=="Walleye Pollock"); N_r<-200}
  for(m in 1:length(method)){
    if(method[m]=="Otoliths Changed"){ofl_tmp <-dplyr::filter(ofl_sp,Method=="Otoliths Changed")}
    if(method[m]=="Tows Changed"){ofl_tmp <-dplyr::filter(ofl_sp,Method=="Tows Changed")}
    for(s in 1:length(samp_scenario)){
      if(samp_scenario[s]=="-67%"){ofl_scen <-dplyr::filter(ofl_tmp,Scenario=="-67%")}
      if(samp_scenario[s]=="-33%"){ofl_scen <-dplyr::filter(ofl_tmp,Scenario=="-33%")}
      if(samp_scenario[s]=="0%"){ofl_scen <-dplyr::filter(ofl_tmp,Scenario=="0%")}
      if(samp_scenario[s]=="+33%"){ofl_scen <-dplyr::filter(ofl_tmp,Scenario=="+33%")}
      if(samp_scenario[s]=="+67%"){ofl_scen <-dplyr::filter(ofl_tmp,Scenario=="+67%")}
      if(mean_type=="scen_mean"){
        # SD_mean    = sqrt(1/N_r * sum((ofl_scen$Value - mean(ofl_scen$Value))^2))
        # SD_logOFL  = sqrt(1/N_r * sum((log(ofl_scen$Value) - mean(log(ofl_scen$Value)))^2))
      }
      if(mean_type=="0_mean"){
        SD_mean    = sqrt(1/N_r * sum((ofl_scen$Value - as.numeric(ofl_0_mean[[i]][m]))^2))
        SD_logOFL  = sqrt(1/N_r * sum((log(ofl_scen$Value) - log(as.numeric(ofl_0_mean[[i]][m])))^2))
      }
      SD_list_mean[[s]]    <-SD_mean
      SD_list_logOFL[[s]]  <-SD_logOFL
    }
    names(SD_list_mean)  <-c("-67%","-33%","0%","+33%","+67%")
    SD_meth_list[[m]]    <-SD_list_mean
    ###
    names(SD_list_logOFL)  <-c("-67%","-33%","0%","+33%","+67%")
    SD_meth_list_logOFL[[m]]    <-SD_list_logOFL
  }
  names(SD_meth_list)     <-method
  SD_spec_list[[i]]       <-SD_meth_list
  ###
  names(SD_meth_list_logOFL)  <-method
  SD_spec_list_logOFL[[i]]    <-SD_meth_list_logOFL
}

names(SD_spec_list) <-c("Dusky Rockfish","Pacific Ocean Perch","Walleye Pollock") #
names(SD_spec_list_logOFL) <-c("Dusky Rockfish","Pacific Ocean Perch","Walleye Pollock") #
save(SD_spec_list,file="C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/SD_spec_list")
save(SD_spec_list_logOFL,file="C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/SD_spec_list_logOFL")

### Turn SD lists into dataframes
unc<-list();unc_temp<-list();
unc[[1]] <-SD_spec_list
names(unc) <-c("SD")
unc_names  <-c("SD")
scen_loop <-c("-67%","-33%","0%","+33%","+67%")

for(u in 1:length(unc)){unc_temp<-unc[[u]]
for(sp in 1:length(spec)){
  for(m in 1:length(meth)){
    unc_save <-map_df(unc_temp[[sp]][[m]],~as.data.frame(.))
    unc_save <-cbind(unc_save,rep(unc_names[u],nrow(unc_save)),rep(spec[sp],nrow(unc_save)),scen_loop,rep(meth[m],nrow(unc_save)))
    if(m==1){temp_meth <-unc_save}
    if(m==2){unc_df_meth<-rbind(temp_meth,unc_save)}
  }
  if(sp==1){temp_sp1<-unc_df_meth}
  if(sp==2){temp_sp2<-rbind(temp_sp1,unc_df_meth)}
  if(sp==3){unc_df_sp<-rbind(temp_sp2,unc_df_meth)}
}
if(u==1){SD_df<-unc_df_sp}
}
colnames(SD_df) <-c("Value","Metric","Species","Scenario","Method")
SD_df$Method<-revalue(SD_df$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))


### CV OFL calcs (from SD & mean)
scen_loop <-c("-67%","-33%","0%","+33%","+67%")

if(mean_type=="scen_mean"){
  ofl_means$Method<-revalue(ofl_means$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))
  CVs_SD_otos_dr <-filter(SD_df,Metric=="SD" & Method=="Otoliths Changed" & Species=="Dusky Rockfish")$Value/filter(ofl_means,Method=="Otoliths Changed" & Species=="Dusky Rockfish")$Mean
  CVs_SD_tows_dr <-filter(SD_df,Metric=="SD" & Method=="Tows Changed" & Species=="Dusky Rockfish")$Value/filter(ofl_means,Method=="Tows Changed" & Species=="Dusky Rockfish")$Mean
  CVs_SD_otos_pop <-filter(SD_df,Metric=="SD" & Method=="Otoliths Changed" & Species=="Pacific Ocean Perch")$Value/filter(ofl_means,Method=="Otoliths Changed" & Species=="Pacific Ocean Perch")$Mean
  CVs_SD_tows_pop <-filter(SD_df,Metric=="SD" & Method=="Tows Changed" & Species=="Pacific Ocean Perch")$Value/filter(ofl_means,Method=="Tows Changed" & Species=="Pacific Ocean Perch")$Mean
  CVs_SD_otos_pk <-filter(SD_df,Metric=="SD" & Method=="Otoliths Changed" & Species=="Walleye Pollock")$Value/filter(ofl_means,Method=="Otoliths Changed" & Species=="Walleye Pollock")$Mean
  CVs_SD_tows_pk <-filter(SD_df,Metric=="SD" & Method=="Tows Changed" & Species=="Walleye Pollock")$Value/filter(ofl_means,Method=="Tows Changed" & Species=="Walleye Pollock")$Mean
}
if(mean_type=="0_mean"){
  CVs_SD_otos_dr <-filter(SD_df,Metric=="SD" & Method=="Otoliths Changed" & Species=="Dusky Rockfish")$Value/filter(zedmean_df_sp,Method=="Otoliths Changed" & Species=="Dusky Rockfish")$Mean
  CVs_SD_tows_dr <-filter(SD_df,Metric=="SD" & Method=="Tows Changed" & Species=="Dusky Rockfish")$Value/filter(zedmean_df_sp,Method=="Tows Changed" & Species=="Dusky Rockfish")$Mean
  CVs_SD_otos_pop <-filter(SD_df,Metric=="SD" & Method=="Otoliths Changed" & Species=="Pacific Ocean Perch")$Value/filter(zedmean_df_sp,Method=="Otoliths Changed" & Species=="Pacific Ocean Perch")$Mean
  CVs_SD_tows_pop <-filter(SD_df,Metric=="SD" & Method=="Tows Changed" & Species=="Pacific Ocean Perch")$Value/filter(zedmean_df_sp,Method=="Tows Changed" & Species=="Pacific Ocean Perch")$Mean
  CVs_SD_otos_pk <-filter(SD_df,Metric=="SD" & Method=="Otoliths Changed" & Species=="Walleye Pollock")$Value/filter(zedmean_df_sp,Method=="Otoliths Changed" & Species=="Walleye Pollock")$Mean
  CVs_SD_tows_pk <-filter(SD_df,Metric=="SD" & Method=="Tows Changed" & Species=="Walleye Pollock")$Value/filter(zedmean_df_sp,Method=="Tows Changed" & Species=="Walleye Pollock")$Mean
}

ofl_CVs_SD_otos_dr<-as.data.frame(cbind(CVs_SD_otos_dr,rep("CV (SD)",length(CVs_SD_otos_dr)),rep("Dusky Rockfish", length(CVs_SD_otos_dr)),scen_loop,rep("Otoliths Changed",length(CVs_SD_otos_dr))))
ofl_CVs_SD_tows_dr<-as.data.frame(cbind(CVs_SD_tows_dr,rep("CV (SD)",length(CVs_SD_tows_dr)),rep("Dusky Rockfish", length(CVs_SD_otos_dr)),scen_loop,rep("Tows Changed",length(CVs_SD_otos_dr))))
colnames(ofl_CVs_SD_otos_dr) <-c("Value","Metric","Species","Scenario","Method")
colnames(ofl_CVs_SD_tows_dr) <-c("Value","Metric","Species","Scenario","Method")

ofl_CVs_SD_otos_pop<-as.data.frame(cbind(CVs_SD_otos_pop,rep("CV (SD)",length(CVs_SD_otos_pop)),rep("Pacific Ocean Perch", length(CVs_SD_otos_dr)),scen_loop,rep("Otoliths Changed",length(CVs_SD_otos_dr))))
ofl_CVs_SD_tows_pop<-as.data.frame(cbind(CVs_SD_tows_pop,rep("CV (SD)",length(CVs_SD_tows_pop)),rep("Pacific Ocean Perch", length(CVs_SD_otos_dr)),scen_loop,rep("Tows Changed",length(CVs_SD_otos_dr))))
colnames(ofl_CVs_SD_otos_pop) <-c("Value","Metric","Species","Scenario","Method")
colnames(ofl_CVs_SD_tows_pop) <-c("Value","Metric","Species","Scenario","Method")

ofl_CVs_SD_otos_pk<-as.data.frame(cbind(CVs_SD_otos_pk,rep("CV (SD)",length(CVs_SD_otos_pk)),rep("Walleye Pollock", length(CVs_SD_otos_dr)),scen_loop,rep("Otoliths Changed",length(CVs_SD_otos_dr))))
ofl_CVs_SD_tows_pk<-as.data.frame(cbind(CVs_SD_tows_pk,rep("CV (SD)",length(CVs_SD_tows_pk)),rep("Walleye Pollock", length(CVs_SD_otos_dr)),scen_loop,rep("Tows Changed",length(CVs_SD_otos_dr))))
colnames(ofl_CVs_SD_otos_pk) <-c("Value","Metric","Species","Scenario","Method")
colnames(ofl_CVs_SD_tows_pk) <-c("Value","Metric","Species","Scenario","Method")

ofl_CVs_SD                <-rbind(ofl_CVs_SD_otos_dr,ofl_CVs_SD_tows_dr,ofl_CVs_SD_otos_pop,ofl_CVs_SD_tows_pop,ofl_CVs_SD_otos_pk,ofl_CVs_SD_tows_pk)
ofl_CVs_SD$Value          <-as.numeric(ofl_CVs_SD$Value)
ofl_CVs <-rbind(ofl_CVs_SD)

### Joining SD OFL, CV OFL, and CI dfs for plotting
ofl_unc          <-rbind(SD_df,ofl_CVs)
ofl_unc$Metric   <-revalue(ofl_unc$Metric,c("CV (SD)"="CV OFL","SD"="SD OFL"))
ofl_unc$Scenario <-factor(ofl_unc$Scenario,levels=c("-67%","-33%","0%","+33%","+67%"))
ofl_unc$Metric   <-factor(ofl_unc$Metric,levels=c("SD OFL","CV OFL"))
write.csv(ofl_unc,"ofl_unc.csv")


ofl_unc_ci          <-read.csv("ofl_unc_ci.csv",row.names = 1,header=T)
ofl_unc_ci$Metric   <-revalue(ofl_unc_ci$Metric,c("CV OFL (SD)"="CV OFL"))

ofl_unc             <-read.csv("ofl_unc.csv",row.names = 1,header=T)
unc_ci_OFL          <-dplyr::full_join(ofl_unc,ofl_unc_ci,by=c("Method","Species","Metric","Scenario"))
unc_ci_OFL$Scenario <-factor(unc_ci_OFL$Scenario,levels=c("-67%","-33%","0%","+33%","+67%"))
unc_ci_OFL$Metric   <-factor(unc_ci_OFL$Metric,levels=c("SD OFL","CV OFL"))
write.csv(unc_ci_OFL,"unc_ci_OFL.csv")
