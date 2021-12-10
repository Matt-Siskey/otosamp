###############################################################################################################
################################# NomSS, Tows, and Input SS ###################################################
###############################################################################################################
library(ggplot2); library(dplyr); library(plyr); library(purrr); library(reshape2); library(mgcv); 
library(Rcpp); library(TMB); library(glmmTMB);library(tidyr); library(RODBC); library(data.table)
rm(list=ls())
pathR<-"C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/otosamp/"
method  <-c("ototow_otos","ototow_tows") #
stock   <-"GOA"
samp_scenario <-c("1 Boat","2 Boat","3 Boat","4 Boat","5 Boat")
species <-c("30152","30060","21740") #
spec_boots <-200
exp_boots <-100

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
      # colnames(inputSS)                <-yrs
      # inputSS[,(ncol(inputSS)+1)]      <-seq(1,spec_boots,1)
      # colnames(inputSS)[ncol(inputSS)] <-"Spec_Boot"
      # inputSS     <-reshape2::melt(inputSS,id.vars="Spec_Boot",value.name="InputSS")
      # inputSS     <-arrange(inputSS,Spec_Boot)
      # inputSS[,4] <-rep(paste(species_common,sep=""),nrow(inputSS))
      # inputSS[,5] <-rep(paste(method[m],sep=""),nrow(inputSS))
      # inputSS[,6] <-rep(paste(samp_scenario[alt]),nrow(inputSS))
      # colnames(inputSS) <-c("Spec_Boot","Year","InputSS","Species","Method","Scenario")
      
      for(i in 1:spec_boots){
        if(i==1){counter<-1}
        if(i>1){counter<-counter+nyrs}
        # for(ii in 1:exp_boots){
          # if(ii>1){counter<-counter+nyrs}
          iss[counter:(counter+(nyrs-1)),1] <-yrs
          iss[counter:(counter+(nyrs-1)),2] <-as.numeric(inputSS[[alt]][[i]][1:nyrs])
          iss[counter:(counter+(nyrs-1)),3] <-rep(samp_scenario[alt],nyrs)
          # iss[counter:(counter+(nyrs-1)),4] <-rep(alt,nyrs)
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


################################################################################
# Calc mean/median iss for 0% sampling scenario
iss_0 <-iss[which(iss$Scenario=="3 Boat"),]
iss_0_mean<-aggregate(as.numeric(iss_0[,2]),list(iss_0$Year,iss_0$Species,iss_0$Method),mean)
iss_0_median<-aggregate(as.numeric(iss_0[,2]),list(iss_0$Year,iss_0$Species,iss_0$Method),median)
# write.csv(iss_0_mean,"iss_0_mean.csv")
# write.csv(iss_0_median,"iss_0_median.csv")

# Calc otos per tow
otoPtow <-as.numeric(nomSS$Value)/as.numeric(tows$Value)
otoPtow <-as.data.frame(cbind(nomSS[,1],as.numeric(otoPtow),nomSS[,3],nomSS[,4],nomSS[,5],rep("Oto/Tow",length(nomSS[,4])),nomSS[,7],nomSS[,8]))
otoPtow$V2 <-as.numeric(otoPtow$V2) 
colnames(otoPtow) <-c("Year","Value","Scenario","Spec_Boot","Species","Metric","Exp_Boot","Method")

# Combine nomSS, tows, and iss data frames
dat <-rbind(tows,otoPtow)
# dat <-rbind(nomSS,tows,otoPtow)
dat$Value<-as.numeric(dat$Value)
dat_means<-aggregate(dat[,2],list(dat$Metric,dat$Year,dat$Scenario,dat$Species,dat$Method),mean)
colnames(dat_means) <-c("Metric","Year","Scenario","Species","Method","Value")

iss$Year <-as.character(iss$Year)
iss$Value <-as.numeric(iss$Value)
iss$Spec_Boot <-as.numeric(iss$Spec_Boot)
iss_means<-aggregate(iss[,2],list(iss$Year,iss$Scenario,iss$Species,iss$Method),mean)
colnames(iss_means) <-c("Year","Scenario","Species","Method","InputSS")

iss$Value<-as.numeric(iss$Value)
# iss[which(iss$Value==0),]$Value <-1
# iss[which(iss$Value=="NaN"),]$Value <-1
iss$Spec_Boot<-as.numeric(iss$Spec_Boot)
iss_mean<-aggregate(iss[,2],list(iss$Year,iss$Scenario,iss$Species,iss$Method),mean)
colnames(iss_mean) <-c("Year","Scenario","Species","Method","InputSS")
iss_specmean<-aggregate(iss[,2],list(iss$Spec_Boot,iss$Year,iss$Scenario,iss$Species,iss$Method),mean)
colnames(iss_specmean) <-c("Spec_Boot","Year","Scenario","Species","Method","nomSS")
iss_trtmean<-aggregate(iss[,2],list(iss$Scenario,iss$Species,iss$Method),mean)
colnames(iss_trtmean) <-c("Scenario","Species","Method","InputSS")
write.csv(iss_trtmean,"iss_trtmean.csv")

dat_join<-dplyr::full_join(dat_means,iss_means,by=c("Method","Species","Year","Scenario"))

nomSS$Value<-as.numeric(nomSS$Value)
nomSS$Spec_Boot<-as.numeric(nomSS$Spec_Boot)
nomSS_mean<-aggregate(nomSS[,2],list(nomSS$Year,nomSS$Scenario,nomSS$Species,nomSS$Method),mean)
colnames(nomSS_mean) <-c("Year","Scenario","Species","Method","nomSS")
nomSS_specmean<-aggregate(nomSS[,2],list(nomSS$Spec_Boot,nomSS$Year,nomSS$Scenario,nomSS$Species,nomSS$Method),mean)
colnames(nomSS_specmean) <-c("Spec_Boot","Year","Scenario","Species","Method","nomSS")

tows$Value<-as.numeric(tows$Value)
tows$Spec_Boot<-as.numeric(tows$Spec_Boot)
tows_mean<-aggregate(tows[,2],list(tows$Year,tows$Scenario,tows$Species,tows$Method),mean)
colnames(tows_mean) <-c("Year","Scenario","Species","Method","Tows")
tows_specmean<-aggregate(tows[,2],list(tows$Spec_Boot,tows$Year,tows$Scenario,tows$Species,tows$Method),mean)
colnames(tows_specmean) <-c("Spec_Boot","Year","Scenario","Species","Method","Tows")

# write.csv(nomSS_mean,"nomSS.csv")
# write.csv(tows_mean,"tows.csv")
# write.csv(iss_means,"iss.csv")

dat_join_nomSSspecmean<-dplyr::full_join(nomSS_specmean,iss,by=c("Method","Species","Year","Scenario","Spec_Boot"))
dat_join_towspecmean<-dplyr::full_join(tows_specmean,iss,by=c("Method","Species","Year","Scenario","Spec_Boot"))

################################################################################
### GLMMs for InputSS~Otos and InputSS~Tows
# dat_join$Method   <-revalue(dat_join$Method,c("oto"="Oto","tow"="Tow","ototow_both"="Ototow: Both","ototow_otos"="Ototow: Otos","ototow_tows"="Ototow: Tows"))
dat_join$Method   <-revalue(dat_join$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))
dat_join$Metric   <-revalue(dat_join$Metric,c("Oto/Tow"="Otoliths/Tow"))
dat_join$Scenario <-revalue(dat_join$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
dat_join$Scenario <-factor(dat_join$Scenario,levels=c("+67%","+33%","0%","-33%","-67%"))

dat_join_nomSSspecmean$Method   <-revalue(dat_join_nomSSspecmean$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))
dat_join_nomSSspecmean$Scenario <-revalue(dat_join_nomSSspecmean$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
dat_join_nomSSspecmean$Scenario <-factor(dat_join_nomSSspecmean$Scenario,levels=c("+67%","+33%","0%","-33%","-67%"))
colnames(dat_join_nomSSspecmean) <-c("Spec_Boot","Year","Scenario","Species","Method","nomSS","InputSS","Metric")

dat_join_towspecmean$Method   <-revalue(dat_join_towspecmean$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))
dat_join_towspecmean$Scenario <-revalue(dat_join_towspecmean$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
dat_join_towspecmean$Scenario <-factor(dat_join_towspecmean$Scenario,levels=c("+67%","+33%","0%","-33%","-67%"))
colnames(dat_join_towspecmean) <-c("Spec_Boot","Year","Scenario","Species","Method","Tows","InputSS","Metric")

coefLOGLOG_mm_yr  <-function(x){coef(glmmTMB( log(InputSS) ~ 1 + log(nomSS)*( 1 + log(nomSS) | Year ),data=x))$cond$Year$"log(nomSS)"}
coefLOGLOG_mm_avg <-function(x){glmmTMB( log(InputSS) ~ 1 + log(nomSS)*( 1 + log(nomSS) | Year ),data=x)[["fit"]][["par"]][[2]]}
coefLOGLOG_mm_yr_tow  <-function(x){coef(glmmTMB( log(InputSS) ~ 1 + log(Tows)*( 1 + log(Tows) | Year ),data=x))$cond$Year$"log(Tows)"}
coefLOGLOG_mm_avg_tow <-function(x){glmmTMB( log(InputSS) ~ 1 + log(Tows)*( 1 + log(Tows) | Year ),data=x)[["fit"]][["par"]][[2]]}

# fit<-glmmTMB( log(InputSS) ~ 1 + log(nomSS)*( 1 + log(nomSS) | Year ),data=dat)
# summary(fit)
# mean(coef(fit)$cond$Year$"log(nomSS)")
# re.df<-as.data.frame(ranef(fit))
# fit[["fit"]][["par"]][[2]]
# mean(re.df[which(re.df$term=="log(nomSS)"),]$condval)

####################
### Data structuring
# Data: Year-specific; spec_boot mean otos
dat.dr.Ospecmean  <-dat_join_nomSSspecmean[which(dat_join_nomSSspecmean$Species=="Dusky Rockfish"),]
if(any(dat.dr.Ospecmean$InputSS<1)) {dat.dr.Ospecmean[which(dat.dr.Ospecmean$InputSS<1),]$InputSS<-1}
dat.pop.Ospecmean <-dat_join_nomSSspecmean[which(dat_join_nomSSspecmean$Species=="Pacific Ocean Perch"),]
if(any(dat.pop.Ospecmean$InputSS<1)) {dat.pop.Ospecmean[which(dat.pop.Ospecmean$InputSS<1),]$InputSS<-1}
dat.pk.Ospecmean  <-dat_join_nomSSspecmean[which(dat_join_nomSSspecmean$Species=="Walleye Pollock"),]
if(any(dat.pk.Ospecmean$InputSS<1)) {dat.pk.Ospecmean[which(dat.pk.Ospecmean$InputSS<1),]$InputSS<-1}
dat.Ospecmean     <-rbind(dat.dr.Ospecmean,dat.pop.Ospecmean,dat.pk.Ospecmean)
# Data: Year-specific; spec_boot mean tows
dat.dr.Tspecmean  <-dat_join_towspecmean[which(dat_join_towspecmean$Species=="Dusky Rockfish"),]
if(any(dat.dr.Tspecmean$InputSS<1)) {dat.dr.Tspecmean[which(dat.dr.Tspecmean$InputSS<1),]$InputSS<-1}
dat.pop.Tspecmean <-dat_join_towspecmean[which(dat_join_towspecmean$Species=="Pacific Ocean Perch"),]
if(any(dat.pop.Tspecmean$InputSS<1)) {dat.pop.Tspecmean[which(dat.pop.Tspecmean$InputSS<1),]$InputSS<-1}
dat.pk.Tspecmean  <-dat_join_towspecmean[which(dat_join_towspecmean$Species=="Walleye Pollock"),]
if(any(dat.pk.Tspecmean$InputSS<1)) {dat.pk.Tspecmean[which(dat.pk.Tspecmean$InputSS<1),]$InputSS<-1}
dat.Tspecmean <-rbind(dat.dr.Tspecmean,dat.pop.Tspecmean,dat.pk.Tspecmean)

##################
# Mixed-model w/ variation among year; otos
# Year-specific log-log slopes
dr.spl.Ospecmean_mm  <-with(dat.dr.Ospecmean,split(dat.dr.Ospecmean,list(Species=Species,Method=Method)))
dr.out.Ospecmean_mm  <-unique(dat.dr.Ospecmean[,c("Species","Year")])
dr.out.Ospecmean_mm  <-transform(dr.out.Ospecmean_mm,slope=sapply(dr.spl.Ospecmean_mm,(coefLOGLOG_mm_yr)))
colnames(dr.out.Ospecmean_mm)<-c("Species","Year","Otoliths Changed","Tows Changed")
pop.spl.Ospecmean_mm <-with(dat.pop.Ospecmean,split(dat.pop.Ospecmean,list(Species=Species,Method=Method)))
pop.out.Ospecmean_mm <-unique(dat.pop.Ospecmean[,c("Species","Year")])
pop.out.Ospecmean_mm <-transform(pop.out.Ospecmean_mm,slope=sapply(pop.spl.Ospecmean_mm,coefLOGLOG_mm_yr))
colnames(pop.out.Ospecmean_mm)<-c("Species","Year","Otoliths Changed","Tows Changed")
pk.spl.Ospecmean_mm  <-with(dat.pk.Ospecmean,split(dat.pk.Ospecmean,list(Species=Species,Method=Method)))
pk.out.Ospecmean_mm  <-unique(dat.pk.Ospecmean[,c("Species","Year")])
pk.out.Ospecmean_mm  <-transform(pk.out.Ospecmean_mm,slope=sapply(pk.spl.Ospecmean_mm,coefLOGLOG_mm_yr))
colnames(pk.out.Ospecmean_mm)<-c("Species","Year","Otoliths Changed","Tows Changed")
out.Ospecmean_mm_yr     <-rbind(dr.out.Ospecmean_mm,pop.out.Ospecmean_mm,pk.out.Ospecmean_mm)
# write.csv(out.Ospecmean_mm_yr,"C:/Users/matthew.siskey/Desktop/out.Ospecmean_mm_yr.csv")
# Average log-log slopes
dr.spl.Ospecmean_mm  <-with(dat.dr.Ospecmean,split(dat.dr.Ospecmean,list(Species=Species,Method=Method)))
dr.out.Ospecmean_mm  <-unique(dat.dr.Ospecmean[,c("Species","Method")])
dr.out.Ospecmean_mm  <-transform(dr.out.Ospecmean_mm,slope=sapply(dr.spl.Ospecmean_mm,(coefLOGLOG_mm_avg)))
colnames(dr.out.Ospecmean_mm)<-c("Species","Method","Slope")
pop.spl.Ospecmean_mm <-with(dat.pop.Ospecmean,split(dat.pop.Ospecmean,list(Species=Species,Method=Method)))
pop.out.Ospecmean_mm <-unique(dat.pop.Ospecmean[,c("Species","Method")])
pop.out.Ospecmean_mm <-transform(pop.out.Ospecmean_mm,slope=sapply(pop.spl.Ospecmean_mm,coefLOGLOG_mm_avg))
colnames(pop.out.Ospecmean_mm)<-c("Species","Method","Slope")
pk.spl.Ospecmean_mm  <-with(dat.pk.Ospecmean,split(dat.pk.Ospecmean,list(Species=Species,Method=Method)))
pk.out.Ospecmean_mm  <-unique(dat.pk.Ospecmean[,c("Species","Method")])
pk.out.Ospecmean_mm  <-transform(pk.out.Ospecmean_mm,slope=sapply(pk.spl.Ospecmean_mm,coefLOGLOG_mm_avg))
colnames(pk.out.Ospecmean_mm)<-c("Species","Method","Slope")
out.Ospecmean_mm_avg     <-rbind(dr.out.Ospecmean_mm,pop.out.Ospecmean_mm,pk.out.Ospecmean_mm)
# write.csv(out.Ospecmean_mm_avg,"C:/Users/matthew.siskey/Desktop/out.Ospecmean_mm_avg.csv")


# Mixed-model w/ variation among year; tows
# Year-specific log-log slopes
dr.spl.Tspecmean_mm  <-with(dat.dr.Tspecmean,split(dat.dr.Tspecmean,list(Species=Species,Method=Method)))
dr.out.Tspecmean_mm  <-unique(dat.dr.Tspecmean[,c("Species","Year")])
dr.out.Tspecmean_mm  <-transform(dr.out.Tspecmean_mm,slope=sapply(dr.spl.Tspecmean_mm,(coefLOGLOG_mm_yr_tow)))
colnames(dr.out.Tspecmean_mm)<-c("Species","Year","Otoliths Changed","Tows Changed")
pop.spl.Tspecmean_mm <-with(dat.pop.Tspecmean,split(dat.pop.Tspecmean,list(Species=Species,Method=Method)))
pop.out.Tspecmean_mm <-unique(dat.pop.Tspecmean[,c("Species","Year")])
pop.out.Tspecmean_mm <-transform(pop.out.Tspecmean_mm,slope=sapply(pop.spl.Tspecmean_mm,coefLOGLOG_mm_yr_tow))
colnames(pop.out.Tspecmean_mm)<-c("Species","Year","Otoliths Changed","Tows Changed")
pk.spl.Tspecmean_mm  <-with(dat.pk.Tspecmean,split(dat.pk.Tspecmean,list(Species=Species,Method=Method)))
pk.out.Tspecmean_mm  <-unique(dat.pk.Tspecmean[,c("Species","Year")])
pk.out.Tspecmean_mm  <-transform(pk.out.Tspecmean_mm,slope=sapply(pk.spl.Tspecmean_mm,coefLOGLOG_mm_yr_tow))
colnames(pk.out.Tspecmean_mm)<-c("Species","Year","Otoliths Changed","Tows Changed")
out.Tspecmean_mm_yr     <-rbind(dr.out.Tspecmean_mm,pop.out.Tspecmean_mm,pk.out.Tspecmean_mm)
# write.csv(out.Tspecmean_mm_yr,"C:/Users/matthew.siskey/Desktop/out.Tspecmean_mm_yr.csv")
# Average log-log slopes
dr.spl.Tspecmean_mm  <-with(dat.dr.Tspecmean,split(dat.dr.Tspecmean,list(Species=Species,Method=Method)))
dr.out.Tspecmean_mm  <-unique(dat.dr.Tspecmean[,c("Species","Method")])
dr.out.Tspecmean_mm  <-transform(dr.out.Tspecmean_mm,slope=sapply(dr.spl.Tspecmean_mm,(coefLOGLOG_mm_avg_tow)))
colnames(dr.out.Tspecmean_mm)<-c("Species","Method","Slope")
pop.spl.Tspecmean_mm <-with(dat.pop.Tspecmean,split(dat.pop.Tspecmean,list(Species=Species,Method=Method)))
pop.out.Tspecmean_mm <-unique(dat.pop.Tspecmean[,c("Species","Method")])
pop.out.Tspecmean_mm <-transform(pop.out.Tspecmean_mm,slope=sapply(pop.spl.Tspecmean_mm,coefLOGLOG_mm_avg_tow))
colnames(pop.out.Tspecmean_mm)<-c("Species","Method","Slope")
pk.spl.Tspecmean_mm  <-with(dat.pk.Tspecmean,split(dat.pk.Tspecmean,list(Species=Species,Method=Method)))
pk.out.Tspecmean_mm  <-unique(dat.pk.Tspecmean[,c("Species","Method")])
pk.out.Tspecmean_mm  <-transform(pk.out.Tspecmean_mm,slope=sapply(pk.spl.Tspecmean_mm,coefLOGLOG_mm_avg_tow))
colnames(pk.out.Tspecmean_mm)<-c("Species","Method","Slope")
out.Tspecmean_mm_avg     <-rbind(dr.out.Tspecmean_mm,pop.out.Tspecmean_mm,pk.out.Tspecmean_mm)
# write.csv(out.Tspecmean_mm_avg,"C:/Users/matthew.siskey/Desktop/out.Tspecmean_mm_avg.csv")

# setwd("C:/Users/matthew.siskey/Desktop/")
# out.Ospecmean_mm_yr <-read.csv("out.Ospecmean_mm_yr.csv",header=T,row.names = 1)
# out.Tspecmean_mm_yr <-read.csv("out.Tspecmean_mm_yr.csv",header=T,row.names = 1)
# colnames(out.Ospecmean_mm_yr)<-c("Species","Year","Otoliths Changed","Tows Changed")
# colnames(out.Tspecmean_mm_yr)<-c("Species","Year","Otoliths Changed","Tows Changed")
# out.Ospecmean_mm_avg <-read.csv("out.Ospecmean_mm_avg.csv")
# out.Tspecmean_mm_avg <-read.csv("out.Tspecmean_mm_avg.csv")
out.Ospecmean_mm_yr_melt<-reshape2::melt(out.Ospecmean_mm_yr,id.vars=c("Species","Year"),measure.vars=c("Otoliths Changed","Tows Changed"))
out.Ospecmean_mm_yr_melt$variable<-as.character(out.Ospecmean_mm_yr_melt$variable)
out.Ospecmean_mm_yr_melt[,5]<-rep("NomSS",nrow(out.Ospecmean_mm_yr_melt))
colnames(out.Ospecmean_mm_yr_melt)<-c("Species","Year","Method","Value","Type")
out.Tspecmean_mm_yr_melt<-reshape2::melt(out.Tspecmean_mm_yr,id.vars=c("Species","Year"),measure.vars=c("Otoliths Changed","Tows Changed"))
out.Tspecmean_mm_yr_melt$variable<-as.character(out.Tspecmean_mm_yr_melt$variable)
out.Tspecmean_mm_yr_melt[,5]<-rep("Tows",nrow(out.Tspecmean_mm_yr_melt))
colnames(out.Tspecmean_mm_yr_melt)<-c("Species","Year","Method","Value","Type")

loglogslopes_yr <-rbind(out.Ospecmean_mm_yr_melt,out.Tspecmean_mm_yr_melt)

out.Ospecmean_mm_avg[,4]<-rep("NomSS",nrow(out.Ospecmean_mm_avg))
colnames(out.Ospecmean_mm_avg)<-c("Species","Method","Value","Type")
out.Tspecmean_mm_avg[,4]<-rep("Tows",nrow(out.Tspecmean_mm_avg))
colnames(out.Tspecmean_mm_avg)<-c("Species","Method","Value","Type")

loglogslopes_avg <-rbind(out.Ospecmean_mm_avg,out.Tspecmean_mm_avg)
write.csv(loglogslopes_avg,"loglogslopes_avg.csv")

################################################################################
### OFL
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
    pathR   <-paste("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/AMAK_saves_",method[m],sep="")
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
# write.csv(ofl,"ofl.csv")

################################################################################
### Recruitment
tmp_meth_rec<-list();tmp_spec_rec<-list()

for(s in 1:length(species)){
  if(species[s]=="30152"){
    species_common <-"Dusky Rockfish"
    # nyrs<-44
    # yrs<-c("1984","1987","1990","1993","1996","1999","2001","2003","2005","2007","2009","2011","2013","2015","2017","2019")
  }
  if(species[s]=="30060"){
    species_common <-"Pacific Ocean Perch"
    # nyrs<-60
    # yrs<-c("1990","1993","1996","1999","2003","2005","2007","2009","2011","2013","2015","2017","2019")
  }
  if(species[s]=="21740"){
    species_common <-"Walleye Pollock"
    # nyrs<-14
    # yrs<-c("1990","1993","1996","1999","2001","2003","2005","2007","2009","2011","2013","2015","2017","2019")
  }
  for(m in 1:length(method)){
    pathR   <-paste("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/AMAK_saves_",method[m],sep="")
    load(paste(pathR,"/pred_rec_",species[s],"_",stock,"_",method[m],sep=""))
    AMAK_rec <-matrix(NA,ncol=6,nrow=250)
    
    for(i in 1:length(samp_scenario)){
      if(i==1){counter=0}
      if(i==2){counter=50}
      if(i==3){counter=100}
      if(i==4){counter=150}
      if(i==5){counter=200}
      
      for(ii in 1:50){
        counter=counter+1
        # AMAK_rec[counter,1] <-mean(pred_rec_top[[i]][[ii]])
        if(species[s]=="30152"){AMAK_rec[counter,1] <-pred_rec_top[[i]][[ii]][length((pred_rec_top[[i]][[ii]]))-5]}
        if(species[s]=="30060"){AMAK_rec[counter,1] <-pred_rec_top[[i]][[ii]][length((pred_rec_top[[i]][[ii]]))-3]}
        if(species[s]=="21740"){AMAK_rec[counter,1] <-pred_rec_top[[i]][[ii]][length((pred_rec_top[[i]][[ii]]))-2]}
        AMAK_rec[counter,2] <-paste(samp_scenario[i])
        AMAK_rec[counter,3] <-paste(species_common)
        AMAK_rec[counter,4] <-"Rec"
        AMAK_rec[counter,5] <-ii
        AMAK_rec[counter,6] <-method[m]
      }
    }
    tmp_meth_rec[[m]] <-AMAK_rec
  }
  names(tmp_meth_rec) <-method
  tmp_spec_rec[[s]]   <-tmp_meth_rec
}

names(tmp_spec_rec)<-species

rec_dr  <-map_df(tmp_spec_rec[[1]],~as.data.frame(.))
rec_pop <-map_df(tmp_spec_rec[[2]],~as.data.frame(.))
rec_pk  <-map_df(tmp_spec_rec[[3]],~as.data.frame(.))

colnames(rec_dr) <-c("Value","Scenario","Species","Metric","Boot","Method")
rec_dr$Value<-as.numeric(rec_dr$Value)
colnames(rec_pop) <-c("Value","Scenario","Species","Metric","Boot","Method")
rec_pop$Value<-as.numeric(rec_pop$Value)
colnames(rec_pk) <-c("Value","Scenario","Species","Metric","Boot","Method")
rec_pk$Value<-as.numeric(rec_pk$Value)

rec <-rbind(rec_dr,rec_pop,rec_pk) # 


################################################################################
### SE Recruitment
tmp_meth_se.rec<-list();tmp_spec_se.rec<-list()

for(s in 1:length(species)){
  if(species[s]=="30152"){
    species_common <-"Dusky Rockfish"
    nyrs<-44
    # yrs<-c("1984","1987","1990","1993","1996","1999","2001","2003","2005","2007","2009","2011","2013","2015","2017","2019")
  }
  if(species[s]=="30060"){
    species_common <-"Pacific Ocean Perch"
    nyrs<-60
    # yrs<-c("1990","1993","1996","1999","2003","2005","2007","2009","2011","2013","2015","2017","2019")
  }
  if(species[s]=="21740"){
    species_common <-"Walleye Pollock"
    nyrs<-14
    # yrs<-c("1990","1993","1996","1999","2001","2003","2005","2007","2009","2011","2013","2015","2017","2019")
  }
  for(m in 1:length(method)){
    pathR   <-paste("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/AMAK_saves_",method[m],sep="")
    load(paste(pathR,"/se.pred_rec_",species[s],"_",stock,"_",method[m],sep=""))
    AMAK_se.rec <-matrix(NA,ncol=6,nrow=250)
    
    for(i in 1:length(samp_scenario)){
      if(i==1){counter=0}
      if(i==2){counter=50}
      if(i==3){counter=100}
      if(i==4){counter=150}
      if(i==5){counter=200}
      
      for(ii in 1:50){
        counter=counter+1
        AMAK_se.rec[counter,1] <-se.pred_rec_top[[i]][[ii]]
        AMAK_se.rec[counter,2] <-paste(samp_scenario[i])
        AMAK_se.rec[counter,3] <-paste(species_common)
        AMAK_se.rec[counter,4] <-"SE Rec"
        AMAK_se.rec[counter,5] <-ii
        AMAK_se.rec[counter,6] <-method[m]
      }
    }
    tmp_meth_se.rec[[m]] <-AMAK_se.rec
  }
  names(tmp_meth_se.rec) <-method
  tmp_spec_se.rec[[s]]   <-tmp_meth_se.rec
}

names(tmp_spec_se.rec)<-species


################################################################################
### Log Theta
pathR   <-"C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/AMAK_saves_ototow_otos"
# pathR   <-"C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/AMAK_saves_ototow_tows"
# pathR   <-"C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/AMAK_saves_ototow_tows+otos"
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
  # if(species[s]=="21740"){
  #   species_common <-"Walleye Pollock"
  #   nyrs<-14
  #   yrs<-c("1990","1993","1996","1999","2001","2003","2005","2007","2009","2011","2013","2015","2017","2019")
  # }
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
# LOGtheta_pk  <-map_df(tmp_spec_LOGtheta[[3]],~as.data.frame(.))

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
    pathR   <-paste("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/AMAK_saves_",method[m],sep="")
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
# comp_pk  <-map_df(tmp_spec_comp[[3]],~as.data.frame(.))

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
ofl <-read.csv("ofl.csv",row.names = 1,header=T)
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
# boot_mad<-list(); boot_CVMAD<-list();boot_mad_sc<-list(); boot_CVMAD_sc<-list();boot_CVMAD_meth<-list();boot_mad_meth<-list();
# boot_mad_sp<-list(); boot_CVMAD_sp<-list();ci_CVMAD_sc<-list();ci_mad_sc<-list(); ci_CVMAD_meth<-list();ci_mad_meth<-list();
# boot_mad_log<-list();boot_mad_log_sc<-list();boot_mad_log_meth<-list();boot_mad_log_sp<-list();ci_CVMAD_sp<-list();ci_mad_sp<-list();
dat <-ofl
N_r <-200

for(sp in 1:length(spec)){
  dat_sp<-dat[which(dat$Species==spec[sp]),]
  for(m in 1:length(meth)){
    dat_m<-dat_sp[which(dat_sp$Method==meth[m]),]
    for(sc in 1:length(scen)){
      dat_sc<-dat_m[which(dat_m$Scenario==scen[sc]),]
      for(i in 1:1000){
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
save(boot_sd_sp,file="C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/ofl_boot_sd_sp_0mean")
save(boot_sd_log_sp,file="C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/ofl_boot_sd_log_sp_0mean")

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
write.csv(ci_plot,"ofl_unc_ci.csv")


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
write.csv(bunc_df,"unc_boots.csv")


### Run Levene's test on OFL distributions to compare variances
library(car)
ofl<-read.csv("ofl.csv",row.names = 1,header=T)

dat1<-filter(ofl,Species=="Dusky Rockfish",Method=="ototow_otos",Scenario=="1 Boat" | Scenario=="2 Boat")
leveneTest(Value~factor(Scenario),data=dat1)
# fligner.test(Value~factor(Scenario),data=dat1)
dat2<-filter(ofl,Species=="Dusky Rockfish",Method=="ototow_otos",Scenario=="2 Boat" | Scenario=="3 Boat")
leveneTest(Value~factor(Scenario),data=dat2)
# fligner.test(Value~factor(Scenario),data=dat2)
dat3<-filter(ofl,Species=="Dusky Rockfish",Method=="ototow_otos",Scenario=="3 Boat" | Scenario=="4 Boat")
leveneTest(Value~factor(Scenario),data=dat3)
# fligner.test(Value~factor(Scenario),data=dat3)
dat4<-filter(ofl,Species=="Dusky Rockfish",Method=="ototow_otos",Scenario=="4 Boat" | Scenario=="5 Boat")
leveneTest(Value~factor(Scenario),data=dat4)
# fligner.test(Value~factor(Scenario),data=dat4)

dat1<-filter(ofl,Species=="Dusky Rockfish",Method=="ototow_tows",Scenario=="1 Boat" | Scenario=="2 Boat")
leveneTest(Value~factor(Scenario),data=dat1)
# fligner.test(Value~factor(Scenario),data=dat1)
dat2<-filter(ofl,Species=="Dusky Rockfish",Method=="ototow_tows",Scenario=="2 Boat" | Scenario=="3 Boat")
leveneTest(Value~factor(Scenario),data=dat2)
# fligner.test(Value~factor(Scenario),data=dat2)
dat3<-filter(ofl,Species=="Dusky Rockfish",Method=="ototow_tows",Scenario=="3 Boat" | Scenario=="4 Boat")
leveneTest(Value~factor(Scenario),data=dat3)
# fligner.test(Value~factor(Scenario),data=dat3)
dat4<-filter(ofl,Species=="Dusky Rockfish",Method=="ototow_tows",Scenario=="4 Boat" | Scenario=="5 Boat")
leveneTest(Value~factor(Scenario),data=dat4)
# fligner.test(Value~factor(Scenario),data=dat4)


dat1<-filter(ofl,Species=="Pacific Ocean Perch",Method=="ototow_otos",Scenario=="1 Boat" | Scenario=="2 Boat")
leveneTest(Value~factor(Scenario),data=dat1)
# fligner.test(Value~factor(Scenario),data=dat1)
dat2<-filter(ofl,Species=="Pacific Ocean Perch",Method=="ototow_otos",Scenario=="2 Boat" | Scenario=="3 Boat")
leveneTest(Value~factor(Scenario),data=dat2)
# fligner.test(Value~factor(Scenario),data=dat2)
dat3<-filter(ofl,Species=="Pacific Ocean Perch",Method=="ototow_otos",Scenario=="3 Boat" | Scenario=="4 Boat")
leveneTest(Value~factor(Scenario),data=dat3)
# fligner.test(Value~factor(Scenario),data=dat3)
dat4<-filter(ofl,Species=="Pacific Ocean Perch",Method=="ototow_otos",Scenario=="4 Boat" | Scenario=="5 Boat")
leveneTest(Value~factor(Scenario),data=dat4)
# fligner.test(Value~factor(Scenario),data=dat4)

dat1<-filter(ofl,Species=="Pacific Ocean Perch",Method=="ototow_tows",Scenario=="1 Boat" | Scenario=="2 Boat")
leveneTest(Value~factor(Scenario),data=dat1)
# fligner.test(Value~factor(Scenario),data=dat1)
dat2<-filter(ofl,Species=="Pacific Ocean Perch",Method=="ototow_tows",Scenario=="2 Boat" | Scenario=="3 Boat")
leveneTest(Value~factor(Scenario),data=dat2)
# fligner.test(Value~factor(Scenario),data=dat2)
dat3<-filter(ofl,Species=="Pacific Ocean Perch",Method=="ototow_tows",Scenario=="3 Boat" | Scenario=="4 Boat")
leveneTest(Value~factor(Scenario),data=dat3)
# fligner.test(Value~factor(Scenario),data=dat3)
dat4<-filter(ofl,Species=="Pacific Ocean Perch",Method=="ototow_tows",Scenario=="4 Boat" | Scenario=="5 Boat")
leveneTest(Value~factor(Scenario),data=dat4)
# fligner.test(Value~factor(Scenario),data=dat4)


dat1<-filter(ofl,Species=="Walleye Pollock",Method=="ototow_otos",Scenario=="1 Boat" | Scenario=="2 Boat")
leveneTest(Value~factor(Scenario),data=dat1)
# fligner.test(Value~factor(Scenario),data=dat1)
dat2<-filter(ofl,Species=="Walleye Pollock",Method=="ototow_otos",Scenario=="2 Boat" | Scenario=="3 Boat")
leveneTest(Value~factor(Scenario),data=dat2)
# fligner.test(Value~factor(Scenario),data=dat2)
dat3<-filter(ofl,Species=="Walleye Pollock",Method=="ototow_otos",Scenario=="3 Boat" | Scenario=="4 Boat")
leveneTest(Value~factor(Scenario),data=dat3)
# fligner.test(Value~factor(Scenario),data=dat3)
dat4<-filter(ofl,Species=="Walleye Pollock",Method=="ototow_otos",Scenario=="4 Boat" | Scenario=="5 Boat")
leveneTest(Value~factor(Scenario),data=dat4)
# fligner.test(Value~factor(Scenario),data=dat4)

dat1<-filter(ofl,Species=="Walleye Pollock",Method=="ototow_tows",Scenario=="1 Boat" | Scenario=="2 Boat")
leveneTest(Value~factor(Scenario),data=dat1)
# fligner.test(Value~factor(Scenario),data=dat1)
dat2<-filter(ofl,Species=="Walleye Pollock",Method=="ototow_tows",Scenario=="2 Boat" | Scenario=="3 Boat")
leveneTest(Value~factor(Scenario),data=dat2)
# fligner.test(Value~factor(Scenario),data=dat2)
dat3<-filter(ofl,Species=="Walleye Pollock",Method=="ototow_tows",Scenario=="3 Boat" | Scenario=="4 Boat")
leveneTest(Value~factor(Scenario),data=dat3)
# fligner.test(Value~factor(Scenario),data=dat3)
dat4<-filter(ofl,Species=="Walleye Pollock",Method=="ototow_tows",Scenario=="4 Boat" | Scenario=="5 Boat")
leveneTest(Value~factor(Scenario),data=dat4)
# fligner.test(Value~factor(Scenario),data=dat4)



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
      zedmean_save <-cbind(zedmean_save,spec[sp],scen_loop,meth[m]) #rep(unc_names[u],nrow(unc_save)),
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


###############################################################################################################
################################################ Plotting #####################################################
###############################################################################################################

################################################################################
#### OFL Distributions
rm(list=ls())
setwd("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/otosamp/")

ofl<-read.csv("ofl.csv",row.names=1,header=T)
ofl$Scenario <-revalue(ofl$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
ofl$Scenario <-factor(ofl$Scenario,levels=c("+67%","+33%","0%","-33%","-67%"))
ofl$Method <-revalue(ofl$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))

# mu <-ddply(AMAK_output,c("Metric","Scenario","Species"),summarize,grp.mean=mean(Value))
# mu <-ddply(ofl,c("Method","Metric","Scenario","Species"),summarize,grp.mean=mean(Value))
# mu_0 <-mu[which(mu$Scenario=="0%"),]
# colnames(mu_0)<-c("Method","Metric","Scenario","Species","OFL")
# med <-ddply(ofl,c("Method","Metric","Scenario","Species"),summarize,grp.median=median(Value))
# med_0 <-med[which(med$Scenario=="0%"),]
# colnames(med_0)<-c("Method","Metric","Scenario","Species","OFL")

### Otos changed method
ofl_dr_black_otos  <-8784.8 # median 0% iss; OG comps; multi
ofl_pop_black_otos <-35673 # median 0% iss; OG comps; multi
ofl_pk_black_otos  <-126620 # 1st yr proj = 113710 # median 0% iss; OG comps; multi; 2nd proj yr = 108900

# Tows changed method
ofl_dr_black_tows  <-8784.8 # median 0% iss; OG comps; multi
ofl_pop_black_tows <-35673 # median 0% iss; OG comps; multi
ofl_pk_black_tows  <-126620 # 1st yr proj = 110510 # median 0% iss; OG comps; multi; 2nd proj yr = 104800

species_common <-c("Dusky Rockfish","Pacific Ocean Perch","Walleye Pollock") #
ofl_black_otos<-as.data.frame(cbind(rep("OFL",length(species_common)),rep("Black",length(species_common)),rep("Otoliths Changed",length(species_common)),
                                    species_common,rbind(ofl_dr_black_otos,ofl_pop_black_otos,ofl_pk_black_otos)))
ofl_black_tows<-as.data.frame(cbind(rep("OFL",length(species_common)),rep("Black",length(species_common)),rep("Tows Changed",length(species_common)),
                                    species_common,rbind(ofl_dr_black_tows,ofl_pop_black_tows,ofl_pk_black_tows)))
ofl_black <-rbind(ofl_black_otos,ofl_black_tows)
colnames(ofl_black)<-c("Metric","Type","Method","Species","OFL")
ofl_black$OFL<-as.numeric(ofl_black$OFL)
rownames(ofl_black)<-NULL
ofl_black$Metric<-as.factor(ofl_black$Metric)


### Otos changed method
# ofl_dr_grey_otos  <-8701.5 # median 0% iss; OG comps; multi
# ofl_pop_grey_otos <-35673 # median 0% iss; OG comps; multi
# 
# # Tows changed method
# ofl_dr_grey_tows  <-8701.5 # median 0% iss; OG comps; multi
# ofl_pop_grey_tows <-35673 # median 0% iss; OG comps; multi

# species_common <-c("Dusky Rockfish","Pacific Ocean Perch") #
# ofl_grey_otos<-as.data.frame(cbind(rep("OFL",length(species_common)),rep("Grey",length(species_common)),rep("Otoliths Changed",length(species_common)),
#                                     species_common,rbind(ofl_dr_grey_otos,ofl_pop_grey_otos)))
# ofl_grey_tows<-as.data.frame(cbind(rep("OFL",length(species_common)),rep("Grey",length(species_common)),rep("Tows Changed",length(species_common)),
#                                     species_common,rbind(ofl_dr_grey_tows,ofl_pop_grey_tows)))
# ofl_grey <-rbind(ofl_grey_otos,ofl_grey_tows)
# colnames(ofl_grey)<-c("Metric","Type","Method","Species","OFL")
# ofl_grey$OFL<-as.numeric(ofl_grey$OFL)
# rownames(ofl_grey)<-NULL
# ofl_grey$Metric<-as.factor(ofl_grey$Metric)


zero_comma <- function (x, ...) {
  ifelse(x==0,"0",x)
  format(abs(x), ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

colnames(ofl)<-c("Value","Treatment","Species","Metric","Boot","Method")

ggplot()+
  geom_density(data=ofl,aes(Value,..scaled..,fill=Treatment),color="black",alpha=0.5)+
  # geom_vline(data=mu,aes(xintercept=grp.mean,color=Scenario),linetype="dashed",size=1.2)+
  geom_vline(data=ofl_black,aes(xintercept=OFL),color="black",linetype="dashed",size=1.2)+
  # geom_vline(data=ofl_grey,aes(xintercept=OFL),color="grey",linetype="dashed",size=1.2)+
  # geom_vline(data=med_0,aes(xintercept=ABC),color="springgreen1",linetype="dashed",size=1.2)+
  facet_grid(Method~Species,scales="free")+
  # facet_wrap(Method~Species,scales="free",ncol=3)+
  ylab("\n Scaled Density \n")+
  xlab("\n Value \n")+
  theme_bw()+
  # scale_x_continuous(labels=scales::comma)+
  scale_x_continuous(labels=zero_comma)+
  # guides(fill = guide_legend(override.aes = list(size=5),order=1))+
  theme(legend.position = "right",
        axis.title = element_text(face="bold", size = 24),
        axis.text = element_text(color="black",size=14),
        # axis.text.x = element_text(color="black",size=18),
        axis.ticks=element_line(color="black"),
        plot.title = element_text(hjust = 0.5, face="bold", size = 24),
        plot.margin=unit(c(0.5,0.5,0.1,0.1),"cm"),
        plot.background=element_rect(fill="white"),
        legend.text = element_text(size=18),
        legend.title = element_text(size=25, face = "bold"),
        strip.text = element_text(size=18, vjust=0.75),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_line(color="white"),
        # panel.grid.major=element_line(color="gray"),
        panel.spacing.x = unit(5, "mm"),
        panel.spacing.y = unit(5, "mm"))


ggsave("Fig_ofl_density.jpeg", plot=last_plot(), device = "jpeg",
       path = 'C:/Users/matthew.siskey/Desktop',
       width=20, height=12, units="in", dpi=1200)


################################################################################
### OFL Uncertainties & CIs
library(ggh4x);

colnames(unc_ci_OFL) <-c("Value","Metric","Species","Treatment","Method","Upper.CI","Lower.CI")

ggplot(data=unc_ci_OFL,aes(x=Treatment,y=Value,group=Method,fill=Method))+
  # geom_bar(color="black",fill="gray50",stat="identity")+
  geom_line(aes(color=Method),size=1.2,show.legend=FALSE)+
  geom_point(pch=21,size=4,alpha=0.5)+
  geom_errorbar(aes(ymin=`Lower.CI`,ymax=`Upper.CI`,color=Method),width=0.2,size=0.75,show.legend=FALSE)+
  # facet_wrap(Type~Species,ncol=3,scales="free_y",strip.position= "top")+
  # facet_grid(Species~Type,scales="free")+
  facet_grid2(Metric~Species,scales="free_y",independent="y")+
  expand_limits(y=0)+
  ylab("\n Metric \n")+
  xlab("\n Treatment \n")+
  theme_bw()+
  scale_color_manual(values=c("black","grey60"))+
  scale_fill_manual(values=c("black","grey60"))+
  # scale_fill_manual(values=c("black","red","grey75","grey50"))+
  # scale_color_manual(values=c("black","red","grey75","grey50"))+
  # scale_x_continuous(labels=function(x) ifelse(x==0,"0",x))+ #scales::comma
  # scale_x_continuous(labels=scales::comma)+
  # scale_x_continuous(labels=zero_comma)+
  guides(fill = guide_legend(override.aes = list(size=5),order=1))+
  theme(legend.position = "right",
        axis.title = element_text(face="bold", size = 24),
        axis.text = element_text(color="black",size=14),
        # axis.text.x = element_text(color="black",size=18),
        axis.ticks=element_line(color="black"),
        plot.title = element_text(hjust = 0.5, face="bold", size = 24),
        plot.margin=unit(c(0.5,0.5,0.1,0.1),"cm"),
        plot.background=element_rect(fill="white"),
        legend.text = element_text(size=18),
        legend.title = element_text(size=25, face = "bold"),
        strip.text = element_text(size=18, vjust=0.75, face = "bold"),
        # strip.text.y = element_blank(),
        # strip.placement = "inside",
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_line(color="white"),
        panel.grid.major=element_line(color="white"),
        panel.spacing.x = unit(5, "mm"),
        panel.spacing.y = unit(5, "mm"))

ggsave("Figure3_OFL_unc_0mean.jpeg", plot=last_plot(), device = "jpeg",
       path = 'C:/Users/matthew.siskey/Desktop',
       width=15, height=7, units="in", dpi=1200)


################################################################################
### GLMM slopes and Notable Year non-log plot
# LLslopes<-read.csv("C:/Users/matthew.siskey/Desktop/loglogslopes.csv")
# LLslopes$Year<-as.character(LLslopes$Year)
loglogslopes$Year<-as.character(loglogslopes$Year)
loglogslopes_otos <-dplyr::filter(loglogslopes,Type=="NomSS" & Method=="Otoliths Changed")
loglogslopes_tows <-dplyr::filter(loglogslopes,Type=="Tows" & Method=="Tows Changed")
loglogslopes_plot <-rbind(loglogslopes_otos,loglogslopes_tows)

StatBin2 <- ggproto(
  "StatBin2", StatBin, compute_group = function (data, scales, binwidth = NULL, bins = NULL, center = NULL, boundary = NULL, 
        closed = c("right", "left"), pad = FALSE, breaks = NULL, origin = NULL, right = NULL, drop = NULL, width = NULL) {
    if (!is.null(breaks)) {if (!scales$x$is_discrete()) {breaks <- scales$x$transform(breaks)}
      bins <- ggplot2:::bin_breaks(breaks, closed)}
    else if (!is.null(binwidth)) {if (is.function(binwidth)) {binwidth <- binwidth(data$x)}
      bins <- ggplot2:::bin_breaks_width(scales$x$dimension(), binwidth, center = center, boundary = boundary, closed = closed)}
    else {bins <- ggplot2:::bin_breaks_bins(scales$x$dimension(), bins, center = center, boundary = boundary, closed = closed)}
    res <- ggplot2:::bin_vector(data$x, bins, weight = data$weight, pad = pad)
    # drop 0-count bins completely before returning the dataframe
    res <- res[res$count > 0, ] 
    res
  })

ggplot(data=loglogslopes_plot,aes(Value,fill=Type))+
  # geom_bar()+
  geom_histogram(stat=StatBin2,position="identity",binwidth=0.1,alpha=0.9,color="black")+
  # geom_density(alpha=0.7)+
  facet_grid(Species~Method)+
  # scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  # scale_x_discrete(labels=c("1984","","1990","","1996","","2001","","2005","","2009","","2013","","2017",""))+
  ylab("\n  Count of Annual Log-Log Slopes \n")+
  # xlab("\n Year \n")+
  scale_fill_manual(values=c("black","grey60"))+
  theme_bw()+
  guides(fill = guide_legend(override.aes = list(shape=21,size=5)),
         shape= guide_legend(override.aes = list(size=5)))+
  theme(legend.position = "none",
        legend.box = "vertical",
        legend.direction = "vertical",
        axis.title = element_text(face="bold", size = 20),
        axis.text = element_text(color="black",size=22),
        axis.text.x = element_text(color="black",size=18),
        axis.ticks=element_line(color="black"),
        plot.title = element_text(hjust = 0.5, face="bold", size = 24),
        plot.margin=unit(c(0.5,0.5,0.1,0.1),"cm"),
        plot.background=element_rect(fill="white"),
        legend.text = element_text(size=18, face="bold"),
        legend.title = element_text(size=25, face = "bold"),
        strip.text.x = element_text(size=18, vjust=0.75, face = "bold"),
        strip.text.y = element_text(size=18, vjust=0.75, face = "bold"),
        # strip.background = element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_line(color="white"),
        panel.grid.major=element_line(color="white"),
        panel.spacing.x = unit(5, "mm"),
        panel.spacing.y = unit(5, "mm"))

ggsave("FigX_loglogslopes_hist.jpeg", plot=last_plot(), device = "jpeg",
       path = 'C:/Users/matthew.siskey/Desktop',
       width=10, height=10, units="in", dpi=1000)


################################################################################
### Single Year ISS~Otos/Tow; ISS~Tows
dat_join$Scenario <-revalue(dat_join$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
dat_join$Method <-revalue(dat_join$Method,c("ototow_otos"="Otoliths Changed","ototow_tows"="Tows Changed"))
dat_join$Metric <-revalue(dat_join$Metric,c("Oto/Tow"="Otoliths/Tow"))
dat.trim<-dplyr::filter(dat_join,Year=="1993"|Year=="2007"|Year=="2019")
dat.trim_otos <-dplyr::filter(dat.trim,Metric=="Otoliths/Tow" & Method=="Otoliths Changed")
dat.trim_tows <-dplyr::filter(dat.trim,Metric=="Tows" & Method=="Tows Changed")
dat.trim <-rbind(dat.trim_otos,dat.trim_tows)
# dat.trim[which(dat.trim$InputSS=="NaN"),]$InputSS<-1

ggplot(data=dat.trim,aes(x=Value,y=InputSS,fill=Method,group=Method,shape=Year))+
  # geom_line(size=1.2,aes(color=Method))+
  geom_path(aes(color=Method,group=Year),size=1.2,show.legend=FALSE)+
  geom_point(aes(shape=Year),size=3,show.legend=TRUE)+
  # geom_smooth(aes(group=Method,color=Method),method='lm',formula=y~x)+
  facet_grid(Species~Metric,scale="free_x")+
  scale_shape_manual(values=c(21,22,23))+
  ylab("\n Input Sample Size \n")+
  xlab("\nOtoliths/Tow                 Tows\n")+
  # scale_color_manual(values=c("black","red","yellow","blue","grey50","green"))+
  # scale_fill_manual(values=c("black","red","yellow","blue","grey50","green"))+
  scale_color_manual(values=c("black","grey60"))+
  scale_fill_manual(values=c("black","grey60"))+
  # scale_y_continuous(limits=c(0,350),breaks=c(0,100,200,300))+
  # scale_y_continuous(limits=c(-100,1500),breaks=c(0,500,1000,1500))+
  # scale_y_continuous(limits=c(-1000,4000),breaks=c(0,2000,4000))+
  # scale_y_continuous(limits=c(-1000,10000),breaks=c(0,5000,10000))+
  # scale_x_continuous(limits=c(0,250),breaks=c(0,50,100,150,200))+
  theme_bw()+
  guides(fill = guide_legend(override.aes = list(shape=21,size=5)),
         shape= guide_legend(override.aes = list(size=5)))+
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.direction = "vertical",
        axis.title = element_text(face="bold", size = 20),
        axis.text = element_text(color="black",size=22),
        axis.text.x = element_text(color="black",size=18),
        axis.ticks=element_line(color="black"),
        plot.title = element_text(hjust = 0.5, face="bold", size = 24),
        plot.margin=unit(c(0.5,0.5,0.1,0.1),"cm"),
        plot.background=element_rect(fill="white"),
        legend.text = element_text(size=18, face="bold"),
        legend.title = element_text(size=25, face = "bold"),
        strip.text.x = element_blank(),
        strip.text.y = element_text(size=18, vjust=0.75, face = "bold"),
        # strip.background = element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_line(color="white"),
        panel.grid.major=element_line(color="white"),
        panel.spacing.x = unit(5, "mm"),
        panel.spacing.y = unit(5, "mm"))


ggsave("Fig3_ISS~otos_tows.jpeg", plot=last_plot(), device = "jpeg",
       path = 'C:/Users/matthew.siskey/Desktop',
       width=10, height=9.5, units="in", dpi=1000)

