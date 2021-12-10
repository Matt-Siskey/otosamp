###############################################################################################################
######################################### Cost-Revenue Analysis ###############################################
###############################################################################################################
library(ggplot2); library(dplyr); library(plyr); library(purrr); library(reshape2); library(data.table)
rm(list=ls())

################################################################################
#### Revenue Analysis
setwd("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/otosamp")
### Define real-world ABC, SE ABC, and OFL (e.g., ABC = 100mt, SE ABC = 20mt, OFL = 90mt)
samp_change <-c("0.33","0.67","0","1.33","1.67") # Same suite of changes used in BE, but char
samp_scen   <-c("1 Boat","2 Boat","3 Boat","4 Boat","5 Boat")
species     <-c("30152","30060","21740")
method      <-c("ototow_otos","ototow_tows")
load("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/SD_spec_list_logOFL")

abc         <-c(7101,36177,105722) # dr,pop,pk from assessments, 2021 (?) values
ofl         <-c(8655,42977,123455) # dr,pop,pk from assessments, 2021 (?) values
sd.log_ofl_0  <-c(SD_spec_list_logOFL[["Dusky Rockfish"]][["ototow_otos"]][["3 Boat"]],
                  SD_spec_list_logOFL[["Pacific Ocean Perch"]][["ototow_otos"]][["3 Boat"]],
                  SD_spec_list_logOFL[["Walleye Pollock"]][["ototow_otos"]][["3 Boat"]]) # among-boot SD of log(OFL) for 0% change scenario; used instead of SE OFL b/c SE OFL is too big
rev           <-c(974.4,432.1,304.2)  # usd per mt (dr,pop,pk)

### Lists
rev.samp_sd   <-list(); rev.meth_sd <-list(); rev.sp_sd <-list()
abc.change_sp_sd <-list(); abc.change_meth_sd <-list(); abc.change_samp_sd <-list();
abc_sp_sd <-list(); abc_meth_sd <-list(); abc_samp_sd <-list()
sd.ofl.new.list <-list(); sd.ofl.new.list_dr <-list(); sd.ofl.new.list_pop <-list(); sd.ofl.new.list_pk <-list()

# 1. Look up the OFL, e.g., 100 mt
# 1B.  Calculate among-boot SD of log(OFL) for 0% change scenario, and use this in place of the SE from the assessment,
# so that pnorm() and qnorm() use a quantity on the same scale, e.g., log-SD(0%) = 0.038
# 2. Look up the ABC, e.g., 90mt
# 3. Calculate the p-star that would result in that same ABC using the log(ABC/OFL),
# e.g., pnorm(log(ABC/OFL), mean=0, sd=0.038)  = 0.00278
# 4. For a given data change, calculate the new SD OFL across bootstrap replicates,
# e.g., if reduced data results in among-bootstrap SD of log(OFL) = 0.048, calculate new log(ABC/OFL)
# given the implied p-star, qnorm( 0.00278, mean=0, sd= 0.048) = -0.133, such that
# new ABC = exp(-0.133) * 100mt = 87.5mt
# 5. Calculate the reduction in ABC due to increased uncertainty, e.g., 2.5 mt
# 6. Multiply that reduction by price, e.g., 1000 $/mt, to get a reduction

for(sp in 1:length(species)){
  for(m in 1:length(method)){
    for(samp in 1:length(samp_change)){
      ### Calculate the p-star that would result in that same OFL, e.g., pnorm(90, mean=100, sd=20)  = 0.309
      log_ratio <-log(abc[sp]/ofl[sp])
      # log_ratio <-ln(abc[sp]/ofl[sp])
      p.star_sd <-pnorm(log_ratio, mean=0, sd=SD_spec_list_logOFL[[sp]][[m]][[3]])

      ### For a given data change, calculate the new SE OFL as the SD across bootstrap replicates
      ### e.g., if reduced data results in SE = 25mt, calculate new OFL given the implied p-star
      ### e.g., qnorm(0.309, mean=100, sd=25) = 87.5mt
      log_ratio_new_sd <-qnorm(p.star_sd, mean=0, sd=SD_spec_list_logOFL[[sp]][[m]][[samp]])
      abc.new_sd <-exp(log_ratio_new_sd)*ofl[sp]

      ### Calculate the reduction in ABC due to increased uncertainty, e.g., 2.5mt
      abc.change_sd <-abc.new_sd-abc[sp]

      ### Multiply that reduction by price, e.g., 1000 $/mt, to get a reduction of $2500 from increased uncertainty
      rev.new_sd                 <-abc.change_sd*rev[sp]
      rev.samp_sd[[samp]]        <-rev.new_sd
      abc.change_samp_sd[[samp]] <-abc.change_sd
      abc_samp_sd[[samp]]        <-abc.new_sd
    }
    names(rev.samp_sd)        <-samp_scen
    rev.meth_sd[[m]]          <-rev.samp_sd
    names(abc.change_samp_sd) <-samp_scen
    abc.change_meth_sd[[m]]   <-abc.change_samp_sd
    names(abc_samp_sd)        <-samp_scen
    abc_meth_sd[[m]]          <-abc_samp_sd
  }
  names(rev.meth_sd)        <-method
  rev.sp_sd[[sp]]           <-rev.meth_sd
  names(abc.change_meth_sd) <-method
  abc.change_sp_sd[[sp]]    <-abc.change_meth_sd
  names(abc_meth_sd)        <-method
  abc_sp_sd[[sp]]           <-abc_meth_sd
}
names(rev.sp_sd)        <-species
names(abc.change_sp_sd) <-species
names(abc_sp_sd)        <-species


### Turn list of CIs for SD and CV into dataframes
unc_names  <-c("Revenue")
spec <-c("Dusky Rockfish","Pacific Ocean Perch","Walleye Pollock")
meth <-c("Otoliths Changed","Tows Changed")
scen <-c("1 Boat","2 Boat","3 Boat","4 Boat","5 Boat")

# scen_loop <-c("-67%","-67%","-33%","-33%","0%","0%","+33%","+33%","+67%","+67%")

for(sp in 1:length(spec)){
  for(m in 1:length(meth)){
    for(sc in 1:length(scen)){
      rev_temp <-map_df(rev.sp_sd[[sp]][[m]][[sc]],~as.data.frame(.))
      rev_temp <-cbind(rev_temp,rep(unc_names,nrow(rev_temp)),rep(spec[sp],nrow(rev_temp)),rep(scen[sc],nrow(rev_temp)),rep(meth[m],nrow(rev_temp)))
      if(sc==1){temp_sc1 <-rev_temp}
      if(sc==2){temp_sc2 <-rbind(temp_sc1,rev_temp)}
      if(sc==3){temp_sc3 <-rbind(temp_sc2,rev_temp)}
      if(sc==4){temp_sc4 <-rbind(temp_sc3,rev_temp)}
      if(sc==5){rev_df_sc <-rbind(temp_sc4,rev_temp)}
    }
    if(m==1){temp_meth <-rev_df_sc}
    if(m==2){rev_df_meth<-rbind(temp_meth,rev_df_sc)}
  }
  if(sp==1){temp_sp1<-rev_df_meth}
  if(sp==2){temp_sp2<-rbind(temp_sp1,rev_df_meth)}
  if(sp==3){rev_df_sp<-rbind(temp_sp2,rev_df_meth)}
}

colnames(rev_df_sp)<-c("Revenue","Type","Species","Scenario","Method")
write.csv(rev_df_sp,"rev.csv")


################################################################################
#### Bootstrap Revenue Analysis
library(ggplot2); library(dplyr); library(plyr); library(purrr); library(reshape2)
rm(list=ls())
setwd("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/otosamp")
### Define real-world ABC, SE ABC, and OFL (e.g., ABC = 100mt, SE ABC = 20mt, OFL = 90mt)
# samp_change <-c("0.33","0.67","1.33","1.67") # Same suite of changes used in BE, but char
samp_scen   <-c("1 Boat","2 Boat","3 Boat","4 Boat","5 Boat")
species     <-c("30152","30060","21740")
method      <-c("Otoliths Changed","Tows Changed")
load("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/ofl_boot_sd_log_sp_0mean") # Created in pipeline_output.R

abc         <-c(7101,36177,105722) # dr,pop,pk from assessments, 2021 (?) values
ofl         <-c(8655,42977,123455) # dr,pop,pk from assessments, 2021 (?) values

### Lists
rev           <-c(974.4,432.1,304.2)  # usd per mt (dr,pop,pk)
rev.samp_sd   <-list(); rev.meth_sd <-list(); rev.sp_sd_boot <-list(); rev.new_sd_rep<-list();
abc.change_sp_sd <-list(); abc.change_meth_sd <-list(); abc.change_samp_sd <-list();
abc_sp_sd <-list(); abc_meth_sd <-list(); abc_samp_sd <-list()
sd.ofl.new.list <-list(); sd.ofl.new.list_dr <-list(); sd.ofl.new.list_pop <-list(); sd.ofl.new.list_pk <-list()

# 1. Look up the OFL, e.g., 100 mt
# 1B.  Calculate among-boot SD of log(OFL) for 0% change scenario, and use this in place of the SE from the assessment,
# so that pnorm() and qnorm() use a quantity on the same scale, e.g., log-SD(0%) = 0.038
# 2. Look up the ABC, e.g., 90mt
# 3. Calculate the p-star that would result in that same ABC using the log(ABC/OFL),
# e.g., pnorm(log(ABC/OFL), mean=0, sd=0.038)  = 0.00278
# 4. For a given data change, calculate the new SD OFL across bootstrap replicates,
# e.g., if reduced data results in among-bootstrap SD of log(OFL) = 0.048, calculate new log(ABC/OFL)
# given the implied p-star, qnorm( 0.00278, mean=0, sd= 0.048) = -0.133, such that
# new ABC = exp(-0.133) * 100mt = 87.5mt
# 5. Calculate the reduction in ABC due to increased uncertainty, e.g., 2.5 mt
# 6. Multiply that reduction by price, e.g., 1000 $/mt, to get a reduction

for(sp in 1:length(species)){
  for(m in 1:length(method)){
    for(samp in 1:length(samp_scen)){
      for(r in 1:1000){
        ### Calculate the p-star that would result in that same OFL, e.g., pnorm(90, mean=100, sd=20)  = 0.309
        log_ratio <-log(abc[sp]/ofl[sp])
        p.star_sd <-pnorm(log_ratio, mean=0, sd=boot_sd_log_sp[[sp]][[m]][[3]][[r]])

        ### For a given data change, calculate the new SE OFL as the SD across bootstrap replicates
        ### e.g., if reduced data results in SE = 25mt, calculate new OFL given the implied p-star
        ### e.g., qnorm(0.309, mean=100, sd=25) = 87.5mt
        log_ratio_new_sd <-qnorm(p.star_sd, mean=0, sd=boot_sd_log_sp[[sp]][[m]][[samp]][[r]])
        abc.new_sd <-exp(log_ratio_new_sd)*ofl[sp]

        ### Calculate the reduction in ABC due to increased uncertainty, e.g., 2.5mt
        abc.change_sd <-abc.new_sd-abc[sp]

        ### Multiply that reduction by price, e.g., 1000 $/mt, to get a reduction of $2500 from increased uncertainty
        rev.new_sd                 <-abc.change_sd*rev[sp]
        rev.new_sd_rep[[r]]<-rev.new_sd
      }

      rev.samp_sd[[samp]]        <-rev.new_sd_rep
      # abc.change_samp_sd[[samp]] <-abc.change_sd
      # abc_samp_sd[[samp]]        <-abc.new_sd
    }
    names(rev.samp_sd)        <-samp_scen
    rev.meth_sd[[m]]          <-rev.samp_sd
    # names(abc.change_samp_sd) <-samp_scen
    # abc.change_meth_sd[[m]]   <-abc.change_samp_sd
    # names(abc_samp_sd)        <-samp_scen
    # abc_meth_sd[[m]]          <-abc_samp_sd
  }
  names(rev.meth_sd)        <-method
  rev.sp_sd_boot[[sp]]           <-rev.meth_sd
  # names(abc.change_meth_sd) <-method
  # abc.change_sp_sd[[sp]]    <-abc.change_meth_sd
  # names(abc_meth_sd)        <-method
  # abc_sp_sd[[sp]]           <-abc_meth_sd
}
names(rev.sp_sd_boot)        <-species
# names(abc.change_sp_sd) <-species
# names(abc_sp_sd)        <-species

save(rev.sp_sd_boot,file="C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/Rev_spec_list_logOFL_boots")


################################################################################
#### CIs
ci_rev_sc<-list();ci_rev_meth<-list();ci_rev_sp<-list();
spec <-c("Dusky Rockfish","Pacific Ocean Perch","Walleye Pollock")
meth <-c("Otoliths Changed","Tows Changed")
scen <-c("1 Boat","2 Boat","3 Boat","4 Boat","5 Boat")

for(sp in 1:length(spec)){
  for(m in 1:length(meth)){
    for(sc in 1:length(scen)){
      ci_rev_sc[[sc]]    <-quantile(as.numeric(rev.sp_sd_boot[[sp]][[m]][[sc]]),probs=c(0.025,0.975))
    }
    names(ci_rev_sc)<-scen;
    ci_rev_meth[[m]]    <-ci_rev_sc
  }
  names(ci_rev_meth)<-meth;
  ci_rev_sp[[sp]]    <-ci_rev_meth
}
names(ci_rev_sp) <-spec;

#### Turn list of CIs for SD and CV into dataframes
unc_names  <-c("Revenue CI")
scen <-c("1 Boat","2 Boat","3 Boat","4 Boat","5 Boat")

for(sp in 1:length(spec)){
  for(m in 1:length(meth)){
    for(sc in 1:length(scen)){
      ci <-map_df(ci_rev_sp[[sp]][[m]][[sc]],~as.data.frame(.))
      ci <-cbind(ci,rep(unc_names,nrow(ci)),rep(c("Lower","Upper"),1),rep(spec[sp],nrow(ci)),rep(scen[sc],nrow(ci)),rep(meth[m],nrow(ci)))
      if(sc==1){temp_sc1 <-ci}
      if(sc==2){temp_sc2 <-rbind(temp_sc1,ci)}
      if(sc==3){temp_sc3 <-rbind(temp_sc2,ci)}
      if(sc==4){temp_sc4 <-rbind(temp_sc3,ci)}
      if(sc==5){ci_df_sc <-rbind(temp_sc4,ci)}
    }
    if(m==1){temp_meth <-ci_df_sc}
    if(m==2){ci_df_meth<-rbind(temp_meth,ci_df_sc)}
  }
  if(sp==1){temp_sp1<-ci_df_meth}
  if(sp==2){temp_sp2<-rbind(temp_sp1,ci_df_meth)}
  if(sp==3){ci_df_sp<-rbind(temp_sp2,ci_df_meth)}
}


colnames(ci_df_sp)<-c("CI","Metric","Type","Species","Scenario","Method")

ci_df_upper <-ci_df_sp[which(ci_df_sp$Type=="Upper"),]
ci_df_lower <-ci_df_sp[which(ci_df_sp$Type=="Lower"),]
colnames(ci_df_lower)<-c("Lower CI","Metric","Type","Species","Scenario","Method")
rev_ci_plot <-cbind(ci_df_upper[,1],ci_df_lower[,-3])
colnames(rev_ci_plot)<-c("Upper CI","Lower CI","Metric","Species","Scenario","Method")
rev_ci_plot$Scenario   <-revalue(rev_ci_plot$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))

write.csv(rev_ci_plot,"rev_ci.csv")


################################################################################
#### Cost Analysis
### Sum of nomSS
rm(list=ls())
pathR<-"C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/otosamp/"
method  <-c("ototow_otos","ototow_tows")
stock   <-"GOA"
samp_scenario <-c("1 Boat","2 Boat","3 Boat","4 Boat","5 Boat")
species <-c("30152","30060","21740")
tmp_meth_SUMnomSS<-list();tmp_spec_SUMnomSS<-list();tmp_scen_SUMnomSS<-list()
spec_boots <-200
exp_boots <-100

for(s in 1:length(species)){
  if(species[s]=="30152"){
    species_common <-"Dusky Rockfish"
    nyrs<-16
    calyrs <-35
    yrs<-c("1984","1987","1990","1993","1996","1999","2001","2003","2005","2007","2009","2011","2013","2015","2017","2019")
  }
  if(species[s]=="30060"){
    species_common <-"Pacific Ocean Perch"
    nyrs<-13
    calyrs <-29
    yrs<-c("1990","1993","1996","1999","2003","2005","2007","2009","2011","2013","2015","2017","2019")
  }
  if(species[s]=="21740"){
    species_common <-"Walleye Pollock"
    nyrs<-14
    calyrs <-29
    yrs<-c("1990","1993","1996","1999","2001","2003","2005","2007","2009","2011","2013","2015","2017","2019")
  }
  for(m in 1:length(method)){
    nomSS_list <-list()
    load(paste(pathR,"SzAC Results_",method[m],"/pipeline_nomSS_",species[s],"_",method[m],sep=""))
    nomSS_list<-nomSS_alt
    SUMnomSS <-matrix(NA,ncol=7,nrow=spec_boots*exp_boots)

    for(alt in 1:length(samp_scenario)){
      if(alt==1){nomSS_top<-nomSS_list[[1]]}
      if(alt==2){nomSS_top<-nomSS_list[[2]]}
      if(alt==3){nomSS_top<-nomSS_list[[3]]}
      if(alt==4){nomSS_top<-nomSS_list[[4]]}
      if(alt==5){nomSS_top<-nomSS_list[[5]]}

      for(i in 1:spec_boots){
        if(i==1){counter<-1}
        if(i>1){counter<-counter+1}

        for(ii in 1:exp_boots){
          if(ii>1){counter=counter+1}
          SUMnomSS[counter,1] <-sum(as.numeric(nomSS_top[[i]][[ii]][1:nyrs]))/calyrs
          SUMnomSS[counter,2] <-paste(samp_scenario[alt])
          SUMnomSS[counter,3] <-paste(species_common)
          SUMnomSS[counter,4] <-"SUMNomSS"
          SUMnomSS[counter,5] <-i
          SUMnomSS[counter,6] <-ii
          SUMnomSS[counter,7] <-method[m]
        }
      }
      tmp_scen_SUMnomSS[[alt]]<-SUMnomSS
    }
    names(tmp_scen_SUMnomSS)<-samp_scenario
    tmp_meth_SUMnomSS[[m]] <-tmp_scen_SUMnomSS
  }
  names(tmp_meth_SUMnomSS) <-method
  tmp_spec_SUMnomSS[[s]]   <-tmp_meth_SUMnomSS
}

names(tmp_spec_SUMnomSS)<-species

SUMnomSS_dr_otos  <-map_df(tmp_spec_SUMnomSS[[1]][[1]],~as.data.frame(.))
SUMnomSS_pop_otos <-map_df(tmp_spec_SUMnomSS[[2]][[1]],~as.data.frame(.))
SUMnomSS_pk_otos  <-map_df(tmp_spec_SUMnomSS[[3]][[1]],~as.data.frame(.))

SUMnomSS_otos <-rbind(SUMnomSS_dr_otos,SUMnomSS_pop_otos,SUMnomSS_pk_otos)
colnames(SUMnomSS_otos) <-c("SUMnomSS","Scenario","Species","Metric","Spec_Boot","Exp_Boot","Method")

SUMnomSS_dr_tows  <-map_df(tmp_spec_SUMnomSS[[1]][[2]],~as.data.frame(.))
SUMnomSS_pop_tows <-map_df(tmp_spec_SUMnomSS[[2]][[2]],~as.data.frame(.))
SUMnomSS_pk_tows  <-map_df(tmp_spec_SUMnomSS[[3]][[2]],~as.data.frame(.))

SUMnomSS_tows <-rbind(SUMnomSS_dr_tows,SUMnomSS_pop_tows,SUMnomSS_pk_tows)
colnames(SUMnomSS_tows) <-c("SUMnomSS","Scenario","Species","Metric","Spec_Boot","Exp_Boot","Method")


### Mean across spec & exp boots
# Otos changed
SUMnomSS_otos <-aggregate(as.numeric(SUMnomSS_otos[,1]),list(SUMnomSS_otos$Scenario,SUMnomSS_otos$Species),mean)
colnames(SUMnomSS_otos)<-c("Scenario","Species","NomSS")
# Tows changed
SUMnomSS_tows <-aggregate(as.numeric(SUMnomSS_tows[,1]),list(SUMnomSS_tows$Scenario,SUMnomSS_tows$Species),mean)
colnames(SUMnomSS_tows)<-c("Scenario","Species","NomSS")

SUMnomSS_otos[which(SUMnomSS_otos$Species=="Dusky Rockfish"),4] <-SUMnomSS_otos[which(SUMnomSS_otos$Species=="Dusky Rockfish"),]$NomSS*46.29
SUMnomSS_otos[which(SUMnomSS_otos$Species=="Pacific Ocean Perch"),4] <-SUMnomSS_otos[which(SUMnomSS_otos$Species=="Pacific Ocean Perch"),]$NomSS*29.99
SUMnomSS_otos[which(SUMnomSS_otos$Species=="Walleye Pollock"),4] <-SUMnomSS_otos[which(SUMnomSS_otos$Species=="Walleye Pollock"),]$NomSS*13.78
SUMnomSS_otos[,5] <-rep("Otoliths Changed",length(SUMnomSS_otos$Scenario))
colnames(SUMnomSS_otos) <-c("Scenario","Species","NomSS","Cost","Method")

SUMnomSS_tows[which(SUMnomSS_tows$Species=="Dusky Rockfish"),4] <-SUMnomSS_tows[which(SUMnomSS_tows$Species=="Dusky Rockfish"),]$NomSS*46.29
SUMnomSS_tows[which(SUMnomSS_tows$Species=="Pacific Ocean Perch"),4] <-SUMnomSS_tows[which(SUMnomSS_tows$Species=="Pacific Ocean Perch"),]$NomSS*29.99
SUMnomSS_tows[which(SUMnomSS_tows$Species=="Walleye Pollock"),4] <-SUMnomSS_tows[which(SUMnomSS_tows$Species=="Walleye Pollock"),]$NomSS*13.78
SUMnomSS_tows[,5] <-rep("Tows Changed",length(SUMnomSS_tows$Scenario))
colnames(SUMnomSS_tows) <-c("Scenario","Species","NomSS","Cost","Method")

SUMnomSS_cost <-rbind(SUMnomSS_otos,SUMnomSS_tows)
write.csv(SUMnomSS_cost,"SUMnomSS_cost.csv")
SUMnomSS_cost<-read.csv("SUMnomSS_cost.csv",row.names = 1,header=T)

#### Join cost & revenue calcs
rev_df_sp <-read.csv("rev.csv",row.names = 1,header=T)
rev_ci_plot <-read.csv("rev_ci.csv",row.names = 1,header=T)
SUMnomSS_cost <-read.csv("SUMnomSS_cost.csv",row.names = 1,header=T)

rev_df_sp$Scenario <-revalue(rev_df_sp$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
SUMnomSS_cost$Scenario <-revalue(SUMnomSS_cost$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))

cost_rev <-dplyr::full_join(rev_df_sp,rev_ci_plot,by=c("Method","Species","Scenario"))
cost_rev <-dplyr::full_join(cost_rev,SUMnomSS_cost,by=c("Method","Species","Scenario"))
cost_rev <-cost_rev[,-c(2,8)]
write.csv(cost_rev,"cost_rev.csv")


################################################################################
#### Plotting
library(ggplot2); library(dplyr); library(plyr); library(purrr); library(reshape2); library(data.table)
setwd("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Oto Redist Project/Data & Code/Output Files")
cost_rev <-read.csv("cost_rev.csv",row.names = 1,header=T)

cost_rev_dr_otos <-filter(cost_rev,Species=="Dusky Rockfish" & Method=="Otoliths Changed")
cost_rev_dr_tows <-filter(cost_rev,Species=="Dusky Rockfish" & Method=="Tows Changed")
cost_rev_pop_otos <-filter(cost_rev,Species=="Pacific Ocean Perch" & Method=="Otoliths Changed")
cost_rev_pop_tows <-filter(cost_rev,Species=="Pacific Ocean Perch" & Method=="Tows Changed")
cost_rev_pk_otos <-filter(cost_rev,Species=="Walleye Pollock" & Method=="Otoliths Changed")
cost_rev_pk_tows <-filter(cost_rev,Species=="Walleye Pollock" & Method=="Tows Changed")

cost_rev_dr_otos[1,9] <-cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="-67%"),]$NomSS - cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="0%"),]$NomSS
cost_rev_dr_otos[2,9] <-cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="-33%"),]$NomSS - cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="0%"),]$NomSS
cost_rev_dr_otos[3,9] <-cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="0%"),]$NomSS - cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="0%"),]$NomSS
cost_rev_dr_otos[4,9] <-cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="+33%"),]$NomSS - cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="0%"),]$NomSS
cost_rev_dr_otos[5,9] <-cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="+67%"),]$NomSS - cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="0%"),]$NomSS
cost_rev_dr_otos[1,10] <-cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="-67%"),]$Cost - cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="0%"),]$Cost
cost_rev_dr_otos[2,10] <-cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="-33%"),]$Cost - cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="0%"),]$Cost
cost_rev_dr_otos[3,10] <-cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="0%"),]$Cost - cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="0%"),]$Cost
cost_rev_dr_otos[4,10] <-cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="+33%"),]$Cost - cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="0%"),]$Cost
cost_rev_dr_otos[5,10] <-cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="+67%"),]$Cost - cost_rev_dr_otos[which(cost_rev_dr_otos$Scenario=="0%"),]$Cost
colnames(cost_rev_dr_otos) <-c("Revenue_Change","Species","Scenario","Method","Upper.CI","Lower.CI","NomSS","Cost","NomSS_Change","Cost_Change")

cost_rev_dr_tows[1,9] <-cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="-67%"),]$NomSS - cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="0%"),]$NomSS
cost_rev_dr_tows[2,9] <-cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="-33%"),]$NomSS - cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="0%"),]$NomSS
cost_rev_dr_tows[3,9] <-cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="0%"),]$NomSS - cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="0%"),]$NomSS
cost_rev_dr_tows[4,9] <-cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="+33%"),]$NomSS - cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="0%"),]$NomSS
cost_rev_dr_tows[5,9] <-cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="+67%"),]$NomSS - cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="0%"),]$NomSS
cost_rev_dr_tows[1,10] <-cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="-67%"),]$Cost - cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="0%"),]$Cost
cost_rev_dr_tows[2,10] <-cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="-33%"),]$Cost - cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="0%"),]$Cost
cost_rev_dr_tows[3,10] <-cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="0%"),]$Cost - cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="0%"),]$Cost
cost_rev_dr_tows[4,10] <-cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="+33%"),]$Cost - cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="0%"),]$Cost
cost_rev_dr_tows[5,10] <-cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="+67%"),]$Cost - cost_rev_dr_tows[which(cost_rev_dr_tows$Scenario=="0%"),]$Cost
colnames(cost_rev_dr_tows) <-c("Revenue_Change","Species","Scenario","Method","Upper.CI","Lower.CI","NomSS","Cost","NomSS_Change","Cost_Change")

cost_rev_pop_otos[1,9] <-cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="-67%"),]$NomSS - cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="0%"),]$NomSS
cost_rev_pop_otos[2,9] <-cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="-33%"),]$NomSS - cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="0%"),]$NomSS
cost_rev_pop_otos[3,9] <-cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="0%"),]$NomSS - cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="0%"),]$NomSS
cost_rev_pop_otos[4,9] <-cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="+33%"),]$NomSS - cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="0%"),]$NomSS
cost_rev_pop_otos[5,9] <-cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="+67%"),]$NomSS - cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="0%"),]$NomSS
cost_rev_pop_otos[1,10] <-cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="-67%"),]$Cost - cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="0%"),]$Cost
cost_rev_pop_otos[2,10] <-cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="-33%"),]$Cost - cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="0%"),]$Cost
cost_rev_pop_otos[3,10] <-cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="0%"),]$Cost - cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="0%"),]$Cost
cost_rev_pop_otos[4,10] <-cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="+33%"),]$Cost - cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="0%"),]$Cost
cost_rev_pop_otos[5,10] <-cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="+67%"),]$Cost - cost_rev_pop_otos[which(cost_rev_pop_otos$Scenario=="0%"),]$Cost
colnames(cost_rev_pop_otos) <-c("Revenue_Change","Species","Scenario","Method","Upper.CI","Lower.CI","NomSS","Cost","NomSS_Change","Cost_Change")

cost_rev_pop_tows[1,9] <-cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="-67%"),]$NomSS - cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="0%"),]$NomSS
cost_rev_pop_tows[2,9] <-cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="-33%"),]$NomSS - cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="0%"),]$NomSS
cost_rev_pop_tows[3,9] <-cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="0%"),]$NomSS - cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="0%"),]$NomSS
cost_rev_pop_tows[4,9] <-cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="+33%"),]$NomSS - cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="0%"),]$NomSS
cost_rev_pop_tows[5,9] <-cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="+67%"),]$NomSS - cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="0%"),]$NomSS
cost_rev_pop_tows[1,10] <-cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="-67%"),]$Cost - cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="0%"),]$Cost
cost_rev_pop_tows[2,10] <-cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="-33%"),]$Cost - cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="0%"),]$Cost
cost_rev_pop_tows[3,10] <-cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="0%"),]$Cost - cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="0%"),]$Cost
cost_rev_pop_tows[4,10] <-cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="+33%"),]$Cost - cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="0%"),]$Cost
cost_rev_pop_tows[5,10] <-cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="+67%"),]$Cost - cost_rev_pop_tows[which(cost_rev_pop_tows$Scenario=="0%"),]$Cost
colnames(cost_rev_pop_tows) <-c("Revenue_Change","Species","Scenario","Method","Upper.CI","Lower.CI","NomSS","Cost","NomSS_Change","Cost_Change")

cost_rev_pk_otos[1,9] <-cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="-67%"),]$NomSS - cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="0%"),]$NomSS
cost_rev_pk_otos[2,9] <-cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="-33%"),]$NomSS - cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="0%"),]$NomSS
cost_rev_pk_otos[3,9] <-cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="0%"),]$NomSS - cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="0%"),]$NomSS
cost_rev_pk_otos[4,9] <-cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="+33%"),]$NomSS - cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="0%"),]$NomSS
cost_rev_pk_otos[5,9] <-cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="+67%"),]$NomSS - cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="0%"),]$NomSS
cost_rev_pk_otos[1,10] <-cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="-67%"),]$Cost - cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="0%"),]$Cost
cost_rev_pk_otos[2,10] <-cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="-33%"),]$Cost - cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="0%"),]$Cost
cost_rev_pk_otos[3,10] <-cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="0%"),]$Cost - cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="0%"),]$Cost
cost_rev_pk_otos[4,10] <-cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="+33%"),]$Cost - cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="0%"),]$Cost
cost_rev_pk_otos[5,10] <-cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="+67%"),]$Cost - cost_rev_pk_otos[which(cost_rev_pk_otos$Scenario=="0%"),]$Cost
colnames(cost_rev_pk_otos) <-c("Revenue_Change","Species","Scenario","Method","Upper.CI","Lower.CI","NomSS","Cost","NomSS_Change","Cost_Change")

cost_rev_pk_tows[1,9] <-cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="-67%"),]$NomSS - cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="0%"),]$NomSS
cost_rev_pk_tows[2,9] <-cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="-33%"),]$NomSS - cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="0%"),]$NomSS
cost_rev_pk_tows[3,9] <-cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="0%"),]$NomSS - cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="0%"),]$NomSS
cost_rev_pk_tows[4,9] <-cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="+33%"),]$NomSS - cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="0%"),]$NomSS
cost_rev_pk_tows[5,9] <-cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="+67%"),]$NomSS - cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="0%"),]$NomSS
cost_rev_pk_tows[1,10] <-cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="-67%"),]$Cost - cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="0%"),]$Cost
cost_rev_pk_tows[2,10] <-cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="-33%"),]$Cost - cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="0%"),]$Cost
cost_rev_pk_tows[3,10] <-cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="0%"),]$Cost - cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="0%"),]$Cost
cost_rev_pk_tows[4,10] <-cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="+33%"),]$Cost - cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="0%"),]$Cost
cost_rev_pk_tows[5,10] <-cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="+67%"),]$Cost - cost_rev_pk_tows[which(cost_rev_pk_tows$Scenario=="0%"),]$Cost
colnames(cost_rev_pk_tows) <-c("Revenue_Change","Species","Scenario","Method","Upper.CI","Lower.CI","NomSS","Cost","NomSS_Change","Cost_Change")

cost_rev_change <-rbind(cost_rev_dr_otos,cost_rev_dr_tows,cost_rev_pop_otos,cost_rev_pop_tows,cost_rev_pk_otos,cost_rev_pk_tows)
# write.csv(cost_rev_change,"cost_rev_change.csv")

# ### Cost-Revenue
# SUMnomSS_cost_change <-SUMnomSS_cost[which(SUMnomSS_cost$Scenario!="3 Boat"),]
# SUMnomSS_cost$Scenario <-revalue(SUMnomSS_cost$Scenario,c("1 Boat"="-67%","2 Boat"="-33%","3 Boat"="0%","4 Boat"="+33%","5 Boat"="+67%"))
# cost_rev$Uncertainty <-revalue(cost_rev$Uncertainty,c("SD OFL"="SD log(OFL)","MAD OFL"="MAD log(OFL)"))
# SUMnomSS_cost$Scenario <-factor(SUMnomSS_cost$Scenario,levels=c("-67%","-33%","0%","+33%","+67%"))
# cost_rev <-dplyr::full_join(SUMnomSS_cost,rev_ci_final,by=c("Method","Species","Scenario"))


zero_comma_x <- function (x, ...) {
  ifelse(x==0,"0",x)
  format(x, ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

zero_comma_y <- function (y, ...) {
  ifelse(y==0,"0",y)
  format(y, ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

dat <-data.table(cost_rev_change)
dat[,y_min:= (Revenue_Change*0.95)/1e6, by = Species]
dat[,y_max:= (Revenue_Change*1.05)/1e6, by = Species]
dat[which(dat$Species=="Dusky Rockfish"),]$y_min <- -1
dat[which(dat$Species=="Dusky Rockfish"),]$y_max <- 1
dat[which(dat$Species=="Pacific Ocean Perch"),]$y_min <- -1
dat[which(dat$Species=="Pacific Ocean Perch"),]$y_max <- 1
dat[which(dat$Species=="Walleye Pollock"),]$y_min <- -6
dat[which(dat$Species=="Walleye Pollock"),]$y_max <- 6

ggplot(data=cost_rev_change,aes(x=(Cost_Change/1e3),y=(Revenue_Change/1e6),fill=Method,group=Method))+
  geom_blank(data=dat,aes(y = y_min))+
  geom_blank(data=dat,aes(y = y_max))+
  # geom_errorbar(aes(ymin=Lower.CI/1e6,ymax=Upper.CI/1e6,color=Method),width=0.075,size=0.75,show.legend=FALSE)+
  geom_errorbar(aes(ymin=Lower.CI/1e6,ymax=Upper.CI/1e6,color=Method),width=2,size=0.75,show.legend=FALSE)+
  geom_path(aes(color=Method),size=1.2,show.legend=FALSE)+
  geom_hline(yintercept=0,linetype="dashed",col='red')+
  geom_point(color="black",pch=21,size=4,show.legend=TRUE)+
  facet_grid(Species~Method,scales="free_y")+
  ylab("\n Change in Revenue (Million USD/Year) \n")+
  xlab("\n Change in Sampling Costs/Year (000s USD/Year) \n")+
  # xlab("\n Change in # of Otoliths/Year (000s) \n")+
  scale_color_manual(values=c("black","grey60"))+
  scale_fill_manual(values=c("black","grey60"))+
  # scale_x_continuous(limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1),label=zero_comma_x)+
  # scale_x_continuous(limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1),label=zero_comma_x)+
  scale_y_continuous(label=zero_comma_y)+
  theme_bw()+
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
        strip.text.x = element_text(size=18, vjust=0.75, face = "bold"),
        strip.text.y = element_text(size=18, vjust=0.75, face = "bold"),
        # strip.background = element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_line(color="white"),
        panel.grid.major=element_line(color="white"),
        panel.spacing.x = unit(5, "mm"),
        panel.spacing.y = unit(5, "mm"))

ggsave("Cost_Revenue_SD_cost.jpeg", plot=last_plot(), device = "jpeg",
       path = 'C:/Users/matthew.siskey/Desktop',
       width=11, height=10.5, units="in", dpi=1000)
