################################################################################################################
##################################### Calc ISS for sampling scenarios ##########################################
################################################################################################################

################################################ Head ##########################################################

library(tidyr); library(reshape2); library(plyr); library(dplyr); library(RODBC); library(ggplot2); library(data.table)
rm(list=ls())
setwd("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/otosamp")
getData <-FALSE # Pull data from RACE?
getBoot <-TRUE  # Do boot loop to calc input ss?

# source(paste(getwd(),"/Functions/Functions.R",sep="")) #Source functions for use below
load(paste(getwd(),"/seed.list",sep="")) #load seed folder again
bootest.seed <- as.numeric(seed.list[[1]][,"bootest.seed1"]) #read recruitment seed from seedlist function

################################################################################################################
### Must run getData code first
### then getBoot code w/ redist == FALSE, samp_change == 1, getBoot == TRUE, and n_boot == 1
### to get proportions for original data to compare to boot data
################################################################################################################

#### Bootstrap Estimator settings:
### 1 = no change in # tows, but resamp; 0.25 = 75% reduction; 1.75 = 75% increase, etc.
# redist: Whether to initiate change of tow number loop; switch to FALSE if calculating OG, non-boot prop
# samp_change <-c(0.33,0.67,1,1.33,1.67) # data reduction analysis (e.g., changing from 3 to 2 to 1 boats)

species <-21740 #,30060,21740) # Enter species code; # 21740 (Pollock), 30060 (POP), 30152/30150 (Dusky); 21720 (Pacific Cod)
stock   <-'GOA' # Enter stock area; # 'AI', 'EBS', 'GOA'

### Whether to initiate OAC or BE calc, tow, oto/tow, or oto&tow sampling
method <-"ototow" # "tow" "ototow" "oto"
submethod <-c("otos","tows") # "otos" "tows" "both" 

### BE settings
resamp <-TRUE; inputSS <-TRUE ; n_boot <-100; n_boot_top <-100
if(method=="tow"){do.tow <-TRUE; tow_redist <-TRUE;do.spec <-FALSE; spec_redist <-FALSE;do.ototow <-FALSE; OTOtow_redist <-FALSE;}
if(method=="oto"){do.tow <-FALSE; tow_redist <-FALSE;do.spec <-TRUE; spec_redist <-TRUE;do.ototow <-FALSE; OTOtow_redist <-FALSE;}
if(method=="ototow"){do.tow <-FALSE; tow_redist <-FALSE;do.spec <-FALSE; spec_redist <-FALSE;do.ototow <-TRUE; OTOtow_redist <-TRUE;}

# first & plus-group age classes used in data (pollock, dusky)
if(species==30152){firstAC <-4; plusgroup <-25}
if(species==21740){firstAC <-1; plusgroup <-10}
if(species==30060){firstAC <-2; plusgroup <-25}

agecol<-39 # column in specimen file (individual age data) associated with age estimates

SzACResults <-"C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/otosamp"


######################################### Pull Data from RACE ##################################################
### Must be connected to VPN in order to access RACE
if(getData==TRUE){
  # Enter AFSC username/password
  username=""
  password=""
  
  # ODBC connect stuff
  channel=odbcConnect("afsc",uid=username,pwd=password,believeNRows=FALSE)
  
  ### Start calling data
  # Get strata data (this is strata for both GOA and AI)
  strata <- sqlQuery(channel,"SELECT * FROM GOA.GOA_STRATA",believeNRows=FALSE) # This pulls GOA and AI despite 'GOA.GOA_STRATA'
  # strata <- sqlQuery(channel,paste("SELECT * FROM ",stock,".",stock,"_STRATA",sep=""),believeNRows=FALSE)
  strata <- subset(strata,strata$SURVEY==stock)
  
  # Get length data
  sizepop  <- sqlQuery(channel,paste("SELECT * FROM ",stock,".SIZECOMP_TOTAL WHERE(((",stock,
                                     ".SIZECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
  
  # Get age data
  agepop   <- sqlQuery(channel,paste("SELECT * FROM ",stock,".AGECOMP_TOTAL WHERE(((",stock,
                                     ".AGECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
  
  # Get CPUE data
  CPUE     <- sqlQuery(channel,paste("SELECT * FROM ",stock,".CPUE WHERE(((",stock,".CPUE.SPECIES_CODE)=",
                                     species,"))",sep=""),believeNRows=FALSE)
  
  # Get length frequency data
  # lfreq    <- sqlQuery(channel,paste("SELECT * FROM
  #                       (RACEBASE.HAUL INNER JOIN RACEBASE.LENGTH ON RACEBASE.HAUL.HAULJOIN =
  #                       RACEBASE.LENGTH.HAULJOIN)INNER JOIN ",stock,".BIENNIAL_SURVEYS ON
  #                       RACEBASE.LENGTH.CRUISEJOIN = ",stock,".BIENNIAL_SURVEYS.CRUISEJOIN
  #                       WHERE(((RACEBASE.LENGTH.REGION)='",stock,"') AND ((RACEBASE.LENGTH.SPECIES_CODE)=",
  #                                    species,") AND((RACEBASE.HAUL.PERFORMANCE)>=0))",sep=""),believeNRows=FALSE)
  
  # Above code works for GOA but AI pulls still need to have "GOA" in first two calls to 'stock' regardless of stock
  lfreq    <- sqlQuery(channel,paste("SELECT * FROM
                      (RACEBASE.HAUL INNER JOIN RACEBASE.LENGTH ON RACEBASE.HAUL.HAULJOIN =
                      RACEBASE.LENGTH.HAULJOIN)INNER JOIN GOA.BIENNIAL_SURVEYS ON
                      RACEBASE.LENGTH.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN
                      WHERE(((RACEBASE.LENGTH.REGION)='",stock,"') AND ((RACEBASE.LENGTH.SPECIES_CODE)=",
                                     species,") AND((RACEBASE.HAUL.PERFORMANCE)>=0))",sep=""),believeNRows=FALSE)
  
  # Get specimen data
  # specimen <- sqlQuery(channel,paste("SELECT * FROM(RACEBASE.HAUL INNER JOIN RACEBASE.SPECIMEN ON 
  #                      RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN)INNER JOIN ",stock,
  #                                    ".BIENNIAL_SURVEYS ON RACEBASE.SPECIMEN.CRUISEJOIN = ",stock,"
  #                      .BIENNIAL_SURVEYS.CRUISEJOIN WHERE(((RACEBASE.SPECIMEN.SPECIES_CODE)=",species,") AND
  #                       ((RACEBASE.SPECIMEN.REGION)='",stock,"'))",sep=""),believeNRows=FALSE)
  
  # Above code works for GOA but AI pulls still need to have "GOA" in first two calls to 'stock' regardless of stock
  specimen <- sqlQuery(channel,paste("SELECT * FROM(RACEBASE.HAUL INNER JOIN RACEBASE.SPECIMEN ON 
                     RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN)INNER JOIN GOA
                                   .BIENNIAL_SURVEYS ON RACEBASE.SPECIMEN.CRUISEJOIN = GOA
                     .BIENNIAL_SURVEYS.CRUISEJOIN WHERE(((RACEBASE.SPECIMEN.SPECIES_CODE)=",species,") AND
                      ((RACEBASE.SPECIMEN.REGION)='",stock,"'))",sep=""),believeNRows=FALSE)
  
  # Remove redundant columns
  lfreq    <- lfreq[,-which(substr(names(lfreq),nchar(names(lfreq))-1,nchar(names(lfreq)))==".1")]
  lfreq    <- lfreq[,-which(substr(names(lfreq),nchar(names(lfreq))-1,nchar(names(lfreq)))==".2")]
  specimen <- specimen[,-which(substr(names(specimen),nchar(names(specimen))-1,nchar(names(specimen)))==".1")]
  specimen <- specimen[,-which(substr(names(specimen),nchar(names(specimen))-1,nchar(names(specimen)))==".2")]
  
  write.csv(sizepop,paste(getwd(),"/Data/sizepop_",stock,"_",species,".csv",sep=""))
  write.csv(lfreq,paste(getwd(),"/Data/lfreq_",stock,"_",species,".csv",sep=""))
  write.csv(agepop,paste(getwd(),"/Data/agepop_",stock,"_",species,".csv",sep=""))
  write.csv(specimen,paste(getwd(),"/Data/specimen_",stock,"_",species,".csv",sep=""))
  write.csv(CPUE,paste(getwd(),"/Data/CPUE_",stock,"_",species,".csv",sep=""))
  write.csv(strata,paste(getwd(),"/Data/strata_",stock,".csv",sep=""))
}

# ddply(specimen,~YEAR,summarise,Unique_Tows=length(unique(HAULJOIN))) # Tabulate number of unique tows by year

######################################### Conduct Input SS Boot#################################################
# Script to estimate population #'s at size and age
# alt = b = sp = y = 1 # for stepping through to debug

if(getBoot==TRUE){
  # Set directories and read in necessary data
  path  <-setwd("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/otosamp")
  pathD <-paste0(path,"/Data")

  lfreq        <-read.csv(paste0(pathD,"/lfreq_",stock,"_",species,".csv"))
  sizepop_RACE <-read.csv(paste0(pathD,"/sizepop_",stock,"_",species,".csv"))
  cpue         <-read.csv(paste0(pathD,"/CPUE_",stock,"_",species,".csv"))
  strata       <-read.csv(paste0(pathD,"/strata_",stock,".csv"))
  specimen     <-read.csv(paste0(pathD,"/specimen_",stock,"_",species,".csv"))

  specimen                   <-subset(specimen,is.na(specimen$AGE)==FALSE)
  specimen$SPECIMENID_unique <-seq(1:nrow(specimen))
  specimen                   <-specimen[which(specimen$AGE>=firstAC),]
  specimen[agecol]           <-lapply(specimen[agecol], function(x) ifelse(x>plusgroup,plusgroup,x))
  yrs     <-sort(unique(specimen$YEAR))
  
  for(sp in 1:length(species)){
    
    lfreq_sp        <-subset(lfreq,lfreq$SPECIES_CODE==species[sp])
    sizepop_RACE_sp <-subset(sizepop_RACE,sizepop_RACE$SPECIES_CODE==species[sp])
    cpue_sp         <-subset(cpue,cpue$SPECIES_CODE==species[sp])
    
    # Set up results matrices (length then age comp)
    lengths           <-sort(unique(lfreq_sp$LENGTH))
    SZPOP_M           <-matrix(nrow=length(yrs),ncol=length(lengths))
    colnames(SZPOP_M) <-as.character(lengths)
    rownames(SZPOP_M) <-as.character(yrs)
    SZPOP_F           <-matrix(nrow=length(yrs),ncol=length(lengths))
    colnames(SZPOP_F) <-as.character(lengths)
    rownames(SZPOP_F) <-as.character(yrs)
    SZPOP_U           <-matrix(nrow=length(yrs),ncol=length(lengths))
    colnames(SZPOP_U) <-as.character(lengths)
    rownames(SZPOP_U) <-as.character(yrs)
    
    #########################
    # Loop thru years
    for(y in 1:length(yrs)){
      
      lfreq_sp_y        <-subset(lfreq_sp,lfreq_sp$YEAR==yrs[y])
      cpue_sp_y         <-subset(cpue_sp,cpue_sp$YEAR==yrs[y])
      sizepop_RACE_sp_y <-subset(sizepop_RACE_sp,sizepop_RACE_sp$YEAR==yrs[y])
      stratum <-sort(unique(strata$STRATUM))
      
      ########################################################################################
      # Estimate pop'n @ size
      ########################################################################################
      SZPOP_M_st           <-matrix(nrow=length(stratum),ncol=length(lengths))
      rownames(SZPOP_M_st) <-stratum
      colnames(SZPOP_M_st) <-lengths
      SZPOP_F_st           <-matrix(nrow=length(stratum),ncol=length(lengths))
      rownames(SZPOP_F_st) <-stratum
      colnames(SZPOP_F_st) <-lengths
      SZPOP_U_st           <-matrix(nrow=length(stratum),ncol=length(lengths))
      rownames(SZPOP_U_st) <-stratum
      colnames(SZPOP_U_st) <-lengths
      
      #########################
      # Loop thru strata
      for(st in 1:length(stratum)){
        
        # Subset data to strata level
        strata_st     <-subset(strata,strata$STRATUM==stratum[st])
        cpue_sp_y_st  <-subset(cpue_sp_y,cpue_sp_y$STRATUM==stratum[st])
        hls_cpue      <-unique(cpue_sp_y_st$HAULJOIN)
        lfreq_sp_y_st <-subset(lfreq_sp_y,lfreq_sp_y$HAULJOIN %in% hls_cpue)
        
        # Subset data to sex-specific (M=males, F=females, U=unsexed)
        lfreq_sp_y_M_st <-subset(lfreq_sp_y_st,lfreq_sp_y_st$SEX==1)
        lfreq_sp_y_F_st <-subset(lfreq_sp_y_st,lfreq_sp_y_st$SEX==2)
        lfreq_sp_y_U_st <-subset(lfreq_sp_y_st,lfreq_sp_y_st$SEX==3)
        
        # Determine number of hauls of catch with lengths
        count <-length(unique(c(lfreq_sp_y_M_st$HAULJOIN,lfreq_sp_y_F_st$HAULJOIN,lfreq_sp_y_U_st$HAULJOIN)))
        
        # Identify hauls with catch but no lengths
        hls_l   <-unique(c(lfreq_sp_y_M_st$HAULJOIN,lfreq_sp_y_F_st$HAULJOIN,lfreq_sp_y_U_st$HAULJOIN))
        hls_c   <-cpue_sp_y_st$HAULJOIN[which(is.na(cpue_sp_y_st$CATCHJOIN)==FALSE)]
        hls_nol <-hls_c[which(is.na(match(hls_c,hls_l)==TRUE))]
        
        # Calc pop'n #'s in strata
        st_num <-mean(cpue_sp_y_st$NUMCPUE)*strata_st$AREA
        
        # Calc CPUE ratio among hauls
        cprat <-tapply(cpue_sp_y_st$NUMCPUE,cpue_sp_y_st$HAULJOIN,mean)/sum(cpue_sp_y_st$NUMCPUE)
        
        # Calc Total lengths sampled by haul
        n_st <-tapply(lfreq_sp_y_st$FREQUENCY,lfreq_sp_y_st$HAULJOIN,sum)
        
        # Calc sex-specific numbers at length
        n_h_M               <-tapply(lfreq_sp_y_M_st$FREQUENCY,list(lfreq_sp_y_M_st$HAULJOIN,lfreq_sp_y_M_st$LENGTH),sum)
        n_h_M[is.na(n_h_M)] <- 0
        n_h_F               <-tapply(lfreq_sp_y_F_st$FREQUENCY,list(lfreq_sp_y_F_st$HAULJOIN,lfreq_sp_y_F_st$LENGTH),sum)
        n_h_F[is.na(n_h_F)] <- 0
        n_h_U               <-tapply(lfreq_sp_y_U_st$FREQUENCY,list(lfreq_sp_y_U_st$HAULJOIN,lfreq_sp_y_U_st$LENGTH),sum)
        n_h_U[is.na(n_h_U)] <- 0
        
        # Sex-specific ratio of total
        ratio_h_M <-n_h_M/as.vector(n_st[match(as.numeric(rownames(n_h_M)),as.numeric(names(n_st)))])
        ratio_h_F <-n_h_F/as.vector(n_st[match(as.numeric(rownames(n_h_F)),as.numeric(names(n_st)))])
        ratio_h_U <-n_h_U/as.vector(n_st[match(as.numeric(rownames(n_h_U)),as.numeric(names(n_st)))])
        
        if(length(hls_nol)>0){
          
          # Estimate size comp for hauls with catch that did not sample lengths
          ratio_h_M_unk <-colSums(ratio_h_M)/count
          ratio_h_F_unk <-colSums(ratio_h_F)/count
          ratio_h_U_unk <-colSums(ratio_h_U)/count
          total         <-sum(ratio_h_M_unk,ratio_h_F_unk,ratio_h_U_unk)
          ratio_h_M_unk <-ratio_h_M_unk/total
          ratio_h_F_unk <-ratio_h_F_unk/total
          ratio_h_U_unk <-ratio_h_U_unk/total
          
          # Add unkown size com hauls to sex-specific ratio of total
          ratio_h_M_unk_add           <-matrix(ratio_h_M_unk,nrow=length(hls_nol),ncol=length(ratio_h_M_unk),byrow=TRUE)
          rownames(ratio_h_M_unk_add) <-hls_nol
          colnames(ratio_h_M_unk_add) <-colnames(ratio_h_M)
          ratio_h_M                   <-rbind(ratio_h_M,ratio_h_M_unk_add)
          ratio_h_F_unk_add           <-matrix(ratio_h_F_unk,nrow=length(hls_nol),ncol=length(ratio_h_F_unk),byrow=TRUE)
          rownames(ratio_h_F_unk_add) <-hls_nol
          colnames(ratio_h_F_unk_add) <-colnames(ratio_h_F)
          ratio_h_F                   <-rbind(ratio_h_F,ratio_h_F_unk_add)
          ratio_h_U_unk_add           <-matrix(ratio_h_U_unk,nrow=length(hls_nol),ncol=length(ratio_h_U_unk),byrow=TRUE)
          rownames(ratio_h_U_unk_add) <-hls_nol
          colnames(ratio_h_U_unk_add) <-colnames(ratio_h_U)
          ratio_h_U                   <-rbind(ratio_h_U,ratio_h_U_unk_add)
        }
        # Put it all together to get numbers-at-sex-at-length by strata, and put it in results matrix
        szpop_M                                                  <-round(colSums(ratio_h_M*as.vector(cprat[match(as.numeric(rownames(ratio_h_M)),as.numeric(names(cprat)))])*st_num),digits=0)
        SZPOP_M_st[st,match(as.numeric(names(szpop_M)),lengths)] <-szpop_M
        szpop_F                                                  <-round(colSums(ratio_h_F*as.vector(cprat[match(as.numeric(rownames(ratio_h_F)),as.numeric(names(cprat)))])*st_num),digits=0)
        SZPOP_F_st[st,match(as.numeric(names(szpop_F)),lengths)] <-szpop_F
        szpop_U                                                  <-round(colSums(ratio_h_U*as.vector(cprat[match(as.numeric(rownames(ratio_h_U)),as.numeric(names(cprat)))])*st_num),digits=0)
        SZPOP_U_st[st,match(as.numeric(names(szpop_U)),lengths)] <-szpop_U
      } # End stratum loop
      
      ### Now sum up across strata and see if it matches with RACE output
      # Males
      SZPOP_M_st[is.na(SZPOP_M_st)] <- 0
      SZPOP_M_y                     <-colSums(SZPOP_M_st)
      SZPOP_M[y,]                   <-SZPOP_M_y
      
      # Females
      SZPOP_F_st[is.na(SZPOP_F_st)] <- 0
      SZPOP_F_y                     <-colSums(SZPOP_F_st)
      SZPOP_F[y,]                   <-SZPOP_F_y
      
      # Unsexed
      SZPOP_U_st[is.na(SZPOP_U_st)] <- 0
      SZPOP_U_y                     <-colSums(SZPOP_U_st)
      SZPOP_U[y,]                   <-SZPOP_U_y
      
      SZPOP_T           <-matrix(mapply(sum,SZPOP_M,SZPOP_F,SZPOP_U,MoreArgs=list(na.rm=TRUE)),ncol=length(lengths))
      colnames(SZPOP_T) <-as.character(lengths)
      rownames(SZPOP_T) <-as.character(yrs)
      
      # Set Size pop'n ests for age pop'n est script
      # sizepop_sp_y           <-cbind(lengths,SZPOP_M_y,SZPOP_F_y,SZPOP_U_y)
      # colnames(sizepop_sp_y) <-c("LENGTH","MALES","FEMALES","UNSEXED")
      # sizepop_sp_y           <-as.daa.frame(sizepop_sp_y)
      
    } # end year loop for length comps
  } # end species loop for length comps
  
  for(bm in 1:length(submethod)){
    pathR <-paste(SzACResults,"/SzAC Results_",method,"_",submethod[bm],sep="")
    dir.create(file.path(paste(SzACResults,"/SzAC Results_",method,"_",submethod[bm],sep="")),showWarnings = FALSE)
    
    if(submethod[bm]=="otos"){
      samp_change_tows <-c(1)
      samp_change_otos <-c(0.33,0.67,1,1.33,1.67)
      samp_change      <-c(0.33,0.67,1,1.33,1.67)
    }
    if(submethod[bm]=="tows"){
      samp_change_tows <-c(0.33,0.67,1,1.33,1.67)
      samp_change_otos <-c(1)
      samp_change      <-c(0.33,0.67,1,1.33,1.67)
    }
    if(submethod[bm]=="both"){
      samp_change_tows <-c(0.33,0.67,1,1.33,1.67)
      samp_change_otos <-c(0.33,0.67,1,1.33,1.67)
      samp_change      <-c(0.33,0.67,1,1.33,1.67)
    }

  P_at_b_alt      <-list()
  age_count_M_alt <-list()
  age_count_F_alt <-list()
  age_count_U_alt <-list()
  tow_count_alt   <-list()
  nomSS_alt       <-list()
  samp_suite_alt  <-list()
  spec_list       <-list()
  specimen_alt    <-list()
  
  for(alt in 1:length(samp_change)){
    # lfreq        <-read.csv(paste0(pathD,"/lfreq_",stock,"_",species,".csv"))
    # sizepop_RACE <-read.csv(paste0(pathD,"/sizepop_",stock,"_",species,".csv"))
    agepop_RACE  <-read.csv(paste0(pathD,"/agepop_",stock,"_",species,".csv"))
    # cpue         <-read.csv(paste0(pathD,"/CPUE_",stock,"_",species,".csv"))
    # strata       <-read.csv(paste0(pathD,"/strata_",stock,".csv"))
    specimen     <-read.csv(paste0(pathD,"/specimen_",stock,"_",species,".csv"))
    
    specimen                   <-subset(specimen,is.na(specimen$AGE)==FALSE)
    specimen$SPECIMENID_unique <-seq(1:nrow(specimen))
    specimen                   <-specimen[which(specimen$AGE>=firstAC),]
    specimen[agecol]           <-lapply(specimen[agecol], function(x) ifelse(x>plusgroup,plusgroup,x))
    
    specimen_boot_save <-list()
    samp_suite      <-list()
    nomSS_top       <-list()
    tow_count_top   <-list()
    age_count_M_top <-list()
    age_count_F_top <-list()
    age_count_U_top <-list()
    oto_count_top   <-list()
    P_at_b_top      <-list()
    
    for(bt in 1:n_boot_top){
      set.seed(bootest.seed[bt])
      # samp_suite <-plyr::compact(samp_suite)
      # if(length(samp_suite)==0){bt<-1}
      # if(length(samp_suite)>0){bt<-length(samp_suite)+1}
      yrs     <-sort(unique(specimen$YEAR))
      
      for(y in 1:length(yrs)){
        specimen_y     <-subset(specimen,specimen$YEAR==yrs[y])
        
      if(OTOtow_redist==TRUE){
        specimen_ototow    <- data.frame()
        specimen_tows      <- unique(specimen_y$HAULJOIN)
        if(length(samp_change_tows)>1){specimen_TOWresamp <-sample(as.factor(specimen_tows),
                                                                         size=round(length(specimen_tows)*samp_change_tows[alt]),replace=resamp)}
        if(length(samp_change_tows)==1){specimen_TOWresamp <-sample(as.factor(specimen_tows),
                                                                          size=round(length(specimen_tows)*samp_change_tows),replace=resamp)}
        
        tow_freq  <-data.frame(table(specimen_TOWresamp))
        tow_count <-sum(tow_freq$Freq)
        
        for(t in 1:length(specimen_TOWresamp)){
          TOWgrab      <- subset(specimen_y,specimen_y$HAULJOIN==specimen_TOWresamp[t])
          TOWgrab_otos <- unique(TOWgrab$SPECIMENID_unique)
          
          if(length(samp_change_otos)>1){specimen_resamp <- sample(as.factor(TOWgrab_otos),size=ceiling(length(TOWgrab_otos)*samp_change_otos[alt]),replace=resamp)}
          if(length(samp_change_otos)==1){specimen_resamp <- sample(as.factor(TOWgrab_otos),size=ceiling(length(TOWgrab_otos)*samp_change_otos),replace=resamp)}
          oto_freq             <- data.frame(table(specimen_resamp))
          if(nrow(oto_freq)==0){next}
          oto_freq_sing    <-oto_freq[which(oto_freq$Freq==1),]
          oto_freq_mult    <-oto_freq[which(oto_freq$Freq>1),]
          
          if(nrow(oto_freq_sing)>0){
            specimen_sing <-matrix(NA,sum(oto_freq_sing$Freq),ncol(specimen_y),
                                        dimnames=list(1:sum(oto_freq_sing$Freq),colnames(specimen_y)))}
          
          if(nrow(oto_freq_mult)>0){
            mult_dim <-list()
            for(i in oto_freq_mult$specimen_resamp){
              mult_dim[[i]]  <- oto_freq_mult[which(oto_freq_mult$specimen_resamp==i),
              ]$Freq*nrow(specimen_y[which(specimen_y$SPECIMENID_unique==i),])}
            mult_dim_sum       <-Reduce("+",do.call(c,mult_dim))
            specimen_mult <-matrix(NA,mult_dim_sum,ncol(specimen_y),
                                        dimnames=list(seq(1:mult_dim_sum),colnames(specimen_y)))}
          
          if(nrow(oto_freq_sing)>0){
            bootrow=0
            for(i in oto_freq_sing$specimen_resamp){
              whichrows                    = which(i==specimen_y[,"SPECIMENID_unique"])
              bootrow                      = max(bootrow) + 1:length(whichrows)
              specimen_sing[bootrow,] = as.matrix(specimen_y[whichrows,])
            }}
          
          if(nrow(oto_freq_mult)>0){
            bootrow=0
            for(i in oto_freq_mult$specimen_resamp){
              whichrows = which(i==specimen_y[,"SPECIMENID_unique"])
              reps      <-oto_freq_mult[which(oto_freq_mult$specimen_resamp==i),2]
              for(ii in 1:reps){
                bootrow                      = max(bootrow) + 1:length(whichrows)
                specimen_mult[bootrow,] = as.matrix(specimen_y[whichrows,])
              }}}
          
          if(nrow(oto_freq_mult)>0 & nrow(oto_freq_sing)>0){
            specimen_temp  <-rbind(specimen_sing,specimen_mult)}
          if(nrow(oto_freq_mult)==0 & nrow(oto_freq_sing)>0){specimen_temp <-rbind(specimen_sing)}
          if(nrow(oto_freq_sing)==0 & nrow(oto_freq_mult)>0){specimen_temp <-rbind(specimen_mult)}
          
          specimen_temp                                <-as.data.frame(specimen_temp)
          specimen_temp[,(ncol(specimen_temp)+1)]      <-rep(t,nrow(specimen_temp))
          colnames(specimen_temp)[ncol(specimen_temp)] <-"HAULID_unique"
          specimen_temp$HAULID_unique                  <-rep(t,nrow(specimen_temp))
      
          specimen_ototow <- rbind.data.frame(specimen_ototow,specimen_temp)
        }
        specimen_bt <-specimen_ototow[,c(3,16,36,37,39,55,56,57)]
        specimen_bt <-apply(specimen_bt,2,as.numeric)
        specimen_bt <-as.data.frame(specimen_bt)
      }
      if(y==1){specimen_boot <-specimen_bt}
      if(y>1){specimen_boot <-rbind(specimen_boot,specimen_bt)}
    }
    
    specimen_boot_save[[bt]] <-specimen_boot

    # Get data parameters together
    if(length(species)>1){species <-sort(unique(specimen_boot$SPECIES_CODE))}
    yrs     <-sort(unique(specimen_boot$YEAR))
    sex     <-sort(unique(specimen_boot$SEX))
    stratum <-sort(unique(strata$STRATUM))
    
    Msize_boot_list <-list()
    Mage_boot_list  <-list()
    Fsize_boot_list <-list()
    Fage_boot_list  <-list()
    Usize_boot_list <-list()
    Uage_boot_list  <-list()
    Tsize_boot_list <-list()
    Tage_boot_list  <-list()
    
    nomSS_b       <-list()
    tow_count_b   <-list()
    age_count_M_b <-list()
    age_count_F_b <-list()
    age_count_U_b <-list()
    oto_count_b   <-list()
    
    #########################
    # Start boot loop
    for(b in 1:n_boot){
      
      #########################
      # Loop thru species
      for(sp in 1:length(species)){
        
        # lfreq_sp        <-subset(lfreq,lfreq$SPECIES_CODE==species[sp])
        # sizepop_RACE_sp <-subset(sizepop_RACE,sizepop_RACE$SPECIES_CODE==species[sp])
        if(length(species)>1){specimen_sp  <-subset(specimen,specimen$SPECIES_CODE==species[sp])}
        if(length(species)==1){specimen_sp <-specimen_boot}
        specimen_sp$SPECIMENID_unique <-seq(1:nrow(specimen_sp))
        agepop_RACE_sp  <-subset(agepop_RACE,agepop_RACE$SPECIES_CODE==species[sp])
        
        # Set up results matrices (length then age comp)
        # lengths           <-sort(unique(lfreq_sp$LENGTH))
        ages               <-sort(unique(specimen_sp$AGE))
        AGEPOP_M           <-matrix(nrow=length(yrs),ncol=length(ages))
        colnames(AGEPOP_M) <-as.character(ages)
        rownames(AGEPOP_M) <-as.character(yrs)
        AGEPOP_F           <-matrix(nrow=length(yrs),ncol=length(ages))
        colnames(AGEPOP_F) <-as.character(ages)
        rownames(AGEPOP_F) <-as.character(yrs)
        AGEPOP_U           <-matrix(nrow=length(yrs),ncol=length(ages))
        colnames(AGEPOP_U) <-as.character(ages)
        rownames(AGEPOP_U) <-as.character(yrs)
        
        nomSS_y     <-list()
        tow_count_y <-list()
        age_count_M <-list()
        age_count_F <-list()
        age_count_U <-list()
        oto_count_y <-list()
        
        #########################
        # Loop thru years
        for(y in 1:length(yrs)){
          
          sizepop_sp_y      <-as.data.frame(cbind(lengths,SZPOP_M[y,],SZPOP_F[y,],SZPOP_U[y,]))
          colnames(sizepop_sp_y) <-c("LENGTHS","MALES","FEMALES","UNSEXED")
          rownames(sizepop_sp_y) <-NULL
          sizepop_RACE_sp_y <-subset(sizepop_RACE_sp,sizepop_RACE_sp$YEAR==yrs[y])
          specimen_sp_y     <-subset(specimen_sp,specimen_sp$YEAR==yrs[y])
          agepop_RACE_sp_y  <-subset(agepop_RACE_sp,agepop_RACE_sp$SURVEY_YEAR==yrs[y])
          
          ########################################################################################
          # Estimate pop'n @ age
          ########################################################################################
          # specimen_sp_y<-subset(specimen_sp,specimen_sp$YEAR==yrs[y])
          
          ### Tow & specimen-per-tow resampling: this changes % of tows and % of otos per tow
          ### Tow & specimen resampling: working through list of resampled tow IDs, grabbing resampled ind info 
          if(OTOtow_redist==TRUE){
            specimen_sp_y_ototow    <- data.frame()
            specimen_sp_y_tows      <- unique(specimen_sp_y$HAULID_unique)
            specimen_sp_y_TOWresamp <- sample(as.factor(specimen_sp_y_tows),size=round(length(specimen_sp_y_tows)),
                                                                            replace=resamp)

            tow_freq         <-data.frame(table(specimen_sp_y_TOWresamp))
            tow_count_y[[y]] <-sum(tow_freq$Freq)

            for(t in 1:length(specimen_sp_y_TOWresamp)){
              TOWgrab      <- subset(specimen_sp_y,specimen_sp_y$HAULID_unique==specimen_sp_y_TOWresamp[t])
              TOWgrab_otos <- unique(TOWgrab$SPECIMENID_unique)

              specimen_sp_y_resamp <- sample(as.factor(TOWgrab_otos),size=ceiling(length(TOWgrab_otos)),
                                                                     replace=resamp)
              oto_freq             <- data.frame(table(specimen_sp_y_resamp))
              if(nrow(oto_freq)==0){next}
              oto_freq_sing    <-oto_freq[which(oto_freq$Freq==1),]
              oto_freq_mult    <-oto_freq[which(oto_freq$Freq>1),]

              if(nrow(oto_freq_sing)>0){
                specimen_sp_y_sing <-matrix(NA,sum(oto_freq_sing$Freq),ncol(specimen_sp_y),
                                            dimnames=list(1:sum(oto_freq_sing$Freq),colnames(specimen_sp_y)))}

              if(nrow(oto_freq_mult)>0){
                mult_dim <-list()
                for(i in oto_freq_mult$specimen_sp_y_resamp){
                  mult_dim[[i]]  <- oto_freq_mult[which(oto_freq_mult$specimen_sp_y_resamp==i),
                  ]$Freq*nrow(specimen_sp_y[which(specimen_sp_y$SPECIMENID_unique==i),])}
                mult_dim_sum       <-Reduce("+",do.call(c,mult_dim))
                specimen_sp_y_mult <-matrix(NA,mult_dim_sum,ncol(specimen_sp_y),
                                            dimnames=list(seq(1:mult_dim_sum),colnames(specimen_sp_y)))}

              if(nrow(oto_freq_sing)>0){
                bootrow=0
                for(i in oto_freq_sing$specimen_sp_y_resamp){
                  whichrows                    = which(i==specimen_sp_y[,"SPECIMENID_unique"])
                  bootrow                      = max(bootrow) + 1:length(whichrows)
                  specimen_sp_y_sing[bootrow,] = as.matrix(specimen_sp_y[whichrows,])
                }
                }

              if(nrow(oto_freq_mult)>0){
                bootrow=0
                for(i in oto_freq_mult$specimen_sp_y_resamp){
                  whichrows = which(i==specimen_sp_y[,"SPECIMENID_unique"])
                  reps      <-oto_freq_mult[which(oto_freq_mult$specimen_sp_y_resamp==i),2]
                  for(ii in 1:reps){
                    bootrow                      = max(bootrow) + 1:length(whichrows)
                    specimen_sp_y_mult[bootrow,] = as.matrix(specimen_sp_y[whichrows,])
                  }}}

              if(nrow(oto_freq_mult)>0 & nrow(oto_freq_sing)>0){
                specimen_sp_y_temp  <-rbind(specimen_sp_y_sing,specimen_sp_y_mult)}
              if(nrow(oto_freq_mult)==0 & nrow(oto_freq_sing)>0){specimen_sp_y_temp <-rbind(specimen_sp_y_sing)}
              if(nrow(oto_freq_sing)==0 & nrow(oto_freq_mult)>0){specimen_sp_y_temp <-rbind(specimen_sp_y_mult)}

              specimen_sp_y_ototow <- rbind.data.frame(specimen_sp_y_ototow,specimen_sp_y_temp)
            }
            specimen_sp_y <-specimen_sp_y_ototow #[,c(3,16,36,37,39,55,56)]
            specimen_sp_y <-apply(specimen_sp_y,2,as.numeric)
            specimen_sp_y <-as.data.frame(specimen_sp_y)
          }
          
          
          ### Specimen per tow resampling: user can change % of otos per tow
          ### Specimen resampling & grabbing ind info
          if(spec_redist==TRUE){
            otorows=0
            specimen_sp_y_tows <- unique(specimen_sp_y$HAULJOIN)
            specimen_sp_y_otos <- data.frame()
            
            # specimen_sp_y_otos <- matrix(NA,ceiling(nrow(specimen_sp_y)*samp_change[alt]),ncol(specimen_sp_y),
            #                              dimnames=list(seq(1:ceiling(nrow(specimen_sp_y)*samp_change[alt])),
            #                                            colnames(specimen_sp_y)))
            
            
            tow_freq         <-data.frame(table(specimen_sp_y_tows))
            tow_count_y[[y]] <-sum(tow_freq$Freq)
            
            for(t in 1:length(specimen_sp_y_tows)){
              specimen_sp_y_tow    <- subset(specimen_sp_y,specimen_sp_y$HAULJOIN==specimen_sp_y_tows[t])
              towUID               <- unique(specimen_sp_y_tow$SPECIMENID_unique)
              if(length(samp_change_otos)>1){specimen_sp_y_resamp <- sample(as.factor(towUID),size=ceiling(length(towUID)*samp_change_otos[alt]),replace=resamp)}
              if(length(samp_change_otos)==1){specimen_sp_y_resamp <- sample(as.factor(towUID),size=ceiling(length(towUID)*samp_change_otos),replace=resamp)}
              oto_freq             <- data.frame(table(specimen_sp_y_resamp))
              if(nrow(oto_freq)==0){next}
              oto_freq_sing    <-oto_freq[which(oto_freq$Freq==1),]
              oto_freq_mult    <-oto_freq[which(oto_freq$Freq>1),]
              
              # oto_count_t[[t]] <-sum(oto_freq$Freq)
              
              if(nrow(oto_freq_sing)>0){
                # sing_dim <-list()
                # for(i in oto_freq_sing$specimen_sp_y_resamp){
                # sing_dim[[i]]  <- oto_freq_sing[which(oto_freq_sing$specimen_sp_y_resamp==i),
                # ]$Freq*nrow(specimen_sp_y[which(specimen_sp_y$SPECIMENID_unique==i),])
                # }
                # sing_dim_sum       <-Reduce("+",do.call(c,sing_dim))
                # specimen_sp_y_sing <-matrix(NA,sing_dim_sum,ncol(specimen_sp_y),
                # dimnames=list(1:sing_dim_sum,colnames(specimen_sp_y)))
                specimen_sp_y_sing <-matrix(NA,sum(oto_freq_sing$Freq),ncol(specimen_sp_y),
                                            dimnames=list(1:sum(oto_freq_sing$Freq),colnames(specimen_sp_y)))
              }
              
              if(nrow(oto_freq_mult)>0){
                mult_dim <-list()
                for(i in oto_freq_mult$specimen_sp_y_resamp){
                  mult_dim[[i]]  <- oto_freq_mult[which(oto_freq_mult$specimen_sp_y_resamp==i),
                  ]$Freq*nrow(specimen_sp_y[which(specimen_sp_y$SPECIMENID_unique==i),])
                }
                mult_dim_sum       <-Reduce("+",do.call(c,mult_dim))
                specimen_sp_y_mult <-matrix(NA,mult_dim_sum,ncol(specimen_sp_y),
                                            dimnames=list(seq(1:mult_dim_sum),colnames(specimen_sp_y)))}
              
              if(nrow(oto_freq_sing)>0){
                bootrow=0
                for(i in oto_freq_sing$specimen_sp_y_resamp){
                  whichrows                    = which(i==specimen_sp_y[,"SPECIMENID_unique"])
                  bootrow                      = max(bootrow) + 1:length(whichrows)
                  specimen_sp_y_sing[bootrow,] = as.matrix(specimen_sp_y[whichrows,])
                }}
              
              if(nrow(oto_freq_mult)>0){
                bootrow=0
                for(i in oto_freq_mult$specimen_sp_y_resamp){
                  whichrows = which(i==specimen_sp_y[,"SPECIMENID_unique"])
                  reps      <-oto_freq_mult[which(oto_freq_mult$specimen_sp_y_resamp==i),2]
                  for(ii in 1:reps){
                    bootrow                      = max(bootrow) + 1:length(whichrows)
                    specimen_sp_y_mult[bootrow,] = as.matrix(specimen_sp_y[whichrows,])
                  }}}
              
              if(nrow(oto_freq_mult)>0 & nrow(oto_freq_sing)>0){
                specimen_sp_y_temp  <-rbind(specimen_sp_y_sing,specimen_sp_y_mult)}
              if(nrow(oto_freq_mult)==0 & nrow(oto_freq_sing)>0){specimen_sp_y_temp <-rbind(specimen_sp_y_sing)}
              if(nrow(oto_freq_sing)==0 & nrow(oto_freq_mult)>0){specimen_sp_y_temp <-rbind(specimen_sp_y_mult)}
              # specimen_sp_y <-as.data.frame(specimen_sp_y[complete.cases(specimen_sp_y[,3]),])
              
              # otorows                      = max(otorows)+1:nrow(specimen_sp_y_temp)
              # specimen_sp_y_otos[otorows,] = as.matrix(specimen_sp_y_temp)
              
              specimen_sp_y_otos <- rbind.data.frame(specimen_sp_y_otos,specimen_sp_y_temp)
              
            }
            specimen_sp_y <-specimen_sp_y_otos[,c(3,16,36,37,39,55,56)]
            # mode(specimen_sp_y) = "numeric"
            specimen_sp_y <-apply(specimen_sp_y,2,as.numeric)
            specimen_sp_y <-as.data.frame(specimen_sp_y)
          }
          
          
          ### Specimen tow resampling: user can change % of tows by year
          ### Tow resampling & grabbing info from replicate tows
          if(tow_redist==TRUE){
            specimen_sp_y_tows   <- unique(specimen_sp_y$HAULJOIN)
            
            if(length(samp_change_tows)>1){specimen_sp_y_resamp <- sample(as.factor(specimen_sp_y_tows),
                                           size=round(length(specimen_sp_y_tows)*samp_change_tows[alt]),
                                           replace=resamp)}
            if(length(samp_change_tows)==1){specimen_sp_y_resamp <- sample(as.factor(specimen_sp_y_tows),
                                                                          size=round(length(specimen_sp_y_tows)*samp_change_tows),
                                                                          replace=resamp)}
            
            tow_freq         <-data.frame(table(specimen_sp_y_resamp))
            tow_freq_sing    <-tow_freq[which(tow_freq$Freq==1),]
            tow_freq_mult    <-tow_freq[which(tow_freq$Freq>1),]
            
            tow_count_y[[y]] <-sum(tow_freq$Freq)
            
            if(nrow(tow_freq_sing)>0){
              sing_dim <-list()
              for(i in tow_freq_sing$specimen_sp_y_resamp){
                sing_dim[[i]]  <- tow_freq_sing[which(tow_freq_sing$specimen_sp_y_resamp==i),
                ]$Freq*nrow(specimen_sp_y[which(specimen_sp_y$HAULJOIN==i),])
              }
              sing_dim_sum       <-Reduce("+",do.call(c,sing_dim))
              specimen_sp_y_sing <-matrix(NA,sing_dim_sum,ncol(specimen_sp_y),
                                          dimnames=list(1:sing_dim_sum,colnames(specimen_sp_y)))}
            
            if(nrow(tow_freq_mult)>0){
              mult_dim <-list()
              for(i in tow_freq_mult$specimen_sp_y_resamp){
                mult_dim[[i]]  <- tow_freq_mult[which(tow_freq_mult$specimen_sp_y_resamp==i),
                ]$Freq*nrow(specimen_sp_y[which(specimen_sp_y$HAULJOIN==i),])
              }
              mult_dim_sum       <-Reduce("+",do.call(c,mult_dim))
              specimen_sp_y_mult <-matrix(NA,mult_dim_sum,ncol(specimen_sp_y),
                                          dimnames=list(seq(1:mult_dim_sum),colnames(specimen_sp_y)))}
            
            if(nrow(tow_freq_sing)>0){
              bootrow=0
              for(i in tow_freq_sing$specimen_sp_y_resamp){
                whichrows                    = which(i==specimen_sp_y[,"HAULJOIN"])
                bootrow                      = max(bootrow) + 1:length(whichrows)
                specimen_sp_y_sing[bootrow,] = as.matrix(specimen_sp_y[whichrows,])
              }}
            
            if(nrow(tow_freq_mult)>0){
              bootrow=0
              for(i in tow_freq_mult$specimen_sp_y_resamp){
                whichrows = which(i==specimen_sp_y[,"HAULJOIN"])
                reps      <-tow_freq_mult[which(tow_freq_mult$specimen_sp_y_resamp==i),2]
                for(ii in 1:reps){
                  bootrow                      = max(bootrow) + 1:length(whichrows)
                  specimen_sp_y_mult[bootrow,] = as.matrix(specimen_sp_y[whichrows,])
                }}}
            
            if(nrow(tow_freq_mult)>0 & nrow(tow_freq_sing)>0){
              specimen_sp_y  <-rbind(specimen_sp_y_sing,specimen_sp_y_mult)}
            if(nrow(tow_freq_mult)==0 & nrow(tow_freq_sing)>0){specimen_sp_y <-rbind(specimen_sp_y_sing)}
            if(nrow(tow_freq_sing)==0 & nrow(tow_freq_mult)>0){specimen_sp_y <-rbind(specimen_sp_y_mult)}
            # specimen_sp_y <-as.data.frame(specimen_sp_y[complete.cases(specimen_sp_y[,3]),])
            # specimen_sp_y <-as.data.frame(specimen_sp_y)
            
            specimen_sp_y <-specimen_sp_y[,c(3,16,36,37,39,55,56)]
            mode(specimen_sp_y) = "numeric"
            specimen_sp_y <-as.data.frame(specimen_sp_y)
          }
          
          
          # if(tow_redist==FALSE){tow_count_y[[y]]<-length(unique(specimen_sp_y$HAULJOIN))}
          nomSS_y[[y]]   <-nrow(specimen_sp_y)
          spec_list[[y]] <-specimen_sp_y
          
          #########################
          # Loop thru sex
          for(sx in 1:length(sex)){
            
            specimen_sp_y_sx    <-subset(specimen_sp_y,specimen_sp_y$SEX==sex[sx])
            agepop_RACE_sp_y_sx <-subset(agepop_RACE_sp_y,agepop_RACE_sp_y$SEX==sex[sx])
            
            # Remove matrices to wipe clean each loop
            if(sx==1 & exists('pop_age_est_M')==TRUE)
              rm(pop_age_est_M)
            if(sx==2 & exists('pop_age_est_F')==TRUE)
              rm(pop_age_est_F)
            if(sx==3 & exists('pop_age_est_U')==TRUE)
              rm(pop_age_est_U)
            
            # Test if theres specimen data for particular sex
            if(length(specimen_sp_y_sx$SEX) == 0) {
              # cat(paste("No specimen data for sex", sx, "\n"))
              next
            }
            
            # If sex unknown and there is specimen data then use all specimen data
            if(sx==3)
              specimen_sp_y_sx<-specimen_sp_y
            
            # If there is no sizecomp data, we are wasting our time
            if(sex[sx] == 1 & sum(sizepop_sp_y$MALES)==0) {
              cat(paste("No sizecomp data for sex", sx," & year",y, "\n"))
              next
            }
            if(sex[sx] == 2 & sum(sizepop_sp_y$FEMALES)==0) {
              cat(paste("No sizecomp data for sex", sx," & year",y, "\n"))
              next
            }
            if(sex[sx] == 3 & sum(sizepop_sp_y$UNSEXED)==0) {
              # cat(paste("No sizecomp data for sex", sx, "\n"))
              next
            }
            
            # Get vector of possible lengths
            lenlist<-seq(from = min(sizepop_sp_y$LENGTH), to = max(sizepop_sp_y$LENGTH), by = 10)
            
            # Get number of ages by length and age
            age_num <- tapply(specimen_sp_y_sx$AGE, list(specimen_sp_y_sx$LENGTH, specimen_sp_y_sx$AGE), length)
            age_num[is.na(age_num)] <- 0
            
            # Turn these into fractions
            age_frac <- age_num/apply(age_num, 1, sum)
            
            # Find lengths from age data where there is no sizecomp data
            no.lengths <-unique(sort(specimen_sp_y_sx$LENGTH))[is.na(match(unique(sort(specimen_sp_y_sx$LENGTH)), sizepop_sp_y$LENGTH))]
            
            # Find lengths from sizecomp data where there is no age data.
            no.ages <- sizepop_sp_y$LENGTH[is.na(match(sizepop_sp_y$LENGTH, unique(specimen_sp_y_sx$LENGTH)))]
            if(length(no.ages) == 0)
              no.ages <- sizepop_sp_y$LENGTH
            no.age.sizecomp <- sizepop_sp_y[match(no.ages, sizepop_sp_y$LENGTH),]
            
            # Nothing else we can do when there are age data with no sizecomp data, so get rid of these records
            if(length(no.lengths)>0){
              age_frac <- age_frac[is.na(match(as.numeric(dimnames(age_frac)[[1]]), no.lengths)),  ]
            }
            
            if(nrow(age_frac)==nrow(age_num)){
              if(sx==1){age_count_M[[y]] <-sum(age_num)}
              if(sx==2){age_count_F[[y]] <-sum(age_num)}
              if(sx==3){age_count_U[[y]] <-sum(age_num)}
            }
            
            if(nrow(age_frac)!=nrow(age_num)){
              if(sx==1){
                age_num_trim     <-age_num[,match(as.numeric(colnames(age_num)),as.numeric(colnames(age_frac)))]
                age_count_M[[y]] <-sum(age_num_trim)}
              if(sx==2){
                age_num_trim     <-age_num[,match(as.numeric(colnames(age_num)),as.numeric(colnames(age_frac)))]
                age_count_F[[y]] <-sum(age_num_trim)}
              if(sx==3){          
                age_num_trim     <-age_num[,match(as.numeric(colnames(age_num)),as.numeric(colnames(age_frac)))]
                age_count_U[[y]] <-sum(age_num_trim)}
            }
            
            
            # Estimate numbers by age and length
            if(sex[sx] ==1)
              pop_age_est_M <- age_frac * sizepop_sp_y$MALES[match(as.numeric(dimnames(age_frac)[[1]]), 
                                                                   as.numeric(sizepop_sp_y$LENGTH), nomatch = 0, 
                                                                   incomparables = no.lengths)]
            if(sex[sx] ==2)
              pop_age_est_F <- age_frac * sizepop_sp_y$FEMALES[match(as.numeric(dimnames(age_frac)[[1]]), 
                                                                     as.numeric(sizepop_sp_y$LENGTH), nomatch = 0, 
                                                                     incomparables = no.lengths)]
            if(sex[sx] ==3)
              pop_age_est_U <- age_frac * sizepop_sp_y$UNSEXED[match(as.numeric(dimnames(age_frac)[[1]]), 
                                                                     as.numeric(sizepop_sp_y$LENGTH), nomatch = 0, 
                                                                     incomparables = no.lengths)]
            
            #########################
            # End sex loop
          }
          
          # Now sum up the numbers at age for all lengths and remove any 0s, and check to see if matches with RACE output
          
          # Males
          if(exists("pop_age_est_M")==TRUE){if(length(pop_age_est_M)>0){
            age_est_M                                            <-apply(pop_age_est_M, 2,sum)
            if(length(which(age_est_M==0))>0)
              age_est_M                                          <-age_est_M[-which(age_est_M==0)]
            AGEPOP_M[y,match(as.numeric(names(age_est_M)),ages)] <-age_est_M
            AGEPOP_M[y,is.na(AGEPOP_M[y,])]                      <-0
          }}
          
          # Females
          if(exists("pop_age_est_F")==TRUE){if(length(pop_age_est_F)>0){
            age_est_F                                            <-apply(pop_age_est_F, 2,sum)
            if(length(which(age_est_F==0))>0)
              age_est_F                                          <-age_est_F[-which(age_est_F==0)]
            AGEPOP_F[y,match(as.numeric(names(age_est_F)),ages)] <-age_est_F
            AGEPOP_F[y,is.na(AGEPOP_F[y,])]                      <-0
          }}
          
          # Unsexed
          if(exists("pop_age_est_U")==TRUE){if(length(pop_age_est_U)>0){
            age_est_U                                            <-apply(pop_age_est_U, 2,sum)
            if(length(which(age_est_U==0))>0)
              age_est_U                                          <-age_est_U[-which(age_est_U==0)]
            AGEPOP_U[y,match(as.numeric(names(age_est_U)),ages)] <-age_est_U
            AGEPOP_U[y,is.na(AGEPOP_U[y,])]                      <-0
          }}
          
          
          #########################
          # End year loop
        }
        
        age_count_M_b[[b]] <-age_count_M
        age_count_F_b[[b]] <-age_count_F
        age_count_U_b[[b]] <-age_count_U
        
        # Finalize results matrices
        AGEPOP_T           <-matrix(mapply(sum,AGEPOP_M,AGEPOP_F,AGEPOP_U,MoreArgs=list(na.rm=TRUE)),ncol=length(ages))
        colnames(AGEPOP_T) <-as.character(ages)
        rownames(AGEPOP_T) <-as.character(yrs)
        
        #########################
        # End species loop
      }
      
      # if(any(rowSums(AGEPOP_T>0)==1)==TRUE){break}
      
      # Msize_boot_list[[b]] <-SZPOP_M
      Mage_boot_list[[b]]  <-AGEPOP_M
      
      # Fsize_boot_list[[b]] <-SZPOP_F
      Fage_boot_list[[b]]  <-AGEPOP_F
      
      # Usize_boot_list[[b]] <-SZPOP_U
      Uage_boot_list[[b]]  <-AGEPOP_U
      
      # Tsize_boot_list[[b]] <-SZPOP_T
      Tage_boot_list[[b]]  <-AGEPOP_T
      
      names(tow_count_y) <-yrs
      tow_count_b[[b]]   <-tow_count_y
      names(nomSS_y)     <-yrs
      nomSS_b[[b]]       <-nomSS_y
      
      #########################
      # End boot loop
      # print("alt");print(alt);print("boot");print(bt);print("exp_boot");print(b);
    }
    
    age_count_M_top[[bt]] <-age_count_M_b
    age_count_F_top[[bt]] <-age_count_F_b
    age_count_U_top[[bt]] <-age_count_U_b
    tow_count_top[[bt]]   <-tow_count_b
    nomSS_top[[bt]]       <-nomSS_b
    
    ########################################################################################
    # Standardizes Year x Age matrix to proportional values across age classes w/i each year
    standardize = function(mat) t(mat) / rep(1,ncol(mat))%o%rowSums(mat)
    
    ### Wrangle calcs
    Mage_boot_num   <-Reduce("+",Mage_boot_list)/length(Mage_boot_list)
    Mage_boot_prop  <-standardize(Mage_boot_num)
    Mage_boot_prop  <-t(Mage_boot_prop)
    # Msize_boot_num  <-Reduce("+",Msize_boot_list)/length(Msize_boot_list)
    
    Fage_boot_num   <-Reduce("+",Fage_boot_list)/length(Fage_boot_list)
    Fage_boot_prop  <-standardize(Fage_boot_num)
    Fage_boot_prop  <-t(Fage_boot_prop)
    # Fsize_boot_num  <-Reduce("+",Fsize_boot_list)/length(Fsize_boot_list)
    
    Uage_boot_num   <-Reduce("+",Uage_boot_list)/length(Uage_boot_list)
    Uage_boot_prop  <-standardize(Uage_boot_num)
    Uage_boot_prop  <-t(Uage_boot_prop)
    # Usize_boot_num  <-Reduce("+",Usize_boot_list)/length(Usize_boot_list)
    
    Tage_boot_num   <-Reduce("+",Tage_boot_list)/length(Tage_boot_list)
    Tage_boot_num_t <-t(Tage_boot_num)
    Tage_boot_prop  <-standardize(Tage_boot_num)
    Tage_boot_prop  <-t(Tage_boot_prop)
    # Tsize_boot_num  <-Reduce("+",Tsize_boot_list)/length(Tsize_boot_list)
    
    if(n_boot>1){
      Mage_boot_list_prop        <-list()
      for(i in 1:n_boot){
        it                       <-standardize(Mage_boot_list[[i]])
        Mage_boot_list_prop[[i]] <-t(it)
      }
      
      Fage_boot_list_prop        <-list()
      for(i in 1:n_boot){
        it                       <-standardize(Fage_boot_list[[i]])
        Fage_boot_list_prop[[i]] <-t(it)
      }
      
      Uage_boot_list_prop        <-list()
      for(i in 1:n_boot){
        it                       <-standardize(Uage_boot_list[[i]])
        Uage_boot_list_prop[[i]] <-t(it)
      }
      
      Tage_boot_list_prop        <-list()
      for(i in 1:n_boot){
        it                       <-standardize(Tage_boot_list[[i]])
        Tage_boot_list_prop[[i]] <-t(it)
      }
      
      Mage_boot_prop <-Reduce("+",Mage_boot_list_prop)/length(Mage_boot_list_prop)
      Fage_boot_prop <-Reduce("+",Fage_boot_list_prop)/length(Fage_boot_list_prop)
      Uage_boot_prop <-Reduce("+",Uage_boot_list_prop)/length(Uage_boot_list_prop)
      Tage_boot_prop <-Reduce("+",Tage_boot_list_prop)/length(Tage_boot_list_prop)
      
      P_at_b_top[[bt]] <-Tage_boot_list_prop
      
    }
    
    ########################################################################################
    # Calculate input sample size as harmonic mean across boostrap samples
    # Need to run everything with no change, no resampling to get proportions calculated from original data set
    if(inputSS==TRUE){
      
      # Calculate N_bt
      calculate_n = function(boot_ct, obs_ct){
        boot_ct  = standardize(boot_ct)
        obs_ct   = standardize(obs_ct)
        num_ct   = boot_ct*(1-boot_ct)
        denom_ct = (boot_ct - obs_ct)^2
        colSums(num_ct)/colSums(denom_ct)
      }
      
      # Calculate harmonic mean of N_bt
      harmonic.mean = function(vec) 1/mean(1/vec)
      
      # Set 'observed' proportions to mean of bootstrap replicates, calc N_bt
      N_bt = matrix(NA, nrow=n_boot, ncol=length(yrs))
      colnames(N_bt) <-yrs
      
      for( bI in 1:n_boot ){
        N_bt[bI,] = t(calculate_n(boot_ct=Tage_boot_list_prop[[bI]], obs_ct=Tage_boot_prop))
      }
      
      # Calculate input sample size (harmonic mean across bootstrap samples)
      Nsamp_t = array(NA,dim=length(yrs))
      Nsamp_t = apply(N_bt, MARGIN=2, FUN=harmonic.mean )
      samp_suite[[bt]] <-Nsamp_t
      
      #########################################################################################
      } # End input ss calc
      print("alt");print(alt);print("boot");print(bt);print(samp_suite[[bt]])
    
    if(bt==10||bt==20||bt==30||bt==40||bt==50||bt==60||bt==70||bt==80||bt==90||bt==100||bt==110||bt==120||bt==130||bt==140||bt==150||bt==160||bt==170||bt==180||bt==190||bt==200){
      save(specimen_boot_save,file=paste(pathR,"/pipeline_specimen_",species,"_",method,"_",submethod[bm],"_alt",alt,"_bt",bt,sep=""))
      save(samp_suite,file=paste(pathR,"/pipeline_samp_change_",species,"_",method,"_",submethod[bm],"_alt",alt,"_bt",bt,sep=""))
      save(tow_count_top,file=paste(pathR,"/pipeline_towcount_",species,"_",method,"_",submethod[bm],"_alt",alt,"_bt",bt,sep=""))
      save(nomSS_top,file=paste(pathR,"/pipeline_nomSS_",species,"_",method,"_",submethod[bm],"_alt",alt,"_bt",bt,sep=""))}
    
    ########################################################################################
    } # End n_boot_top loop
    specimen_alt[[alt]]    <-specimen_boot_save
    P_at_b_alt[[alt]]      <-P_at_b_top
    age_count_M_alt[[alt]] <-age_count_M_top
    age_count_F_alt[[alt]] <-age_count_F_top
    age_count_U_alt[[alt]] <-age_count_U_top
    tow_count_alt[[alt]]   <-tow_count_top
    nomSS_alt[[alt]]       <-nomSS_top
    samp_suite_alt[[alt]]  <-samp_suite
  
  ########################################################################################

  } # End change_samp loop
  
  # save(specimen_boot_save,file=paste(pathR,"/pipeline_specimen_boot_save_",species,"_",method,"_",submethod,sep=""))
  # save(specimen_alt,file=paste(pathR,"/pipeline_specimen_alt_",species,"_",method,"_",submethod[bm],sep=""))
  # save(samp_suite_alt,file=paste(pathR,"/pipeline_samp_change_alt_",species,"_",method,"_",submethod[bm],sep=""))
  # save(tow_count_alt,file=paste(pathR,"/pipeline_towcount_",species,"_",method,"_",submethod[bm],sep=""))
  # save(nomSS_alt,file=paste(pathR,"/pipeline_nomSS_",species,"_",method,"_",submethod[bm],sep=""))
  # if(n_boot>1){save(P_at_b_alt,file=paste(pathR,"/pipeline_P_at_b_top_",species,"_",method,"_",submethod[bm],sep=""))}
  }
} # End getBoot loop

################################################################################################################
################################ Calc P_at for simulated datasets ##############################################
################################################################################################################

library(tidyr); library(reshape2); library(plyr); library(dplyr); library(RODBC); library(ggplot2); library(data.table)
rm(list=ls())
setwd("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/otosamp")

species   <-21740 #,30060,21740) # Enter species code; # 21740 (Pollock), 30060 (POP), 30152/30150 (Dusky); 21720 (Pacific Cod)
stock     <-'GOA' # Enter stock area; # 'AI', 'EBS', 'GOA'
method    <-"ototow" # "tow" "oto" "ototow"
submethod <-c("otos","tows") # 

# first & plus-group age classes used in data (pollock, dusky)
if(species==30152){firstAC <-4; plusgroup <-25}
if(species==21740){firstAC <-1; plusgroup <-10}
if(species==30060){firstAC <-2; plusgroup <-25}

# Define settings
agecol      <-5 # column in specimen file (individual age data) associated with age estimates
samp_change <-c(0.33,0.67,1,1.33,1.67) #
n_boot      <-200

# Set directories and read in necessary data
path  <-setwd("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/otosamp")
pathD <-paste0(path,"/Data")

# Script to estimate population #'s at size and age
comps_meth <-list()

for(m in 1:length(submethod)){
  pathR <-paste(path,"/SzAC Results_",method,"_",submethod[m],sep="")
  load(paste(pathR,"/pipeline_specimen_alt_",species,"_",method,"_",submethod[m],sep=""))
  # if(!exists("specimen_alt")){load(paste(pathR,"/pipeline_specimen_alt_",species,"_",method,"_",submethod[m],sep=""))}
  # load(paste(pathR,"/pipeline_specimen_stitch_",species,"_",method,"_",submethod,sep="")) #specimen_stitch is called specimen_main when loaded
  
  comps_boot <-list()
  comps_alt  <-list()
  
  for(alt in 1:length(samp_change)){
    
    specimen_boot <-specimen_alt[[alt]]
    # specimen_boot <-specimen_main[[alt]]
    
    for(b in 1:n_boot){
      lfreq        <-read.csv(paste0(pathD,"/lfreq_",stock,"_",species,".csv"))
      sizepop_RACE <-read.csv(paste0(pathD,"/sizepop_",stock,"_",species,".csv"))
      agepop_RACE  <-read.csv(paste0(pathD,"/agepop_",stock,"_",species,".csv"))
      cpue         <-read.csv(paste0(pathD,"/CPUE_",stock,"_",species,".csv"))
      strata       <-read.csv(paste0(pathD,"/strata_",stock,".csv"))
      # specimen                   <-read.csv(paste0(pathD,"/specimen_",stock,"_",species,".csv"))
      specimen <-specimen_boot[[b]]
      specimen                   <-subset(specimen,is.na(specimen$AGE)==FALSE)
      # specimen$SPECIMENID_unique <-seq(1:nrow(specimen))
      specimen                   <-specimen[which(specimen$AGE>=firstAC),]
      specimen[agecol]           <-lapply(specimen[agecol], function(x) ifelse(x>plusgroup,plusgroup,x))
      
      # Get data parameters together
      yrs     <-sort(unique(specimen$YEAR))
      sex     <-sort(unique(specimen$SEX))
      stratum <-sort(unique(strata$STRATUM))
      
      
      #########################
      # Loop thru species
      for(sp in 1:length(species)){
        
        lfreq_sp        <-subset(lfreq,lfreq$SPECIES_CODE==species[sp])
        sizepop_RACE_sp <-subset(sizepop_RACE,sizepop_RACE$SPECIES_CODE==species[sp])
        if(length(species)>1){specimen_sp     <-subset(specimen,specimen$SPECIES_CODE==species[sp])}
        if(length(species)==1){specimen_sp     <-specimen}
        agepop_RACE_sp  <-subset(agepop_RACE,agepop_RACE$SPECIES_CODE==species[sp])
        cpue_sp         <-subset(cpue,cpue$SPECIES_CODE==species[sp])
        
        # Set up results matrices (length then age comp)
        lengths           <-sort(unique(lfreq_sp$LENGTH))
        SZPOP_M           <-matrix(nrow=length(yrs),ncol=length(lengths))
        colnames(SZPOP_M) <-as.character(lengths)
        rownames(SZPOP_M) <-as.character(yrs)
        SZPOP_F           <-matrix(nrow=length(yrs),ncol=length(lengths))
        colnames(SZPOP_F) <-as.character(lengths)
        rownames(SZPOP_F) <-as.character(yrs)
        SZPOP_U           <-matrix(nrow=length(yrs),ncol=length(lengths))
        colnames(SZPOP_U) <-as.character(lengths)
        rownames(SZPOP_U) <-as.character(yrs)
        
        ages               <-sort(unique(specimen_sp$AGE))
        AGEPOP_M           <-matrix(nrow=length(yrs),ncol=length(ages))
        colnames(AGEPOP_M) <-as.character(ages)
        rownames(AGEPOP_M) <-as.character(yrs)
        AGEPOP_F           <-matrix(nrow=length(yrs),ncol=length(ages))
        colnames(AGEPOP_F) <-as.character(ages)
        rownames(AGEPOP_F) <-as.character(yrs)
        AGEPOP_U           <-matrix(nrow=length(yrs),ncol=length(ages))
        colnames(AGEPOP_U) <-as.character(ages)
        rownames(AGEPOP_U) <-as.character(yrs)
        
        # Set up test matrix for comparison for size/age pop'n est with RACE output
        RACEmatch_Sz           <-matrix(nrow=length(yrs),ncol=4)
        colnames(RACEmatch_Sz) <-c("Year","Male","Female","Unsexed")
        RACEmatch_Sz           <-as.data.frame(RACEmatch_Sz)
        RACEmatch_Sz$Year      <-yrs
        
        RACEmatch_Ag           <-matrix(nrow=length(yrs),ncol=4)
        colnames(RACEmatch_Ag) <-c("Year","Male","Female","Unsexed")
        RACEmatch_Ag           <-as.data.frame(RACEmatch_Ag)
        RACEmatch_Ag$Year      <-yrs
        
        #########################
        # Loop thru years
        for(y in 1:length(yrs)){
          
          lfreq_sp_y        <-subset(lfreq_sp,lfreq_sp$YEAR==yrs[y])
          cpue_sp_y         <-subset(cpue_sp,cpue_sp$YEAR==yrs[y])
          sizepop_RACE_sp_y <-subset(sizepop_RACE_sp,sizepop_RACE_sp$YEAR==yrs[y])
          specimen_sp_y     <-subset(specimen_sp,specimen_sp$YEAR==yrs[y])
          agepop_RACE_sp_y  <-subset(agepop_RACE_sp,agepop_RACE_sp$SURVEY_YEAR==yrs[y])
          
          ########################################################################################
          # Estimate pop'n @ size
          ########################################################################################
          SZPOP_M_st           <-matrix(nrow=length(stratum),ncol=length(lengths))
          rownames(SZPOP_M_st) <-stratum
          colnames(SZPOP_M_st) <-lengths
          SZPOP_F_st           <-matrix(nrow=length(stratum),ncol=length(lengths))
          rownames(SZPOP_F_st) <-stratum
          colnames(SZPOP_F_st) <-lengths
          SZPOP_U_st           <-matrix(nrow=length(stratum),ncol=length(lengths))
          rownames(SZPOP_U_st) <-stratum
          colnames(SZPOP_U_st) <-lengths
          
          #########################
          # Loop thru strata
          for(st in 1:length(stratum)){
            
            # Subset data to strata level
            strata_st<-subset(strata,strata$STRATUM==stratum[st])
            cpue_sp_y_st<-subset(cpue_sp_y,cpue_sp_y$STRATUM==stratum[st])
            hls_cpue<-unique(cpue_sp_y_st$HAULJOIN)
            lfreq_sp_y_st<-subset(lfreq_sp_y,lfreq_sp_y$HAULJOIN %in% hls_cpue)
            
            # Subset data to sex-specific (M=males, F=females, U=unsexed)
            lfreq_sp_y_M_st<-subset(lfreq_sp_y_st,lfreq_sp_y_st$SEX==1)
            lfreq_sp_y_F_st<-subset(lfreq_sp_y_st,lfreq_sp_y_st$SEX==2)
            lfreq_sp_y_U_st<-subset(lfreq_sp_y_st,lfreq_sp_y_st$SEX==3)
            
            # Determine number of hauls of catch with lengths
            count<-length(unique(c(lfreq_sp_y_M_st$HAULJOIN,lfreq_sp_y_F_st$HAULJOIN,lfreq_sp_y_U_st$HAULJOIN)))
            
            # Identify hauls with catch but no lengths
            hls_l<-unique(c(lfreq_sp_y_M_st$HAULJOIN,lfreq_sp_y_F_st$HAULJOIN,lfreq_sp_y_U_st$HAULJOIN))
            hls_c<-cpue_sp_y_st$HAULJOIN[which(is.na(cpue_sp_y_st$CATCHJOIN)==FALSE)]
            hls_nol<-hls_c[which(is.na(match(hls_c,hls_l)==TRUE))]
            
            # Calc pop'n #'s in strata
            st_num<-mean(cpue_sp_y_st$NUMCPUE)*strata_st$AREA
            
            # Calc CPUE ratio among hauls
            cprat<-tapply(cpue_sp_y_st$NUMCPUE,cpue_sp_y_st$HAULJOIN,mean)/sum(cpue_sp_y_st$NUMCPUE)
            
            # Calc Total lengths sampled by haul
            n_st<-tapply(lfreq_sp_y_st$FREQUENCY,lfreq_sp_y_st$HAULJOIN,sum)
            
            # Calc sex-specific numbers at length
            n_h_M<-tapply(lfreq_sp_y_M_st$FREQUENCY,list(lfreq_sp_y_M_st$HAULJOIN,lfreq_sp_y_M_st$LENGTH),sum)
            n_h_M[is.na(n_h_M)] <- 0
            n_h_F<-tapply(lfreq_sp_y_F_st$FREQUENCY,list(lfreq_sp_y_F_st$HAULJOIN,lfreq_sp_y_F_st$LENGTH),sum)
            n_h_F[is.na(n_h_F)] <- 0
            n_h_U<-tapply(lfreq_sp_y_U_st$FREQUENCY,list(lfreq_sp_y_U_st$HAULJOIN,lfreq_sp_y_U_st$LENGTH),sum)
            n_h_U[is.na(n_h_U)] <- 0
            
            # Sex-specific ratio of total
            ratio_h_M<-n_h_M/as.vector(n_st[match(as.numeric(rownames(n_h_M)),as.numeric(names(n_st)))])
            ratio_h_F<-n_h_F/as.vector(n_st[match(as.numeric(rownames(n_h_F)),as.numeric(names(n_st)))])
            ratio_h_U<-n_h_U/as.vector(n_st[match(as.numeric(rownames(n_h_U)),as.numeric(names(n_st)))])
            
            if(length(hls_nol)>0){
              
              # Estimate size comp for hauls with catch that did not sample lengths
              ratio_h_M_unk<-colSums(ratio_h_M)/count
              ratio_h_F_unk<-colSums(ratio_h_F)/count
              ratio_h_U_unk<-colSums(ratio_h_U)/count
              total<-sum(ratio_h_M_unk,ratio_h_F_unk,ratio_h_U_unk)
              ratio_h_M_unk<-ratio_h_M_unk/total
              ratio_h_F_unk<-ratio_h_F_unk/total
              ratio_h_U_unk<-ratio_h_U_unk/total
              
              # Add unkown size com hauls to sex-specific ratio of total
              ratio_h_M_unk_add<-matrix(ratio_h_M_unk,nrow=length(hls_nol),ncol=length(ratio_h_M_unk),byrow=TRUE)
              rownames(ratio_h_M_unk_add)<-hls_nol
              colnames(ratio_h_M_unk_add)<-colnames(ratio_h_M)
              ratio_h_M<-rbind(ratio_h_M,ratio_h_M_unk_add)
              ratio_h_F_unk_add<-matrix(ratio_h_F_unk,nrow=length(hls_nol),ncol=length(ratio_h_F_unk),byrow=TRUE)
              rownames(ratio_h_F_unk_add)<-hls_nol
              colnames(ratio_h_F_unk_add)<-colnames(ratio_h_F)
              ratio_h_F<-rbind(ratio_h_F,ratio_h_F_unk_add)
              ratio_h_U_unk_add<-matrix(ratio_h_U_unk,nrow=length(hls_nol),ncol=length(ratio_h_U_unk),byrow=TRUE)
              rownames(ratio_h_U_unk_add)<-hls_nol
              colnames(ratio_h_U_unk_add)<-colnames(ratio_h_U)
              ratio_h_U<-rbind(ratio_h_U,ratio_h_U_unk_add)
              
            }
            
            # Put it all together to get numbers-at-sex-at-length by strata, and put it in results matrix
            szpop_M<-round(colSums(ratio_h_M*as.vector(cprat[match(as.numeric(rownames(ratio_h_M)),as.numeric(names(cprat)))])*st_num),digits=0)
            SZPOP_M_st[st,match(as.numeric(names(szpop_M)),lengths)]<-szpop_M
            szpop_F<-round(colSums(ratio_h_F*as.vector(cprat[match(as.numeric(rownames(ratio_h_F)),as.numeric(names(cprat)))])*st_num),digits=0)
            SZPOP_F_st[st,match(as.numeric(names(szpop_F)),lengths)]<-szpop_F
            szpop_U<-round(colSums(ratio_h_U*as.vector(cprat[match(as.numeric(rownames(ratio_h_U)),as.numeric(names(cprat)))])*st_num),digits=0)
            SZPOP_U_st[st,match(as.numeric(names(szpop_U)),lengths)]<-szpop_U
            
            # End stratum loop
          }
          
          
          ### Now sum up across strata and see if it matches with RACE output
          # Males
          SZPOP_M_st[is.na(SZPOP_M_st)] <- 0
          SZPOP_M_y<-colSums(SZPOP_M_st)
          test_M_sz<-matrix(nrow=length(lengths),ncol=3)
          colnames(test_M_sz)<-c("Length","Calc","RACE")
          test_M_sz[,1]<-lengths
          test_M_sz[,2]<-SZPOP_M_y
          test_M_sz[match(sizepop_RACE_sp_y$LENGTH,lengths),3]<-sizepop_RACE_sp_y$MALES
          test_M_sz[is.na(test_M_sz)] <- 0
          test_M_sz<-as.data.frame(test_M_sz)
          RACEmatch_Sz$Male[y]<-max(abs(test_M_sz$Calc[which(test_M_sz$Calc>0)]-test_M_sz$RACE[which(test_M_sz$RACE>0)])/test_M_sz$RACE[which(test_M_sz$RACE>0)])
          SZPOP_M[y,]<-SZPOP_M_y
          
          # Females
          SZPOP_F_st[is.na(SZPOP_F_st)] <- 0
          SZPOP_F_y<-colSums(SZPOP_F_st)
          test_F_sz<-matrix(nrow=length(lengths),ncol=3)
          colnames(test_F_sz)<-c("Length","Calc","RACE")
          test_F_sz[,1]<-lengths
          test_F_sz[,2]<-SZPOP_F_y
          test_F_sz[match(sizepop_RACE_sp_y$LENGTH,lengths),3]<-sizepop_RACE_sp_y$FEMALES
          test_F_sz[is.na(test_F_sz)] <- 0
          test_F_sz<-as.data.frame(test_F_sz)
          RACEmatch_Sz$Female[y]<-max(abs(test_F_sz$Calc[which(test_F_sz$Calc>0)]-test_F_sz$RACE[which(test_F_sz$RACE>0)])/test_F_sz$RACE[which(test_F_sz$RACE>0)])
          SZPOP_F[y,]<-SZPOP_F_y
          
          # Unsexed
          SZPOP_U_st[is.na(SZPOP_U_st)] <- 0
          SZPOP_U_y<-colSums(SZPOP_U_st)
          # test_U_sz<-matrix(nrow=length(lengths),ncol=3)
          # colnames(test_U_sz)<-c("Length","Calc","RACE")
          # test_U_sz[,1]<-lengths
          # test_U_sz[,2]<-SZPOP_U_y
          # test_U_sz[match(sizepop_RACE_sp_y$LENGTH,lengths),3]<-sizepop_RACE_sp_y$UNSEXED
          # test_U_sz[is.na(test_U_sz)] <- 0
          # test_U_sz<-as.data.frame(test_U_sz)
          # RACEmatch_Sz$Unsexed[y]<-max(abs(test_U_sz$Calc[which(test_U_sz$Calc>0)]-test_U_sz$RACE[which(test_U_sz$RACE>0)])/test_U_sz$RACE[which(test_U_sz$RACE>0)])
          SZPOP_U[y,]<-SZPOP_U_y
          
          # Set Size pop'n ests for age pop'n est script
          sizepop_sp_y<-cbind(lengths,SZPOP_M_y,SZPOP_F_y,SZPOP_U_y)
          colnames(sizepop_sp_y)<-c("LENGTH","MALES","FEMALES","UNSEXED")
          sizepop_sp_y<-as.data.frame(sizepop_sp_y)
          
          
          ########################################################################################
          # Estimate pop'n @ age
          ########################################################################################
          #########################
          # Loop thru sex
          for(sx in 1:length(sex)){
            
            specimen_sp_y_sx    <-subset(specimen_sp_y,specimen_sp_y$SEX==sex[sx])
            agepop_RACE_sp_y_sx <-subset(agepop_RACE_sp_y,agepop_RACE_sp_y$SEX==sex[sx])
            
            # Remove matrices to wipe clean each loop
            if(sx==1 & exists('pop_age_est_M')==TRUE)
              rm(pop_age_est_M)
            if(sx==2 & exists('pop_age_est_F')==TRUE)
              rm(pop_age_est_F)
            if(sx==3 & exists('pop_age_est_U')==TRUE)
              rm(pop_age_est_U)
            
            # Test if theres specimen data for particular sex
            if(length(specimen_sp_y_sx$SEX) == 0) {
              #   cat(paste("No specimen data for sex", sx, "\n"))
              next
            }
            
            # If sex unknown and there is specimen data then use all specimen data
            if(sx==3)
              specimen_sp_y_sx<-specimen_sp_y
            
            # If there is no sizecomp data, we are wasting our time
            if(sex[sx] == 1 & sum(sizepop_sp_y$MALES)==0) {
              cat(paste("No sizecomp data for sex", sx," & year",y, "\n"))
              next
            }
            if(sex[sx] == 2 & sum(sizepop_sp_y$FEMALES)==0) {
              cat(paste("No sizecomp data for sex", sx," & year",y, "\n"))
              next
            }
            if(sex[sx] == 3 & sum(sizepop_sp_y$UNSEXED)==0) {
              #   cat(paste("No sizecomp data for sex", sx, "\n"))
              next
            }
            
            # Get vector of possible lengths
            lenlist<-seq(from = min(sizepop_sp_y$LENGTH), to = max(sizepop_sp_y$LENGTH), by = 10)
            
            # Get number of ages by length and age
            age_num <- tapply(specimen_sp_y_sx$AGE, list(specimen_sp_y_sx$LENGTH, specimen_sp_y_sx$AGE), length)
            age_num[is.na(age_num)] <- 0
            
            # Turn these into fractions
            age_frac <- age_num/apply(age_num, 1, sum)
            
            # Find lengths from age data where there is no sizecomp data
            no.lengths <-unique(sort(specimen_sp_y_sx$LENGTH))[is.na(match(unique(sort(specimen_sp_y_sx$LENGTH)), sizepop_sp_y$LENGTH))]
            
            # Find lengths from sizecomp data where there is no age data.
            no.ages <- sizepop_sp_y$LENGTH[is.na(match(sizepop_sp_y$LENGTH, unique(specimen_sp_y_sx$LENGTH)))]
            if(length(no.ages) == 0)
              no.ages <- sizepop_sp_y$LENGTH
            no.age.sizecomp <- sizepop_sp_y[match(no.ages, sizepop_sp_y$LENGTH),]
            
            # Nothing else we can do when there are age data with no sizecomp data, so get rid of these records
            if(length(no.lengths)>0){
              age_frac <- age_frac[is.na(match(as.numeric(dimnames(age_frac)[[1]]), no.lengths)),  ]
            }
            
            # Estimate numbers by age and length
            if(sex[sx] ==1)
              pop_age_est_M <- age_frac * sizepop_sp_y$MALES[match(as.numeric(dimnames(age_frac)[[1]]), 
                                                                   as.numeric(sizepop_sp_y$LENGTH), nomatch = 0, 
                                                                   incomparables = no.lengths)]
            if(sex[sx] ==2)
              pop_age_est_F <- age_frac * sizepop_sp_y$FEMALES[match(as.numeric(dimnames(age_frac)[[1]]), 
                                                                     as.numeric(sizepop_sp_y$LENGTH), nomatch = 0, 
                                                                     incomparables = no.lengths)]
            if(sex[sx] ==3)
              pop_age_est_U <- age_frac * sizepop_sp_y$UNSEXED[match(as.numeric(dimnames(age_frac)[[1]]), 
                                                                     as.numeric(sizepop_sp_y$LENGTH), nomatch = 0, 
                                                                     incomparables = no.lengths)]
            
            #########################
            # End sex loop
          }
          
          # Now sum up the numbers at age for all lengths and remove any 0s, and check to see if matches with RACE output
          
          # Males
          if(exists("pop_age_est_M")==TRUE){if(length(pop_age_est_M)>0){
            age_est_M <- apply(pop_age_est_M, 2,sum)
            if(length(which(age_est_M==0))>0)
              age_est_M <- age_est_M[-which(age_est_M==0)]
            agepop_RACE_sp_y_M<-subset(agepop_RACE_sp_y,agepop_RACE_sp_y$SEX==sex[1])
            test_M<-matrix(nrow=length(names(age_est_M)),ncol=3)
            colnames(test_M)<-c("Age","Calc","RACE")
            test_M[,1]<-as.numeric(names(age_est_M))
            test_M[,2]<-age_est_M
            for(i in 1:length(test_M[,3])){
              if(length(match(test_M[i,1],agepop_RACE_sp_y_M$AGE))>0)
                test_M[i,3]<-agepop_RACE_sp_y_M$AGEPOP[match(test_M[i,1],agepop_RACE_sp_y_M$AGE)]
            }
            test_M<-as.data.frame(test_M)
            RACEmatch_Ag$Male[y]<-max(abs(test_M$Calc-test_M$RACE)/test_M$RACE)
            
            AGEPOP_M[y,match(as.numeric(names(age_est_M)),ages)]<-age_est_M
            AGEPOP_M[y,is.na(AGEPOP_M[y,])] <- 0
          }}
          
          # Females
          if(exists("pop_age_est_F")==TRUE){if(length(pop_age_est_F)>0){
            age_est_F <- apply(pop_age_est_F, 2,sum)
            if(length(which(age_est_F==0))>0)
              age_est_F <- age_est_F[-which(age_est_F==0)]
            agepop_RACE_sp_y_F<-subset(agepop_RACE_sp_y,agepop_RACE_sp_y$SEX==sex[2])
            test_F<-matrix(nrow=length(names(age_est_F)),ncol=3)
            colnames(test_F)<-c("Age","Calc","RACE")
            test_F[,1]<-as.numeric(names(age_est_F))
            test_F[,2]<-age_est_F
            for(i in 1:length(test_F[,3])){
              if(length(match(test_F[i,1],agepop_RACE_sp_y_F$AGE))>0)
                test_F[i,3]<-agepop_RACE_sp_y_F$AGEPOP[match(test_F[i,1],agepop_RACE_sp_y_F$AGE)]
            }
            test_F<-as.data.frame(test_F)
            RACEmatch_Ag$Female[y]<-max(abs(test_F$Calc-test_F$RACE)/test_F$RACE)
            AGEPOP_F[y,match(as.numeric(names(age_est_F)),ages)]<-age_est_F
            AGEPOP_F[y,is.na(AGEPOP_F[y,])] <- 0
          }}
          
          # Unsexed
          if(exists("pop_age_est_U")==TRUE){if(length(pop_age_est_U)>0){
            age_est_U <- apply(pop_age_est_U, 2,sum)
            if(length(which(age_est_U==0))>0)
              age_est_U <- age_est_U[-which(age_est_U==0)]
            agepop_RACE_sp_y_U<-subset(agepop_RACE_sp_y,agepop_RACE_sp_y$SEX==sex[3])
            test_U<-matrix(nrow=length(names(age_est_U)),ncol=3)
            colnames(test_U)<-c("Age","Calc","RACE")
            test_U[,1]<-as.numeric(names(age_est_U))
            test_U[,2]<-age_est_U
            for(i in 1:length(test_U[,3])){
              if(length(match(test_U[i,1],agepop_RACE_sp_y_U$AGE))>0)
                test_U[i,3]<-agepop_RACE_sp_y_U$AGEPOP[match(test_U[i,1],agepop_RACE_sp_y_U$AGE)]
            }
            test_U<-as.data.frame(test_U)
            RACEmatch_Ag$Unsexed[y]<-max(abs(test_U$Calc-test_U$RACE)/test_U$RACE)
            AGEPOP_U[y,match(as.numeric(names(age_est_U)),ages)]<-age_est_U
            AGEPOP_U[y,is.na(AGEPOP_U[y,])] <- 0
          }}
          
          
          #########################
          # End year loop
        }
        
        # Finalize results matrices
        SZPOP_T <- matrix(mapply(sum,SZPOP_M,SZPOP_F,SZPOP_U,MoreArgs=list(na.rm=TRUE)),ncol=length(lengths))
        colnames(SZPOP_T)<-as.character(lengths)
        rownames(SZPOP_T)<-as.character(yrs)
        
        AGEPOP_T <- matrix(mapply(sum,AGEPOP_M,AGEPOP_F,AGEPOP_U,MoreArgs=list(na.rm=TRUE)),ncol=length(ages))
        colnames(AGEPOP_T)<-as.character(ages)
        rownames(AGEPOP_T)<-as.character(yrs)
        
        #########################
        # End species loop
      }
      standardize = function(mat) t(mat) / rep(1,ncol(mat))%o%rowSums(mat)
      comps<-t(standardize(AGEPOP_T))
      comps_boot[[b]]<-comps
      # if(b==1){print(alt);print(b)}
      # if(b==10){print(alt);print(b)}
      # if(b==20){print(alt);print(b)}
      # if(b==30){print(alt);print(b)}
      if(b==10||b==20||b==30||b==40||b==50||b==60||b==70||b==80||b==90||b==100||b==110||b==120||b==130||b==140||b==150||b==160||b==170||b==180||b==190||b==200){
        print(alt);print(b)}
    } 
    comps_alt[[alt]]<-comps_boot
    save(comps_alt,file=paste(pathR,"/pipeline_comps_alt_",species,"_",method,"_",submethod[m],sep=""))
  }
  comps_meth[[m]] <-comps_alt
}



###############################################################################################################
################################ Run Model w/ ISS & save output ###############################################
###############################################################################################################

### Code below loads file writer functions, writes new files, runs AMAK, and saves output
rm(list=ls()) # clear workspace

# Create lists, switch objs, & define settings for models
log_theta_top<-list(); comp_record_top<-list(); 
pred_rec_top<-list(); proj_rec_top<-list(); se.pred_rec_top<-list(); se.proj_rec_top<-list(); 
ofl_top <-list(); se.ofl_top<-list(); abc_top<-list(); se.abc_top<-list();  
effn_list_top <-list(); EffN_top<-list(); inputN_srv2_top<-list(); age_mult_top<-list();
abc_meth <-list(); ofl_meth<-list();

method      <-"ototow" # "tow" "oto" "ototow"
submethod   <-c("otos","tows")
samp_change <-c("0.33","0.67","1","1.33","1.67") # Same suite of changes used in BE, but char
species     <-"21740" # "21740" "30152" "30060"
stock       <-"GOA"
n_boot      <-200

# Set/create directories for running models & saving output
path_TOP     <-"C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code"
source(paste(path_TOP,"/otosamp/Functions/Functions.R",sep="")) #Source functions for use below


for(m in 1:length(submethod)){
  dir.create(file.path(paste(path_TOP,"/AMAK_saves_",method,"_",submethod[m],sep="")),showWarnings = FALSE)
  AMAK_saves   <-paste(path_TOP,"/AMAK_saves_",method,"_",submethod[m],sep="")
  pathR        <-paste(path_TOP,"/otosamp/SzAC Results","_",method,"_",submethod[m],sep="")
  
  load(paste(pathR,"/pipeline_comps_alt_",species,"_",method,"_",submethod[m],sep=""))
  load(paste(pathR,"/pipeline_samp_change_alt_",species,"_",method,"_",submethod[m],sep=""))
  
  for(s in 4:4){
    
    inputN_srv2_p<-list(); EffN_p<-list(); abc<-list(); se.abc <-list(); effn_list <-list(); se.proj_rec<-list(); 
    ofl<-list(); se.ofl<-list();
    log_theta<-list(); comp_record<-list();age_mult_boot<-list();pred_rec<-list();se.pred_rec<-list(); proj_rec<-list(); 
    
    iss_s   <-samp_suite_alt[[s]]
    comps_s <-comps_alt[[s]]
    
    for(p in 1:170){
      age_comp <-comps_s[[p]]
      iss      <-as.vector(iss_s[[p]])
      if(any(iss<1)){iss[iss<1]<-1}
      if(any(iss>800)){iss[iss>800]<-800}
      row.names(age_comp)  <-NULL; colnames(age_comp)   <-NULL
      
      ### Set model path, write new files with iss from above inserted
      ### Compile model first, then run model below
      if(species=="30060"){ ### writing code in here for POP runs, to switch from DM to multi if log_theta>5
        CompDist        <-2 # 1=multinomial 2=dirichlet-multinomial
        log_theta_prior <-0.5
        cv_theta_prior  <-0.05
        ph_theta        <-"2" # "2"=turn on estimation; "-2"=turn off estimation
        fleet           <-"srv1" # Specify fleet name you want to save theta, abc, and SE abc for
        # modname         <-"Model_17_1_DM"
        modname         <-"pop"
        modelpath       <-paste(path_TOP, "/AMAK/2020 GOA POP Assessment_DM",sep="") #set model folder
        
        modelFiles      <-list.files(modelpath, include.dirs = F, full.names = F, recursive = T)
        # modelFiles_keep <-c("Model_17_1_DM.tpl","Model_17_1_DM.exe","Model_17_1_DM.cpp","Model_17_1_DM.htp","Model_17_1_DM.obj","MAT.DAT")
        modelFiles_keep <-c("pop.tpl","pop.exe","pop.cpp","pop.htp","pop.obj","MAT.DAT")
        modelFiles_lose <-list(modelFiles[!(modelFiles %in% grep(paste(modelFiles_keep,collapse = "|"), modelFiles, value=TRUE))])
        do.call(unlink, modelFiles_lose)
        
        setwd(modelpath)
        writeDat_POP(dat="goa_pop_2020.dat") #write data file
        writeCtl_POP(dat="goa_pop_2020.ctl") #write data file
        x               <-system(paste(modname," -nohess",sep=""),intern=T)
        # x               <-system("Model_17_1_DM -nohess",intern=T)
        RepFile         <-readLines(paste(modname,".rep",sep=""))
        if(CompDist==2){log_theta[[p]] <-strsplit(RepFile[grep(paste("log_theta_",fleet,"_age",sep=""),RepFile)+1]," ")}
        
        if(log_theta[[p]]<5){x <-system(paste(modname,sep=""),intern=T)}
        
        if(log_theta[[p]]>5){
          CompDist        <-1 # 1=multinomial 2=dirichlet-multinomial
          log_theta_prior <-0.5
          cv_theta_prior  <-0.05
          ph_theta        <-"-2" # "2"=turn on estimation; "-2"=turn off estimation
          fleet           <-"srv1" # Specify fleet name you want to save theta, abc, and SE abc for
          # modname         <-"Model_17_1_DM"
          modname         <-"pop"
          modelpath       <-paste(path_TOP, "/AMAK/2020 GOA POP Assessment_DM",sep="") #set model folder
          
          modelFiles      <-list.files(modelpath, include.dirs = F, full.names = F, recursive = T)
          # modelFiles_keep <-c("Model_17_1_DM.tpl","Model_17_1_DM.exe","Model_17_1_DM.cpp","Model_17_1_DM.htp","Model_17_1_DM.obj","MAT.DAT")
          modelFiles_keep <-c("pop.tpl","pop.exe","pop.cpp","pop.htp","pop.obj","MAT.DAT")
          modelFiles_lose <-list(modelFiles[!(modelFiles %in% grep(paste(modelFiles_keep,collapse = "|"), modelFiles, value=TRUE))])
          do.call(unlink, modelFiles_lose)
          
          writeDat_POP(dat="goa_pop_2020.dat") #write data file
          writeCtl_POP(dat="goa_pop_2020.ctl") #write data file
          setwd(modelpath)
          x <-system(paste(modname,sep=""),intern=T)
        }
        dir.create(file.path(paste(AMAK_saves,"/",species,"_",stock,"_","iss",s,"_","boot",p,"_",method,sep="")),showWarnings = FALSE)
        AMAK_saves_run<-paste(AMAK_saves,"/",species,"_",stock,"_","iss",s,"_","boot",p,"_",method,sep="")
        modelFiles_keep_all <-c(modelFiles_keep,"goa_pop_2020.dat","goa_pop_2020.ctl",paste(modname,".rep",sep=""),paste(modname,".std",sep=""))
        file.copy(from=paste(modelpath,"/",modelFiles_keep_all,sep=""),to=paste(AMAK_saves_run,"/",modelFiles_keep_all,sep=""))  
      }
      
      if(species=="30152"){
        CompDist        <-2 # 1=multinomial 2=dirichlet-multinomial
        log_theta_prior <-0.5
        cv_theta_prior  <-0.05
        ph_theta        <-"2" # "2"=turn on estimation; "-2"=turn off estimation
        fleet           <-"srv1" # Specify fleet name you want to save theta, abc, and SE abc for
        modname         <-"base"
        modelpath       <-paste(path_TOP, "/AMAK/2020 GOA Dusky Rockfish Assessment_DM",sep="") #set om folder
        setwd(modelpath)
        
        modelFiles      <-list.files(modelpath, include.dirs = F, full.names = F, recursive = T)
        modelFiles_keep <-c("base.tpl","base.exe","base.cpp","base.htp","base.obj","MAT.DAT")
        modelFiles_lose <-list(modelFiles[!(modelFiles %in% grep(paste(modelFiles_keep,collapse = "|"), modelFiles, value=TRUE))])
        do.call(unlink, modelFiles_lose)
        
        # setwd(modelpath)
        writeDat_dusky(dat="goa_dr_2020.dat") #write data file
        writeCtl_dusky(dat="goa_dr_2020.ctl") #write data file
        x <-system(paste("base -nohess"),intern=T)
        RepFile    <-readLines("report.rep")
        if(CompDist==2){log_theta[[p]] <-strsplit(RepFile[grep(paste("log_theta_",fleet,"_age",sep=""),RepFile)+1]," ")}
        
        if(log_theta[[p]]<5){x <-system(paste("base"),intern=T)}
        
        if(log_theta[[p]]>5){
          CompDist        <-1 # 1=multinomial 2=dirichlet-multinomial
          log_theta_prior <-0.5
          cv_theta_prior  <-0.05
          ph_theta        <-"-2" # "2"=turn on estimation; "-2"=turn off estimation
          fleet           <-"srv1" # Specify fleet name you want to save theta, abc, and SE abc for
          modname         <-"base"
          modelpath       <-paste(path_TOP, "/AMAK/2020 GOA Dusky Rockfish Assessment_DM",sep="") #set om folder
          
          modelFiles      <-list.files(modelpath, include.dirs = F, full.names = F, recursive = T)
          modelFiles_keep <-c("base.tpl","base.exe","base.cpp","base.htp","base.obj","MAT.DAT")
          modelFiles_lose <-list(modelFiles[!(modelFiles %in% grep(paste(modelFiles_keep,collapse = "|"), modelFiles, value=TRUE))])
          do.call(unlink, modelFiles_lose)
          
          
          writeDat_dusky(dat="goa_dr_2020.dat") #write data file
          writeCtl_dusky(dat="goa_dr_2020.ctl") #write data file
          setwd(modelpath)
          x <-system(paste("base"),intern=T)
        }
        dir.create(file.path(paste(AMAK_saves,"/",species,"_",stock,"_","iss",s,"_","boot",p,"_",method,sep="")),showWarnings = FALSE)
        AMAK_saves_run<-paste(AMAK_saves,"/",species,"_",stock,"_","iss",s,"_","boot",p,"_",method,sep="")
        modelFiles_keep_all <-c(modelFiles_keep,"goa_dr_2020.dat","goa_dr_2020.ctl","report.rep",paste(modname,".std",sep=""))
        file.copy(from=paste(modelpath,"/",modelFiles_keep_all,sep=""),to=paste(AMAK_saves_run,"/",modelFiles_keep_all,sep=""))  
      }
      
      if(species=="21740"){
        nyrs <-14
        CompDist        <-1 # 1=multinomial 2=dirichlet-multinomial
        log_theta_prior <-0.5
        cv_theta_prior  <-0.05
        ph_theta        <-"-2" # "2"=turn on estimation; "-2"=turn off estimation
        fleet           <-"srv2"
        modname         <-"pk20_8"
        modelpath <- paste(path_TOP, "/AMAK/2020 GOA Walleye Pollock Assessment_DM",sep="") #set om folder
        
        ### Conduct Francis reweighting (x3 model runs)
        rewt_runs <-3
        inputN_srv2_r<-list(); EffN_r<-list(); age_mult_r<-list();
        
        for(r in 1:rewt_runs){
          modelFiles      <-list.files(modelpath, include.dirs = F, full.names = F, recursive = T)
          modelFiles_keep <-c("pk20_8.tpl","pk20_8.exe","pk20_8.cpp","pk20_8.htp","pk20_8.obj")
          modelFiles_lose <-list(modelFiles[!(modelFiles %in% grep(paste(modelFiles_keep,collapse = "|"), modelFiles, value=TRUE))])
          do.call(unlink, modelFiles_lose)
          
          setwd(modelpath)
          if(r==1){writeDat_pollock(dat=paste(modname,".dat",sep=""))} #write data file
          if(r>1){iss <-iss_rewt; writeDat_pollock(dat=paste(modname,".dat",sep=""))}
          x           <-system(paste(modname),intern=T)
          RepFile     <-readLines(paste(modname,".rep",sep=""))
          agecomp_out <-strsplit(RepFile[(grep("Observed and expected age comp_srv2",RepFile)+1):(grep("Pearson residuals age comp_srv2",RepFile)-1)]," ")
          inputN_srv2 <-as.numeric(strsplit(RepFile[(grep("Input N_srv2",RepFile)+1)]," ")[[1]][-1])
          inputN_srv2_r[[r]]<-inputN_srv2
          
          firstAC <-1; plusgroup <-10; ages <-seq(firstAC,plusgroup,1)
          ac_out       <-matrix(NA,ncol=length(ages)*2,nrow=nyrs)
          for(ac in 1:nyrs){ac_out[ac,]<-as.numeric(agecomp_out[[ac]][-1])}
          obs_agecomp  <-ac_out[,firstAC:plusgroup]
          pred_agecomp <-ac_out[,(plusgroup+1):(plusgroup+10)]
          
          residuals <-NULL
          Top <-0; Bot <-0
          for(a in 1:length(ages)){
            Bot <- Bot + (obs_agecomp[,a]-pred_agecomp[,a])^2
            Top <- Top + pred_agecomp[,a]*(1-pred_agecomp[,a])
          }
          
          # mean_obs_age  <-sum(obs_agecomp*ages)
          # mean_pred_age <-sum(pred_agecomp*ages)
          mean_obs_age  <-rowSums(obs_agecomp*ages)
          mean_pred_age <-rowSums(pred_agecomp*ages)
          v_jy          <-rowSums(pred_agecomp*(ages^2))-mean_pred_age^2
          sd            <-sqrt(v_jy/inputN_srv2)
          residual      <-(mean_obs_age-mean_pred_age)/sd
          residuals     <-c(residuals,residual) # not sure why residuals=NULL and then c()'d here, but that's what AEP did -\_(")_/-
          EffN          <-Top/Bot
          EffN_r[[r]]   <-EffN
          
          if(!is.na(residuals[1])){  
            age_mult <-1.0/var(residuals)
            iss_rewt <-age_mult*iss
            age_mult_r[[r]] <-age_mult
          }
        }
        
        age_mult_boot[[p]] <-age_mult_r
        EffN_p[[p]]        <-EffN_r
        inputN_srv2_p[[p]] <-inputN_srv2_r
        
        dir.create(file.path(paste(AMAK_saves,"/",species,"_",stock,"_","iss",s,"_","boot",p,"_",method,sep="")),showWarnings = FALSE)
        AMAK_saves_run<-paste(AMAK_saves,"/",species,"_",stock,"_","iss",s,"_","boot",p,"_",method,sep="")
        modelFiles_keep_all <-c(modelFiles_keep,paste(modname,".dat",sep=""),paste(modname,".rep",sep=""),paste(modname,".std",sep=""))
        file.copy(from=paste(modelpath,"/",modelFiles_keep_all,sep=""),to=paste(AMAK_saves_run,"/",modelFiles_keep_all,sep="")) 
      }
      
      # add output to saved obj list if need to run model more than once
      # if(exists(paste(modname,".std",sep=""),where=modelpath)){
      ReportFile                     <-read.table(paste(modname,".std",sep=""),stringsAsFactors=F,header=T)
      if(CompDist==2){log_theta[[p]] <-ReportFile[grep(paste("log_theta_",fleet,"_age",sep=""),ReportFile$name),]$value}
      
      if(species=="30060"){
        comp_record[[p]] <-CompDist
        abc[[p]]         <-ReportFile[grep("ABC",ReportFile$name),]$value
        se.abc[[p]]      <-ReportFile[grep("ABC",ReportFile$name),]$std.dev
        ofl[[p]]         <-ReportFile[grep("OFL",ReportFile$name),]$value
        se.ofl[[p]]      <-ReportFile[grep("OFL",ReportFile$name),]$std.dev
        pred_rec[[p]]    <-ReportFile[which(ReportFile$name=="pred_rec"),]$value
        se.pred_rec[[p]] <-ReportFile[which(ReportFile$name=="pred_rec"),]$std.dev
        
        nyrs             <-13
        RepFile          <-readLines(paste(modname,".rep",sep=""))
        # OFL              <-strsplit(RepFile[grep("OFL for 2021",RepFile)+1]," ")
        effn_mat         <-strsplit(RepFile[grep("yrs_srv1_age",RepFile):(grep("yrs_srv1_age",RepFile)+nyrs)]," ")
        effn_names       <-effn_mat[[1]]
        effn             <-matrix(NA,ncol=4,nrow=nyrs)
        
        for(f in 2:(nyrs+1)){
          effn[f-1,1] <-as.numeric(effn_mat[[f]][1])
          effn[f-1,2] <-as.numeric(effn_mat[[f]][2])
          effn[f-1,3] <-as.numeric(effn_mat[[f]][3])
          effn[f-1,4] <-as.numeric(effn_mat[[f]][4])
        }
        colnames(effn)<-effn_names
      }
      
      if(species=="30152"){
        comp_record[[p]] <-CompDist
        abc[[p]]         <-ReportFile[grep("ABC",ReportFile$name),]$value
        se.abc[[p]]      <-ReportFile[grep("ABC",ReportFile$name),]$std.dev
        ofl[[p]]         <-ReportFile[grep("OFL",ReportFile$name),]$value
        se.ofl[[p]]      <-ReportFile[grep("OFL",ReportFile$name),]$std.dev
        pred_rec[[p]]    <-ReportFile[which(ReportFile$name=="pred_rec"),]$value
        se.pred_rec[[p]] <-ReportFile[which(ReportFile$name=="pred_rec"),]$std.dev
        
        nyrs       <-16
        RepFile    <-readLines("report.rep")
        # OFL        <-strsplit(RepFile[grep("OFL for 2021",RepFile)+1]," ")
        effn_mat   <-strsplit(RepFile[grep("yrs_srv1_age",RepFile):(grep("yrs_srv1_age",RepFile)+nyrs)]," ")
        effn_names <-effn_mat[[1]]
        effn       <-matrix(NA,ncol=4,nrow=nyrs)
        
        for(f in 2:(nyrs+1)){
          effn[f-1,1] <-as.numeric(effn_mat[[f]][1])
          effn[f-1,2] <-as.numeric(effn_mat[[f]][2])
          effn[f-1,3] <-as.numeric(effn_mat[[f]][3])
          effn[f-1,4] <-as.numeric(effn_mat[[f]][4])
        }
        colnames(effn)<-effn_names
      }
      
      if(species=="21740"){
        #Of course ABC is named completely different things between models, b/c THAT makes sense
        abc[[p]]         <-ReportFile[grep("Ecattot_proj_abc",ReportFile$name),]$value[1]
        se.abc[[p]]      <-ReportFile[grep("Ecattot_proj_abc",ReportFile$name),]$std.dev[1]
        ofl[[p]]         <-ReportFile[grep("Ecattot_proj_ofl",ReportFile$name),]$value
        se.ofl[[p]]      <-ReportFile[grep("Ecattot_proj_ofl",ReportFile$name),]$std.dev
        pred_rec[[p]]    <-ReportFile[which(ReportFile$name=="recruit"),]$value
        proj_rec[[p]]    <-ReportFile[which(ReportFile$name=="recruit_proj"),]$value
        se.pred_rec[[p]] <-ReportFile[which(ReportFile$name=="recruit"),]$std.dev
        se.proj_rec[[p]] <-ReportFile[which(ReportFile$name=="recruit_proj"),]$std.dev
        
        nyrs       <-14
        RepFile    <-readLines(paste(modname,".rep",sep=""))
        # OFL        <-strsplit(RepFile[(grep("Catch at age",RepFile)+1)]," ")
        effn_mat   <-strsplit(RepFile[grep("srv_acyrs2",RepFile):(grep("srv_acyrs2",RepFile)+nyrs)]," ")
        effn_names <-effn_mat[[1]]
        effn       <-matrix(NA,ncol=3,nrow=nyrs)
        
        for(f in 2:(nyrs+1)){
          effn[f-1,1] <-as.numeric(effn_mat[[f]][1])
          effn[f-1,2] <-as.numeric(effn_mat[[f]][2])
          effn[f-1,3] <-as.numeric(effn_mat[[f]][3])
        }
        colnames(effn)<-effn_names
      }
      
      effn_list[[p]] <-effn  # save boot rep results
      print(paste("iss=",s," & ","boot=",p,sep=""))

      if(p==10||p==20||p==30||p==40||p==50||p==60||p==70||p==80||p==90||p==100||p==110||p==120||p==130||p==140||p==150||p==160||p==170||p==180||p==190||p==200){
        save(ofl,file=paste(AMAK_saves,"/ofl_",species,"_",stock,"_",method,"_",submethod[m],"_alt",s,"_boot",p,sep=""))}

    } # end boot loop
    
    ### saving boot-level lists to samp_change-level lists
    if(species!="21740"){log_theta_top[[s]]   <-log_theta}
    comp_record_top[[s]] <-comp_record
    effn_list_top[[s]]   <-effn_list  # save all boot rep results for each samp_change
    abc_top[[s]]         <-abc
    se.abc_top[[s]]      <-se.abc
    pred_rec_top[[s]]    <-pred_rec
    se.pred_rec_top[[s]] <-se.pred_rec
    ofl_top[[s]]         <-ofl
    se.ofl_top[[s]]      <-se.ofl
    if(species=="21740"){
      proj_rec_top[[s]]    <-proj_rec
      se.proj_rec_top[[s]] <-se.proj_rec
      age_mult_top[[s]]    <-age_mult_boot
      EffN_top[[s]]        <-EffN_p
      inputN_srv2_top[[s]] <-inputN_srv2_p}
    
    # saving lists (within a samp_change)
    if(species!="21740"){save(log_theta_top,file=paste(AMAK_saves,"/log_theta_",species,"_",stock,"_",method,"_",submethod[m],sep=""))}
    save(comp_record,file=paste(AMAK_saves,"/comp_record_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
    save(effn_list,file=paste(AMAK_saves,"/effn_out_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
    save(abc,file=paste(AMAK_saves,"/abc_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
    save(se.abc,file=paste(AMAK_saves,"/se.abc_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
    save(pred_rec,file=paste(AMAK_saves,"/pred_rec_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
    save(se.pred_rec,file=paste(AMAK_saves,"/se.pred_rec_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
    save(ofl,file=paste(AMAK_saves,"/ofl_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
    save(se.ofl,file=paste(AMAK_saves,"/se.ofl_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
    # pollock does Francis reweighting, so save lists specific to that
    if(species=="21740"){
      save(proj_rec,file=paste(AMAK_saves,"/proj_rec_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
      save(se.proj_rec,file=paste(AMAK_saves,"/se.proj_rec_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
      save(age_mult_boot,file=paste(AMAK_saves,"/age_mult_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
      save(EffN_p,file=paste(AMAK_saves,"/EffN_calc_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
      save(inputN_srv2_p,file=paste(AMAK_saves,"/inputN_srv2_",species,"_",stock,"_",method,"_",submethod[m],sep=""))}
    
    
    # saving lists (after every samp_change)
    if(species!="21740"){save(log_theta_top,file=paste(AMAK_saves,"/log_theta_",species,"_",stock,"_",method,"_",submethod[m],sep=""))}
    save(comp_record_top,file=paste(AMAK_saves,"/comp_record_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
    save(effn_list_top,file=paste(AMAK_saves,"/effn_out_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
    save(abc_top,file=paste(AMAK_saves,"/abc_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
    save(se.abc_top,file=paste(AMAK_saves,"/se.abc_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
    save(pred_rec_top,file=paste(AMAK_saves,"/pred_rec_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
    save(se.pred_rec_top,file=paste(AMAK_saves,"/se.pred_rec_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
    save(ofl_top,file=paste(AMAK_saves,"/ofl_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
    save(se.ofl_top,file=paste(AMAK_saves,"/se.ofl_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
    # pollock does Francis reweighting, so save lists specific to that
    if(species=="21740"){
      save(proj_rec_top,file=paste(AMAK_saves,"/proj_rec_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
      save(se.proj_rec_top,file=paste(AMAK_saves,"/se.proj_rec_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
      save(age_mult_top,file=paste(AMAK_saves,"/age_mult_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
      save(EffN_top,file=paste(AMAK_saves,"/EffN_calc_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
      save(inputN_srv2_top,file=paste(AMAK_saves,"/inputN_srv2_",species,"_",stock,"_",method,"_",submethod[m],sep=""))}
    
    
    ### naming list elements (final)
    if(s==length(samp_change) & p==n_boot){
      if(species!="21740"){names(log_theta_top)   <-samp_change}
      names(comp_record_top) <-samp_change
      names(effn_list_top)   <-samp_change
      names(abc_top)         <-samp_change
      names(se.abc_top)      <-samp_change
      names(pred_rec_top)    <-samp_change
      names(se.pred_rec_top) <-samp_change
      names(ofl_top)         <-samp_change
      names(se.ofl_top)      <-samp_change
      if(species=="21740"){
        names(proj_rec_top)    <-samp_change
        names(se.proj_rec_top) <-samp_change
        names(age_mult_top)    <-samp_change
        names(EffN_top)        <-samp_change
        names(inputN_srv2_top) <-samp_change}
      
      # saving lists (final)
      if(species!="21740"){save(log_theta_top,file=paste(AMAK_saves,"/log_theta_",species,"_",stock,"_",method,"_",submethod[m],sep=""))}
      save(comp_record_top,file=paste(AMAK_saves,"/comp_record_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
      save(effn_list_top,file=paste(AMAK_saves,"/effn_out_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
      save(abc_top,file=paste(AMAK_saves,"/abc_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
      save(se.abc_top,file=paste(AMAK_saves,"/se.abc_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
      save(pred_rec_top,file=paste(AMAK_saves,"/pred_rec_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
      save(se.pred_rec_top,file=paste(AMAK_saves,"/se.pred_rec_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
      save(ofl_top,file=paste(AMAK_saves,"/ofl_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
      save(se.ofl_top,file=paste(AMAK_saves,"/se.ofl_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
      # pollock does Francis reweighting, so save lists specific to that
      if(species=="21740"){
        save(proj_rec_top,file=paste(AMAK_saves,"/proj_rec_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
        save(se.proj_rec_top,file=paste(AMAK_saves,"/se.proj_rec_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
        save(age_mult_top,file=paste(AMAK_saves,"/age_mult_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
        save(EffN_top,file=paste(AMAK_saves,"/EffN_calc_",species,"_",stock,"_",method,"_",submethod[m],sep=""))
        save(inputN_srv2_top,file=paste(AMAK_saves,"/inputN_srv2_",species,"_",stock,"_",method,"_",submethod[m],sep=""))}
    }
  } # end samp_change loop
  abc_meth[[m]] <-abc_top
  ofl_meth[[m]] <-ofl_top
} # end method loop

