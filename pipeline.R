#### Head ####
# Nominal SS -> Expansion -> Input SS Calc -> Effective SS calc
setwd("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/Updated Code from Pete")
# install.packages("remotes")
# remotes::install_github("James-Thorson/VAST")

############################ Pull Data from RACE ###############################

# Enter AFSC username/password
username="SISKEYM"
password="WaCmMl_4IwUQ27#6"

# ODBC connect stuff
library(RODBC)
channel=odbcConnect("afsc",uid=username,pwd=password,believeNRows=FALSE)

# Start calling data
# Get strata data (this is strata for both GOA and AI)
strata<-sqlQuery(channel,"SELECT * FROM GOA.GOA_STRATA",believeNRows=FALSE)
strata_GOA <- subset(strata,strata$SURVEY=='GOA')
strata_AI  <- subset(strata,strata$SURVEY=='AI')
# strata_EBS <- subset(strata,strata$SURVEY=='EBS') 

#=================================DUSKY========================================#
### GOA dusky ###
# Enter species code
species<-30152 #30150

# Get length data
sizepop_goa_dusky  <- sqlQuery(channel,paste("SELECT * FROM GOA.SIZECOMP_TOTAL WHERE (((GOA.SIZECOMP_TOTAL.SPECIES_CODE)=",
                                             species,"))",sep=""),believeNRows=FALSE)
# Get length frequency data
lfreq_goa_dusky    <- sqlQuery(channel,paste(
                        "SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.LENGTH ON RACEBASE.HAUL.HAULJOIN = 
                        RACEBASE.LENGTH.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.LENGTH.CRUISEJOIN = 
                        GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.LENGTH.REGION)='GOA') AND 
                        ((RACEBASE.LENGTH.SPECIES_CODE)=",species,") AND ((RACEBASE.HAUL.PERFORMANCE)>=0))",sep=""),
                        believeNRows=FALSE)
# Get age data
agepop_goa_dusky   <- sqlQuery(channel,paste("SELECT * FROM GOA.AGECOMP_TOTAL WHERE (((GOA.AGECOMP_TOTAL.SPECIES_CODE)=",
                                             species,"))",sep=""),believeNRows=FALSE)
# Get specimen data
specimen_goa_dusky <- sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.SPECIMEN ON 
                                             RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN) INNER JOIN 
                                             GOA.BIENNIAL_SURVEYS ON RACEBASE.SPECIMEN.CRUISEJOIN = 
                                             GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.SPECIMEN.SPECIES_CODE)=",
                                             species,") AND ((RACEBASE.SPECIMEN.REGION)='GOA'))",sep=""),believeNRows=FALSE)
# Get CPUE data
CPUE_goa_dusky     <- sqlQuery(channel,paste("SELECT * FROM GOA.CPUE WHERE (((GOA.CPUE.SPECIES_CODE)=",
                                             species,"))",sep=""),believeNRows=FALSE)
# Remove redundant columns
lfreq_goa_dusky    <- lfreq_goa_dusky[,-which(substr(names(lfreq_goa_dusky),nchar(names(lfreq_goa_dusky))-1,nchar(names(lfreq_goa_dusky)))==".1")]
lfreq_goa_dusky    <- lfreq_goa_dusky[,-which(substr(names(lfreq_goa_dusky),nchar(names(lfreq_goa_dusky))-1,nchar(names(lfreq_goa_dusky)))==".2")]
specimen_goa_dusky <- specimen_goa_dusky[,-which(substr(names(specimen_goa_dusky),nchar(names(specimen_goa_dusky))-1,nchar(names(specimen_goa_dusky)))==".1")]
specimen_goa_dusky <- specimen_goa_dusky[,-which(substr(names(specimen_goa_dusky),nchar(names(specimen_goa_dusky))-1,nchar(names(specimen_goa_dusky)))==".2")]

{### AI dusky ###
# Enter species code
# species<-30150
# 
# # Get length data
# sizepop_ai_dusky<-sqlQuery(channel,paste("SELECT * FROM AI.SIZECOMP_TOTAL WHERE (((AI.SIZECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# lfreq_ai_dusky<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.LENGTH ON RACEBASE.HAUL.HAULJOIN = RACEBASE.LENGTH.HAULJOIN) INNER JOIN AI.BIENNIAL_SURVEYS ON RACEBASE.LENGTH.CRUISEJOIN = AI.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.LENGTH.REGION)='AI') AND ((RACEBASE.LENGTH.SPECIES_CODE)=",species,") AND ((RACEBASE.HAUL.PERFORMANCE)>=0))",sep=""),believeNRows=FALSE)
# # Get age data
# agepop_ai_dusky<-sqlQuery(channel,paste("SELECT * FROM AI.AGECOMP_TOTAL WHERE (((AI.AGECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# specimen_ai_dusky<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.SPECIMEN ON RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN) INNER JOIN AI.BIENNIAL_SURVEYS ON RACEBASE.SPECIMEN.CRUISEJOIN = AI.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.SPECIMEN.SPECIES_CODE)=",species,") AND ((RACEBASE.SPECIMEN.REGION)='AI'))",sep=""),believeNRows=FALSE)
# # Get CPUE data
# CPUE_ai_dusky<-sqlQuery(channel,paste("SELECT * FROM AI.CPUE WHERE (((AI.CPUE.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# # Remove redundant columns
# lfreq_ai_dusky<-lfreq_ai_dusky[,-which(substr(names(lfreq_ai_dusky),nchar(names(lfreq_ai_dusky))-1,nchar(names(lfreq_ai_dusky)))==".1")]
# lfreq_ai_dusky<-lfreq_ai_dusky[,-which(substr(names(lfreq_ai_dusky),nchar(names(lfreq_ai_dusky))-1,nchar(names(lfreq_ai_dusky)))==".2")]
# specimen_ai_dusky<-specimen_ai_dusky[,-which(substr(names(specimen_ai_dusky),nchar(names(specimen_ai_dusky))-1,nchar(names(specimen_ai_dusky)))==".1")]
# specimen_ai_dusky<-specimen_ai_dusky[,-which(substr(names(specimen_ai_dusky),nchar(names(specimen_ai_dusky))-1,nchar(names(specimen_ai_dusky)))==".2")]

### EBS dusky ###
# Enter species code
# species<-30152
# 
# # Get length data
# sizepop_ebs_dusky<-sqlQuery(channel,paste("SELECT * FROM EBS.SIZECOMP_TOTAL WHERE (((EBS.SIZECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# lfreq_ebs_dusky<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.LENGTH ON RACEBASE.HAUL.HAULJOIN = RACEBASE.LENGTH.HAULJOIN) INNER JOIN EBS.BIENNIAL_SURVEYS ON RACEBASE.LENGTH.CRUISEJOIN = EBS.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.LENGTH.REGION)='EBS') AND ((RACEBASE.LENGTH.SPECIES_CODE)=",species,") AND ((RACEBASE.HAUL.PERFORMANCE)>=0))",sep=""),believeNRows=FALSE)
# # Get age data
# agepop_ebs_dusky<-sqlQuery(channel,paste("SELECT * FROM EBS.AGECOMP_TOTAL WHERE (((EBS.AGECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# specimen_ebs_dusky<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.SPECIMEN ON RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN) INNER JOIN EBS.BIENNIAL_SURVEYS ON RACEBASE.SPECIMEN.CRUISEJOIN = EBS.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.SPECIMEN.SPECIES_CODE)=",species,") AND ((RACEBASE.SPECIMEN.REGION)='EBS'))",sep=""),believeNRows=FALSE)
# # Get CPUE data
# CPUE_ebs_dusky<-sqlQuery(channel,paste("SELECT * FROM EBS.CPUE WHERE (((EBS.CPUE.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# # Remove redundant columns
# lfreq_ebs_dusky<-lfreq_ebs_dusky[,-which(substr(names(lfreq_goa_dusky),nchar(names(lfreq_goa_dusky))-1,nchar(names(lfreq_goa_dusky)))==".1")]
# lfreq_ebs_dusky<-lfreq_ebs_dusky[,-which(substr(names(lfreq_goa_dusky),nchar(names(lfreq_goa_dusky))-1,nchar(names(lfreq_goa_dusky)))==".2")]
# specimen_ebs_dusky<-specimen_ebs_dusky[,-which(substr(names(specimen_goa_dusky),nchar(names(specimen_goa_dusky))-1,nchar(names(specimen_goa_dusky)))==".1")]
# specimen_ebs_dusky<-specimen_ebs_dusky[,-which(substr(names(specimen_goa_dusky),nchar(names(specimen_goa_dusky))-1,nchar(names(specimen_goa_dusky)))==".2")]

#=================================POLLOCK======================================#
### GOA pollock ###
# Enter species code
# species<-21740
# 
# # Get length data
# sizepop_goa_poll<-sqlQuery(channel,paste("SELECT * FROM GOA.SIZECOMP_TOTAL WHERE (((GOA.SIZECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# lfreq_goa_poll<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.LENGTH ON RACEBASE.HAUL.HAULJOIN = RACEBASE.LENGTH.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.LENGTH.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.LENGTH.REGION)='GOA') AND ((RACEBASE.LENGTH.SPECIES_CODE)=",species,") AND ((RACEBASE.HAUL.PERFORMANCE)>=0))",sep=""),believeNRows=FALSE)
# # Get age data
# agepop_goa_poll<-sqlQuery(channel,paste("SELECT * FROM GOA.AGECOMP_TOTAL WHERE (((GOA.AGECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# specimen_goa_poll<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.SPECIMEN ON RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.SPECIMEN.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.SPECIMEN.SPECIES_CODE)=",species,") AND ((RACEBASE.SPECIMEN.REGION)='GOA'))",sep=""),believeNRows=FALSE)
# # Get CPUE data
# CPUE_goa_poll<-sqlQuery(channel,paste("SELECT * FROM GOA.CPUE WHERE (((GOA.CPUE.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# # Remove redundant columns
# lfreq_goa_poll<-lfreq_goa_poll[,-which(substr(names(lfreq_goa_poll),nchar(names(lfreq_goa_poll))-1,nchar(names(lfreq_goa_poll)))==".1")]
# lfreq_goa_poll<-lfreq_goa_poll[,-which(substr(names(lfreq_goa_poll),nchar(names(lfreq_goa_poll))-1,nchar(names(lfreq_goa_poll)))==".2")]
# specimen_goa_poll<-specimen_goa_poll[,-which(substr(names(specimen_goa_poll),nchar(names(specimen_goa_poll))-1,nchar(names(specimen_goa_poll)))==".1")]
# specimen_goa_poll<-specimen_goa_poll[,-which(substr(names(specimen_goa_poll),nchar(names(specimen_goa_poll))-1,nchar(names(specimen_goa_poll)))==".2")]

# ### AI pollock ###
# # Enter species code
# species<-21740
# 
# # Get length data
# sizepop_ai_poll<-sqlQuery(channel,paste("SELECT * FROM AI.SIZECOMP_TOTAL WHERE (((AI.SIZECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# lfreq_ai_poll<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.LENGTH ON RACEBASE.HAUL.HAULJOIN = RACEBASE.LENGTH.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.LENGTH.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.LENGTH.REGION)='AI') AND ((RACEBASE.LENGTH.SPECIES_CODE)=",species,") AND ((RACEBASE.HAUL.PERFORMANCE)>=0))",sep=""),believeNRows=FALSE)
# # Get age data
# agepop_ai_poll<-sqlQuery(channel,paste("SELECT * FROM AI.AGECOMP_TOTAL WHERE (((AI.AGECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# specimen_ai_poll<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.SPECIMEN ON RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.SPECIMEN.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.SPECIMEN.SPECIES_CODE)=",species,") AND ((RACEBASE.SPECIMEN.REGION)='AI'))",sep=""),believeNRows=FALSE)
# # Get CPUE data
# CPUE_ai_poll<-sqlQuery(channel,paste("SELECT * FROM AI.CPUE WHERE (((AI.CPUE.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# # Remove redundant columns
# lfreq_ai_poll<-lfreq_ai_poll[,-which(substr(names(lfreq_ai_poll),nchar(names(lfreq_ai_poll))-1,nchar(names(lfreq_ai_poll)))==".1")]
# lfreq_ai_poll<-lfreq_ai_poll[,-which(substr(names(lfreq_ai_poll),nchar(names(lfreq_ai_poll))-1,nchar(names(lfreq_ai_poll)))==".2")]
# specimen_ai_poll<-specimen_ai_poll[,-which(substr(names(specimen_ai_poll),nchar(names(specimen_ai_poll))-1,nchar(names(specimen_ai_poll)))==".1")]
# specimen_ai_poll<-specimen_ai_poll[,-which(substr(names(specimen_ai_poll),nchar(names(specimen_ai_poll))-1,nchar(names(specimen_ai_poll)))==".2")]

# ### EBS pollock ###
# # Enter species code
# species<-21740
# 
# # Get length data
# sizepop_goa_poll<-sqlQuery(channel,paste("SELECT * FROM GOA.SIZECOMP_TOTAL WHERE (((GOA.SIZECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# lfreq_goa_poll<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.LENGTH ON RACEBASE.HAUL.HAULJOIN = RACEBASE.LENGTH.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.LENGTH.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.LENGTH.REGION)='GOA') AND ((RACEBASE.LENGTH.SPECIES_CODE)=",species,") AND ((RACEBASE.HAUL.PERFORMANCE)>=0))",sep=""),believeNRows=FALSE)
# # Get age data
# agepop_goa_poll<-sqlQuery(channel,paste("SELECT * FROM GOA.AGECOMP_TOTAL WHERE (((GOA.AGECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# specimen_goa_poll<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.SPECIMEN ON RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.SPECIMEN.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.SPECIMEN.SPECIES_CODE)=",species,") AND ((RACEBASE.SPECIMEN.REGION)='GOA'))",sep=""),believeNRows=FALSE)
# # Get CPUE data
# CPUE_goa_poll<-sqlQuery(channel,paste("SELECT * FROM GOA.CPUE WHERE (((GOA.CPUE.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# # Remove redundant columns
# lfreq_goa_poll<-lfreq_goa_poll[,-which(substr(names(lfreq_goa_poll),nchar(names(lfreq_goa_poll))-1,nchar(names(lfreq_goa_poll)))==".1")]
# lfreq_goa_poll<-lfreq_goa_poll[,-which(substr(names(lfreq_goa_poll),nchar(names(lfreq_goa_poll))-1,nchar(names(lfreq_goa_poll)))==".2")]
# specimen_goa_poll<-specimen_goa_poll[,-which(substr(names(specimen_goa_poll),nchar(names(specimen_goa_poll))-1,nchar(names(specimen_goa_poll)))==".1")]
# specimen_goa_poll<-specimen_goa_poll[,-which(substr(names(specimen_goa_poll),nchar(names(specimen_goa_poll))-1,nchar(names(specimen_goa_poll)))==".2")]

#=================================POP==========================================#
### GOA POP ###
# Enter species code
# species<-30060
# 
# # Get length data
# sizepop_goa_pop<-sqlQuery(channel,paste("SELECT * FROM GOA.SIZECOMP_TOTAL WHERE (((GOA.SIZECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# lfreq_goa_pop<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.LENGTH ON RACEBASE.HAUL.HAULJOIN = RACEBASE.LENGTH.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.LENGTH.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.LENGTH.REGION)='GOA') AND ((RACEBASE.LENGTH.SPECIES_CODE)=",species,") AND ((RACEBASE.HAUL.PERFORMANCE)>=0))",sep=""),believeNRows=FALSE)
# # Get age data
# agepop_goa_pop<-sqlQuery(channel,paste("SELECT * FROM GOA.AGECOMP_TOTAL WHERE (((GOA.AGECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# specimen_goa_pop<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.SPECIMEN ON RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.SPECIMEN.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.SPECIMEN.SPECIES_CODE)=",species,") AND ((RACEBASE.SPECIMEN.REGION)='GOA'))",sep=""),believeNRows=FALSE)
# # Get CPUE data
# CPUE_goa_pop<-sqlQuery(channel,paste("SELECT * FROM GOA.CPUE WHERE (((GOA.CPUE.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# # Remove redundant columns
# lfreq_goa_pop<-lfreq_goa_pop[,-which(substr(names(lfreq_goa_pop),nchar(names(lfreq_goa_pop))-1,nchar(names(lfreq_goa_pop)))==".1")]
# lfreq_goa_pop<-lfreq_goa_pop[,-which(substr(names(lfreq_goa_pop),nchar(names(lfreq_goa_pop))-1,nchar(names(lfreq_goa_pop)))==".2")]
# specimen_goa_pop<-specimen_goa_pop[,-which(substr(names(specimen_goa_pop),nchar(names(specimen_goa_pop))-1,nchar(names(specimen_goa_pop)))==".1")]
# specimen_goa_pop<-specimen_goa_pop[,-which(substr(names(specimen_goa_pop),nchar(names(specimen_goa_pop))-1,nchar(names(specimen_goa_pop)))==".2")]
# 
# ### AI POP ###
# # Enter species code
# species<-30060
# 
# # Get length data
# sizepop_ai_pop<-sqlQuery(channel,paste("SELECT * FROM AI.SIZECOMP_TOTAL WHERE (((AI.SIZECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# lfreq_ai_pop<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.LENGTH ON RACEBASE.HAUL.HAULJOIN = RACEBASE.LENGTH.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.LENGTH.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.LENGTH.REGION)='AI') AND ((RACEBASE.LENGTH.SPECIES_CODE)=",species,") AND ((RACEBASE.HAUL.PERFORMANCE)>=0))",sep=""),believeNRows=FALSE)
# # Get age data
# agepop_ai_pop<-sqlQuery(channel,paste("SELECT * FROM AI.AGECOMP_TOTAL WHERE (((AI.AGECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# specimen_ai_pop<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.SPECIMEN ON RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.SPECIMEN.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.SPECIMEN.SPECIES_CODE)=",species,") AND ((RACEBASE.SPECIMEN.REGION)='AI'))",sep=""),believeNRows=FALSE)
# # Get CPUE data
# CPUE_ai_pop<-sqlQuery(channel,paste("SELECT * FROM AI.CPUE WHERE (((AI.CPUE.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# # Remove redundant columns
# lfreq_ai_pop<-lfreq_ai_pop[,-which(substr(names(lfreq_ai_pop),nchar(names(lfreq_ai_pop))-1,nchar(names(lfreq_ai_pop)))==".1")]
# lfreq_ai_pop<-lfreq_ai_pop[,-which(substr(names(lfreq_ai_pop),nchar(names(lfreq_ai_pop))-1,nchar(names(lfreq_ai_pop)))==".2")]
# specimen_ai_pop<-specimen_ai_pop[,-which(substr(names(specimen_ai_pop),nchar(names(specimen_ai_pop))-1,nchar(names(specimen_ai_pop)))==".1")]
# specimen_ai_pop<-specimen_ai_pop[,-which(substr(names(specimen_ai_pop),nchar(names(specimen_ai_pop))-1,nchar(names(specimen_ai_pop)))==".2")]
# 
# ### EBS POP ###
# # Enter species code
# species<-30060
# 
# # Get length data
# sizepop_goa_pop<-sqlQuery(channel,paste("SELECT * FROM GOA.SIZECOMP_TOTAL WHERE (((GOA.SIZECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# lfreq_goa_pop<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.LENGTH ON RACEBASE.HAUL.HAULJOIN = RACEBASE.LENGTH.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.LENGTH.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.LENGTH.REGION)='GOA') AND ((RACEBASE.LENGTH.SPECIES_CODE)=",species,") AND ((RACEBASE.HAUL.PERFORMANCE)>=0))",sep=""),believeNRows=FALSE)
# # Get age data
# agepop_goa_pop<-sqlQuery(channel,paste("SELECT * FROM GOA.AGECOMP_TOTAL WHERE (((GOA.AGECOMP_TOTAL.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# specimen_goa_pop<-sqlQuery(channel,paste("SELECT * FROM (RACEBASE.HAUL INNER JOIN RACEBASE.SPECIMEN ON RACEBASE.HAUL.HAULJOIN = RACEBASE.SPECIMEN.HAULJOIN) INNER JOIN GOA.BIENNIAL_SURVEYS ON RACEBASE.SPECIMEN.CRUISEJOIN = GOA.BIENNIAL_SURVEYS.CRUISEJOIN WHERE (((RACEBASE.SPECIMEN.SPECIES_CODE)=",species,") AND ((RACEBASE.SPECIMEN.REGION)='GOA'))",sep=""),believeNRows=FALSE)
# # Get CPUE data
# CPUE_goa_pop<-sqlQuery(channel,paste("SELECT * FROM GOA.CPUE WHERE (((GOA.CPUE.SPECIES_CODE)=",species,"))",sep=""),believeNRows=FALSE)
# # Remove redundant columns
# lfreq_goa_pop<-lfreq_goa_pop[,-which(substr(names(lfreq_goa_pop),nchar(names(lfreq_goa_pop))-1,nchar(names(lfreq_goa_pop)))==".1")]
# lfreq_goa_pop<-lfreq_goa_pop[,-which(substr(names(lfreq_goa_pop),nchar(names(lfreq_goa_pop))-1,nchar(names(lfreq_goa_pop)))==".2")]
# specimen_goa_pop<-specimen_goa_pop[,-which(substr(names(specimen_goa_pop),nchar(names(specimen_goa_pop))-1,nchar(names(specimen_goa_pop)))==".1")]
# specimen_goa_pop<-specimen_goa_pop[,-which(substr(names(specimen_goa_pop),nchar(names(specimen_goa_pop))-1,nchar(names(specimen_goa_pop)))==".2")]
# 
#==============================================================================#
#
### Combine species & region datasets ###
# sizepop_GOA<-rbind(sizepop_goa_dusky,sizepop_goa_pop,sizepop_goa_poll)
# lfreq_GOA<-rbind(lfreq_goa_dusky,lfreq_goa_pop,lfreq_goa_poll)
# agepop_GOA<-rbind(agepop_goa_dusky,agepop_goa_pop,agepop_goa_poll)
# specimen_GOA<-rbind(specimen_goa_dusky,specimen_goa_pop,specimen_goa_poll)
# CPUE_GOA<-rbind(CPUE_goa_dusky,CPUE_goa_pop,CPUE_goa_poll)

# sizepop_AI<-rbind(sizepop_ai_dusky,sizepop_ai_pop,sizepop_ai_poll)
# lfreq_AI<-rbind(lfreq_ai_dusky,lfreq_ai_pop,lfreq_ai_poll)
# agepop_AI<-rbind(agepop_ai_dusky,agepop_ai_pop,agepop_ai_poll)
# specimen_AI<-rbind(specimen_ai_dusky,specimen_ai_pop,specimen_ai_poll)
# CPUE_AI<-rbind(CPUE_ai_dusky,CPUE_ai_pop,CPUE_ai_poll)
# 
# sizepop_EBS<-rbind(sizepop_ebs_dusky,sizepop_ebs_pop,sizepop_ebs_poll)
# lfreq_EBS<-rbind(lfreq_ebs_dusky,lfreq_ebs_pop,lfreq_ebs_poll)
# agepop_EBS<-rbind(agepop_ebs_dusky,agepop_ebs_pop,agepop_ebs_poll)
# specimen_EBS<-rbind(specimen_ebs_dusky,specimen_ebs_pop,specimen_ebs_poll)
# CPUE_EBS<-rbind(CPUE_ebs_dusky,CPUE_ebs_pop,CPUE_ebs_poll)

# sizepop_dusky  <- rbind(sizepop_goa_dusky,sizepop_ai_dusky,sizepop_ebs_dusky)
# lfreq_dusky    <- rbind(lfreq_ai_dusky,lfreq_goa_dusky,lfreq_ebs_dusky)
# agepop_dusky   <- rbind(agepop_ai_dusky,agepop_goa_dusky,agepop_ebs_dusky)
# specimen_dusky <- rbind(specimen_goa_dusky,specimen_ai_dusky,specimen_ebs_dusky)
# CPUE_dusky     <- rbind(CPUE_ai_dusky,CPUE_goa_dusky,CPUE_ebs_dusky)

# Write data
# write.csv(sizepop_GOA,paste(getwd(),"/Data/sizepop_GOA.csv",sep=""))
# write.csv(lfreq_GOA,paste(getwd(),"/Data/lfreq_GOA.csv",sep=""))
# write.csv(agepop_GOA,paste(getwd(),"/Data/agepop_GOA.csv",sep=""))
# write.csv(specimen_GOA,paste(getwd(),"/Data/specimen_GOA.csv",sep=""))
# write.csv(CPUE_GOA,paste(getwd(),"/Data/CPUE_GOA.csv",sep=""))
# write.csv(strata_GOA,paste(getwd(),"/Data/strata_GOA.csv",sep=""))
# 
# write.csv(sizepop_AI,paste(getwd(),"/Data/sizepop_AI.csv",sep=""))
# write.csv(lfreq_AI,paste(getwd(),"/Data/lfreq_AI.csv",sep=""))
# write.csv(agepop_AI,paste(getwd(),"/Data/agepop_AI.csv",sep=""))
# write.csv(specimen_AI,paste(getwd(),"/Data/specimen_AI.csv",sep=""))
# write.csv(CPUE_AI,paste(getwd(),"/Data/CPUE_AI.csv",sep=""))
# write.csv(strata_AI,paste(getwd(),"/Data/strata_AI.csv",sep=""))
# 
# write.csv(sizepop_EBS,paste(getwd(),"/Data/sizepop_EBS.csv",sep=""))
# write.csv(lfreq_EBS,paste(getwd(),"/Data/lfreq_EBS.csv",sep=""))
# write.csv(agepop_EBS,paste(getwd(),"/Data/agepop_EBS.csv",sep=""))
# write.csv(specimen_EBS,paste(getwd(),"/Data/specimen_EBS.csv",sep=""))
# write.csv(CPUE_EBS,paste(getwd(),"/Data/CPUE_EBS.csv",sep=""))
# write.csv(strata_EBS,paste(getwd(),"/Data/strata_EBS.csv",sep=""))
}
write.csv(sizepop_goa_dusky,paste(getwd(),"/Data/sizepop_goa_dusky.csv",sep=""))
write.csv(lfreq_goa_dusky,paste(getwd(),"/Data/lfreq_goa_dusky.csv",sep=""))
write.csv(agepop_goa_dusky,paste(getwd(),"/Data/agepop_goa_dusky.csv",sep=""))
write.csv(specimen_goa_dusky,paste(getwd(),"/Data/specimen_goa_dusky.csv",sep=""))
write.csv(CPUE_goa_dusky,paste(getwd(),"/Data/CPUE_goa_dusky.csv",sep=""))
write.csv(strata_GOA,paste(getwd(),"/Data/strata_goa.csv",sep=""))

################################# Expansion ####################################

# Script to estimate population #'s at age for GOA/AI species
# This script builds from previously calculated pop'n est's at size

# Set directories and read in necessary data
# path<-getwd()
path <- setwd("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/Updated Code from Pete")
pathD<-paste0(path,"/Data")
pathR<-paste0(path,"/AC results")

sizepop<-read.csv(paste0(pathD,"/sizepop_goa_dusky.csv"))
specimen<-read.csv(paste0(pathD,"/specimen_goa_dusky.csv"))
agepop_RACE<-read.csv(paste0(pathD,"/agepop_goa_dusky.csv"))
# sizepop<-sizepop_GOA
# specimen<-specimen_GOA
# agepop_RACE<-agepop_GOA

library(ggplot2)

facet.names <- c("1" = " Male","2" = " Female")

ggplot()+
  geom_point(data=agepop_RACE,aes(x=SURVEY_YEAR,y=AGE,size=AGEPOP))+
  facet_wrap(SEX~.,strip.position = "top",ncol=2,labeller = as_labeller(facet.names))+ #scales="free_y",)
  # scale_y_continuous(limits=c(0,25000),breaks=c(0,0.5e4,1e4,1.5e4,2.0e4,2.5e4),labels=scales::comma)+
  # scale_color_manual(values = c("black","firebrick1"))+
  xlab("\n Year")+
  ylab("\n Age (Years) \n")+
  ggtitle("GOA Dusky AGEPOP_RACE")+
  theme_bw()+
  theme(legend.position = "top",
        axis.title = element_text(face="bold", size = 24),
        axis.text = element_text(color="black",size=22),
        axis.text.x = element_text(color="black",size=22),
        axis.ticks=element_line(color="black"),
        plot.title = element_text(hjust = 0.5, face="bold", size = 24),
        plot.margin=unit(c(0.5,0.5,0.1,0.1),"cm"),
        plot.background=element_rect(fill="white"),
        legend.text = element_text(size=18, face="bold"),
        legend.title = element_text(size=25, face = "bold"),
        strip.text = element_text(size=18, vjust=0.75, face = "bold"),
        # strip.background = element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_line(color="gray"),
        panel.grid.major=element_line(color="gray"))

ggsave("GOA Dusky AGEPOP_RACE.tiff", plot=last_plot(), device = "tiff",
       path = 'C:/Users/matthew.siskey/Desktop', 
       width=14, height=12, units="in", dpi=1000)

specimen<-subset(specimen,is.na(specimen$AGE)==FALSE)

# Get data parameters together
species<-sort(unique(specimen$SPECIES_CODE))
yrs<-sort(unique(specimen$YEAR))
sex<-sort(unique(specimen$SEX))


### Loop thru species ###
for(sp in 1:length(species)){
  
  # split dfs by species
  sizepop_sp<-subset(sizepop,sizepop$SPECIES_CODE==species[sp])
  specimen_sp<-subset(specimen,specimen$SPECIES_CODE==species[sp])
  agepop_RACE_sp<-subset(agepop_RACE,agepop_RACE$SPECIES_CODE==species[sp])
  
  # Set up results matrices
  ages<-sort(unique(specimen_sp$AGE))
  AGEPOP_M<-matrix(nrow=length(yrs),ncol=length(ages))
  colnames(AGEPOP_M)<-as.character(ages)
  rownames(AGEPOP_M)<-as.character(yrs)
  AGEPOP_F<-matrix(nrow=length(yrs),ncol=length(ages))
  colnames(AGEPOP_F)<-as.character(ages)
  rownames(AGEPOP_F)<-as.character(yrs)
  AGEPOP_U<-matrix(nrow=length(yrs),ncol=length(ages))
  colnames(AGEPOP_U)<-as.character(ages)
  rownames(AGEPOP_U)<-as.character(yrs)
  AGEPOP_T<-matrix(nrow=length(yrs),ncol=length(ages))
  colnames(AGEPOP_T)<-as.character(ages)
  rownames(AGEPOP_T)<-as.character(yrs)
  
  # Set up test matrix for comparison with RACE output
  RACEmatch<-matrix(nrow=length(yrs),ncol=4)
  colnames(RACEmatch)<-c("Year","Male","Female","Unsexed")
  RACEmatch<-as.data.frame(RACEmatch)
  RACEmatch$Year<-yrs
  
  ### Loop thru years ###
  for(y in 1:length(yrs)){
    
    # split dfs by year
    sizepop_sp_y<-subset(sizepop_sp,sizepop_sp$YEAR==yrs[y])
    specimen_sp_y<-subset(specimen_sp,specimen_sp$YEAR==yrs[y])
    agepop_RACE_sp_y<-subset(agepop_RACE_sp,agepop_RACE_sp$SURVEY_YEAR==yrs[y])
    
    ### Loop thru sex ###
    for(sx in 1:length(sex)){
      
      # split dfs by sex
      specimen_sp_y_sx<-subset(specimen_sp_y,specimen_sp_y$SEX==sex[sx])
      agepop_RACE_sp_y_sx<-subset(agepop_RACE_sp_y,agepop_RACE_sp_y$SEX==sex[sx])
      
      # Remove matrices to wipe clean each loop
      if(sx==1 & exists("pop_age_est_M")==TRUE)
        rm(pop_age_est_M)
      if(sx==2 & exists("pop_age_est_F")==TRUE)
        rm(pop_age_est_F)
      if(sx==3 & exists("pop_age_est_U")==TRUE)
        rm(pop_age_est_U)
      
      # Test if there's specimen data for particular sex
      if(length(specimen_sp_y_sx$SEX) == 0) {
        cat(paste("No specimen data for sex", sx, ",year" ,y,"sp",sp,"\n"))
        next
      }
      
      # If sex unknown and there is specimen data then use all specimen data
      if(sx==3)
        specimen_sp_y_sx<-specimen_sp_y

      # If there is no sizecomp data, we are wasting our time
      if(sex[sx] == 1 & sum(sizepop_sp_y$MALES)==0) {
        cat(paste("No sizecomp data for sex", sx, "\n"))
        next
      }
      if(sex[sx] == 2 & sum(sizepop_sp_y$FEMALES)==0) {
        cat(paste("No sizecomp data for sex", sx, "\n"))
        next
      }
      if(sex[sx] == 3 & sum(sizepop_sp_y$UNSEXED)==0) {
        cat(paste("No sizecomp data for sex", sx, "\n"))
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
      no.lengths <- unique(sort(specimen_sp_y_sx$LENGTH))[is.na(match(unique(sort(specimen_sp_y_sx$LENGTH)), sizepop_sp_y$LENGTH))]
      # Find lengths from sizecomp data where there is no age data.
      no.ages <- sizepop_sp_y$LENGTH[is.na(match(sizepop_sp_y$LENGTH, unique(specimen_sp_y_sx$LENGTH)))]
      if(length(no.ages) == 0)
        no.ages <- sizepop_sp_y$LENGTH
      no.age.sizecomp <- sizepop_sp_y[match(no.ages, sizepop_sp_y$LENGTH),]
      # Nothing else we can do when there are age data with no sizecomp data, so get rid of these records
      age_frac <- age_frac[is.na(match(as.numeric(dimnames(age_frac)[[1]]), no.lengths)),  ]
      # Estimate numbers by age and length
      # age_frac is a matrix of age x size classes (i.e., within each size class, what fraction are age 1 vs. age 2, etc.)
      # sizepop_sp_y is a df of numbers at size, separated by sex
      if(sex[sx] ==1)
        pop_age_est_M <- age_frac * sizepop_sp_y$MALES[match(as.numeric(dimnames(age_frac)[[1]]), as.numeric(sizepop_sp_y$LENGTH), nomatch = 0, incomparables = no.lengths)]
      if(sex[sx] ==2)
        pop_age_est_F <- age_frac * sizepop_sp_y$FEMALES[match(as.numeric(dimnames(age_frac)[[1]]), as.numeric(sizepop_sp_y$LENGTH), nomatch = 0, incomparables = no.lengths)]
      if(sex[sx] ==3)
        pop_age_est_U <- age_frac * sizepop_sp_y$UNSEXED[match(as.numeric(dimnames(age_frac)[[1]]), as.numeric(sizepop_sp_y$LENGTH), nomatch = 0, incomparables = no.lengths)]
      
      # End sex loop   
    }
    
    # Now sum up the numbers at age for all lengths and remove any 0s, and check to see if matches with RACE output
    
    # Males
    if(exists("pop_age_est_M")==TRUE){
      if(length(pop_age_est_M)>0){
        age_est_M <- apply(pop_age_est_M, 2,sum)
        if(length(which(age_est_M==0))>0)
          age_est_M <- age_est_M[-which(age_est_M==0)]
        agepop_RACE_sp_y_M<-subset(agepop_RACE_sp_y,agepop_RACE_sp_y$SEX==sex[1])
        
        # RACE match?
        test_M<-matrix(nrow=length(names(age_est_M)),ncol=3)
        colnames(test_M)<-c("Age","Calc","RACE")
        test_M[,1]<-as.numeric(names(age_est_M))
        test_M[,2]<-age_est_M
        for(i in 1:length(test_M[,3])){
          if(length(match(test_M[i,1],agepop_RACE_sp_y_M$AGE))>0)
            test_M[i,3]<-agepop_RACE_sp_y_M$AGEPOP[match(test_M[i,1],agepop_RACE_sp_y_M$AGE)]
        }
        test_M<-as.data.frame(test_M)
        RACEmatch$Male[y]<-max(abs(test_M$Calc-test_M$RACE)/test_M$RACE)
        
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
      RACEmatch$Female[y]<-max(abs(test_F$Calc-test_F$RACE)/test_F$RACE)
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
      RACEmatch$Unsexed[y]<-max(abs(test_U$Calc-test_U$RACE)/test_U$RACE)
      AGEPOP_U[y,match(as.numeric(names(age_est_U)),ages)]<-age_est_U
      AGEPOP_U[y,is.na(AGEPOP_U[y,])] <- 0
    }}
    
    ### End year loop ###
  }
  
  # Test if there's disagreement with RACE calcs (larger than 5% for the maximum difference at age - there are some years that are different with RACE that are unexplainable)
  if(length(which(RACEmatch$Male > 0.05)) > 0)
    cat(paste("Disagreement between calcs and RACE for Males of species", species[sp], "\n"))
  if(length(which(RACEmatch$Female > 0.05)) > 0)
    cat(paste("Disagreement between calcs and RACE for Females of species", species[sp], "\n"))
  if(length(which(RACEmatch$Unsexed > 0.05)) > 0)
    cat(paste("Disagreement between calcs and RACE for Unsexed of species", species[sp], "\n"))
  
  # Finalize results matrices
  AGEPOP_T <- matrix(mapply(sum,AGEPOP_M,AGEPOP_F,AGEPOP_U,MoreArgs=list(na.rm=TRUE)),ncol=length(ages))
  colnames(AGEPOP_T)<-as.character(ages)
  rownames(AGEPOP_T)<-as.character(yrs)
  
  # Write results matrices
  write.csv(RACEmatch,paste0(pathR,"/RACEmatch_",species[sp],"_GOA.csv"))
  write.csv(AGEPOP_M,paste0(pathR,"/AGEPOP_M_",species[sp],"_GOA.csv"))
  write.csv(AGEPOP_F,paste0(pathR,"/AGEPOP_F_",species[sp],"_GOA.csv"))
  write.csv(AGEPOP_U,paste0(pathR,"/AGEPOP_U_",species[sp],"_GOA.csv"))
  write.csv(AGEPOP_T,paste0(pathR,"/AGEPOP_T_",species[sp],"_GOA.csv"))
  
  ### End species loop ###
}

library(reshape2)
AGEPOP_T_long <- melt(AGEPOP_T)
colnames(AGEPOP_T_long) <- c("Year","Age","AGEPOP")
AGEPOP_T_long <- as.data.frame(AGEPOP_T_long)

AGEPOP_M_long <- melt(AGEPOP_M)
colnames(AGEPOP_M_long) <- c("Year","Age","AGEPOP")
AGEPOP_M_long <- as.data.frame(AGEPOP_M_long)

AGEPOP_F_long <- melt(AGEPOP_F)
colnames(AGEPOP_F_long) <- c("Year","Age","AGEPOP")
AGEPOP_F_long <- as.data.frame(AGEPOP_F_long)

ggplot()+
  geom_point(data=AGEPOP_M_long,aes(x=Year,y=Age,size=AGEPOP))+
  # facet_wrap(SEX~.,strip.position = "top",ncol=2,labeller = as_labeller(facet.names))+ #scales="free_y",)
  # scale_y_continuous(limits=c(0,25000),breaks=c(0,0.5e4,1e4,1.5e4,2.0e4,2.5e4),labels=scales::comma)+
  # scale_color_manual(values = c("black","firebrick1"))+
  xlab("\n Year")+
  ylab("\n Age (Years) \n")+
  ggtitle("GOA Dusky AGEPOP_Expansion (Male)")+
  theme_bw()+
  theme(legend.position = "top",
        axis.title = element_text(face="bold", size = 24),
        axis.text = element_text(color="black",size=22),
        axis.text.x = element_text(color="black",size=22),
        axis.ticks=element_line(color="black"),
        plot.title = element_text(hjust = 0.5, face="bold", size = 24),
        plot.margin=unit(c(0.5,0.5,0.1,0.1),"cm"),
        plot.background=element_rect(fill="white"),
        legend.text = element_text(size=18, face="bold"),
        legend.title = element_text(size=25, face = "bold"),
        strip.text = element_text(size=18, vjust=0.75, face = "bold"),
        # strip.background = element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_line(color="gray"),
        panel.grid.major=element_line(color="gray"))

ggsave("GOA Dusky AGEPOP_Expansion (Male).tiff", plot=last_plot(), device = "tiff",
       path = 'C:/Users/matthew.siskey/Desktop', 
       width=10, height=12, units="in", dpi=1000)


################################ Input SS Calc #################################
path           <- setwd("C:/Users/matthew.siskey/Desktop/2020_UW-AFSC Post-Doc/Data & Code/Updated Code from Pete")
pathD          <- paste0(path,"/Data")
pathR          <- paste0(path,"/AC results")
dusky.agepop   <- read.csv(paste0(pathR,"/AGEPOP_T_30152_GOA.csv"))
dusky.specimen <- read.csv(paste0(pathD,"/specimen_goa_dusky.csv"))
dusky.cpue     <- read.csv(paste0(pathD,"/CPUE_goa_dusky.csv"))
strata         <- read.csv(paste0(pathD,"/strata_GOA.csv"))
sizepop        <- read.csv(paste0(pathD,"/sizepop_goa_dusky.csv"))
agepop         <- read.csv(paste0(pathD,"/agepop_goa_dusky.csv"))
lfreq          <- read.csv(paste0(pathD,"/lfreq_goa_dusky.csv"))


for(i in 1:100){
  boot_haul = matrix(NA,nrow=length(unique(dusky.specimen$HAULJOIN)),ncol=4,dimnames=list(NULL,c("n_sampled","n_expanded","Age","Year")))
  hauls = unique(dusky.specimen$HAULJOIN)
  hauls = sample(hauls,size=length(hauls),replace=TRUE)
  bootrows = 0  # assume this line is just to start bootrows at 0 every loop
  for(ii in hauls){
    whichtow = which(ii==dusky.specimen$HAULJOIN)
    # Size = sum(dusky.specimen[whichtow,'??????']) * sizeratio
    Size = length(hauls) #calculate size of bootstrap sample -- should be the same as # of hauls every time, right?
    #Bootstrap
    bootrows                         = max(bootrows) + 1:length(whichtow)
    boot_haul[bootrows,"Age"]        = FOE[whichtow,'Age']
    boot_haul[bootrows,"Year"]       = FOE[whichtow,'Year']
    boot_haul[bootrows,"n_sampled"]  = table(factor(sample(x=1:???$n_c,size=Size,prob=Samp_iz[whichtow,'n_sampled'],
                                                           replace=TRUE),levels=1:???$n_c))
    boot_haul[bootrows,"n_expanded"] = boot_haul[bootrows,'n_sampled'] * Samp_iz[whichtow,'expansion_factor']
  }
}


################################################################################
# Step 3:  Estimate sample size
# Approach 3.1:  Estimate by conduct 100 bootstraps, where each involves resampling from samples in Step #2

n_boot = 100
Phat_bct = array( NA, dim=c(n_boot,dim(N_ct)) )

# Loop
for( bI in 1:100 ){
  # Bootstrap both tows and age-samples
  Boot_iz = matrix( NA, nrow=TmbData$n_i, ncol=4, dimnames=list(NULL, c("n_sampled","n_expanded","Age","Year")) )
  # Bootstrap tows
  whichhauls = unique(DF[ , 'HAULJOIN' ])
  whichhauls = sample( whichhauls, size=length(whichhauls), replace=TRUE )
  # Bootstrap ages in each tow
  bootrows = 0
  for( hI in whichhauls ){
    whichrows = which( hI == DF[,'HAULJOIN'] )
    # Calculate size of bootstrap sample
    Size = sum(Samp_iz[whichrows,'n_sampled']) * sizeratio
    # Conduct bootstrap sample
    bootrows = max(bootrows) + 1:length(whichrows)
    Boot_iz[bootrows,"Age"] = Data_Geostat[whichrows,'Age']
    Boot_iz[bootrows,"Year"] = Data_Geostat[whichrows,'Year']
    Boot_iz[bootrows,'n_sampled'] = table(factor(sample(x=1:TmbData$n_c, size=Size, prob=Samp_iz[whichrows,'n_sampled'], replace=TRUE), levels=1:TmbData$n_c))
    Boot_iz[bootrows,'n_expanded'] = Boot_iz[bootrows,'n_sampled'] * Samp_iz[whichrows,'expansion_factor']
  }
  Boot_ct = tapply( Boot_iz[,'n_expanded'], INDEX=list(Boot_iz[,'Age'],Boot_iz[,'Year']), FUN=sum )
  Phat_bct[bI,,] = standardize( Boot_ct )
}

################################################################################

harmonic.mean = function(vec) 1 / mean( 1/vec )
calculate_n = function( boot_ct, obs_ct ){
  boot_ct = standardize(boot_ct)
  obs_ct = standardize(obs_ct)
  num_ct = boot_ct*(1-boot_ct)
  denom_ct = (boot_ct - obs_ct)^2
  colSums(num_ct) / colSums(denom_ct)
}
Nboot_ct = apply( Phat_bct, MARGIN=2:3, FUN=mean )
N_bt = matrix(NA, nrow=n_boot, ncol=TmbData$n_t)
for( bI in 1:n_boot ){
  N_bt[bI,] = calculate_n( boot_ct=Phat_bct[bI,,], obs_ct=Nboot_ct )
}
if( summary_method=="harmonic.mean" ) Nsamp_srt[sI,rI,] = apply(N_bt, MARGIN=2, FUN=harmonic.mean )
if( summary_method=="mean" ) Nsamp_srt[sI,rI,] = apply(N_bt, MARGIN=2, FUN=mean )
