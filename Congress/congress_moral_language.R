#### REQUIREMENTS ####
library(RSQLite)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)

library(lme4)
library(lmerTest)
library(optimx)
library(car)
library(stats)

library(here)


#### DATA ####

# WARNING: If data are not present locally, this will download a ~966 MB file - don't run over a metered connection
data <- NULL
# try to read local copy
try( data <- fread( here('congress_data.csv'), data.table = FALSE ), silent = TRUE )

if( is.null(data) ) {
  # local read failed, get it remotely
  data <- fread("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/FQ8MIL/E2WK6A", 
                  data.table = FALSE )
  # save for future runs
  fwrite(data, file = here('congress_data.csv') )
}


#### PREPROCESSING ####

# Read in speaker info from database
sqlite_file = here::here( 'CongressInfo.sqlite' )
conn = dbConnect(drv=RSQLite::SQLite(), dbname=sqlite_file)
speakerinfo <- dbGetQuery(conn=conn, statement=paste("SELECT * FROM speakerinfo", sep=""))
dbDisconnect(conn)

# Loadings
loadings <- fread(input= here::here( 'DDR', 'document_dictionary_loadings.tsv' ), sep = '\t', header = TRUE)

# Rename columns
loadings <- plyr::rename(loadings, c("ID"="speech_id", "loyaltyVirtue_base" = "Loyaltyvirtue", "authorityVice_base" = "Authorityvice",
                                     "loyaltyVice_base" = "Loyaltyvice", "fairnessVice_base" = "Fairnessvice", "careVirtue_base" = "Carevirtue",
                                     "authorityVirtue_base" = "Authorityvirtue", "sancityVice_base" = "Purityvice", "sancityVirtue_base" = "Purityvirtue",
                                     "careVice_base" = "Carevice", "fairnessVirtue_base" = "Fairnessvirtue"))

data <- merge(loadings, speakerinfo, by="speech_id", all=FALSE)

# Create dummy coded party variable
data$partyd <- as.numeric(data$party == "D")
# Convert congress column to numeric
data$congress <- as.numeric(data$congress)

# Subset to congresses after 97th - shifts in party positions over time
data <- subset(data, congress >=97)

# Party control variables
party_control <- read.csv( here::here( 'party_control.csv' ) )
# Clean up Senate columns
# 1st 2 digits
party_control$SenateDemocrats <- substr(party_control$SenateDemocrats, start=1, stop=2)
party_control$SenateRepublicans <- substr(party_control$SenateRepublicans, start=1, stop=2)
# convert to numeric
party_control$SenateDemocrats <- as.numeric(party_control$SenateDemocrats)
party_control$SenateRepublicans <- as.numeric(party_control$SenateRepublicans)

party_control$senate_control <- ifelse(party_control$SenateDemocrats > party_control$SenateRepublicans, "D", "R")
party_control$house_control <- ifelse(party_control$HouseDemocrats > party_control$HouseRepublicans, "D", "R")

# Fix congress number column
party_control$congress <- as.numeric(gsub("\\D", "", party_control$Congress)) 

# Merge w/ dataframe for mlm analysis
data <- merge(data, party_control, by = "congress", all = FALSE)

# Long form data (used for some plots)

data.long <- melt(data, measure.var = c("Fairnessvirtue", "Carevirtue", 
                                        "Loyaltyvirtue", "Authorityvirtue", 
                                        "Purityvirtue", "Fairnessvice", 
                                        "Carevice", "Loyaltyvice", 
                                        "Authorityvice", "Purityvice"),
                  variable.name = "foundation",
                  value.name = "loading",
                  na.rm = TRUE
)

##### MODELLING #####

# Define list of foundations
foundations <- c( "Carevirtue", "Carevice", "Fairnessvirtue", "Fairnessvice", "Loyaltyvirtue", "Loyaltyvice", "Authorityvirtue",
                  "Authorityvice", "Purityvirtue",  "Purityvice" )

# Average for each speaker across each Congress and chamber in order to get these models to converge (chamber - to account for people who switch chambers within a Congress)
bycongress.speaker <- data %>% group_by(congress, speaker, chamber) %>% summarize(
  Carevirtue = mean(Carevirtue),
  Carevice   = mean(Carevice),
  
  Fairnessvirtue = mean(Fairnessvirtue),
  Fairnessvice = mean(Fairnessvice),
  
  Loyaltyvirtue = mean(Loyaltyvirtue),
  Loyaltyvice = mean(Loyaltyvice),
  
  Authorityvirtue = mean(Authorityvirtue),
  Authorityvice = mean(Authorityvice),
  
  Purityvirtue = mean(Purityvirtue),
  Purityvice = mean(Purityvice),
  
  senate_control = unique(senate_control),
  house_control = unique(house_control),
  President = unique(President),
  party = unique(party)
)

## Data clean-up: remove non-voting members; fix party affiliations for Independents ##
# remove: 11345,	10662, 12118, 11118, 11343
# change to D: 	10402, 11181, 12229, 11647, 11822, 11310 (107-109th Congress)
# change to R: 	11909, 11483 (in 106th Congress)
bycongress.speaker <- bycongress.speaker %>% 
  filter(!(speaker %in% c("11345", "10662", "12118", "11118", "11343")))
bycongress.speaker$party <- ifelse(bycongress.speaker$speaker %in% c("10402", "11181", "12229", "11647", "11822"), "D", bycongress.speaker$party)
bycongress.speaker$party <- ifelse(bycongress.speaker$speaker == "11909", "R", bycongress.speaker$party)
bycongress.speaker$party <- ifelse(bycongress.speaker$speaker == "11310" & bycongress.speaker$congress >= 107 & bycongress.speaker$congress <= 109, "D", bycongress.speaker$party)
bycongress.speaker$party <- ifelse(bycongress.speaker$speaker == "11483" & bycongress.speaker$congress == 106, "R", bycongress.speaker$party)

bycongress.speaker$party_d <- as.numeric(bycongress.speaker$party == "D")

## Create variables: match w/ speaker's party ##
# 1 = match; 0 = mismatch
bycongress.speaker$senate_match <- ifelse(bycongress.speaker$senate_control == bycongress.speaker$party, 1, 0)
bycongress.speaker$house_match <- ifelse(bycongress.speaker$house_control == bycongress.speaker$party, 1, 0)
bycongress.speaker$pres_match <- ifelse(bycongress.speaker$President == bycongress.speaker$party, 1, 0)

# Dummy code chamber: 0 = H, 1 = S
bycongress.speaker$chamber_dummy <- ifelse(bycongress.speaker$chamber == 'H', 0, 1)

# Standardize dvs
for (i in foundations) {
  bycongress.speaker[[paste(i, "raw", sep = "_")]] <- bycongress.speaker[[i]]
  bycongress.speaker[[i]] <- scale(bycongress.speaker[[i]], center = TRUE, scale = TRUE)
}

#### MAIN EFFECTS OF PARTY ####

for (i in foundations)  {
  cat( paste( "\n############### Overall frequency for:", i, "###############\n") )
  dv <- bycongress.speaker[[i]]
  model <- lmer(dv ~ party_d + (1|speaker) + (1|congress), data=bycongress.speaker, REML=TRUE)
  
  print( summary(model) )
  
  # save model
  assign( paste(i, "modParty", sep = "."), model )
}

#### EFFECTS OF PARTY CONTROL ####
for (i in foundations)  {
  cat( paste( "\n############### Effects of party control for:", i, "###############\n") )
  dv <- bycongress.speaker[[i]]
  model <- lmer(dv ~ senate_match + house_match + pres_match + (1|speaker) + (1|congress), data=bycongress.speaker, REML=TRUE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                                                                               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  print( summary(model) )
  
  # save model
  assign( paste(i, "modControl", sep = "."), model )
}

#### SEPARATE MODELS FOR SENATE / HOUSE ####
# Data frames
data_senate <- bycongress.speaker %>% filter(chamber == "S")
data_house <- bycongress.speaker %>% filter(chamber == "H")

## Optimization method changed to nlminb so models will converge
# Senate
for (i in foundations)  {
  cat( paste( "\n############### Effects of party control (Senate members) for:", i, "###############\n") )
  dv <- data_senate[[i]]
  model <- lmer(dv ~ party_d + senate_match + (1|speaker) + (1|congress), data=data_senate, REML=TRUE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                                                                    optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
  print( summary(model) )
  
  # save model
  assign( paste(i, "modS", sep = "."), model )
}

# House

for (i in foundations)  {
  cat( paste( "\n############### Effects of party control (House members) for:", i, "###############\n") )
  dv <- data_house[[i]]
  model <- lmer(dv ~ party_d + house_match + (1|speaker) + (1|congress), data=data_house, REML=TRUE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                                                                optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
  print( summary(model) )
  
  # save model
  assign( paste(i, "modH", sep = "."), model )
}

### Models w/ interactions ###
# Senate
for (i in foundations)  {
  cat( paste( "\n############### Effects of party control (Senate members) for:", i, "###############\n") )
  dv <- data_senate[[i]]
  model <- lmer(dv ~ party_d * senate_match + (1|speaker) + (1|congress), data=data_senate, REML=TRUE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                                                             optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
  print( summary(model) )
  
  # save model
  assign( paste(i, "modS_Int", sep = "."), model )
}

# House

for (i in foundations)  {
  cat( paste( "\n############### Effects of party control (House members) for:", i, "###############\n") )
  dv <- data_house[[i]]
  model <- lmer(dv ~ party_d * house_match + (1|speaker) + (1|congress), data=data_house, REML=TRUE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                                                           optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
  print( summary(model) )
  
  # save model
  assign( paste(i, "modH_Int", sep = "."), model )
}

# Model comparisons
for (i in foundations) {
  print(i)
  print(anova(get(paste(i, "modS", sep = ".")), get(paste(i, "modS_Int", sep = "."))))
}

for (i in foundations) {
  print(i)
  print(anova(get(paste(i, "modH", sep = ".")), get(paste(i, "modH_Int", sep = "."))))
}

#### Cross-chamber effects ####
# Senate
for (i in foundations)  {
  cat( paste( "\n############### Effects of cross-chamber control (Senate members) for:", i, "###############\n") )
  dv <- data_senate[[i]]
  model <- lmer(dv ~ house_match + senate_match + party_d + (1|speaker) + (1|congress), data=data_senate, REML=TRUE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                                                            optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
  print( summary(model) )
  
  # save model
  assign( paste(i, "modCS", sep = "."), model )
}

# House
for (i in foundations)  {
  cat( paste( "\n############### Effects of cross-chamber control (House members) for:", i, "###############\n") )
  dv <- data_house[[i]]
  model <- lmer(dv ~ senate_match + house_match + party_d + (1|speaker) + (1|congress), data=data_house, REML=TRUE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                                                                          optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  
  print( summary(model) )
  
  # save model
  assign( paste(i, "modCH", sep = "."), model )
}

#### Presidency effects ####
for (i in foundations)  {
  cat( paste( "\n############### Effects of White House control for:", i, "###############\n") )
  dv <- bycongress.speaker[[i]]
  model <- lmer(dv ~ pres_match + (1|speaker) + (1|congress), data=bycongress.speaker, REML=TRUE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                                                                               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  print( summary(model) )
  
  # save model
  assign( paste(i, "modP", sep = "."), model )
}


#### Effects of single-party governance ####
# Create variable (108, 109, and 111th Congresses)
bycongress.speaker$single_party_gov <- ifelse(bycongress.speaker$congress == 108 | bycongress.speaker$congress == 109 | bycongress.speaker$congress == 111, 1, 0)

for (i in foundations)  {
  cat( paste( "\n############### Effects of single party governance for:", i, "###############\n") )
  dv <- bycongress.speaker[[i]]
  model <- lmer(dv ~ single_party_gov + (1|speaker) + (1|congress), data=bycongress.speaker, REML=TRUE, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                                                                 optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  print( summary(model) )
  
  # save model
  assign( paste(i, "modSing", sep = "."), model )
}
