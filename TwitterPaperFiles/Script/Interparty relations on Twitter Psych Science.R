library(dplyr)
library(tidyverse)
library(psych)
library(lavaan)
library(rtweet)
library(lme4)
library(lmerTest)
library(emmeans)
library(tidytext)
library(tm)
library(quanteda)
library(irrCAC)
library(tweetscores)
library(parameters)
library(data.table)
library(sjPlot)
library(effsize)

#To use this code, read the CSV files posted on the OSF page for this project in and name it like this:
    #Study 1a should be called S1a
    #Study 1b should be called S1b
    #Study 2a should be called S2a
    #Study 2a should be called S2b
    #Study 3 should be called S3
    #Study 4 should be called S4
    #Study S1 should be called Ss1
    #Study S2 should be called Ss2


##### Study 1a-b ####

###### method ####
#Reliability; using Gwet's ac because most responses are in one category, which biases kappa
#1a
describe(S1a$coders_agreed)
conTable = table(S1a[,15:16])
gwet.ac1.table(conTable)
#1b
describe(S1b$coders_agreed)
conTable = table(S1b[,55:56])
gwet.ac1.table(conTable)

table(S1a$category)
table(S1b$category)




###### results ####
#create log-transformed DVs
S1a$t_retweets <- log1p(S1a$retweets + .01)
S1a$t_likes <- log1p(S1a$likes + .01)
S1b$t_retweets <- log1p(S1b$retweets + .01)
S1b$t_likes <- log1p(S1b$likes + .01)
#since only about 10% of our tweets come from Senators listed as Asian, Black, or Latinx, 
#estimates for each of those groups vary widely, so I'm collapsing those groups as POC and setting white as reference group
S1a$sen_raceWhite <- ifelse(S1a$sen_race == "white"," white","POC")
S1b$sen_raceWhite <- ifelse(S1b$sen_race == "white"," white","POC")


#STUDY 1A, set aside 'neither' tweets:
S1a_SvA <- subset(S1a, category != "neither")

#Likes with covariates
model<-(lmer(t_likes ~ engage + party2 + followers + year + length + media_or_link + sen_gender + sen_raceWhite + (1|senator), data = S1a_SvA))
parameters(model)
eff_size(emmeans(model, "engage"), sigma = sigma(model), edf = df.residual(model))

#RTs with covariates
model<-(lmer(t_retweets ~ engage + party2 + followers + year + length + media_or_link + sen_gender + sen_raceWhite + (1|senator), data = S1a_SvA))
parameters(model)
eff_size(emmeans(model, "engage"), sigma = sigma(model), edf = df.residual(model))


#STUDY 1B, set aside 'neither' tweets:
S1b_SvA <- subset(S1b, category != "neither")


#Likes with covariates
model<-(lmer(t_likes ~ engage + party2 + ideology + followers + year + length + media_or_link + sen_gender + sen_raceWhite + (1|senator), data = S1b_SvA))
parameters(model)
eff_size(emmeans(model, "engage"), sigma = sigma(model), edf = df.residual(model))

#RTs with covariates
model<-(lmer(t_retweets ~ engage + party2 + ideology + followers + year + length + media_or_link + sen_gender + sen_raceWhite + (1|senator), data = S1b_SvA))
parameters(model)
eff_size(emmeans(model, "engage"), sigma = sigma(model), edf = df.residual(model))



#Raw like and retweet counts for Senators’ tweets modeling engaging, dismissing, or neither
describeBy(S1a$likes,S1a$category)
describeBy(S1a$retweets,S1a$category)

describeBy(S1b$likes,S1b$category)
describeBy(S1b$retweets,S1b$category)





##### Study 2a ####
#READ IN DATAFILE AS "S2a"
###### method ####
table(S2a$english_check)
table(S2a$quality_check)

S2a <- S2a %>%
  filter(english_check == 1) %>%
  filter(is.na(quality_check) | quality_check == 1)

#formatting datafile
S2a <- as.data.frame(S2a)
S2a_long <- reshape(S2a, direction='long', 
                    varying=list(c("news_FT", "cafe_FT")), 
                    timevar='setting',
                    times=c("news","cafe"),
                    v.names="FT",
                    idvar='ResponseId')

S2a_long$FT <- as.numeric(S2a_long$FT)
S2a_long$ID <- (S2a_long$ResponseId)

S2a_long$target <- ifelse(S2a_long$cafe_target == "engage" & S2a_long$setting == "cafe","engage",
                          ifelse(S2a_long$news_target == "engage" & S2a_long$setting == "news","engage",
                                 ifelse(S2a_long$cafe_target == "dismiss" & S2a_long$setting == "cafe","dismiss",
                                        ifelse(S2a_long$news_target == "dismiss" & S2a_long$setting == "news","dismiss",""))))

S2a_long$issue <-  ifelse(S2a_long$cafe_issue == "climate change" & S2a_long$setting == "cafe","climate",
                          ifelse(S2a_long$cafe_issue == "extremists" & S2a_long$setting == "cafe","extremists",
                                 ifelse(S2a_long$cafe_issue == "COVID-19 vaccine distribution" & S2a_long$setting == "cafe","vax",
                                        ifelse(S2a_long$cafe_issue == "gun regulations" & S2a_long$setting == "cafe","guns",
                                               ifelse(S2a_long$cafe_issue == "the COVID-19 pandemic" & S2a_long$setting == "cafe","covid",
                                                      ifelse(S2a_long$cafe_issue == "tax plans" & S2a_long$setting == "cafe","tax",
                                                             ifelse(S2a_long$news_issue == "climate change" & S2a_long$setting == "news","climate",
                                                                    ifelse(S2a_long$news_issue == "gun regulations" & S2a_long$setting == "news","guns",
                                                                           ifelse(S2a_long$news_issue == "COVID-19 vaccine distribution" & S2a_long$setting == "news","vax",
                                                                                  ifelse(S2a_long$news_issue == "extremists" & S2a_long$setting == "news","extremists",
                                                                                         ifelse(S2a_long$news_issue == "the COVID-19 pandemic" & S2a_long$setting == "news","covid",
                                                                                                ifelse(S2a_long$news_issue == "tax plans" & S2a_long$setting == "news","tax",""))))))))))))
S2a_long$target <- as.factor(S2a_long$target)

S2a_long$stimuli_code <- ifelse(S2a_long$stimuli == "tweet", 1, -1)
S2a_long$role_code <- ifelse(S2a_long$role == "citizen", -1, 1)

S2a_long <- S2a_long %>%
  filter(ideo == "lib" & issue == "climate" & climate_stance == 2
         | ideo == "lib" & issue == "guns" & gun_stance == 2 
         | ideo == "lib" & issue == "vax" & vax_race_stance == 2
         | ideo == "lib" & issue == "tax" & tax_stance == 2 
         | ideo == "lib" & issue == "covid" & vaccine_stance == 2 
         | ideo == "lib" & issue == "extremists" & extrem_stance == 1
         | ideo == "con" & issue == "climate" & climate_stance == 1
         | ideo == "con" & issue == "guns" & gun_stance == 1
         | ideo == "con" & issue == "vax" & vax_race_stance == 1
         | ideo == "con" & issue == "tax" & tax_stance == 1
         | ideo == "con" & issue == "covid" & vaccine_stance == 2
         | ideo == "con" & issue == "extremists" & extrem_stance == 1)

S2a_long <- S2a_long %>%
  group_by(ResponseId) %>%
  mutate(ideology_C = ideology - 3.5) %>%
  mutate(ideology_SQ = ideology_C*ideology_C) %>%
  ungroup()

#reporting demographics after removing folks whose responses we didn't retain in analyses
S2a_long_demo <- S2a_long %>%
  filter(duplicated(ResponseId) == FALSE)

describe(S2a_long_demo$age)
table(S2a_long_demo$gender)/613




S2a_long_SenTwe <- S2a_long %>%
  filter(stimuli == "tweet" & role == "senator")

S2a_long_CitTwe <- S2a_long %>%
  filter(stimuli == "tweet" & role == "citizen")

S2a_long_SenVig <- S2a_long %>%
  filter(stimuli == "vignette" & role == "senator")

S2a_long_CitVig <- S2a_long %>%
  filter(stimuli == "vignette" & role == "citizen")

S2a_long_Sen <- S2a_long %>%
  filter(role == "senator")

S2a_long_Cit <- S2a_long %>%
  filter(role == "citizen")

S2a_long_Vig <- S2a_long %>%
  filter(stimuli == "vignette")

S2a_long_Twe <- S2a_long %>%
  filter(stimuli == "tweet")

###### results ####

describeBy(S2a_long_Sen$FT,S2a_long_Sen$target)
describeBy(S2a_long_Cit$FT,S2a_long_Cit$target)
describeBy(S2a_long_Vig$FT,S2a_long_Vig$target)
describeBy(S2a_long_Twe$FT,S2a_long_Twe$target)

#three-way
parameters(lmer(FT ~ target*role_code*stimuli_code + (1|ID),data = S2a_long))


S2a_long$role_Rcoded <- ifelse(S2a_long$role == "senator", 0, 1)
S2a_long$stimuli_Rcoded <- ifelse(S2a_long$stimuli == "vignette", 0, 1)


parameters(lmer(FT ~ target*role*stimuli_code + (1|ID),data = S2a_long)) #simple slope, citizens
parameters(lmer(FT ~ target*role_Rcoded*stimuli_code + (1|ID),data = S2a_long)) #simple slope, Senators
parameters(lmer(FT ~ target*role_code*stimuli + (1|ID),data = S2a_long)) #simple slope, Tweets
parameters(lmer(FT ~ target*role_code*stimuli_Rcoded + (1|ID),data = S2a_long)) #simple slope, vignette
parameters(lmer(FT ~ target*role_Rcoded*stimuli + (1|ID),data = S2a_long)) #senator tweet
parameters(lmer(FT ~ target*role_Rcoded*stimuli_Rcoded + (1|ID),data = S2a_long)) #senator vignette
parameters(lmer(FT ~ target*role*stimuli + (1|ID),data = S2a_long)) #citizen tweet
parameters(lmer(FT ~ target*role*stimuli_Rcoded + (1|ID),data = S2a_long)) #citizen vignette



##### Study 2b ####
#READ IN DATAFILE AS "S2b"

###### method ####
table(S2b$english_check)
table(S2b$quality_check)
S2b <- S2b %>%
  filter(english_check == 1) %>%
  filter(is.na(quality_check) | quality_check == 1)

#creating variables and data file
S2b$ideology_C <- (S2b$ideology - 3.5)
S2b$ideology_SQ <- (S2b$ideology_C*S2b$ideology_C)

S2b <- as.data.frame(S2b)
S2b_long <- pivot_longer(S2b, 14:19, names_to = "stimulus",values_to = "FT")
S2b_long$target <- ifelse(S2b_long$stimulus == "engage1","engage",ifelse(S2b_long$stimulus == "engage2","engage",ifelse(S2b_long$stimulus == "engage3","engage","dismiss")))
S2b_long$topic <- ifelse(S2b_long$stimulus == "engage1","climate",ifelse(S2b_long$stimulus == "engage2","guns",ifelse(S2b_long$stimulus == "engage3","covid",ifelse(S2b_long$stimulus == "dismiss1","vax",ifelse(S2b_long$stimulus == "dismiS2b","extremists",ifelse(S2b_long$stimulus == "dismisS2b","tax",""))))))

S2b_long$FT <- as.numeric(S2b_long$FT)
S2b_long$ID <- (S2b_long$ResponseId)
S2b_long$target <- as.factor(S2b_long$target)

S2b_long$stimuli_code <- ifelse(S2b_long$stimuli == "tweet", -1, 1)
S2b_long$role_code <- ifelse(S2b_long$role == "citizen", -1, 1)

S2b_long_lib <- subset(S2b_long, ideology < 4)
S2b_long_con <- subset(S2b_long, ideology > 3)

describe(S2b_long_lib$FT)
S2b_long_lib$FT <- ifelse(S2b_long_lib$topic == "climate" & S2b_long_lib$climate_stance == 1,"",S2b_long_lib$FT)
S2b_long_lib$FT <- as.numeric(S2b_long_lib$FT)
S2b_long_lib$FT <- ifelse(S2b_long_lib$topic == "guns" & S2b_long_lib$gun_stance == 1,"",S2b_long_lib$FT)
S2b_long_lib$FT <- as.numeric(S2b_long_lib$FT)
S2b_long_lib$FT <- ifelse(S2b_long_lib$topic == "vax" & S2b_long_lib$vax_race_stance == 1,"",S2b_long_lib$FT)
S2b_long_lib$FT <- as.numeric(S2b_long_lib$FT)
S2b_long_lib$FT <- ifelse(S2b_long_lib$topic == "tax" & S2b_long_lib$tax_stance == 1,"",S2b_long_lib$FT)
S2b_long_lib$FT <- as.numeric(S2b_long_lib$FT)
S2b_long_lib$FT <- ifelse(S2b_long_lib$topic == "covid" & S2b_long_lib$vaccine_stance == 1,"",S2b_long_lib$FT)
S2b_long_lib$FT <- as.numeric(S2b_long_lib$FT)
S2b_long_lib$FT <- ifelse(S2b_long_lib$topic == "extremists" & S2b_long_lib$extreme_stance == 2,"",S2b_long_lib$FT)
S2b_long_lib$FT <- as.numeric(S2b_long_lib$FT)
describe(S2b_long_lib$FT)

describe(S2b_long_con$FT)
S2b_long_con$FT <- ifelse(S2b_long_con$topic == "climate" & S2b_long_con$climate_stance == 2,"",S2b_long_con$FT)
S2b_long_con$FT <- as.numeric(S2b_long_con$FT)
S2b_long_con$FT <- ifelse(S2b_long_con$topic == "guns" & S2b_long_con$gun_stance == 2,"",S2b_long_con$FT)
S2b_long_con$FT <- as.numeric(S2b_long_con$FT)
S2b_long_con$FT <- ifelse(S2b_long_con$topic == "vax" & S2b_long_con$vax_race_stance == 2,"",S2b_long_con$FT)
S2b_long_con$FT <- as.numeric(S2b_long_con$FT)
S2b_long_con$FT <- ifelse(S2b_long_con$topic == "tax" & S2b_long_con$tax_stance == 2,"",S2b_long_con$FT)
S2b_long_con$FT <- as.numeric(S2b_long_con$FT)
S2b_long_con$FT <- ifelse(S2b_long_con$topic == "covid" & S2b_long_con$vaccine_stance == 1,"",S2b_long_con$FT)
S2b_long_con$FT <- as.numeric(S2b_long_con$FT)
S2b_long_con$FT <- ifelse(S2b_long_con$topic == "extremists" & S2b_long_con$extreme_stance == 2,"",S2b_long_con$FT)
S2b_long_con$FT <- as.numeric(S2b_long_con$FT)
describe(S2b_long_con$FT)

S2b_long <- rbind(S2b_long_lib,S2b_long_con)
S2b_long <- S2b_long[complete.cases(S2b_long[ , 28]),]

S2b_long_demo <- S2b_long %>%
  filter(duplicated(ResponseId) == FALSE)

describe(S2b_long_demo$age)
table(S2b_long_demo$gender)/199


S2b_long_Sen <- S2b_long %>%
  filter(role == "senator")

S2b_long_Cit <- S2b_long %>%
  filter(role == "citizen")

S2b_long_Vig <- S2b_long %>%
  filter(stimuli == "vignette")

S2b_long_Twe <- S2b_long %>%
  filter(stimuli == "tweet")

###### results ####

describeBy(S2b_long_Sen$FT,S2b_long_Sen$target)
describeBy(S2b_long_Cit$FT,S2b_long_Cit$target)
describeBy(S2b_long_Vig$FT,S2b_long_Vig$target)
describeBy(S2b_long_Twe$FT,S2b_long_Twe$target)
#three-way
parameters(lmer(FT ~ target*role_code*stimuli_code + (1|ID),data = S2b_long))


S2b_long$role_Rcoded <- ifelse(S2b_long$role == "senator", 0, 1)
S2b_long$stimuli_Rcoded <- ifelse(S2b_long$stimuli == "vignette", 0, 1)

parameters(lmer(FT ~ target*role*stimuli_code + (1|ID),data = S2b_long)) #simple slope, citizens
parameters(lmer(FT ~ target*role_Rcoded*stimuli_code + (1|ID),data = S2b_long)) #simple slope, Senators
parameters(lmer(FT ~ target*role_code*stimuli + (1|ID),data = S2b_long)) #simple slope, Tweets
parameters(lmer(FT ~ target*role_code*stimuli_Rcoded + (1|ID),data = S2b_long)) #simple slope, vignette

parameters(lmer(FT ~ target*role_Rcoded*stimuli + (1|ID),data = S2b_long)) #senator tweet
parameters(lmer(FT ~ target*role_Rcoded*stimuli_Rcoded + (1|ID),data = S2b_long)) #senator vignette
parameters(lmer(FT ~ target*role*stimuli + (1|ID),data = S2b_long)) #citizen tweet
parameters(lmer(FT ~ target*role*stimuli_Rcoded + (1|ID),data = S2b_long)) #citizen vignette



##### Study 3 ##### 
#READ IN DATAFILE AS "S3"
S3 <- read.csv("study3.csv")

###### method ####
table(S3$english_check) #answer should be 2
table(S3$quality_check) #answer should be 1
S3 <- S3 %>%
  filter(english_check == 2) %>%
  filter(is.na(quality_check) | quality_check == 1)

#demographics
describe(S3$age)
table(S3$gender) / 323


S3L <- pivot_longer(S3,14:697,names_to = "tweet",values_to = "DV")
S3L <- drop_na(S3L,DV)
S3L <- separate(S3L,tweet,sep = "_",into = c("party_behavior_DVtype","tweet_number"))
S3L <- separate(S3L,party_behavior_DVtype,sep = c(1,2) ,into = c("party_short","behavior","measure"))
S3L <- unite(S3L,"tweet_number",c("tweet_number","party_short"))
S3L$behavior <- ifelse(S3L$behavior == "E", "engage","dismiss")
S3L$measure <- ifelse(S3L$measure == "A", "Approval",ifelse(S3L$measure == "F", "SenLiking",ifelse(S3L$measure == "L", "Like","Retweet")))
S3L$tweet_number <- as.factor(S3L$tweet_number)
S3LA <- filter(S3L, measure == "Approval")
S3LF <- filter(S3L, measure == "SenLiking")
S3LLR <- filter(S3L, measure == "Like" | measure == "Retweet")
TR_wideLR <- pivot_wider(S3LLR, names_from = "measure",values_from = "DV")

#reported correlation between Likes, RTs: r = .61
cor.test(TR_wideLR$Retweet,TR_wideLR$Like)

S3L$zDV <- ifelse(S3L$measure == "Like", scale(2 - S3L$DV),ifelse(S3L$measure == "Retweet", scale(2 - S3L$DV),ifelse(S3L$measure == "Approval", scale(S3L$DV),scale(S3L$DV))))
TR_wideLR$Like <- (1 - TR_wideLR$Like) #so higher values = preference
TR_wideLR$Retweet <- (1 - TR_wideLR$Retweet)  #so higher values = preference
TR_wideLR$zDV <- scale(((TR_wideLR$Like + TR_wideLR$Retweet)/2))
S3LLR <- pivot_longer(TR_wideLR,25:26,names_to = "measure",values_to = "DV")
S3LA$zDV <- scale(S3LA$DV)
S3LF$zDV <- scale(S3LF$DV)
S3L <- rbind(S3LA,S3LF,S3LLR)
S3L$ID <- S3L$ResponseId #shorter name

#create frequent reactor variable
S3L$interact_freq <- ifelse(S3L$interact_senator == 3, 1, 0)
S3L$interact_freqREV <- (1 - S3L$interact_freq) #make frequent reactors the reference group

#creating centered party affiliation and ideology variables and extremity variables
S3L$party_affil_C <- (S3L$party_affil - 3.5) 
S3L$party_affil_SQ <- (S3L$party_affil_C*S3L$party_affil_C)
S3L$ideology_C <- (S3L$ideology - 4)
S3L$ideology_SQ <- (S3L$ideology_C*S3L$ideology_C)

#affective polarization: for people on one side of party affiliation scale, their inparty warmth minus outparty warmth
S3L$AffPol <- ifelse(S3L$party_affil < 4, (S3L$DEM_FT - S3L$REP_FT), (S3L$REP_FT - S3L$DEM_FT))


S3L$DV_type <- ifelse(S3L$measure == "Like", "LRT",ifelse(S3L$measure == "Retweet", "LRT", ifelse(S3L$measure == "Approval", "approval", "FT")))
S3L$DV_typeLRT <- ifelse(S3L$DV_type == "LRT", " LRT", ifelse(S3L$DV_type == "approval", "approval", "FT"))
S3L$DV_typeFT <- ifelse(S3L$DV_type == "FT", " FT", ifelse(S3L$DV_type == "approval", "approval", "LRT"))

S3L$behaviorEng <- ifelse(S3L$behavior == "engage", " Engage", "Dismiss")
S3L$Behavior <- ifelse(S3L$behavior == "dismiss", "Dismiss","Engage")
S3L <- S3L %>% 
  mutate(Behavior = Behavior %>%
  fct_relevel("Engage", "Dismiss"))
S3L$DV_typeG <- ifelse(S3L$DV_type == "approval", "Approval of tweet", ifelse(S3L$DV_type == "FT", "Liking Senator", ifelse(S3L$DV_type == "LRT", "Intent to Like, Retweet","")))
S3L <- S3L %>% 
  mutate(DV_typeG = DV_typeG %>%
  fct_relevel("Approval of tweet", "Liking Senator","Intent to Like, Retweet"))

S3$interact_freq <- ifelse(S3$interact_senator == 3, 1, 0)
S3$ReactionFrequency <- ifelse(S3$interact_senator == 3, "Frequent Reactors", "Everyone Else")
S3$party_affil_C <- (S3$party_affil - 3.5) 
S3$party_affil_SQ <- (S3$party_affil_C*S3$party_affil_C)
S3$ideology_C <- (S3$ideology - 4)
S3$ideology_SQ <- (S3$ideology_C*S3$ideology_C)
S3$AffPol <- ifelse(S3$party_affil < 4, (S3$DEM_FT - S3$REP_FT), (S3$REP_FT - S3$DEM_FT))
S3$use_twitter_coded <- ifelse(S3$use_twitter == 1, "yes", "no")


###### results ####
#REACTION FREQUENCY BY TWEET TYPE INTERACTION
model <- lmer(zDV~behavior*interact_freqREV + (1|tweet_number)+(1|ID), S3L)
parameters(model)
parameters(lmer(zDV~behavior*interact_freq + (1|tweet_number)+(1|ID), S3L))
eff_size(emmeans(model, pairwise ~ behavior|interact_freqREV,, lmer.df = "satterthwaite",lmerTest.limit = 4559), sigma = sigma(model), edf = df.residual(model))


#REACTION FREQUENCY BY TWEET TYPE BY MEASURE INTERACTION
parameters(lmer(zDV ~ behavior*interact_freqREV*DV_type + (1|tweet_number) + (1|ID), S3L)) #
parameters(lmer(zDV ~ behavior*interact_freq*DV_type + (1|tweet_number) + (1|ID), S3L))

parameters(lmer(zDV ~ behavior*interact_freqREV*DV_typeFT + (1|tweet_number) + (1|ID), S3L)) #
parameters(lmer(zDV ~ behavior*interact_freq*DV_typeFT + (1|tweet_number) + (1|ID), S3L))

parameters(lmer(zDV ~ behavior*interact_freqREV*DV_typeLRT + (1|tweet_number) + (1|ID), S3L)) #
parameters(lmer(zDV ~ behavior*interact_freq*DV_typeLRT + (1|tweet_number) + (1|ID), S3L))


#how much frequent reactors differ from everyone else
S3$freq_coded <- ifelse(S3$interact_senator == 3, "frequent", "everyone else")

#to make difference score variable, need to average across DV ratings...
TRM <- S3L %>% 
  group_by(behavior, ID) %>% 
  mutate(avgDV = mean(zDV, na.rm = TRUE)) %>%
  ungroup() %>%
  unite("ID_beh",c("ID","behavior"), remove = FALSE) %>%
  distinct(ID_beh, .keep_all = TRUE)

TRM2 = TRM %>% 
  pivot_wider(id_cols = ID, names_from=behavior, values_from=avgDV)
TRM <- left_join(TRM2, TRM, by = "ID", keep = TRUE)
TRM <- TRM %>% distinct(TRM$ID.x, .keep_all = TRUE)
TRM$dif <- (TRM$engage - TRM$dismiss)

cohens_d(TRM$dif,TRM$freq_coded) 

#the rest of the variables we can just look at the initial, un-transformed dataset
cohens_d(S3$party_affil_SQ,S3$ReactionFrequency)
cohens_d(S3$party_status,S3$ReactionFrequency)
cohens_d(S3$AffPol,S3$ReactionFrequency)
cohens_d(S3$compromise,S3$ReactionFrequency) #Reverse score the D here for opposition to compromise

cohens_d(S3$use_twitter,S3$ReactionFrequency) #Reverse score the D here for more twitter use
cohens_d(S3$ind_status,S3$ReactionFrequency)
cohens_d(S3$gender_binary,S3$ReactionFrequency) #Reverse score the D here for men-leaning
cohens_d(S3$party_affil_C,S3$ReactionFrequency) #Reverse score the D here for more Repub. leaning
cohens_d(S3$age,S3$ReactionFrequency)



##### Study 4 #####
###### method #####
S4 <- read.csv("study4.csv")
#READ IN DATAFILE AS "S4"
table(S4$english_check)
table(S4$quality_check)
S4 <- S4 %>%
  filter(english_check == 2) %>%
  filter(is.na(quality_check) | quality_check == 1)
describe(S4$age)
table(S4$gender)/261


#arranging dataframe for analyses:
S4L <- pivot_longer(S4,15:869,names_to = "tweet",values_to = "DV")
S4L <- drop_na(S4L,DV)
S4L <- separate(S4L,tweet,sep = "_",into = c("tweet_number","DV_type"))
S4L <- separate(S4L,tweet_number,sep = c(1,2) ,into = c("party_short","behavior","number"),remove = FALSE)
S4L$behavior <- ifelse(S4L$behavior == "E", "Engaging","Dismissing")

#creating variables
S4L$ID <- S4L$ResponseId #shorter name
#creating centered party and ideology and extremity variables
S4L$party_affil_C <- (S4L$party_affil - 3.5)
S4L$party_affil_SQ <- (S4L$party_affil_C*S4L$party_affil_C)
S4L$ideology_C <- (S4L$ideology - 4)
S4L$ideology_SQ <- (S4L$ideology_C*S4L$ideology_C)
#affective polarization: given participant's party, warmth toward inparty minus toward outparty
S4L$AffPol <- ifelse(S4L$party_short == "D", (S4L$DEM_FT - S4L$REP_FT), (S4L$REP_FT - S4L$DEM_FT))
S4L$DV<-as.numeric(S4L$DV)
S4L$Behavior<-as.factor(S4L$behavior)
S4L <- S4L %>% 
  mutate(Behavior = Behavior %>%
           fct_relevel("Engaging", "Dismissing"))

#intercorrelations between attributes
S4L_comb <- pivot_wider(S4L,names_from = "DV_type",values_from = "DV")
names(S4L_comb)
alpha(S4L_comb[c("tolerant","cooperative","rational","legit","open")])
cor_table <- S4L_comb %>%
  select("tolerant","cooperative","rational","legit","open")
tab_corr(cor_table, "pairwise",digits = 2,triangle = "lower", p.numeric = T)



S4L_tol <- subset(S4L, DV_type == "tolerant")
S4L_coop <- subset(S4L, DV_type == "cooperative")
S4L_rational <- subset(S4L, DV_type == "rational")
S4L_legit <- subset(S4L, DV_type == "legit")
S4L_open <- subset(S4L, DV_type == "open")


S4L_tol$studyID <- S4L_tol$tweet_number
S4L_coop$studyID <- S4L_coop$tweet_number
S4L_rational$studyID <- S4L_rational$tweet_number
S4L_legit$studyID <- S4L_legit$tweet_number
S4L_open$studyID <- S4L_open$tweet_number


setDT(S4L_tol)[, tolerant := mean(DV), by = tweet_number]
setDT(S4L_coop)[, cooperative := mean(DV), by = tweet_number]
setDT(S4L_rational)[, rational := mean(DV), by = tweet_number]
setDT(S4L_legit)[, legitimizing := mean(DV), by = tweet_number]
setDT(S4L_open)[, open := mean(DV), by = tweet_number]


S3L_combining <- separate(S3L,tweet_number,"_",into = c("number","partyShort"))
S3L_combining$behaviorShort <- ifelse(S3L_combining$behavior == "engage","E","D")
S3L_combining <- unite(S3L_combining,"studyID",c("partyShort","behaviorShort","number"))
S3L_combining$studyID <- sub("_", "", S3L_combining$studyID)
S3L_combining$studyID <- sub("_", "", S3L_combining$studyID)
S3L_combining$X <- seq.int(nrow(S3L_combining))
tweet1 <- left_join(S3L_combining, S4L_tol, by = "studyID")
tweet1[c(43:77)] <- NULL
tweet1 <- tweet1[!duplicated(tweet1$X), ]
tweet2 <- left_join(tweet1, S4L_coop, by = "studyID")
tweet2[c(44:78)] <- NULL
tweet2 <- tweet2[!duplicated(tweet2$X), ]
tweet2 <- left_join(tweet2, S4L_rational, by = "studyID")
tweet2[c(45:79)] <- NULL
tweet2 <- tweet2[!duplicated(tweet2$X), ]
tweet4 <- left_join(tweet2, S4L_legit, by = "studyID")
tweet4[c(46:80)] <- NULL
tweet4 <- tweet4[!duplicated(tweet4$X), ]
tweet5 <- left_join(tweet4, S4L_open, by = "studyID")
tweet5[c(47:81)] <- NULL
S3_S4 <- tweet5[!duplicated(tweet5$X), ]
S3_S4$behavior <- S3_S4$behavior.x
S3_S4$ID <- S3_S4$ResponseId.x
S3_S4$NUM <- S3_S4$studyID
S3_S4$DVs <- S3_S4$DV_type.x
S3_S4$reactors <- ifelse(S3_S4$interact_freq == 1, "FI","nonFI")
S3_S4$reactorsREV <- ifelse(S3_S4$interact_freq == 0, " nonFI","FI")
S3_S4$TC <- (S3_S4$tolerant - describe(S3_S4$tolerant)$mean)
S3_S4$CC <- (S3_S4$cooperative - describe(S3_S4$cooperative)$mean)
S3_S4$RC <- (S3_S4$rational - describe(S3_S4$rational)$mean)
S3_S4$LC <- (S3_S4$legitimizing - describe(S3_S4$legitimizing)$mean)
S3_S4$OC <- (S3_S4$open - describe(S3_S4$open)$mean)


###### results #####

parameters(lmer(zDV~reactors*TC+behavior+(1|NUM)+(1|ID),S3_S4))
parameters(lmer(zDV~reactorsREV*TC+behavior+(1|NUM)+(1|ID),S3_S4))
parameters(lmer(zDV~reactors*CC+ behavior+(1|NUM)+(1|ID),S3_S4))
parameters(lmer(zDV~reactorsREV*CC+ behavior+(1|NUM)+(1|ID),S3_S4))
parameters(lmer(zDV~reactors*RC+behavior+(1|NUM)+(1|ID),S3_S4))
parameters(lmer(zDV~reactorsREV*RC+behavior+(1|NUM)+(1|ID),S3_S4))
parameters(lmer(zDV~reactors*LC+ behavior+(1|NUM)+(1|ID),S3_S4))
parameters(lmer(zDV~reactorsREV*LC+ behavior+(1|NUM)+(1|ID),S3_S4))
parameters(lmer(zDV~reactors*OC+behavior+(1|NUM)+(1|ID),S3_S4))
parameters(lmer(zDV~reactorsREV*OC+behavior+(1|NUM)+(1|ID),S3_S4))




##### SOM #####
###### Study 1a-b SOM #####



#Comparing all tweets initially collected in 1a to those selected with relevant keywords
all_1a <- read.csv("1a_all_tweets_LIWC.csv")

#do selected tweets differ on... basic tweet features?
data_tidy$length <- nchar(data_tidy$Text)
t.test(length ~ selected, data = data_tidy, var.equal = TRUE)
cohens_d(length ~ selected, data = data_tidy)
describeBy(data_tidy$length, group = data_tidy$selected)

t.test(year ~ selected, data = data_tidy, var.equal = TRUE)
cohensD(year ~ selected, data = data_tidy)
describeBy(data_tidy$year, group = data_tidy$selected)

t.test(as.numeric(ideology) ~ selected, data = data_tidy, var.equal = TRUE)
cohensD(as.numeric(ideology) ~ selected, data = data_tidy)
describeBy(as.numeric(data_tidy$ideology), group = data_tidy$selected)

t.test(sen_followers ~ selected, data = data_tidy, var.equal = TRUE)
cohensD(sen_followers ~ selected, data = data_tidy)
describeBy(data_tidy$sen_followers, group = data_tidy$selected)

#do selected tweets differ on... Senator features?
chisq.test(data_tidy$selected, data_tidy$caucus, correct=FALSE)
table(data_tidy$selected, data_tidy$caucus)

chisq.test(data_tidy$selected, data_tidy$gender, correct=FALSE)
table(data_tidy$selected, data_tidy$gender)

chisq.test(data_tidy$selected, data_tidy$white, correct=FALSE)
table(data_tidy$selected, data_tidy$white)


##do selected tweets differ on... linguistic features coded in LIWC dictionaries
t.test(tone_pos ~ selected, data = data_tidy, var.equal = TRUE)
cohensD(tone_pos ~ selected, data = data_tidy)
describeBy(data_tidy$tone_pos, group = data_tidy$selected)

t.test(emo_pos ~ selected, data = data_tidy, var.equal = TRUE)
cohensD(emo_pos ~ selected, data = data_tidy)
describeBy(data_tidy$emo_pos, group = data_tidy$selected)

t.test(tone_neg ~ selected, data = data_tidy, var.equal = TRUE)
cohensD(tone_neg ~ selected, data = data_tidy)
describeBy(data_tidy$tone_neg, group = data_tidy$selected)

t.test(emo_neg ~ selected, data = data_tidy, var.equal = TRUE)
cohensD(emo_neg ~ selected, data = data_tidy)
describeBy(data_tidy$emo_neg, group = data_tidy$selected)

t.test(moral ~ selected, data = data_tidy, var.equal = TRUE)
cohensD(moral ~ selected, data = data_tidy)
describeBy(data_tidy$moral, group = data_tidy$selected)

t.test(certitude ~ selected, data = data_tidy, var.equal = TRUE)
cohensD(certitude ~ selected, data = data_tidy)
describeBy(data_tidy$certitude, group = data_tidy$selected)

t.test(polite ~ selected, data = data_tidy, var.equal = TRUE)
cohensD(polite ~ selected, data = data_tidy)
describeBy(data_tidy$polite, group = data_tidy$selected)

###do selected tweets differ on... linguistic features coded in established literature dictionaries?
t.test(pos_affect_count ~ selected, data = data_tidy, var.equal = TRUE)
cohensD(pos_affect_count ~ selected, data = data_tidy)

t.test(neg_affect_count ~ selected, data = data_tidy, var.equal = TRUE)
cohensD(neg_affect_count ~ selected, data = data_tidy)

t.test(shared_count ~ selected, data = data_tidy, var.equal = TRUE)
cohensD(shared_count ~ selected, data = data_tidy)






#Independent coding sample ratings:
#READ IN DATAFILE AS "S1b_code"
table(S1b_code$english_check) #answer should be 2
table(S1b_code$quality_check) #answer should be 1

S1b_code <- S1b_code %>%
  filter(english_check == 2) %>%
  filter(is.na(quality_check) | quality_check == 1)

describe(S1b_code$age)
table(S1b_code$gender)/536

S1b_ED_long <- pivot_longer(S1b_code,14:184,names_to = "tweet",values_to = "DV")
S1b_ED_long <- drop_na(S1b_ED_long,DV)
S1b_ED_long <- separate(S1b_ED_long,tweet,sep = c(1,2),into = c("party_short","behavior","tweetnum"),remove = FALSE)
S1b_ED_long$DV <- (S1b_ED_long$DV - 3) #centering
setDT(S1b_ED_long)[, avgDV := mean(DV), by = tweet]
S1b_ED_long$Behavior <- ifelse(S1b_ED_long$behavior == "E", "Engaging","Dismissing")
S1b_ED_long <- S1b_ED_long %>% 
  mutate(Behavior = Behavior %>%
           fct_relevel("Engaging", "Dismissing"))
TC_sums <- S1b_ED_long %>%
  group_by(tweet) %>%
  summarise(avgDis = mean(DV))


parameters(lmer(DV ~ Behavior + (1|ResponseId) + (1|tweet), S1b_ED_long))

describeBy(S1b_ED_long$avgDV,S1b_ED_long$Behavior)

#Percent of tweets that are above, below midpoint DIVIDED BY number of tweets
sum(TC_sums$avgDis > 0)/118
sum(TC_sums$avgDis < 0)/53





#key test WITHOUT covariates
#Likes, no covariates
model<-(lmer(t_likes ~ engage + (1|senator), data = S1a_SvA))
parameters(model)
eff_size(emmeans(model, "engage"), sigma = sigma(model), edf = df.residual(model))
model<-(lmer(t_likes ~ engage + (1|senator), data = S1b_SvA))
parameters(model)
eff_size(emmeans(model, "engage"), sigma = sigma(model), edf = df.residual(model))
#RTs, no covariates
model<-(lmer(t_retweets ~ engage + (1|senator), data = S1a_SvA))
parameters(model)
eff_size(emmeans(model, "engage"), sigma = sigma(model), edf = df.residual(model))
model<-(lmer(t_retweets ~ engage + (1|senator), data = S1b_SvA))
parameters(model)
eff_size(emmeans(model, "engage"), sigma = sigma(model), edf = df.residual(model))


#COMPARISONS WITH NEITHER TWEETS
#1A Likes
model<-(lmer(t_likes ~ engage + dismiss + (1|senator), data = S1a))
parameters(model)
eff_size(emmeans(model, "engage"), sigma = sigma(model), edf = df.residual(model))
eff_size(emmeans(model, "dismiss"), sigma = sigma(model), edf = df.residual(model))
#1A Likes with covariates
model<-(lmer(t_likes ~ engage + dismiss + party2 + followers + year+ length + media_or_link + sen_gender + sen_raceWhite + (1|senator), data = S1a))
parameters(model)
eff_size(emmeans(model, "engage"), sigma = sigma(model), edf = df.residual(model))
eff_size(emmeans(model, "dismiss"), sigma = sigma(model), edf = df.residual(model))

#1A RTs
model<-(lmer(t_retweets ~ engage + dismiss + (1|senator), data = S1a))
parameters(model)
eff_size(emmeans(model, "engage"), sigma = sigma(model), edf = df.residual(model))
eff_size(emmeans(model, "dismiss"), sigma = sigma(model), edf = df.residual(model))
#1A RTs with covariates
model<-(lmer(t_retweets ~ engage + dismiss + party2 + followers + year+ length + media_or_link + sen_gender + sen_raceWhite + (1|senator), data = S1a))
parameters(model)
eff_size(emmeans(model, "engage"), sigma = sigma(model), edf = df.residual(model))
eff_size(emmeans(model, "dismiss"), sigma = sigma(model), edf = df.residual(model))


#1B Likes
model<-(lmer(t_likes ~ engage + dismiss + (1|senator), data = S1b))
parameters(model)
eff_size(emmeans(model, "engage"), sigma = sigma(model), edf = df.residual(model))
eff_size(emmeans(model, "dismiss"), sigma = sigma(model), edf = df.residual(model))
#1B Likes with covariates
model<-(lmer(t_likes ~ engage + dismiss + party2 + ideology + followers + year + length + media_or_link + sen_gender + sen_raceWhite + (1|senator), data = S1b))
parameters(model)
eff_size(emmeans(model, "engage"), sigma = sigma(model), edf = df.residual(model))
eff_size(emmeans(model, "dismiss"), sigma = sigma(model), edf = df.residual(model))

#1B RTs
model<-(lmer(t_retweets ~ engage + dismiss + (1|senator), data = S1b))
parameters(model)
eff_size(emmeans(model, "engage"), sigma = sigma(model), edf = df.residual(model))
eff_size(emmeans(model, "dismiss"), sigma = sigma(model), edf = df.residual(model))

#1B RTs with covariates
model<-(lmer(t_retweets ~ engage + dismiss + party2 + ideology + followers + year + length + media_or_link + sen_gender + sen_raceWhite + (1|senator), data = S1b))
parameters(model)
eff_size(emmeans(model, "engage"), sigma = sigma(model), edf = df.residual(model))
eff_size(emmeans(model, "dismiss"), sigma = sigma(model), edf = df.residual(model))



#ANALYZING RAW LIKE, RT COUNTS
#1a
model1<-(lmer(likes ~ engage + neither + (1|senator), data = S1a))
model2<-(lmer(likes ~ engage + neither + party + followers + year+ length + media_or_link + sen_raceWhite + sen_gender + (1|senator), data = S1a))
model3<-(lmer(retweets ~ engage + neither + (1|senator), data = S1a))
model4<-(lmer(retweets ~ engage + neither + party + followers + year+ length + media_or_link + sen_raceWhite + sen_gender +  (1|senator), data = S1a))

parameters(model1)
eff_size(emmeans(model1, "engage", lmer.df = "satterthwaite",lmerTest.limit = 5000), sigma = sigma(model1), edf = df.residual(model1))
parameters(model2)
eff_size(emmeans(model2, "engage", lmer.df = "satterthwaite",lmerTest.limit = 5000), sigma = sigma(model2), edf = df.residual(model2))

parameters(model3)
eff_size(emmeans(model3, "engage", lmer.df = "satterthwaite",lmerTest.limit = 5000), sigma = sigma(model3), edf = df.residual(model3))
parameters(model4)
eff_size(emmeans(model4, "engage", lmer.df = "satterthwaite",lmerTest.limit = 5000), sigma = sigma(model4), edf = df.residual(model4))

#1b
model1<-(lmer(likes ~ engage + neither + (1|senator), data = S1b))
model2<-(lmer(likes ~ engage + neither + party2 + followers + year+ length + media_or_link + sen_raceWhite + sen_gender + (1|senator), data = S1b))
model3<-(lmer(retweets ~ engage + neither + (1|senator), data = S1b))
model4<-(lmer(retweets ~ engage + neither + party2 + followers + year+ length + media_or_link + sen_raceWhite + sen_gender +  (1|senator), data = S1b))

parameters(model1)
eff_size(emmeans(model1, "engage"), sigma = sigma(model1), edf = df.residual(model1))
parameters(model2)
eff_size(emmeans(model2, "engage"), sigma = sigma(model2), edf = df.residual(model2))

parameters(model3)
eff_size(emmeans(model3, "engage"), sigma = sigma(model3), edf = df.residual(model3))
parameters(model4)
eff_size(emmeans(model4, "engage"), sigma = sigma(model4), edf = df.residual(model4))



model1<-(lmer(likes ~ engage + dismiss+ (1|senator), data = S1a))
model2<-(lmer(likes ~ engage + dismiss+ party2 + followers + year+ length + media_or_link + sen_raceWhite + sen_gender + (1|senator), data = S1a))
model3<-(lmer(retweets ~ engage + dismiss + (1|senator), data = S1a))
model4<-(lmer(retweets ~ engage + dismiss + party2 + followers + year+ length + media_or_link + sen_raceWhite + sen_gender + (1|senator), data = S1a))

parameters(model1)
eff_size(emmeans(model1, "engage"), sigma = sigma(model1), edf = df.residual(model1))
eff_size(emmeans(model1, "dismiss"), sigma = sigma(model1), edf = df.residual(model1))
parameters(model2)
eff_size(emmeans(model2, "engage", lmer.df = "satterthwaite",lmerTest.limit = 4990), sigma = sigma(model2), edf = df.residual(model2))
eff_size(emmeans(model2, "dismiss", lmer.df = "satterthwaite",lmerTest.limit = 4990), sigma = sigma(model2), edf = df.residual(model2))

parameters(model3)
eff_size(emmeans(model3, "engage"), sigma = sigma(model3), edf = df.residual(model3))
eff_size(emmeans(model3, "dismiss"), sigma = sigma(model3), edf = df.residual(model3))
parameters(model4)
eff_size(emmeans(model4, "engage", lmer.df = "satterthwaite",lmerTest.limit = 4990), sigma = sigma(model4), edf = df.residual(model4))
eff_size(emmeans(model4, "dismiss", lmer.df = "satterthwaite",lmerTest.limit = 4990), sigma = sigma(model4), edf = df.residual(model4))

model1<-(lmer(likes ~ engage + dismiss+ (1|senator), data = S1b))
model2<-(lmer(likes ~ engage + dismiss+ party2 + followers + year+ length + media_or_link + sen_raceWhite + sen_gender + (1|senator), data = S1b))
model3<-(lmer(retweets ~ engage + dismiss+ (1|senator), data = S1b))
model4<-(lmer(retweets ~ engage + dismiss+ party2 + followers + year+ length + media_or_link + sen_raceWhite + sen_gender + (1|senator), data = S1b))

parameters(model1)
eff_size(emmeans(model1, "engage", lmer.df = "satterthwaite",lmerTest.limit = 5000), sigma = sigma(model1), edf = df.residual(model1))
eff_size(emmeans(model1, "dismiss", lmer.df = "satterthwaite",lmerTest.limit = 5000), sigma = sigma(model1), edf = df.residual(model1))
parameters(model2)
eff_size(emmeans(model2, "engage", lmer.df = "satterthwaite",lmerTest.limit = 4990), sigma = sigma(model2), edf = df.residual(model2))
eff_size(emmeans(model2, "dismiss", lmer.df = "satterthwaite",lmerTest.limit = 4990), sigma = sigma(model2), edf = df.residual(model2))

parameters(model3)
eff_size(emmeans(model3, "engage"), sigma = sigma(model3), edf = df.residual(model3))
eff_size(emmeans(model3, "dismiss"), sigma = sigma(model3), edf = df.residual(model3))
parameters(model4)
eff_size(emmeans(model4, "engage"), sigma = sigma(model4), edf = df.residual(model4))
eff_size(emmeans(model4, "dismiss"), sigma = sigma(model4), edf = df.residual(model4))




#REPLICATING KEY RESULTS WITHOUT THE 20 OVERLAPPING TWEETS
tweets1a_IND_EvD <- subset(S1a, in_1b == "no_match")
tweets1b_IND_EvD <- subset(S1b, in_1a == "no_match")
tweets1a_IND_EvD$sen_raceWhite <- ifelse(tweets1a_IND_EvD$sen_race == "white"," white", "POC")
tweets1b_IND_EvD$sen_raceWhite <- ifelse(tweets1b_IND_EvD$sen_race == "white"," white", "POC")
tweets1b_IND_EvD$sen_race[tweets1b_IND_EvD$sen_race == "white"]<- " white"
tweets1b_IND_EvD$sen_gender[tweets1b_IND_EvD$sen_gender == "male"]<- " male"

#1a
model1<-(lmer(t_likes ~ engage + neither + (1|senator), data = tweets1a_IND_EvD))
model2<-(lmer(t_likes ~ engage + neither + party2 + followers + year+ length + media_or_link + sen_raceWhite + sen_gender+ (1|senator), data = tweets1a_IND_EvD))
model3<-(lmer(t_retweets ~ engage + neither + (1|senator), data = tweets1a_IND_EvD))
model4<-(lmer(t_retweets ~ engage + neither + party2 + followers + year+ length + media_or_link + sen_raceWhite + sen_gender+ (1|senator), data = tweets1a_IND_EvD))

parameters(model1)
eff_size(emmeans(model1, "engage", lmer.df = "satterthwaite",lmerTest.limit = 4979), sigma = sigma(model1), edf = df.residual(model1))
eff_size(emmeans(model1, "neither", lmer.df = "satterthwaite",lmerTest.limit = 4979), sigma = sigma(model1), edf = df.residual(model1))
parameters(model2)
eff_size(emmeans(model2, "engage", lmer.df = "satterthwaite",lmerTest.limit = 4969), sigma = sigma(model2), edf = df.residual(model2))
eff_size(emmeans(model2, "neither", lmer.df = "satterthwaite",lmerTest.limit = 4969), sigma = sigma(model2), edf = df.residual(model2))

parameters(model3)
eff_size(emmeans(model3, "engage"), sigma = sigma(model3), edf = df.residual(model3))
eff_size(emmeans(model3, "neither"), sigma = sigma(model3), edf = df.residual(model3))
parameters(model4)
eff_size(emmeans(model4, "engage", lmer.df = "satterthwaite",lmerTest.limit = 4969), sigma = sigma(model4), edf = df.residual(model4))
eff_size(emmeans(model4, "neither"), sigma = sigma(model4), edf = df.residual(model4))


model1<-(lmer(t_likes ~ dismiss + engage + (1|senator), data = tweets1a_IND_EvD))
model2<-(lmer(t_likes ~ dismiss + engage + party2 + followers + year+ length + media_or_link + sen_raceWhite + sen_gender+ (1|senator), data = tweets1a_IND_EvD))
model3<-(lmer(t_retweets ~ dismiss + engage + (1|senator), data = tweets1a_IND_EvD))
model4<-(lmer(t_retweets ~ dismiss + engage + party2 + followers + year+ length + media_or_link + sen_raceWhite + sen_gender+ (1|senator), data = tweets1a_IND_EvD))

parameters(model1)
eff_size(emmeans(model1, "engage", lmer.df = "satterthwaite",lmerTest.limit = 4979), sigma = sigma(model1), edf = df.residual(model1))
eff_size(emmeans(model1, "dismiss", lmer.df = "satterthwaite",lmerTest.limit = 4979), sigma = sigma(model1), edf = df.residual(model1))
parameters(model2)
eff_size(emmeans(model2, "engage", lmer.df = "satterthwaite",lmerTest.limit = 4969), sigma = sigma(model2), edf = df.residual(model2))
eff_size(emmeans(model2, "dismiss", lmer.df = "satterthwaite",lmerTest.limit = 4969), sigma = sigma(model2), edf = df.residual(model2))

parameters(model3)
eff_size(emmeans(model3, "engage"), sigma = sigma(model3), edf = df.residual(model3))
eff_size(emmeans(model3, "dismiss"), sigma = sigma(model3), edf = df.residual(model3))
parameters(model4)
eff_size(emmeans(model4, "engage"), sigma = sigma(model4), edf = df.residual(model4))
eff_size(emmeans(model4, "dismiss"), sigma = sigma(model4), edf = df.residual(model4))


#1b
model1<-(lmer(t_likes ~ engage +  neither +(1|senator), data = tweets1b_IND_EvD))
model2<-(lmer(t_likes ~ engage + neither + party2 + followers + year+ length + media_or_link + sen_raceWhite + sen_gender+ (1|senator), data = tweets1b_IND_EvD))
model3<-(lmer(t_retweets ~ engage +  neither +(1|senator), data = tweets1b_IND_EvD))
model4<-(lmer(t_retweets ~ engage + neither + party2 + followers + year+ length + media_or_link + sen_raceWhite + sen_gender+ (1|senator), data = tweets1b_IND_EvD))

parameters(model1)
eff_size(emmeans(model1, "engage"), sigma = sigma(model1), edf = df.residual(model1))
eff_size(emmeans(model1, "neither"), sigma = sigma(model1), edf = df.residual(model1))
parameters(model2)
eff_size(emmeans(model2, "engage"), sigma = sigma(model2), edf = df.residual(model2))
eff_size(emmeans(model2, "neither"), sigma = sigma(model2), edf = df.residual(model2))

parameters(model3)
eff_size(emmeans(model3, "engage"), sigma = sigma(model3), edf = df.residual(model3))
eff_size(emmeans(model3, "neither"), sigma = sigma(model3), edf = df.residual(model3))
parameters(model4)
eff_size(emmeans(model4, "engage"), sigma = sigma(model4), edf = df.residual(model4))
eff_size(emmeans(model4, "neither"), sigma = sigma(model4), edf = df.residual(model4))



model1<-(lmer(t_likes ~ dismiss +  engage +(1|senator), data = tweets1b_IND_EvD))
model2<-(lmer(t_likes ~ dismiss + engage + party2 + followers + year+ length + media_or_link + sen_raceWhite + sen_gender+ (1|senator), data = tweets1b_IND_EvD))
model3<-(lmer(t_retweets ~ dismiss +  engage +(1|senator), data = tweets1b_IND_EvD))
model4<-(lmer(t_retweets ~ dismiss + engage + party2 + followers + year+ length + media_or_link + sen_raceWhite + sen_gender+ (1|senator), data = tweets1b_IND_EvD))

parameters(model1)
eff_size(emmeans(model1, "engage"), sigma = sigma(model1), edf = df.residual(model1))
eff_size(emmeans(model1, "dismiss"), sigma = sigma(model1), edf = df.residual(model1))
parameters(model2)
eff_size(emmeans(model2, "engage"), sigma = sigma(model2), edf = df.residual(model2))
eff_size(emmeans(model2, "dismiss"), sigma = sigma(model2), edf = df.residual(model2))

parameters(model3)
eff_size(emmeans(model3, "engage"), sigma = sigma(model3), edf = df.residual(model3))
eff_size(emmeans(model3, "dismiss"), sigma = sigma(model3), edf = df.residual(model3))
parameters(model4)
eff_size(emmeans(model4, "engage"), sigma = sigma(model4), edf = df.residual(model4))
eff_size(emmeans(model4, "dismiss"), sigma = sigma(model4), edf = df.residual(model4))




#MEDIATION BY LINGUISTIC VARIABLES
tweets1a_TM <- S1a %>%
  filter(category != "neither")
tweets1b_TM <- S1b %>%
  filter(category != "neither")

#DOES CIVILITY MODERATE: 1A FAVORITES + RTs, NO COVARIATES
model <- '
level: 1
#direct effect
t_likes ~ b1*TOXICITY + c*category

# mediator
TOXICITY ~ a1*category

#indirect effect
ab := a1*b1
direct : = c
#total effect
total := c+ab
engagep mediated : = ab / total

level: 2
senator~1
'
fit <- sem(model, tweets1a_TM, cluster = "senator")
summary(fit)
parameterEstimates(fit, boot.ci.type="bca.simple")


model <- '
level: 1
#direct effect
t_retweets ~ b1*TOXICITY + c*category

# mediator
TOXICITY ~ a1*category

#indirect effect
ab := a1*b1
direct : = c
#total effect
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))

level: 2
senator~1
'
fit <- sem(model, tweets1a_TM, cluster = "senator")
summary(fit)
parameterEstimates(fit, boot.ci.type="bca.simple")

#DOES CIVILITY MODERATE: 1B FAVORITES + RTs, NO COVARIATES
model <- '
level: 1
#direct effect
t_likes ~ b1*TOXICITY + c*category

# mediator
TOXICITY ~ a1*category

#indirect effect
ab := a1*b1
direct : = c
#total effect
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))

level: 2
senator~1
'
fit <- sem(model, tweets1b_TM, cluster = "senator")
summary(fit)
parameterEstimates(fit, boot.ci.type="bca.simple")


model <- '
level: 1
#direct effect
t_retweets ~ b1*TOXICITY + c*category

# mediator
TOXICITY ~ a1*category

#indirect effect
ab := a1*b1
direct : = c
#total effect
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))

level: 2
senator~1
'
fit <- sem(model, tweets1b_TM, cluster = "senator")
summary(fit)
parameterEstimates(fit, boot.ci.type="bca.simple")

#the positive affect, negative affect, and negative moral word lists are available elsewhere on the internet
#READ IN DATAFILES AS "pos_affect_words", "neg_affect_words", and "shared_words"
pos_affect_words_list <-  as.vector(pos_affect_words$pos_affect_words)
neg_affect_words_list <-  as.vector(neg_affect_words$neg_affect_words)
shared_words_list <-  as.vector(shared_words$shared_words)


tweets1a_TM$text <- as.character(tweets1a_TM$text)
S1a_tidy <- tweets1a_TM %>% unnest_tokens(words, text)

S1a_tidy$pos_affect_words <- ifelse((str_detect(removePunctuation(char_tolower(S1a_tidy$words)),paste(pos_affect_words_list, collapse="|", sep="|"))),1,0)
S1a_tidy$neg_affect_words <- ifelse((str_detect(removePunctuation(char_tolower(S1a_tidy$words)),paste(neg_affect_words_list, collapse="|", sep="|"))),1,0)
S1a_tidy$shared_words <- ifelse((str_detect(removePunctuation(char_tolower(S1a_tidy$words)),paste(shared_words_list, collapse="|", sep="|"))),1,0)


shared <- S1a_tidy %>% 
  group_by(ID) %>%
  summarise(shared_count=(sum(shared_words))) %>%
  ungroup()
pos_affect <- S1a_tidy %>% 
  group_by(ID) %>%
  summarise(pos_affect_count=(sum(pos_affect_words))) %>%
  ungroup
neg_affect <- S1a_tidy %>% 
  group_by(ID) %>%
  summarise(neg_affect_count=(sum(neg_affect_words))) %>%
  ungroup

S1a_tidy <- left_join(S1a_tidy, shared)
S1a_tidy <- left_join(S1a_tidy, pos_affect)
S1a_tidy <- left_join(S1a_tidy, neg_affect)
counts <- c("ID", "pos_affect_count","neg_affect_count", "shared_count")
countsA <- S1a_tidy[counts]
countsA <- countsA %>% distinct(countsA$ID, .keep_all = TRUE)
S1a_tidy <- left_join(tweets1a_TM, countsA)

S1b_tidy <- tweets1b_TM %>% unnest_tokens(words, text)
S1b_tidy$pos_affect_words <- ifelse((str_detect(removePunctuation(char_tolower(S1b_tidy$words)),paste(pos_affect_words_list, collapse="|", sep="|"))),1,0)
S1b_tidy$neg_affect_words <- ifelse((str_detect(removePunctuation(char_tolower(S1b_tidy$words)),paste(neg_affect_words_list, collapse="|", sep="|"))),1,0)
S1b_tidy$shared_words <- ifelse((str_detect(removePunctuation(char_tolower(S1b_tidy$words)),paste(shared_words_list, collapse="|", sep="|"))),1,0)

shared <- S1b_tidy %>% 
  group_by(ID) %>%
  summarise(shared_count=(sum(shared_words))) %>%
  ungroup
pos_affect <- S1b_tidy %>% 
  group_by(ID) %>%
  summarise(pos_affect_count=(sum(pos_affect_words))) %>%
  ungroup
neg_affect <- S1b_tidy %>% 
  group_by(ID) %>%
  summarise(neg_affect_count=(sum(neg_affect_words))) %>%
  ungroup

S1b_tidy <- left_join(S1b_tidy, shared)
S1b_tidy <- left_join(S1b_tidy, pos_affect)
S1b_tidy <- left_join(S1b_tidy, neg_affect)
countsB <- S1b_tidy[counts]
countsB <- countsB %>% distinct(countsB$ID, .keep_all = TRUE)
S1b_tidy <- left_join(tweets1b_TM, countsB)




S1a_tidy_EvD <- subset(S1a_tidy, category != "neither")
S1a_tidy_EvD$category <- as.character(S1a_tidy_EvD$category)
S1a_tidy_EvD$category <- as.factor(S1a_tidy_EvD$category)


S1b_tidy_EvD <- subset(S1b_tidy, category != "neither")
S1b_tidy_EvD$category <- as.character(S1b_tidy_EvD$category)
S1b_tidy_EvD$category <- as.factor(S1b_tidy_EvD$category)

likes_toxic <- '
level: 1
t_likes ~ b1*TOXICITY + c*category
TOXICITY ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
RT_toxic <- '
level: 1
t_retweets ~ b1*TOXICITY + c*category
TOXICITY ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
summary(sem(likes_toxic, S1a_tidy_EvD, cluster = "senator"))
summary(sem(RT_toxic, S1a_tidy_EvD, cluster = "senator"))
summary(sem(likes_toxic, S1b_tidy_EvD, cluster = "senator"))
summary(sem(RT_toxic, S1b_tidy_EvD, cluster = "senator"))


likes_negAff <- '
level: 1
t_likes ~ b1*neg_affect_count + c*category
neg_affect_count ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
RT_negAff <- '
level: 1
t_retweets ~ b1*neg_affect_count + c*category
neg_affect_count ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
summary(sem(likes_negAff, S1a_tidy_EvD, cluster = "senator"))
summary(sem(RT_negAff, S1a_tidy_EvD, cluster = "senator"))
summary(sem(likes_negAff, S1b_tidy_EvD, cluster = "senator"))
summary(sem(RT_negAff, S1b_tidy_EvD, cluster = "senator"))


likes_posAff <- '
level: 1
t_likes ~ b1*pos_affect_count + c*category
pos_affect_count ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
RT_posAff <- '
level: 1
t_retweets ~ b1*pos_affect_count + c*category
pos_affect_count ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
summary(sem(likes_posAff, S1a_tidy_EvD, cluster = "senator"))
summary(sem(RT_posAff, S1a_tidy_EvD, cluster = "senator"))
summary(sem(likes_posAff, S1b_tidy_EvD, cluster = "senator"))
summary(sem(RT_posAff, S1b_tidy_EvD, cluster = "senator"))


likes_negMoral <- '
level: 1
t_likes ~ b1*shared_count + c*category
shared_count ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
RT_negMoral <- '
level: 1
t_retweets ~ b1*shared_count + c*category
shared_count ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
summary(sem(likes_negMoral, S1a_tidy_EvD, cluster = "senator"))
summary(sem(RT_negMoral, S1a_tidy_EvD, cluster = "senator"))
summary(sem(likes_negMoral, S1b_tidy_EvD, cluster = "senator"))
summary(sem(RT_negMoral, S1b_tidy_EvD, cluster = "senator"))


likes_negAff <- '
level: 1
t_likes ~ b1*neg_affect_count + c*category
neg_affect_count ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
RT_negAff <- '
level: 1
t_retweets ~ b1*neg_affect_count + c*category
neg_affect_count ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
summary(sem(likes_negAff, S1a_tidy_EvD, cluster = "senator"))
summary(sem(RT_negAff, S1a_tidy_EvD, cluster = "senator"))
summary(sem(likes_negAff, S1b_tidy_EvD, cluster = "senator"))
summary(sem(RT_negAff, S1b_tidy_EvD, cluster = "senator"))


likes_Cert <- '
level: 1
t_likes ~ b1*certitude + c*category
certitude ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
RT_Cert <- '
level: 1
t_retweets ~ b1*certitude + c*category
certitude ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
summary(sem(likes_Cert, S1a_tidy_EvD, cluster = "senator"))
summary(sem(RT_Cert, S1a_tidy_EvD, cluster = "senator"))
summary(sem(likes_Cert, S1b_tidy_EvD, cluster = "senator"))
summary(sem(RT_Cert, S1b_tidy_EvD, cluster = "senator"))



likes_AllNone <- '
level: 1
t_likes ~ b1*allnone + c*category
allnone ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
RT_AllNone <- '
level: 1
t_retweets ~ b1*allnone + c*category
allnone ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
summary(sem(likes_AllNone, S1a_tidy_EvD, cluster = "senator"))
summary(sem(RT_AllNone, S1a_tidy_EvD, cluster = "senator"))
summary(sem(likes_AllNone, S1b_tidy_EvD, cluster = "senator"))
summary(sem(RT_AllNone, S1b_tidy_EvD, cluster = "senator"))



likes_moral <- '
level: 1
t_likes ~ b1*moral + c*category
moral ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
RT_moral <- '
level: 1
t_retweets ~ b1*moral + c*category
moral ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
summary(sem(likes_moral, S1a_tidy_EvD, cluster = "senator"))
summary(sem(RT_moral, S1a_tidy_EvD, cluster = "senator"))
summary(sem(likes_moral, S1b_tidy_EvD, cluster = "senator"))
summary(sem(RT_moral, S1b_tidy_EvD, cluster = "senator"))



likes_tone_pos <- '
level: 1
t_likes ~ b1*tone_pos + c*category
tone_pos ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
RT_tone_pos <- '
level: 1
t_retweets ~ b1*tone_pos + c*category
tone_pos ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
summary(sem(likes_tone_pos, S1a_tidy_EvD, cluster = "senator"))
summary(sem(RT_tone_pos, S1a_tidy_EvD, cluster = "senator"))
summary(sem(likes_tone_pos, S1b_tidy_EvD, cluster = "senator"))
summary(sem(RT_tone_pos, S1b_tidy_EvD, cluster = "senator"))



likes_tone_neg <- '
level: 1
t_likes ~ b1*tone_neg + c*category
tone_neg ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
RT_tone_neg <- '
level: 1
t_retweets ~ b1*tone_neg + c*category
tone_neg ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
summary(sem(likes_tone_neg, S1a_tidy_EvD, cluster = "senator"))
summary(sem(RT_tone_neg, S1a_tidy_EvD, cluster = "senator"))
summary(sem(likes_tone_neg, S1b_tidy_EvD, cluster = "senator"))
summary(sem(RT_tone_neg, S1b_tidy_EvD, cluster = "senator"))



likes_polite <- '
level: 1
t_likes ~ b1*polite + c*category
polite ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
RT_polite <- '
level: 1
t_retweets ~ b1*polite + c*category
polite ~ a1*category
ab := a1*b1
direct : = c
total := c+(a1*b1)
engagep mediated : = (a1*b1) / (c+(a1*b1))
level: 2
senator~1
'
summary(sem(likes_polite, S1a_tidy_EvD, cluster = "senator"))
summary(sem(RT_polite, S1a_tidy_EvD, cluster = "senator"))
summary(sem(likes_polite, S1b_tidy_EvD, cluster = "senator"))
summary(sem(RT_polite, S1b_tidy_EvD, cluster = "senator"))


###### Study 2a SOM ######

#descriptives for figure
describeBy(S2a_long_SenTwe$FT,S2a_long_SenTwe$target)
describeBy(S2a_long_SenVig$FT,S2a_long_SenVig$target)
describeBy(S2a_long_CitTwe$FT,S2a_long_CitTwe$target)
describeBy(S2a_long_CitVig$FT,S2a_long_CitVig$target)

#extremity moderation
model <- (lmer(FT ~ target*ideology_C + target*ideology_SQ + (1|ID), data = S2a_long)) #sig
parameters(model)
mylist <- list(ideology_SQ = .25)
summary(emmeans(model, pairwise ~ target*ideology_C + target*ideology_SQ, at=mylist, lmer.df = "satterthwaite",lmerTest.limit = 4559))
mylist <- list(ideology_SQ = 6.25)
summary(emmeans(model, pairwise ~ target*ideology_C + target*ideology_SQ, at=mylist, lmer.df = "satterthwaite",lmerTest.limit = 4559))


###### Study 2b SOM ######
#descriptives for figure
describeBy(S2b_long_SenTwe$FT,S2b_long_SenTwe$target)
describeBy(S2b_long_SenVig$FT,S2b_long_SenVig$target)
describeBy(S2b_long_CitTwe$FT,S2b_long_CitTwe$target)
describeBy(S2b_long_CitVig$FT,S2b_long_CitVig$target)

#extremity moderation
model<-(lmer(FT ~ target*ideology_C + target*ideology_SQ + (1|ID), data = S2b_long)) #sig
summary(model)
parameters(model)
mylist <- list(ideology_SQ = 0.25)
summary(emmeans(model, pairwise ~ target*ideology_C + target*ideology_SQ, at=mylist, lmer.df = "satterthwaite",lmerTest.limit = 4559))
mylist <- list(ideology_SQ = 6.25)
summary(emmeans(model, pairwise ~ target*ideology_C + target*ideology_SQ, at=mylist, lmer.df = "satterthwaite",lmerTest.limit = 4559))


###### Study 3 SOM ######
#descriptives for figure
S3L_eng <- subset(S3L, behavior == "engage")
S3L_dis <- subset(S3L, behavior == "dismiss")

describeBy(S3L_eng$zDV,S3L_eng$interact_freq)
describeBy(S3L_dis$zDV,S3L_dis$interact_freq)


#Analyses including only participants from our pre-amendment sample
S3L2 <- S3L %>%
  filter(sample == "fullstudy")

parameters(lmer(zDV~behavior*interact_freqREV+(1|tweet_number)+(1|ID),S3L2))
parameters(lmer(zDV~behavior*interact_freq + (1|tweet_number)+(1|ID), S3L2))

parameters(lmer(zDV ~ behavior*interact_freqREV*DV_type + (1|tweet_number) + (1|ID), S3L2)) #
parameters(lmer(zDV ~ behavior*interact_freq*DV_type + (1|tweet_number) + (1|ID), S3L2))
parameters(lmer(zDV ~ behavior*interact_freqREV*DV_typeFT + (1|tweet_number) + (1|ID), S3L2)) #
parameters(lmer(zDV ~ behavior*interact_freq*DV_typeFT + (1|tweet_number) + (1|ID), S3L2))
parameters(lmer(zDV ~ behavior*interact_freqREV*DV_typeLRT + (1|tweet_number) + (1|ID), S3L2)) #
parameters(lmer(zDV ~ behavior*interact_freq*DV_typeLRT + (1|tweet_number) + (1|ID), S3L2))

model <- (lmer(zDV ~ behavior*party_affil_C + behavior*party_affil_SQ + (1|tweet_number) + (1|ID), S3L2))
parameters(model)
parameters(lmer(zDV ~ behaviorEng*party_affil_C + behaviorEng*party_affil_SQ + (1|tweet_number) + (1|ID), S3L2))
mylist <- list(party_affil_SQ = .25)
summary(emmeans(model, pairwise ~ behavior*party_affil_C + behavior*party_affil_SQ, at=mylist, lmer.df = "satterthwaite",lmerTest.limit = 4559))
mylist <- list(party_affil_SQ = 6.25)
summary(emmeans(model, pairwise ~ behavior*party_affil_C + behavior*party_affil_SQ, at=mylist, lmer.df = "satterthwaite",lmerTest.limit = 4559))

parameters(lmer(zDV~behavior*party_affil_C*DV_type+behavior*party_affil_SQ*DV_type+(1|tweet_number)+(1|ID),S3L2))
parameters(lmer(zDV~behavior*party_affil_C*DV_typeFT+behavior*party_affil_SQ*DV_typeFT+(1|tweet_number)+(1|ID),S3L2))
parameters(lmer(zDV~behavior*party_affil_C*DV_typeLRT+behavior*party_affil_SQ*DV_typeLRT+(1|tweet_number)+(1|ID),S3L2))

parameters(lmer(zDV~behaviorEng*party_affil_C*DV_type+behaviorEng*party_affil_SQ*DV_type+(1|tweet_number)+(1|ID),S3L2))
parameters(lmer(zDV~behaviorEng*party_affil_C*DV_typeFT+behaviorEng*party_affil_SQ*DV_typeFT+(1|tweet_number)+(1|ID),S3L2))
parameters(lmer(zDV~behaviorEng*party_affil_C*DV_typeLRT+behaviorEng*party_affil_SQ*DV_typeLRT+(1|tweet_number)+(1|ID),S3L2))


#PREREG'D TEST OF TWEET TYPE BY EXTREMITY INTERACTION, FULL SAMPLE
model <- (lmer(zDV ~ behavior*party_affil_C + behavior*party_affil_SQ + (1|tweet_number) + (1|ID), S3L))
parameters(model)
mylist <- list(party_affil_SQ = .25)
summary(emmeans(model, pairwise ~ behavior*party_affil_C + behavior*party_affil_SQ, at=mylist, lmer.df = "satterthwaite",lmerTest.limit = 4559))
mylist <- list(party_affil_SQ = 6.25)
summary(emmeans(model, pairwise ~ behavior*party_affil_C + behavior*party_affil_SQ, at=mylist, lmer.df = "satterthwaite",lmerTest.limit = 4559))

parameters(lmer(zDV ~ behaviorEng*party_affil_C + behaviorEng*party_affil_SQ + (1|tweet_number) + (1|ID), S3L))

#FIGURE
ggplot(S3L,aes(x = party_affil_C, y = zDV, color = Behavior)) +
  geom_smooth(method = "lm",formula = y ~ x + I(x^2),se = TRUE) +
  xlim(-3,3) +
  labs(title="",x = "Party affiliation",y = "Preference for Engaging", color = "Tweet Type") +
  scale_color_manual(values=c("Black","White")) +
  scale_x_continuous(breaks=c(-2.5,-1.5,-.5,.5,1.5,2.5),labels= c("1 \n Strongly Democrat", "2","3","4","5", "6 \n Strongly Republican")) +
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        strip.background = element_blank())


#PREREG'D TEST OF TWEET TYPE BY EXTREMITY INTERACTION BY MEASURE, FULL SAMPLE
parameters(lmer(zDV~behavior*party_affil_C*DV_type+behavior*party_affil_SQ*DV_type+(1|tweet_number)+(1|ID),S3L))
parameters(lmer(zDV~behavior*party_affil_C*DV_typeFT+behavior*party_affil_SQ*DV_typeFT+(1|tweet_number)+(1|ID),S3L))
parameters(lmer(zDV~behavior*party_affil_C*DV_typeLRT+behavior*party_affil_SQ*DV_typeLRT+(1|tweet_number)+(1|ID),S3L))

parameters(lmer(zDV~behaviorEng*party_affil_C*DV_type+behaviorEng*party_affil_SQ*DV_type+(1|tweet_number)+(1|ID),S3L))
parameters(lmer(zDV~behaviorEng*party_affil_C*DV_typeFT+behaviorEng*party_affil_SQ*DV_typeFT+(1|tweet_number)+(1|ID),S3L))
parameters(lmer(zDV~behaviorEng*party_affil_C*DV_typeLRT+behaviorEng*party_affil_SQ*DV_typeLRT+(1|tweet_number)+(1|ID),S3L))


#OTHER WAYS TO INDEX EXTREMITY
#Absolute distance from midpoint
S3L$party_dist <- as.numeric(ifelse(S3L$party_affil == 6, 3, ifelse(S3L$party_affil == 5, 2, ifelse(S3L$party_affil == 4, 1, ifelse(S3L$party_affil == 3, 1, ifelse(S3L$party_affil == 2, 2, ifelse(S3L$party_affil == 1, 3, "")))))))
model <- (lmer(zDV ~ behavior*party_dist + behavior*party_affil_C + (1|tweet_number) + (1|ID), S3L)) #
parameters(model)
mylist <- list(party_dist = 1)
summary(emmeans(model, pairwise ~ behavior*party_dist + behavior*party_affil_C, at=mylist, lmer.df = "satterthwaite",lmerTest.limit = 4559))
mylist <- list(party_dist = 3)
summary(emmeans(model, pairwise ~ behavior*party_dist + behavior*party_affil_C, at=mylist, lmer.df = "satterthwaite",lmerTest.limit = 4559))


#Ideological extremity
model <- (lmer(zDV ~ behavior*ideology_C + behavior*ideology_SQ + (1|tweet_number) + (1|ID), S3L))
parameters(model)
mylist <- list(ideology_SQ = 0)
summary(emmeans(model, pairwise ~ behavior*ideology_C + behavior*ideology_SQ, at=mylist, lmer.df = "satterthwaite",lmerTest.limit = 4559))
mylist <- list(ideology_SQ = 9)
summary(emmeans(model, pairwise ~ behavior*ideology_C + behavior*ideology_SQ, at=mylist, lmer.df = "satterthwaite",lmerTest.limit = 4559))

summary(lmer(zDV ~ ideology_C + ideology_SQ + (1|tweet_number) + (1|ID), S3L_eng))





#MEDIATOR(S) OF FREQUENT REACTORS' PREFERENCE
#creating datafile to create new DV, average of engaging ratings minus average of dismissing ratings
TRM <- S3L %>% 
  group_by(behavior, ID) %>% 
  mutate(avgDV = mean(zDV, na.rm = TRUE)) %>%
  ungroup() %>%
  unite("ID_beh",c("ID","behavior"), remove = FALSE) %>%
  distinct(ID_beh, .keep_all = TRUE)

TRM2 = TRM %>% 
  pivot_wider(id_cols = ID, names_from=behavior, values_from=avgDV)
TRM <- left_join(TRM2, TRM, by = "ID", keep = TRUE)
TRM <- TRM %>% distinct(TRM$ID.x, .keep_all = TRUE)
TRM$dif <- (TRM$engage - TRM$dismiss)


#A path
parameters(glm(interact_freqREV ~ party_affil_C + party_affil_SQ + AffPol + compromise  + party_status, TRM, family = binomial("logit")))
#to get DF:
summary(glm(interact_freqREV ~ party_affil_C + party_affil_SQ + AffPol + compromise  + party_status, TRM, family = binomial("logit")))


#B path
parameters(lm(dif ~ party_affil_C + party_affil_SQ + AffPol + compromise  + party_status, TRM)) #


base_model <-"
party_affil_SQ ~ a1*interact_freqREV + party_affil_C
AffPol ~ a2*interact_freqREV + b1*party_affil_SQ + party_affil_C
compromise ~ a3*interact_freqREV + b2*party_affil_SQ + party_affil_C
party_status ~ a4*interact_freqREV + b3*party_affil_SQ + party_affil_C
dif ~ f*interact_freqREV + b4*party_affil_SQ + c1*AffPol + c2*compromise + c3*party_status + party_affil_C
direct := f
FED := a1*b4
FAD := a2*c1
FCD := a3*c2
FPD := a4*c3
FEAD:= a1*b1*c1
FECD:= a1*b2*c2
FEPD:= a1*b3*c3
indirectTotal := FED + FAD + FCD + FPD + FEAD + FECD + FEPD
total := f+indirectTotal
engagep_mediated := indirectTotal / total
"
base_model <- sem(base_model, data=TRM)
summary(base_model)


#### Differences between frequent reactors and everyone else:
TRM <- S3L %>% 
  group_by(behavior, ID) %>% 
  mutate(avgDV = mean(zDV, na.rm = TRUE)) %>%
  ungroup() %>%
  unite("ID_beh",c("ID","behavior"), remove = FALSE) %>%
  distinct(ID_beh, .keep_all = TRUE)

TRM2 = TRM %>% 
  pivot_wider(id_cols = ID, names_from=behavior, values_from=avgDV)
TRM <- left_join(TRM2, TRM, by = "ID", keep = TRUE)
TRM <- TRM %>% distinct(TRM$ID.x, .keep_all = TRUE)
TRM$dif <- (TRM$engage - TRM$dismiss)


t.test(S3$age[S3$ReactionFrequency=="Frequent Reactors"],S3$age[S3$ReactionFrequency=="Everyone Else"])

S3$gender_binary <- ifelse(S3$gender == "m", 0, 1)
chisq.test(table(S3$gender_binary,S3$ReactionFrequency))
print(table(S3$gender_binary,S3$ReactionFrequency))


t.test(S3$party_affil[S3$ReactionFrequency=="Frequent Reactors"],S3$party_affil[S3$ReactionFrequency=="Everyone Else"])
table(S3$use_twitter_coded[S3$ReactionFrequency=="Frequent Reactors"])/156
table(S3$use_twitter_coded[S3$ReactionFrequency=="Everyone Else"])/167
chisq.test(table(S3$use_twitter_coded,S3$ReactionFrequency))

t.test(S3$ind_status[S3$ReactionFrequency=="Frequent Reactors"],S3$ind_status[S3$ReactionFrequency=="Everyone Else"])
t.test(S3$ideology[S3$ReactionFrequency=="Frequent Reactors"],S3$ideology[S3$ReactionFrequency=="Everyone Else"])
t.test(S3$ideology_SQ[S3$ReactionFrequency=="Frequent Reactors"],S3$ideology_SQ[S3$ReactionFrequency=="Everyone Else"])
t.test(S3$party_affil_SQ[S3$ReactionFrequency=="Frequent Reactors"],S3$party_affil_SQ[S3$ReactionFrequency=="Everyone Else"])
t.test(S3$DEM_FT[S3$ReactionFrequency=="Frequent Reactors"],S3$DEM_FT[S3$ReactionFrequency=="Everyone Else"])
t.test(S3$REP_FT[S3$ReactionFrequency=="Frequent Reactors"],S3$REP_FT[S3$ReactionFrequency=="Everyone Else"])

t.test(S3$AffPol[S3$ReactionFrequency=="Frequent Reactors"],S3$AffPol[S3$ReactionFrequency=="Everyone Else"])
t.test(S3$compromise[S3$ReactionFrequency=="Frequent Reactors"],S3$compromise[S3$ReactionFrequency=="Everyone Else"])
t.test(S3$party_status[S3$ReactionFrequency=="Frequent Reactors"],S3$party_status[S3$ReactionFrequency=="Everyone Else"])


describeBy(S3$age, S3$ReactionFrequency)
describeBy(S3$party_affil, S3$ReactionFrequency)
describeBy(S3$use_twitter, S3$ReactionFrequency)
describeBy(S3$ind_status, S3$ReactionFrequency)
describeBy(S3$ideology, S3$ReactionFrequency)
describeBy(S3$ideology_SQ, S3$ReactionFrequency)
describeBy(S3$party_affil_SQ, S3$ReactionFrequency)
describeBy(S3$DEM_FT, S3$ReactionFrequency)
describeBy(S3$REP_FT, S3$ReactionFrequency)
describeBy(S3$AffPol, S3$ReactionFrequency)
describeBy(S3$compromise, S3$ReactionFrequency)
describeBy(S3$party_status, S3$ReactionFrequency)


#key tests with extraneous variables as covariates: 
parameters(lmer(zDV~behavior*interact_freq + party_affil_SQ + gender + ind_status + use_twitter + (1|tweet_number)+(1|ID), S3L))
parameters(lmer(zDV~behavior*interact_freqREV + party_affil_SQ + gender + ind_status + use_twitter + (1|tweet_number)+(1|ID), S3L))


#### SEM model with extraneous variables as covariates:
base_model <-"
party_affil_SQ ~ a1*interact_freqREV + party_affil_C + gender + ind_status + use_twitter
AffPol ~ a2*interact_freqREV + b1*party_affil_SQ + party_affil_C + gender + ind_status + use_twitter
compromise ~ a3*interact_freqREV + b2*party_affil_SQ + party_affil_C + gender + ind_status + use_twitter
party_status ~ a4*interact_freqREV + b3*party_affil_SQ + party_affil_C + gender + ind_status + use_twitter
dif ~ f*interact_freqREV + b4*party_affil_SQ + c1*AffPol + c2*compromise + c3*party_status + party_affil_C + gender + ind_status + use_twitter
direct := f
FED := a1*b4
FAD := a2*c1
FCD := a3*c2
FPD := a4*c3
FEAD:= a1*b1*c1
FECD:= a1*b2*c2
FEPD:= a1*b3*c3
indirectTotal := FED + FAD + FCD + FPD + FEAD + FECD + FEPD
total := f+indirectTotal
engagep_mediated := indirectTotal / total
"
base_model <- sem(base_model, data=TRM)
summary(base_model)



###### Study 4 SOM #####

model1 <- (lmer(DV~behavior + (1|tweet_number) + (1|ID), S4L_tol))
model2 <- (lmer(DV~behavior + (1|tweet_number) + (1|ID), S4L_coop))
model3 <- (lmer(DV~behavior + (1|tweet_number) + (1|ID), S4L_rational))
model4 <- (lmer(DV~behavior + (1|tweet_number) + (1|ID), S4L_legit))
model5 <- (lmer(DV~behavior + (1|tweet_number) + (1|ID), S4L_open))
parameters(model1)
parameters(model2)
parameters(model3)
parameters(model4)
parameters(model5)

eff_size(emmeans(model1, ~ behavior), sigma = sigma(model1), edf = df.residual(model1))
eff_size(emmeans(model2, ~ behavior), sigma = sigma(model2), edf = df.residual(model2))
eff_size(emmeans(model3, ~ behavior), sigma = sigma(model3), edf = df.residual(model3))
eff_size(emmeans(model4, ~ behavior), sigma = sigma(model4), edf = df.residual(model4))
eff_size(emmeans(model5, ~ behavior), sigma = sigma(model5), edf = df.residual(model5))



##### Pilots S1-SOM ####
#READ IN DATAFILES AS "Ss1" and "Ss2"

#Attention and quality checks
table(Ss1$english_check) #answer should be 2
table(Ss1$quality_check) #answer should be 1
Ss1 <- Ss1 %>%
  filter(english_check == 2) %>%
  filter(is.na(quality_check) | quality_check == 1)

table(Ss2$english_check) #answer should be 2
table(Ss2$quality_check) #answer should be 1
Ss2 <- Ss2 %>%
  filter(english_check == 2) %>%
  filter(is.na(quality_check) | quality_check == 1)

table(Ss1$gender)/198
describe(Ss1$age) #
describe(Ss1$party_affil)

table(Ss2$gender)/200
describe(Ss2$age) #


#making variables, formatting datafile
Ss1$interact_senator <- as.factor(Ss1$interact_senator)
Ss1$engage_hi <- ifelse(Ss1$interact_senator == "yes regularly", 1,0)
Ss1$engage_rare <- ifelse(Ss1$interact_senator == "yes once", 1,0)
Ss1$engage_never <- ifelse(Ss1$interact_senator == "no", 1,0)

Ss1$party_affil_C <- (Ss1$party_affil - 3.5)
Ss1$party_affil_SQ <- (Ss1$party_affil_C*Ss1$party_affil_C)

Ss1_long <- pivot_longer(Ss1,14:184,names_to = "tweet_number",values_to = "FT")
Ss1_long <- drop_na(Ss1_long,FT)
Ss1_long <- separate(Ss1_long,tweet_number,"_",into = c("target","tweet_number"))
Ss1_long$tweet_num <- as.factor(Ss1_long$tweet_number)

Ss2$interact_senator <- as.factor(Ss2$interact_senator)
Ss2$engage_hi <- ifelse(Ss2$interact_senator == 3, 1,0)
Ss2$engage_rare <- ifelse(Ss2$interact_senator == 2, 1,0)
Ss2$engage_never <- ifelse(Ss2$interact_senator == 1, 1,0)

Ss2$party_affil_C <- (Ss2$party_affil - 3.5)
Ss2$party_affil_SQ <- (Ss2$party_affil_C*Ss2$party_affil_C)

Ss2_long <- pivot_longer(Ss2,15:185,names_to = "tweet_number",values_to = "FT")
Ss2_long <- drop_na(Ss2_long,FT)
Ss2_long <- separate(Ss2_long,tweet_number,"_",into = c("target","tweet_number"))
Ss2_long$tweet_num <- as.factor(Ss2_long$tweet_number)

Ss2_long$ideology_C <- (Ss2_long$ideology - 4)
Ss2_long$ideology_SQ <- (Ss2_long$ideology_C*Ss2_long$ideology_C)

#Do participants prefer engaging or dismissing overall?
#S3
model<-(lmer(FT ~ target + (1|tweet_num) + (1|ResponseId), Ss1_long))
parameters(model)

#S4
model <- (lmer(FT ~ target + (1|tweet_num) + (1|ResponseId), Ss2_long))
parameters(model)



#Do frequent reactors respond differently than everyone else to engaging versus dismissing, and is this related to their partisan extremity?
#make datafile
Ss2$ideology <- NULL
Ss2$comments <- NULL
Ss1$study <- "S1"
Ss2$study <- "S2"
Ss12 <- rbind(Ss1,Ss2)
Ss12 <- pivot_longer(Ss12,14:184,names_to = "tweet_number",values_to = "FT")
Ss12 <- drop_na(Ss12,FT)
Ss12 <- separate(Ss12,tweet_number,"_",into = c("target","tweet_number"))
Ss12$tweet_num <- as.factor(Ss12$tweet_number)
Ss12$engage_FR <- ifelse(Ss12$interact_senator == "yes regularly", 1,0)
Ss12$engage_NFR <- ifelse(Ss12$interact_senator == "yes regularly", 0,1)


#Frequent reactor effect:
parameters(lmer(FT ~ target*engage_NFR + (1|tweet_num) + (1|ResponseId)  + (1|study), Ss12))
#everyone else effect
parameters(lmer(FT ~ target*engage_FR + (1|tweet_num) + (1|ResponseId)   + (1|study), Ss12))


#Values for Figure
Ss12_engage <- subset(Ss12, target == "engage")
Ss12_dismiss <- subset(Ss12, target == "dismiss")

describeBy(Ss12_engage$FT,Ss12_engage$engage_FR)
describeBy(Ss12_dismiss$FT,Ss12_dismiss$engage_FR)



#MODERATION BY EXTREMITY
model <- (lmer(FT ~ target*party_affil_C + target*party_affil_SQ + (1|tweet_num) + (1|ResponseId), Ss12))
parameters(model)
mylist <- list(party_affil_SQ = .25)
summary(emmeans(model, pairwise ~ target*party_affil_C + target*party_affil_SQ, at=mylist, lmer.df = "satterthwaite",lmerTest.limit = 3965))
mylist <- list(party_affil_SQ = 6.25)
summary(emmeans(model, pairwise ~ target*party_affil_C + target*party_affil_SQ, at=mylist, lmer.df = "satterthwaite",lmerTest.limit = 3965))


#Mediation
##frequency -> extremity -> difference score
model <- "
# direct effect
dif ~ c1*engage_NFR
# direct moderating effect
party_affil_SQ ~ a*engage_NFR
# b path
dif ~ b*party_affil_SQ
ab := a*b
total := c1+a*b
engagep_mediated := ab / (c1 + ab)
"
fit1 <- sem(model, data=Ss12)
summary(fit1)
parameterEstimates(fit1, boot.ci.type="bca.simple")


