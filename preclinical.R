## PRECLINICAL 

# EVS 1/2023

library(tidyverse)
library(meta)
library(dmetar)
library(metafor)

## ---- ## COMPARISON 1: healthy+P vs healthy ----

# (FBG only)

## get data 
dat <- readxl::read_xlsx(path = "../Raw-Stats-for-META.xlsx",
                         sheet = "animal_data") %>% 
  filter(variable == "FBG") %>% 
  # get only healthy animals 
  filter(desc_exp == "SD+P") %>% 
  # beautify study name
  mutate(studyID = str_to_title(str_replace(studyID, "20", " 20")))

# make m.genagen object
m.gen <-  metacont(data = dat,
                   n.e = n_exp,
                   mean.e = mean_exp,
                   sd.e = sd_exp,
                   n.c = n_cont,
                   mean.c = mean_cont,
                   sd.c = sd_cont,
                   studlab = studyID,
                   # show Mean differe
                   smd = "md",
                   # random effect
                   fixed = FALSE,
                   m.genhod.tau = "REML",
                   # apply Knapp-Hartung 
                   hakn = TRUE,
                   # add prediction interval of effect
                   prediction = TRUE)

# get summary
summary(m.gen)

# show Cohen's d
smd <- update.meta(m.gen,
                   sm = "SMD",
                   method.smd = "Cohen",
                   hakn = TRUE)

summary(smd)
## look for influential studies with leave-one-out
m.inf <- InfluenceAnalysis(m.gen, random = TRUE)
m.inf # NONE
# make diagnostic plots
#svg(file = "working-plots/comp1_baujat.svg")
plot(m.inf, "baujat")
#dev.off()
# plot pooled effect size after leave-one-out
plot(m.inf, "es")
plot(m.inf, "i2")


### FOREST PLOT 
# make forest plot and save to file
svg(file = "working-plots/forest_comp1_outliers.svg", height = 4, width = 11)
forest(m.gen, 
       sort.subgroup = TRUE,
       label.e = "Bifidobacterium",
       label.c = "Control",
       col.diamond = "red",
       col.predict = "darkred",
       hetstat = FALSE,
       overall.hetstat = FALSE,
       # round off MD and SE
       digits = 2,
       digits.se = 2)
dev.off()

### FUNNEL PLOT

## save forest plot
svg(file = "working-plots/funnel_healthyP_healthy.svg")

funnel(m.gen,
       studlab = TRUE)

dev.off()

## ----- metP-healthy ----


## get data 
dat <- readxl::read_xlsx(path = "../Raw-Stats-for-META.xlsx",
                         sheet = "animal_data") %>% 
  filter(variable == "FBG") %>% 
  # get only MetS/T2D 
  filter(!desc_exp == "SD+P") %>% 
  filter(desc_control == "SD") %>% 
  # beautify study name
  mutate(studyID = str_to_title(str_replace(studyID, "20", " 20")))

# make m.genagen object
m.gen <-  metacont(data = dat,
                   n.e = n_exp,
                   mean.e = mean_exp,
                   sd.e = sd_exp,
                   n.c = n_cont,
                   mean.c = mean_cont,
                   sd.c = sd_cont,
                   studlab = studyID,
                   smd = "md",
                   # random effect
                   fixed = FALSE,
                   m.genhod.tau = "REML",
                   # apply Knapp-Hartung 
                   hakn = TRUE,
                   # add prediction interval of effect
                   prediction = TRUE)

# get summary
summary(m.gen)

## create group for T2D or DIO
#dat <- dat %>% 
 # mutate(phenotype = if_else(str_detect(desc_exp, "T2D"), "T2D", "DIO/MetS"))

# summarize
#dat %>% group_by(phenotype) %>% count() # 10 MetS, 2 T2D

# add as subgroup 
#m.sub <- update.meta(m.gen, 
 #                 subgroup = phenotype,
  #                data = dat)
#summary(m.sub)

## look for influential studies
m.inf <- InfluenceAnalysis(m.gen, random = TRUE)
m.inf # only Hao 2022 is significant
plot(m.inf, "baujat") # Sharma 2016 and Ray 2018

# remove Hao 2022
m1 <- update.meta(m.gen,
                  exclude = c(7))
summary(m1)

# show Cohen's d
smd <- update.meta(m1,
                   sm = "SMD",
                   method.smd = "Cohen",
                   hakn = TRUE)

summary(smd)

### FOREST PLOT
# make forest plot and save to file
svg(file = "working-plots/forest_mets-healthy_outliers.svg", 
    height = 6, width = 11)
forest(m1, 
       #sort.subgroup = TRUE,
       label.e = "Bifidobacterium",
       label.c = "Control",
       col.diamond = "red",
       col.predict = "darkred",
       hetstat = FALSE,
       overall.hetstat = FALSE,
       print.subgroup.name = FALSE,
       # round off MD and SE
       digits = 2,
       digits.se = 2)
dev.off()

### funnel plot

## save forest plot
svg(file = "working-plots/funnel_metP_healthy.svg")

funnel(m.gen,
       studlab = TRUE,
       xlim = c(-300, 60))

dev.off()

# quantitative test
eggers.test(m.gen) # non-significant




## ---- metS-metS ----


## get data 
dat <- readxl::read_xlsx(path = "../Raw-Stats-for-META.xlsx",
                         sheet = "animal_data") %>% 
  filter(variable == "FBG") %>% 
  # get only unhealthy animals
  filter(!desc_exp == "SD+P") %>% 
  filter(!desc_control == "SD") %>% 
  mutate(phenotype = if_else(str_detect(desc_exp, "T2D"), "T2D", "DIO/MetS")) %>% 
  # beautify study name
  mutate(studyID = str_to_title(str_replace(studyID, "20", " 20"))) %>% 
  # round off SD (calculated)
  mutate(sd_exp = round(sd_exp, 2),
         sd_cont = round(sd_cont, 2))


# make m.genagen object
m.gen <-  metacont(data = dat,
                   n.e = n_exp,
                   mean.e = mean_exp,
                   sd.e = sd_exp,
                   n.c = n_cont,
                   mean.c = mean_cont,
                   sd.c = sd_cont,
                   studlab = studyID,
                   sm = "md",
                   # random effect
                   fixed = FALSE,
                   m.genhod.tau = "REML",
                   # apply Knapp-Hartung 
                   hakn = TRUE,
                   # add prediction interval of effect
                   prediction = TRUE)

# get summary
summary(m.gen)

# subgroup analysis
#m.sub <- update.meta(m.gen, 
 #                    subgroup = phenotype,
  #                   data = dat) 
#summary(m.sub)

# influential studies
m.inf <- InfluenceAnalysis(m.gen, random = TRUE)
m.inf # Hao 2022 
plot(m.inf, "baujat") 

# re-run with  Hao 2022 removed
m1 <- update.meta(m.gen, exclude = c(7),
                     data = dat) 
summary(m1)

# show Cohen's d
smd <- update.meta(m1,
                   sm = "SMD",
                   method.smd = "Cohen",
                   hakn = TRUE)

summary(smd)


### FOREST PLOT
svg(file = "working-plots/forest_mets-mets_outliers.svg", 
    height = 6, width = 11)
forest(m1, 
       sort.subgroup = TRUE,
       label.e = "Bifidobacterium",
       label.c = "Control",
       col.diamond = "red",
       col.predict = "darkred",
       hetstat = FALSE,
       overall.hetstat = FALSE,
       print.subgroup.name = FALSE,
       # round off MD and SE
       digits = 2,
       digits.se = 2)
dev.off()

### funnel plot

## save forest plot
svg(file = "working-plots/funnel_metP_met.svg")

funnel(m.gen,
       studlab = TRUE,
       xlim = c(-300, 60))

dev.off()

# quantitative test
eggers.test(m.gen) # non-significant


