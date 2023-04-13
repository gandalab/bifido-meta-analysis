## CLINICAL COMPARISONS: Placebo vs probiotics
# (FBG only)

# EVS 1/2023
## 3/2023 update; remove Bernier 2021 (no fasting)

library(tidyverse)
library(meta)
library(dmetar)
library(metafor)

## get data 
dat <- readxl::read_xlsx(path = "../Raw-Stats-for-META.xlsx",
                         sheet = "human_data",
                         na = "NA") %>% 
  filter(variable == "FBG") %>% 
  # drop NAs 
  drop_na(n_cont) %>% 
# beautify study name
mutate(studyID = str_to_title(str_replace(studyID, "20", " 20"))) %>% 
  # remove Bernier 2021
  filter(!studyID == "Bernier 2021")
  

# make m.genagen object
m.gen <-  metacont(data = dat,
                   n.e = n_exp,
                   mean.e = mean_exp,
                   sd.e = sd_exp,
                   n.c = n_cont,
                   mean.c = mean_cont,
                   sd.c = sd_cont,
                   studlab = studyID,
                   # use Cohen's D (big enough sample size)
                   sm = "MD",
                  # m.genhod.smd = "Cohen",
                   # random effect
                   fixed = FALSE,
                   m.genhod.tau = "REML",
                   # apply Knapp-Hartung 
                   hakn = TRUE,
                   # add prediction interval of effect
                   prediction = TRUE,
                   ## for BERNINI2016
                   q1.e = q1_exp,
                   q1.c = q1_cont,
                   q3.e = q3_exp,
                   q3.c = q3_cont)

# get summary
summary(m.gen)

## ---- subgroup analysis; baseline FBG at least 100 ----

## can't do T2D threshold at 126 because only one study

# create new column
dat <- dat %>% 
  mutate(highFBG = if_else(mean_exp_baseline > 100, "Hyperglycemic", "Normoglycemic"))

# update meta object
sub <- update.meta(m.gen,
                   subgroup = highFBG,
                   data = dat)
summary(sub)


## also report Cohen's D
smd <- update.meta(sub,
                  sm = "SMD",
                  method.smd = "Cohen")
summary(smd)
## ---- influential studies  ----

# look for influential studies 
m.inf <- InfluenceAnalysis(sub, random = TRUE)
m.inf # Culpepper 2019b

## remove influential study
m1 <- update.meta(sub, exclude = 3)

summary(m1)

smd <- update.meta(m1,
                   sm = "SMD",
                   method.smd = "Cohen")
summary(smd)

## ---- Forest plot of subgroup ----

### FOREST PLOT
svg(file = "working-plots/forest_clin_BG.svg", 
    height = 5, width = 11)
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


## ----- funnel plot ----

## save forest plot
svg(file = "working-plots/funnel_clinical_BG.svg")

funnel(m.gen,
       studlab = TRUE)

dev.off()
