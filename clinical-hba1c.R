## CLINICAL COMPARISONS: Placebo vs probiotics
# HbA1c

# EVS 1/2023

library(tidyverse)
library(meta)
library(dmetar)
library(metafor)

## get data 
dat <- readxl::read_xlsx(path = "../Raw-Stats-for-META.xlsx",
                         sheet = "human_data",
                         na = "NA") %>% 
  filter(variable == "HbA1c") %>% 
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
                   sm = "MD",
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

dat <- dat %>% 
  mutate(phen = if_else(studyID %in% c("Minami 2015", "Ming 2021"),
                        "High", "Normal"))
msub <- update.meta(m.gen,
                    subgroup = phen,
                    data = dat)

# show Cohen's d
smd <- update.meta(m.gen,
                   sm = "SMD",
                   method.smd = "Cohen",
                   hakn = TRUE)

summary(smd)
## ---- influential studies  ----

# look for influential studies 
m.inf <- InfluenceAnalysis(msub, random = TRUE)
m.inf # Ming 2021

# remove influential study
m1 <- update.meta(msub,
                  exclude = 2)

summary(m1)
# show Cohen's d
smd <- update.meta(m1,
                   sm = "SMD",
                   method.smd = "Cohen",
                   hakn = TRUE)

summary(smd)

## ---- Forest plot  ----

### FOREST PLOT
svg(file = "working-plots/forest_clin_HbA1c.svg", 
    height = 5, width = 11)
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


## ----- funnel plot ----

## save forest plot
svg(file = "working-plots/funnel_clinical_HbA1c.svg")

funnel(m.gen,
       studlab = TRUE)

dev.off()
