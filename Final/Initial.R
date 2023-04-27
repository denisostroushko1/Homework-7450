
library(survival)
library(km.ci)
library(KMsurv)
library(tidyverse)
library(kableExtra)
library(survminer)
library(FHtest)
library(data.table)
library(survRM2)

df <- read_csv('./Final/clinical.csv')

View(df)

colnames_dict <- 
  c('AGECAT',
  'ARM',
  'BR_MAINT',
  'CROSS_EVAL',
  'CYCLE_CROSS',	
  'CYCLE_MAINT'	,
  'CYCLE_MAINT2',
  'FAILCENS',
  'FAILTIME',
  'FAILTIME2',	
  'MASK_ID'	,
  'OFFTRT_RX',	
  'PCI'	,
  'PD2'	,
  'PFS2',
#  'PS'	,
  'RACE',
  'SEX'	,
  'STRAF_CHEMO'	,
  'STRAF_CYCLE'	,
  'SURVCENS'	,
  'SURVTIME'	,
  'SURVTIME2'	)

colnames_dict <- tolower(colnames_dict)

df <- df[,colnames_dict]

summary(as.factor(df$arm))

summary(as.factor(df$survcens))
summary(df$failtime2)
fit <- survfit(Surv(failtime, survcens) ~ as.factor(arm), data = df %>% filter(survtime <= 12))

# Plot the survival curve
ggsurvplot(fit, data = df %>% filter(survtime <= 12), 
           risk.table = T, 
           pval = T,
           palette = c("#E41A1C", "#377EB8"), 
           title = "Survival Estimates by Group",
           conf.int = T)
