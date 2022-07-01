
library(tidyr)
library(ggplot2)
library(ggpubr)
library(dplyr)

library(caret)
library(stargazer)
library(performance)
library(mgcv)

# ---- prepare data ----

setwd("/Users/sebastiaan/PycharmProjects/NetworkSimilarity/data")

df_cc <- read.csv("creditcard.csv") %>%
  mutate(Class=as.factor(Class))

setwd("/Users/sebastiaan/PycharmProjects/NetworkSimilarity/results/metrics")

df_k2 <- read.csv("metrics_scale_k2.csv") %>% select(class, std, frac, amount, log_std, log_amount) %>% mutate(k=2)
df_k4 <- read.csv("metrics_scale_k4.csv") %>% select(class, std, frac, amount, log_std, log_amount) %>% mutate(k=4)
df_k8 <- read.csv("metrics_scale_k8.csv") %>% select(class, std, frac, amount, log_std, log_amount) %>% mutate(k=8)
df_k16 <- read.csv("metrics_scale_k16.csv") %>% select(class, std, frac, amount, log_std, log_amount) %>% mutate(k=16)
df_k32 <- read.csv("metrics_scale_k32.csv") %>% select(class, std, frac, amount, log_std, log_amount) %>% mutate(k=32)
df_k64 <- read.csv("metrics_scale_k64.csv") %>% select(class, std, frac, amount, log_std, log_amount) %>% mutate(k=64)
df_k128 <- read.csv("metrics_scale_k128.csv") %>% select(class, std, frac, amount, log_std, log_amount) %>% mutate(k=128)
df_k256 <- read.csv("metrics_scale_k256.csv") %>% select(class, std, frac, amount, log_std, log_amount) %>% mutate(k=256)

summary(df_k256)

# ---- descriptive measures ----

df_m <- rbind(df_k16, df_k64, df_k256)

ggplot(df_m, aes(x=log_std, colour=as.factor(class))) + geom_density() +
  facet_grid(.~k, labeller = label_both) + scale_x_continuous(limits=c(-7.5, 7.5)) +
  xlab(expression(atop("", paste("log(", italic("L2"), sigma, ")")))) +
  ylab("Density Distribution of Distance\n") + 
  scale_colour_manual(values = c("#FB6467FF", "#24325FFF"), 
                      name = "Class", labels = c("Legitimate", "Fraudulent")) +
  theme_pubr(base_size = 18, base_family = "Georgia")

class.facets <- c("Legitimate", "Fraudulent")
names(class.facets) <- c("0", "1")

k.facets <- c("k: 16", "k: 64", "k: 256")
names(k.facets) <- c("16", "64", "256")

ggplot(df_m, aes(x=frac, colour=as.factor(class))) + geom_density() +
  facet_grid(class~k, labeller = labeller(k = k.facets, class = class.facets), scales="free") +
  xlab(expression(atop("", paste("Fraction of Fraudulent Neighbours ", phi)))) +
  ylab("Density Distribution of the Fraction\n") + 
  scale_colour_manual(values = c("#FB6467FF", "#24325FFF"), 
                      name = "Class", labels = c("Legitimate", "Fraudulent")) +
  theme_pubr(base_size = 18, base_family = "Georgia", legend="none")

# ---- global model ----

model_k2 <- glm(class ~ std*frac, data=df_k2, family=binomial)
model_k4 <- glm(class ~ std*frac, data=df_k4, family=binomial)
model_k8 <- glm(class ~ std*frac, data=df_k8, family=binomial)
model_k16 <- glm(class ~ std*frac, data=df_k16, family=binomial)
model_k32 <- glm(class ~ std*frac, data=df_k32, family=binomial)
model_k64 <- glm(class ~ std*frac, data=df_k64, family=binomial)
model_k128 <- glm(class ~ std*frac, data=df_k128, family=binomial)
model_k256 <- glm(class ~ std*frac, data=df_k256, family=binomial)

stargazer(model_k2, model_k4, model_k8, model_k16, model_k32, model_k64, model_k128, model_k256)

# ---- logarithmic model ----

log_model_k2 <- glm(class ~ log_std*frac, data=df_k2, family=binomial)
log_model_k4 <- glm(class ~ log_std*frac, data=df_k4, family=binomial)
log_model_k8 <- glm(class ~ log_std*frac, data=df_k8, family=binomial)
log_model_k16 <- glm(class ~ log_std*frac, data=df_k16, family=binomial)
log_model_k32 <- glm(class ~ log_std*frac, data=df_k32, family=binomial)
log_model_k64 <- glm(class ~ log_std*frac, data=df_k64, family=binomial)
log_model_k128 <- glm(class ~ log_std*frac, data=df_k128, family=binomial)
log_model_k256 <- glm(class ~ log_std*frac, data=df_k256, family=binomial)

stargazer(log_model_k2, log_model_k4, log_model_k8, log_model_k16, log_model_k32, log_model_k64, log_model_k128, log_model_k256)


