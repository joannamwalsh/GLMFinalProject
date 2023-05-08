library(tidyverse)
library(ggplot2)
library(table1)

#load all results and combine the files
aov2unordered <- read.csv("dataset3_aov_2_unordered_df.csv")
aov2unordered <- aov2unordered %>% mutate(model_type = "unordered", drmethod = "ANOVA (p < 10^(-2))")

aov90unordered <- read.csv("dataset3_aov_90_unordered_df.csv")
aov90unordered <- aov90unordered %>% mutate(model_type = "unordered", drmethod = "ANOVA (p < 10^(-90))")

aov90ordered <- read.csv("dataset3_aov_90_ordered_df.csv")
aov90ordered <- aov90ordered %>% mutate(model_type = "ordered", drmethod = "ANOVA (p < 10^(-90))")

pca0.4unordered <- read.csv("dataset3_pca_0.4_unordered_df.csv")
pca0.4unordered <- pca0.4unordered %>% mutate(model_type = "unordered", drmethod = "PCA (cumVar = 40%)")

pca0.4ordered <- read.csv("dataset3_pca_0.4_ordered_df.csv")
pca0.4ordered <- pca0.4ordered %>% mutate(model_type = "ordered", drmethod = "PCA (cumVar = 40%)")

pca0.9unordered <- read.csv("dataset3_pca_0.9_unordered_df.csv")
pca0.9unordered <- pca0.9unordered %>% mutate(model_type = "unordered", drmethod = "PCA (cumVar = 90%)")

lassounordered <- read.csv("dataset3_lasso_unordered_df.csv")
lassounordered <- lassounordered %>% mutate(model_type = "unordered", drmethod = "LASSO")
  
names(lassounordered)

CNN <- read.csv("resultsfinal.csv")
CNN <- CNN %>% select(-1)
CNN <- CNN[-11, ]
CNN <- CNN[, c(1, 2, 6:10, 3, 4, 5)]
names(CNN) <- names(lassounordered)[1:10]
CNN <- CNN %>% mutate(model_type = "", drmethod = "CNN")

#create a table1 of all the specific accuracies and proportions under/over diagnosed
allresults <- rbind(aov90unordered, aov90ordered, aov2unordered, pca0.4unordered, pca0.4ordered, pca0.9unordered, lassounordered, CNN)
allresults$drmethod <- factor(allresults$drmethod, levels = c("ANOVA (p < 10^(-90))", "ANOVA (p < 10^(-2))", "PCA (cumVar = 40%)", "PCA (cumVar = 90%)", "LASSO", "CNN"))
resultstable <- table1( ~ . | drmethod*model_type, data = allresults[, c(3:7, 9:12)], overall = F, digits = 2)
resultstable
resultstableforprint <- t1kable(resultstable, booktabs = T, format = "latex", longtable = TRUE)
writeLines(resultstableforprint, "resultstable.tex")

#create faceted density plots of the three main measures 
allresults$drmethod <- factor(allresults$drmethod, levels = c("ANOVA (p < 10^(-90))", "PCA (cumVar = 40%)", "PCA (cumVar = 90%)", "ANOVA (p < 10^(-2))", "LASSO", "CNN"))
allresults$model_type <- factor(allresults$model_type, levels = c("unordered", "ordered", ""))
allresults$model_type[allresults$model_type == ""] <- "unordered"
ggplot(data = allresults) + geom_density(aes(x = Multiclass.ROC, y = ..scaled.., fill = drmethod, color = drmethod, linetype = model_type), alpha = 0.5) + theme_bw() + labs(x = "Multiclass AUC", y = "Density", fill = "Model", linetype = "Type") +
  theme(
    legend.position = c(.6, .9),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.box = "horizontal",
    legend.margin = margin(6, 6, 6, 6)
  ) + scale_color_discrete(guide = 'none') + guides(fill = guide_legend(order = 1), linetype = guide_legend(order = 2))


ggplot(data = allresults) + geom_density(aes(x = Accuracy, y = ..scaled.., fill = drmethod, color = drmethod, linetype = model_type), alpha = 0.5) + theme_bw() + labs(x = "Accuracy", y = "Density", fill = "Model", linetype = "Type") +
  theme(
  legend.position = c(.4, .9),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.box = "horizontal",
  legend.margin = margin(6, 6, 6, 6)
) + scale_color_discrete(guide = 'none') + guides(fill = guide_legend(order = 1), linetype = guide_legend(order = 2))

ggplot(data = allresults) + geom_density(aes(x = Loss, y = ..scaled.., fill = drmethod, color = drmethod, linetype = model_type), alpha = 0.5) + theme_bw() + labs(x = "Loss", y = "Density", fill = "Model", linetype = "Type") +
  theme(
    legend.position = c(.6, .9),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.box = "horizontal",
    legend.margin = margin(6, 6, 6, 6)
  ) + scale_color_discrete(guide = 'none') + guides(fill = guide_legend(order = 1), linetype = guide_legend(order = 2))

names(allresults)
allacc <- allresults %>% select(c(2:7, 11:12))
allacclong <- allacc %>% pivot_longer(cols = "Accuracy":"Accuracy.for.Dementia")
allacclong$name <- factor(allacclong$name, levels = c("Accuracy.for.ND", "Accuracy.for.VMD", "Accuracy.for.MILDD", "Accuracy.for.MODD", "Accuracy.for.Dementia", "Accuracy"))
ggplot(data = allacclong) + geom_density(aes(x = value, y = ..scaled.., fill = drmethod, color = drmethod, linetype = model_type), alpha = 0.5) + theme_bw() + facet_wrap(~ name, ncol = 3) + labs(x = "Accuracy", y = "Density", fill = "Model", linetype = "Type") + scale_color_discrete(guide = 'none')

props <- allresults %>% select(9:12)
propslong <- props %>% pivot_longer(cols = "Proportion.Overdiagnosed":"Proportion.Underdiagnosed")
ggplot(data = propslong) + geom_density(aes(x = value, y = ..scaled.., fill = drmethod, color = drmethod, linetype = model_type), alpha = 0.5) + theme_bw() + facet_grid(~ name) + labs(x = "Proportion", y = "Density", fill = "Model", linetype = "Type") + scale_color_discrete(guide = 'none')


