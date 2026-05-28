setwd("/drives/drive1/ff_chaya/Angel/bin/")

# Read in files 

#allwaves
allwaves <- readRDS("/drives/drive1/ff_chaya/R_versions/FF_allwaves_wY22_CVID_3.24/FF_allwaves_wY22_CVID_3.24_datafile.RDS")
allwaves_var_labels <- read.table("/drives/drive1/ff_chaya/R_versions/FF_allwaves_wY22_CVID_3.24/FF_allwaves_wY22_CVID_3.24_var_labels.txt",
                                  sep = "\t")
#biomarker
biomarker <- readRDS("/drives/drive1/ff_chaya/R_versions/FF_biomarker_CVID_5.24/FF_biomarker_CVID_5.24_datafile.RDS")
biomarker_var_labels <- read.table ("/drives/drive1/ff_chaya/R_versions/FF_biomarker_CVID_5.24/FF_biomarker_CVID_5.24_var_labels.txt", 
                                    sep = "\t")

#LS7
LS7 <- readRDS ("/drives/drive1/ff_chaya/R_versions/FFS_LE8_LS7_all_mergedFFallwvs_Y22sh_CIMT2/FFS_LE8_LS7_all_mergedFFallwvs_Y22sh_CIMT2_datafile.RDS")
LS7_var_labels <- read.table ("/drives/drive1/ff_chaya/R_versions/FFS_LE8_LS7_all_mergedFFallwvs_Y22sh_CIMT2/FFS_LE8_LS7_all_mergedFFallwvs_Y22sh_CIMT2_var_labels.txt", 
                              sep = "\t")

# Write RDS file of subset data frame to Angel/bin
intvar<-"CVID"
intvar<-c(intvar,names(LS7)[grep("Entr",names(LS7))])
intvar<-c(intvar,names(LS7)[grep("Median_Mean",names(LS7))])
intvar<-c(intvar,names(LS7)[grep("Gldm",names(LS7))])
intvar<-c(intvar,names(LS7)[grep("Sgld",names(LS7))])
intvar<-c(intvar,names(LS7)[grep("le8",names(LS7))])
intvar<-c(intvar,names(LS7)[grep("cvid",names(LS7))])

#realpath data folder
# /drives/drive1/ff_chaya/Angel/data
intvar

gs <- LS7 [,intvar] 

gs$Entr_Mean <- rowMeans(gs[,2:3])
gs$Median_Mean <- rowMeans(gs[,4:5])
gs$Gldm_Mean<- rowMeans(gs[, 6:7])
gs$SgldAsm_Mean<- rowMeans(gs[,c(8,10)])
gs$SgldHom_Mean<- rowMeans(gs[,c(9,11)])

summary (lm (formula = gs$Entr_Mean ~ gs$le8_no_slp_score)) #This is significantly different but the effect size is small and negatively associated; left is y and right is x. 

install.packages('ggplot2',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("ggplot2")

install.packages('broom',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library(broom)

install.packages('tidyr',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library(tidyr)

install.packages('Framed.sty',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library()


ggplot(gs, aes(x = le8_no_slp_score, y = Entr_Mean)) + geom_point() + theme_bw() + stat_smooth(method = 'lm', se = FALSE) 

summary (lm (formula = gs$Entr_Mean ~ gs$le8_no_slp_score))

#Plot the box plots for each average score; improve the quality of graph
#note path and send to Eve to download higher quality images for you. 
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/boxplot_summaries.042525.pdf")
#Entr_Mean
ggplot(gs, aes(x = "", y = Entr_Mean)) + 
  geom_boxplot(fill = "white", color = "black", width = 0.3, outlier.shape = 16, outlier.size = 2) + 
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Entropy Mean Distribution",
    y = "Entropy Mean",
    x = ""
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  )
#Median_Mean
ggplot(gs, aes(x = "", y = Median_Mean)) + 
  geom_boxplot(fill = "white", color = "black", width = 0.3, outlier.shape = 16, outlier.size = 2) + 
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Median Mean Distribution",
    y = "Median Mean",
    x = ""
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  )
#Gldm_Mean
ggplot(gs, aes(x = "", y = Gldm_Mean)) + 
  geom_boxplot(fill = "white", color = "black", width = 0.3, outlier.shape = 16, outlier.size = 2) + 
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Gldm Mean Distribution",
    y = "Gldm Mean",
    x = ""
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  )
#SgldAsm_Mean
ggplot(gs, aes(x = "", y = SgldAsm_Mean)) + 
  geom_boxplot(fill = "white", color = "black", width = 0.3, outlier.shape = 16, outlier.size = 2) + 
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "SgldAsm Mean Distribution",
    y = "SgldAsm Mean",
    x = ""
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  )
#SgldHom_Mean
ggplot(gs, aes(x = "", y = SgldHom_Mean)) + 
  geom_boxplot(fill = "white", color = "black", width = 0.3, outlier.shape = 16, outlier.size = 2) + 
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "SgldHom Mean Distribution",
    y = "SgldHom Mean",
    x = ""
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  )

gs_long<- gs|>
  pivot_longer(
    cols = c(Entr_Mean, Median_Mean, Gldm_Mean, SgldAsm_Mean, SgldHom_Mean), 
    names_to = "Feature", 
    values_to = "Value"
  )

ggplot(gs_long, aes(x = Feature, y = Value))+ 
  geom_boxplot(fill = "white", color = "black", width = 0.3, outlier.shape = 16, outlier.size = 2) + 
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  scale_fill_brewer(palette = "Set2")
labs(
  title = "Distribution of Texture Features",
  y = "Value",
  x = "Feature"
) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

dev.off()

#report the means and report the standard deviations. 
summary(gs$Entr_Mean)
summary(gs$Median_Mean)
summary(gs$Gldm_Mean)
summary(gs$SgldAsm_Mean)
summary(gs$SgldHom_Mean)

sd(gs$Entr_Mean, na.rm = T)
sd(gs$Median_Mean, na.rm = T)
sd(gs$Gldm_Mean, na.rm = T)
sd(gs$SgldAsm_Mean, na.rm = T)
sd(gs$SgldHom_Mean, na.rm = T)


# Write RDS file of all the confounding variables
intvar1<-c(intvar,names(LS7)[grep("ff_cm1ethrace",names(LS7))])
intvar1<-c(intvar1,names(LS7)[grep("ff_cm1bsex",names(LS7))])
intvar1<-c(intvar1,names(LS7)[grep("ff_cm1povca",names(LS7))])
intvar1<-c(intvar1,names(LS7)[grep("ck7bmi",names(LS7))])

#create dataframe isolating confounders
gs1 <- LS7 [,intvar1] 

#extract and calculate means from L and R. 
gs1$Entr_Mean <- rowMeans(gs1[,2:3])
gs1$Median_Mean <- rowMeans(gs1[,4:5])
gs1$Gldm_Mean<- rowMeans(gs1[, 6:7])
gs1$SgldAsm_Mean<- rowMeans(gs1[,c(8,10)])
gs1$SgldHom_Mean<- rowMeans(gs1[,c(9,11)])

install.packages('tidyverse',contriburl='file:///mnt/modules/cran/src/contrib',type='source')

library(tidyverse)

#Stratify confounders
#Sex
#First, I will select the columns of interest: sex and average entropy. Then, I will group by sex. Then, I will do a summary. 
#Entropy_Mean
sexconfounders= 
  gs1 |> 
  drop_na(ff_cm1bsex, Entr_Mean)|>
  group_by(ff_cm1bsex) |>
  summarise ('Entr_Mean' = mean(Entr_Mean))|>
  view()
#Median_Mean
sexconfounders1= 
  gs1 |> 
  drop_na(ff_cm1bsex, Median_Mean)|>
  group_by(ff_cm1bsex) |>
  summarise ('Median_Mean' = mean(Median_Mean))|>
  view()
#Gldm_Mean
sexconfounders2= 
  gs1 |> 
  drop_na(ff_cm1bsex, Gldm_Mean)|>
  group_by(ff_cm1bsex) |>
  summarise ('Gldm_Mean' = mean(Gldm_Mean))|>
  view()
#SgldAsm_Mean
sexconfounders3= 
  gs1 |> 
  drop_na(ff_cm1bsex, SgldAsm_Mean)|>
  group_by(ff_cm1bsex) |>
  summarise ('SgldAsm_Mean' = mean(SgldAsm_Mean))|>
  view()
#SgldHom_Mean
sexconfounders4= gs1 |> 
  drop_na(ff_cm1bsex, SgldHom_Mean)|>
  group_by(ff_cm1bsex) |>
  summarise ('SgldHom_Mean' = mean(SgldHom_Mean))|>
  view()
sexconfounderstotal1 <- merge (sexconfounders, sexconfounders1, by = "ff_cm1bsex" )
sexconfounderstotal2 <- merge (sexconfounders2, sexconfounders3, by = "ff_cm1bsex" )
sexconfounderstotal3 <- merge (sexconfounderstotal1, sexconfounderstotal2, by = "ff_cm1bsex" )
sexconfounderstotal <- merge (sexconfounderstotal3, sexconfounders4, by = "ff_cm1bsex" )


#Ethnicity/Race
#Entropy_Mean
ethrconfounders= 
  gs1 |> 
  drop_na(ff_cm1ethrace, Entr_Mean)|>
  group_by(ff_cm1ethrace) |>
  summarise ('Entr_Mean' = mean(Entr_Mean))|>
  view()
#Median_Mean
ethrconfounders1= 
  gs1 |> 
  drop_na(ff_cm1ethrace, Median_Mean)|>
  group_by(ff_cm1ethrace) |>
  summarise ('Median_Mean' = mean(Median_Mean))|>
  view()
#Gldm_Mean
ethrconfounders2= 
  gs1 |> 
  drop_na(ff_cm1ethrace, Gldm_Mean)|>
  group_by(ff_cm1ethrace) |>
  summarise ('Gldm_Mean' = mean(Gldm_Mean))|>
  view()
#SgldAsm_Mean
ethrconfounders3= 
  gs1 |> 
  drop_na(ff_cm1ethrace, SgldAsm_Mean)|>
  group_by(ff_cm1ethrace) |>
  summarise ('SgldAsm_Mean' = mean(SgldAsm_Mean))|>
  view()
#SgldHom_Mean
ethrconfounders4= gs1 |> 
  drop_na(ff_cm1ethrace, SgldHom_Mean)|>
  group_by(ff_cm1ethrace) |>
  summarise ('SgldHom_Mean' = mean(SgldHom_Mean))|>
  view()
ethrconfounderstotal1 <- merge (ethrconfounders, ethrconfounders1, by = "ff_cm1ethrace" )
ethrconfounderstotal2 <- merge (ethrconfounders2, ethrconfounders3, by = "ff_cm1ethrace" )
ethrconfounderstotal3 <- merge (ethrconfounderstotal1, ethrconfounderstotal2, by = "ff_cm1ethrace" )
ethrconfounderstotal <- merge (ethrconfounderstotal3, ethrconfounders4, by = "ff_cm1ethrace" )



#IncomeLvl
#Entropy_Mean
incomeconfounders= 
  gs1 |> 
  drop_na(ff_cm1povca, Entr_Mean)|>
  group_by(ff_cm1povca) |>
  summarise ('Entr_Mean' = mean(Entr_Mean))|>
  view()
#Median_Mean
incomeconfounders1= 
  gs1 |> 
  drop_na(ff_cm1povca, Median_Mean)|>
  group_by(ff_cm1povca) |>
  summarise ('Median_Mean' = mean(Median_Mean))|>
  view()
#Gldm_Mean
incomeconfounders2= 
  gs1 |> 
  drop_na(ff_cm1povca, Gldm_Mean)|>
  group_by(ff_cm1povca) |>
  summarise ('Gldm_Mean' = mean(Gldm_Mean))|>
  view()
#SgldAsm_Mean
incomeconfounders3= 
  gs1 |> 
  drop_na(ff_cm1povca, SgldAsm_Mean)|>
  group_by(ff_cm1povca) |>
  summarise ('SgldAsm_Mean' = mean(SgldAsm_Mean))|>
  view()
#SgldHom_Mean
incomeconfounders4= gs1 |> 
  drop_na(ff_cm1povca, SgldHom_Mean)|>
  group_by(ff_cm1povca) |>
  summarise ('SgldHom_Mean' = mean(SgldHom_Mean))|>
  view()
incomeconfounderstotal1 <- merge (incomeconfounders, incomeconfounders1, by = "ff_cm1povca" )
incomeconfounderstotal2 <- merge (incomeconfounders2, incomeconfounders3, by = "ff_cm1povca" )
incomeconfounderstotal3 <- merge (incomeconfounderstotal1, incomeconfounderstotal2, by = "ff_cm1povca" )
incomeconfounderstotal <- merge (incomeconfounderstotal3, incomeconfounders4, by = "ff_cm1povca" )


#BMI
#Entropy_Mean
bmiconfounders= 
  gs1 |> 
  drop_na(ck7bmi, Entr_Mean)|>
  group_by(ck7bmi) |>
  summarise ('Entr_Mean' = mean(Entr_Mean))|>
  view()
#Median_Mean
bmiconfounders1= 
  gs1 |> 
  drop_na(ck7bmi, Median_Mean)|>
  group_by(ck7bmi) |>
  summarise ('Median_Mean' = mean(Median_Mean))|>
  view()
#Gldm_Mean
bmiconfounders2= 
  gs1 |> 
  drop_na(ck7bmi, Gldm_Mean)|>
  group_by(ck7bmi) |>
  summarise ('Gldm_Mean' = mean(Gldm_Mean))|>
  view()
#SgldAsm_Mean
bmiconfounders3= 
  gs1 |> 
  drop_na(ck7bmi, SgldAsm_Mean)|>
  group_by(ck7bmi) |>
  summarise ('SgldAsm_Mean' = mean(SgldAsm_Mean))|>
  view()
#SgldHom_Mean
bmiconfounders4= gs1 |> 
  drop_na(ck7bmi, SgldHom_Mean)|>
  group_by(ck7bmi) |>
  summarise ('SgldHom_Mean' = mean(SgldHom_Mean))|>
  view()
bmiconfounderstotal1 <- merge (bmiconfounders, bmiconfounders1, by = "ck7bmi" )
bmiconfounderstotal2 <- merge (bmiconfounders2, bmiconfounders3, by = "ck7bmi" )
bmiconfounderstotal3 <- merge (bmiconfounderstotal1, bmiconfounderstotal2, by = "ck7bmi" )
bmiconfounderstotal <- merge (bmiconfounderstotal3, bmiconfounders4, by = "ck7bmi" )


#Ttest for all
#Features List
features <- c( "Entr_Mean", "Median_Mean", "Gldm_Mean", "SgldAsm_Mean", "SgldHom_Mean")
results_list <-list()

for (feature in features) {
  #Ttest for sex
  ttest_sex <- t.test (as.formula(paste(feature, "~ ff_cm1bsex")), data = gs1)
  
  #Anova for Race
  anova_race <- aov(as.formula(paste(feature, " ~ ff_cm1ethrace")), data = gs1)
  summary(anova_race)
  #Anova for Income Level
  anova_pov <- aov(as.formula(paste(feature, " ~ ff_cm1povca")), data = gs1)
  summary(anova_pov)
  #lm for bmi
  lm_bmi <- lm(as.formula(paste(feature, " ~ ck7bmi")), data = gs1)
  summary(lm_bmi)
  #ESTIMATE1 is mean_boy; ESTIMATE 2 is mean_girl; Report pvalues of only the significant ones. 
  #Store tidy summaries
  results_list [[feature]] <- list (
    T_Test_Sex = tidy(ttest_sex), 
    ANOVA_Race = tidy(anova_race), 
    ANOVA_Income = tidy(anova_pov)
  )
  
  
}

print(results_list)

#Count number of each entry then divide number by total

LS7|>
  count(cm1ethrace)
nrow(LS7[LS7$ff_cm1bsex != "-3 Missing"])

#1 white, non-hispanic = 20.6%
#2 black, non-hispanic = 51.2%
#3 hispanic = 24.2%
#4 other = 3.8


install.packages('ggsignif',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/boxplot_summaries.042525.pdf")
# Plot sex difference for grayscale features
#Entr_Mean
library(ggsignif)
ggplot(data = gs1, aes(x = ff_cm1bsex, y = Entr_Mean)) +
  geom_boxplot() + 
  geom_signif(comparisons = list(c("1 Boy", "2 Girl")), map_signif_level = T)+
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Distribution of Vascular Wall Entropy By Sex",
    y = "Average Vascular Wall Entropy",
    x = "Sex"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)
#Median_Mean
ggplot(data = gs1, aes(x = ff_cm1bsex, y = Median_Mean)) +
  geom_boxplot() + 
  geom_signif(comparisons = list(c("1 Boy", "2 Girl")), map_signif_level = T)+
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Distribution of Grayscale Median By Sex",
    y = "Average Grayscale Median",
    x = "Sex"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)
#Gldm_Mean
ggplot(data = gs1, aes(x = ff_cm1bsex, y = Gldm_Mean)) +
  geom_boxplot() + 
  geom_signif(comparisons = list(c("1 Boy", "2 Girl")), map_signif_level = T)+
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Distribution of GLDM By Sex",
    y = "Average GLDM",
    x = "Sex"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)
#SgldAsm_Mean
ggplot(data = gs1, aes(x = ff_cm1bsex, y = SgldAsm_Mean)) +
  geom_boxplot() + 
  geom_signif(comparisons = list(c("1 Boy", "2 Girl")), map_signif_level = T)+
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Distribution of SGLD-ASM By Sex",
    y = "Average SGLD-ASM",
    x = "Sex"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)
#SgldHom_Mean
ggplot(data = gs1, aes(x = ff_cm1bsex, y = SgldHom_Mean)) +
  geom_boxplot() + 
  geom_signif(comparisons = list(c("1 Boy", "2 Girl")), map_signif_level = T)+
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Distribution of SGLD-HOM By Sex",
    y = "Average SGLD-HOM",
    x = "Sex"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)
# Plot ethrace differences
# * I'm only showing the significant comparisons. You should mention this * #
#Entr_Mean
ggplot(data = gs1[gs1$ff_cm1ethrace != "-3 Missing",], 
       aes(x = ff_cm1ethrace, y = Entr_Mean)) +
  geom_boxplot() + 
  geom_signif(comparisons = list(c("1 white, non-hispanic", "2 black, non-hispanic"),
                                 # c("3 hispanic", "1 white, non-hispanic"),
                                 c("3 hispanic", "2 black, non-hispanic")),
              # c("4 other", "1 white, non-hispanic"),
              # c("4 other", "2 black, non-hispanic"),
              # c("4 other", "3 hispanic")), 
              map_signif_level = T)+
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Distribution of Vascular Wall Entropy By Race",
    y = "Average Vascular Wall Entropy",
    x = "Race"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)

#Median_Mean
ggplot(data = gs1[gs1$ff_cm1ethrace != "-3 Missing",], 
       aes(x = ff_cm1ethrace, y = Median_Mean)) +
  geom_boxplot() + 
  geom_signif(comparisons = list(#c("1 white, non-hispanic", "2 black, non-hispanic"),
    c("3 hispanic", "1 white, non-hispanic"),
    c("3 hispanic", "2 black, non-hispanic")),
    #c("4 other", "1 white, non-hispanic")),
    #c("4 other", "2 black, non-hispanic"),
    #c("4 other", "3 hispanic")), 
    map_signif_level = T)+
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Distribution of Average Grayscale Median By Race",
    y = "Average Grayscale Median",
    x = "Race"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)
#Gldm_Mean
ggplot(data = gs1[gs1$ff_cm1ethrace != "-3 Missing",], 
       aes(x = ff_cm1ethrace, y = Gldm_Mean)) +
  geom_boxplot() + 
  geom_signif(comparisons = list(c("1 white, non-hispanic", "2 black, non-hispanic"),
                                 #c("3 hispanic", "1 white, non-hispanic"),
                                 c("3 hispanic", "2 black, non-hispanic")),
              # c("4 other", "1 white, non-hispanic")),
              # c("4 other", "2 black, non-hispanic"),
              # c("4 other", "3 hispanic")), 
              map_signif_level = T)+
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Distribution of Average GLDM By Race",
    y = "Average GLDM",
    x = "Race"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)
#SgldAsm_Mean
ggplot(data = gs1[gs1$ff_cm1ethrace != "-3 Missing",], 
       aes(x = ff_cm1ethrace, y = SgldAsm_Mean)) +
  geom_boxplot() + 
  geom_signif(comparisons = list(c("1 white, non-hispanic", "2 black, non-hispanic"),
                                 # c("3 hispanic", "1 white, non-hispanic"),
                                 c("3 hispanic", "2 black, non-hispanic")),
              # c("4 other", "1 white, non-hispanic"),
              # c("4 other", "2 black, non-hispanic"),
              # c("4 other", "3 hispanic")), 
              map_signif_level = T)+
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Distribution of Average SGLDM-ASM By Race",
    y = "Average SGLDM-ASM",
    x = "Race"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)
#SgldHom_Mean"
ggplot(data = gs1[gs1$ff_cm1ethrace != "-3 Missing",], 
       aes(x = ff_cm1ethrace, y = SgldAsm_Mean)) +
  geom_boxplot() + 
  geom_signif(comparisons = list(c("1 white, non-hispanic", "2 black, non-hispanic"),
                                 #c("3 hispanic", "1 white, non-hispanic"),
                                 c("3 hispanic", "2 black, non-hispanic")),
              # c("4 other", "1 white, non-hispanic")),
              #c("4 other", "2 black, non-hispanic"),
              #c("4 other", "3 hispanic")), 
              map_signif_level = T)+
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Distribution of Average SGLDM-HOM By Race",
    y = "Average SGLDM-HOM",
    x = "Race"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)

# Plot poverty level differences
#Entr_Mean
ggplot(data = gs1, 
       aes(x = ff_cm1povca, y = Entr_Mean)) +
  geom_boxplot() + 
  geom_signif(comparisons = list(#c("1 0-49%", "2 50-99%")),
    #c("3 100-199%", "1 0-49%")),
    c("1 0-49%", "4 200-299%"),
    c("1 0-49%", "300%+")),
    #c("2 50-99%", "3 100-199%")),
    #c("4 200-299%", "2 50-99%")),
    #("300%+", "2 50-99%")),
    #c("300%+", "3 100-199%")),
    #c("4 200-299%", "3 100-199%")),
    #c("4 200-299%", "300%+")), 
    map_signif_level = T)+
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Distribution of Average Vascular Wall Entropy By Income Level",
    y = "Average Vascular Wall Entropy",
    x = "Income Level"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)

#Median_Mean
ggplot(data = gs1, 
       aes(x = ff_cm1povca, y = Median_Mean)) +
  geom_boxplot() + 
  # geom_signif(comparisons = list(c("1 0-49%", "2 50-99%")),
  # c("3 100-199%", "1 0-49%"),
  #c("1 0-49%", "4 200-299%"),
  #c("1 0-49%", "300%+"),
  #c("2 50-99%", "3 100-199%")),
  #c("4 200-299%", "2 50-99%")),
  #c("300%+", "2 50-99%")),
  #c("300%+", "3 100-199%")),
  #c("4 200-299%", "3 100-199%")),
  #c("4 200-299%", "300%+")),
  # c("4 200-299%", "2 black, non-hispanic"),
  # c("4 200-299%", "3 hispanic")), 
  #map_signif_level = T)+
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Distribution of Average Grayscale Median By Income Level",
    y = "Average Grayscale Median",
    x = "Income Level"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)

#Gldm_Mean
ggplot(data = gs1, 
       aes(x = ff_cm1povca, y = Gldm_Mean)) +
  geom_boxplot() + 
  geom_signif(comparisons = list(#c("1 0-49%", "2 50-99%")),
    #c("3 100-199%", "1 0-49%")),
    c("1 0-49%", "4 200-299%"),
    c("1 0-49%", "300%+")),
    #c("2 50-99%", "3 100-199%")),
    #c("4 200-299%", "2 50-99%")),
    #c("300%+", "2 50-99%")),
    #c("300%+", "3 100-199%")),
    #c("4 200-299%", "3 100-199%")),
    #c("4 200-299%", "300%+")),
    
    map_signif_level = T)+
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Distribution of Average GLDM By Income Level",
    y = "Average GLDM",
    x = "Income Level"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)
#SgldAsm_Mean
ggplot(data = gs1, 
       aes(x = ff_cm1povca, y = SgldAsm_Mean)) +
  geom_boxplot() + 
  #geom_signif(comparisons = list(#c("1 0-49%", "2 50-99%")),
  #c("3 100-199%", "1 0-49%")),
  #c("1 0-49%", "4 200-299%")),
  #c("1 0-49%", "300%+")),
  #c("2 50-99%", "3 100-199%")),
  #c("4 200-299%", "2 50-99%")),
  #c("300%+", "2 50-99%")),
  #c("300%+", "3 100-199%")),
  #c("4 200-299%", "3 100-199%")),
  #c("4 200-299%", "300%+")), 
  #  map_signif_level = T)+
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Distribution of Average SGLD-ASM By Income Level",
    y = "Average SGLD-ASM",
    x = "Income Level"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)
#SgldHom_Mean
ggplot(data = gs1, 
       aes(x = ff_cm1povca, y = SgldHom_Mean)) +
  geom_boxplot() + 
  geom_signif(comparisons = list(#c("1 0-49%", "2 50-99%")),
    #c("3 100-199%", "1 0-49%")),
    #c("1 0-49%", "4 200-299%")),
    c("1 0-49%", "300%+")),
    #c("2 50-99%", "3 100-199%")),
    #c("4 200-299%", "2 50-99%")),
    #c("300%+", "2 50-99%")),
    #c("300%+", "3 100-199%")),
    #c("4 200-299%", "3 100-199%")),
    #c("4 200-299%", "300%+")),
    map_signif_level = T)+
  stat_summary(fun = median, geom = "point", shape = 95, size = 10, color = "red")+
  labs(
    title = "Distribution of Average SGLD-HOM By Income Level",
    y = "Average SGLD-HOM",
    x = "Income Level"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)




#Plot BMI Differences
#Entr_Mean
entr_meanBMI<- (lm (formula = gs1$Entr_Mean ~ gs1$ck7bmi))
summary_entr_meanBMI<- summary(entr_meanBMI)

entr_meanintercept <- round(coef(entr_meanBMI)[1], 3)
entr_meanslope <- round(coef(entr_meanBMI)[2], 3)
entr_meanr_squared <- round(summary_entr_meanBMI$r.squared, 3)

eq_label <- paste0("y = ", entr_meanslope, "x + ", entr_meanintercept, "R^2 =", entr_meanr_squared)

gs1|>
  filter(ck7bmi > 10 & ck7bmi < 100)|>
  ggplot( aes(x = ck7bmi, y = Entr_Mean)) + geom_point(alpha = 0.6, size = 2) + theme_bw() + 
  stat_smooth(method = 'lm', se = T)+ 
  #stat_summary(fun = median, geom = "point", shape = 95, size = 8)+
  # geom_smooth(method = 'lm', se = FALSE, color = "black")+
  #annotate("text", x = 20, label, y = max(gs1$Entr_Mean, na.rm= TRUE), label = eq_label, hjust = 0, size = 5)+
  #stat_cor(label.x = 20, label.y = max(gs1$Entr_Mean, na.rm = TRUE)-0.1)+
  labs(
    title = "Association Between Average Vascular Wall Entropy and BMI",
    y = "Average Vascular Wall Entropy",
    x = "Body Mass Index (BMI)"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)

summary (lm (formula = gs1$Entr_Mean ~ gs1$ck7bmi))



#Median_Mean
gs1|>
  filter(ck7bmi > 10 & ck7bmi < 100)|>
  ggplot(aes(x = ck7bmi, y = Median_Mean)) + geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+ 
  #stat_summary(fun = median, geom = "point", shape = 95, size = 10)+
  labs(
    title = "Association Between Grayscale Median and BMI",
    y = "Average Grayscale Median",
    x = "Body Mass Index (BMI)"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)

summary (lm (formula = gs1$Median_Mean ~ gs1$ck7bmi))
#Gldm_Mean
gs1|>
  filter(ck7bmi > 10 & ck7bmi < 100)|>
  ggplot(aes(x = ck7bmi, y = Gldm_Mean)) + geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+ 
  #stat_summary(fun = median, geom = "point", shape = 95, size = 10)+
  labs(
    title = "Association Between GLDM and BMI",
    y = "Average GLDM",
    x = "Body Mass Index (BMI)"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)

summary (lm (formula = gs1$Gldm_Mean ~ gs1$ck7bmi))
#SgldAsm_Mean
gs1|>
  filter(ck7bmi > 10 & ck7bmi < 100)|>
  ggplot(aes(x = ck7bmi, y = SgldAsm_Mean)) + geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+ 
  # stat_summary(fun = median, geom = "point", shape = 95, size = 10)+
  labs(
    title = "Association Between SGLD-ASM and BMI",
    y = "Average SGLD-ASM ",
    x = "Body Mass Index (BMI)"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)

summary (lm (formula = gs1$SgldAsm_Mean ~ gs1$ck7bmi))
#SgldHom_Mean
gs1|>
  filter(ck7bmi > 10 & ck7bmi < 100)|>
  ggplot(aes(x = ck7bmi, y = SgldHom_Mean)) + geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+ 
  #stat_summary(fun = median, geom = "point", shape = 95, size = 10)+
  labs(
    title = "Association Between SGLD-HOM and BMI",
    y = "Average SGLD-HOM ",
    x = "Body Mass Index (BMI)"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),)

summary (lm (formula = gs1$SgldHom_Mean ~ gs1$ck7bmi))

dev.off()


#I tried to add an equation to the scatterplot but I'm not sure if I even need it. 
#Also, I used a scatterplot since this is a continuous variable. (Linear regression mo)


#Grayscale Features:
#Entr_Mean
mean(gs1$Entr_Mean, na.rm = T)
sd(gs1$Entr_Mean, na.rm = T)

#Median_Mean
mean(gs1$Median_Mean, na.rm = T)
sd(gs1$Median_Mean, na.rm = T)
#Gldm_Mean
mean(gs1$Gldm_Mean, na.rm = T)
sd(gs1$Gldm_Mean, na.rm = T)
#SgldAsm_Mean
mean(gs1$SgldAsm_Mean, na.rm = T)
sd(gs1$SgldAsm_Mean, na.rm = T)
#SgldHom_Mean
mean(gs1$SgldHom_Mean, na.rm = T)
sd(gs1$SgldHom_Mean, na.rm = T)

setwd("/drives/drive1/ff_chaya/Angel/bin/")

# Read in files 

#allwaves
allwaves <- readRDS("/drives/drive1/ff_chaya/R_versions/FF_allwaves_wY22_CVID_3.24/FF_allwaves_wY22_CVID_3.24_datafile.RDS")
allwaves_var_labels <- read.table("/drives/drive1/ff_chaya/R_versions/FF_allwaves_wY22_CVID_3.24/FF_allwaves_wY22_CVID_3.24_var_labels.txt",
                                  sep = "\t")
#biomarker
biomarker <- readRDS("/drives/drive1/ff_chaya/R_versions/FF_biomarker_CVID_5.24/FF_biomarker_CVID_5.24_datafile.RDS")
biomarker_var_labels <- read.table ("/drives/drive1/ff_chaya/R_versions/FF_biomarker_CVID_5.24/FF_biomarker_CVID_5.24_var_labels.txt", 
                                    sep = "\t")

#LS7
LS7 <- readRDS ("/drives/drive1/ff_chaya/R_versions/FFS_LE8_LS7_all_mergedFFallwvs_Y22sh_CIMT2/FFS_LE8_LS7_all_mergedFFallwvs_Y22sh_CIMT2_datafile.RDS")
LS7_var_labels <- read.table ("/drives/drive1/ff_chaya/R_versions/FFS_LE8_LS7_all_mergedFFallwvs_Y22sh_CIMT2/FFS_LE8_LS7_all_mergedFFallwvs_Y22sh_CIMT2_var_labels.txt", 
                              sep = "\t")

# Write RDS file of subset data frame to Angel/bin
intvar<-"CVID"
intvar<-c(intvar,names(LS7)[grep("Entr",names(LS7))])
intvar<-c(intvar,names(LS7)[grep("Median_Mean",names(LS7))])
intvar<-c(intvar,names(LS7)[grep("Gldm",names(LS7))])
intvar<-c(intvar,names(LS7)[grep("Sgld",names(LS7))])
intvar<-c(intvar,names(LS7)[grep("le8",names(LS7))])
intvar<-c(intvar,names(LS7)[grep("cvid",names(LS7))])

#realpath data folder
# /drives/drive1/ff_chaya/Angel/data
intvar
gs <- LS7 [,intvar] 

gs$Entr_Mean <- rowMeans(gs[,2:3])
gs$Median_Mean <- rowMeans(gs[,4:5])
gs$Gldm_Mean<- rowMeans(gs[, 6:7])
gs$SgldAsm_Mean<- rowMeans(gs[,c(8,10)])
gs$SgldHom_Mean<- rowMeans(gs[,c(9,11)])

summary (lm (formula = gs$Entr_Mean ~ gs$le8_no_slp_score)) 
#This is significantly different but the effect size is small and negatively associated; left is y and right is x. 

install.packages('pheatmap',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("pheatmap")

install.packages('ggplot2',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("ggplot2")

install.packages('broom',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library(broom)

install.packages('tidyr',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library(tidyr)

install.packages('Framed.sty',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library()

install.packages('dplyr ',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library(dplyr)

# Write RDS file of all the confounding variables
intvar1<-c(intvar,names(LS7)[grep("ff_cm1ethrace",names(LS7))])
intvar1<-c(intvar1,names(LS7)[grep("ff_cm1bsex",names(LS7))])
intvar1<-c(intvar1,names(LS7)[grep("ff_cm1povca",names(LS7))])
intvar1<-c(intvar1,names(LS7)[grep("ck7bmi",names(LS7))])

#create dataframe isolating confounders
gs1 <- LS7 [,intvar1] 

#extract and calculate means from L and R. 
gs1$Entr_Mean <- rowMeans(gs1[,2:3])
gs1$Median_Mean <- rowMeans(gs1[,4:5])
gs1$Gldm_Mean<- rowMeans(gs1[, 6:7])
gs1$SgldAsm_Mean<- rowMeans(gs1[,c(8,10)])
gs1$SgldHom_Mean<- rowMeans(gs1[,c(9,11)])

#SCATTERPLOT SUMMARIES OF THE RELATIONSHIPS BETWEEN THE COVARIATES AND GRAYSCALE FEATURES. 

features <- c( "Entr_Mean", "Median_Mean", "Gldm_Mean", "SgldAsm_Mean", "SgldHom_Mean")
results_list <-list()
for (feature in features) {
  #lm for sex
  lm_sex <- lm(as.formula(paste(feature, " ~ ff_cm1bsex")), data = gs1)
  summary(lm_sex)
  
  #lm for Race
  lm_race <- lm(as.formula(paste(feature, " ~ ff_cm1ethrace")), data = gs1)
  summary(lm_race)
  
  #lm for Income Level
  lm_incomelvl <- lm(as.formula(paste(feature, " ~ ff_cm1povca")), data = gs1)
  summary(lm_incomelvl)
  #lm for bmi
  lm_bmi <- lm(as.formula(paste(feature, " ~ ck7bmi")), data = gs1)
  summary(lm_bmi)
}

#cm1inpov= continuous income level. 

#Call:
# lm(formula = as.formula(paste(feature, " ~ ff_cm1bsex")), data = gs1)

#Residuals:
# Min        1Q    Median        3Q       Max 
#-0.099847 -0.024847 -0.004079  0.021407  0.175403 

#Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       0.291829   0.001401 208.276   <2e-16 ***
# ff_cm1bsex2 Girl -0.001482   0.001884  -0.786    0.432    
#---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.0348 on 1378 degrees of freedom
#(41 observations deleted due to missingness)
#Multiple R-squared:  0.0004485,	Adjusted R-squared:  -0.0002769 
#F-statistic: 0.6183 on 1 and 1378 DF,  p-value: 0.4318

#> lm_race <- lm(as.formula(paste(feature, " ~ ff_cm1ethrace")), data = gs1)
#>   summary(lm_race)

#Call:
#lm(formula = as.formula(paste(feature, " ~ ff_cm1ethrace")), 
#    data = gs1)

#Residuals:
# Min        1Q    Median        3Q       Max 
#-0.097603 -0.024438 -0.003353  0.020932  0.173537 

#Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                        0.286083   0.014164  20.198   <2e-16 ***
# ff_cm1ethrace1 white, non-hispanic 0.006130   0.014316   0.428    0.669    
#ff_cm1ethrace2 black, non-hispanic 0.002019   0.014224   0.142    0.887    
#ff_cm1ethrace3 hispanic            0.009846   0.014291   0.689    0.491    
#ff_cm1ethrace4 other               0.006889   0.014930   0.461    0.645    
#---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.0347 on 1375 degrees of freedom
#(41 observations deleted due to missingness)
#Multiple R-squared:  0.008873,	Adjusted R-squared:  0.00599 
#F-statistic: 3.077 on 4 and 1375 DF,  p-value: 0.01549

#>  lm_incomelvl <- lm(as.formula(paste(feature, " ~ ff_cm1povca")), data = gs1)
#>   summary(lm_incomelvl)

#Call:
# lm(formula = as.formula(paste(feature, " ~ ff_cm1povca")), data = gs1)

#Residuals:
# Min        1Q    Median        3Q       Max 
#-0.103396 -0.025175 -0.004058  0.021190  0.177075 

#Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            0.293896   0.002183 134.627   <2e-16 ***
# ff_cm1povca2 50-99%   -0.002333   0.003129  -0.746   0.4559    
#ff_cm1povca3 100-199% -0.001841   0.002844  -0.647   0.5175    
#ff_cm1povca4 200-299% -0.005373   0.003308  -1.624   0.1046    
#ff_cm1povca300%+      -0.005221   0.002914  -1.792   0.0734 .  
#---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.03479 on 1375 degrees of freedom
#(41 observations deleted due to missingness)
#Multiple R-squared:  0.003336,	Adjusted R-squared:  0.0004362 
#F-statistic:  1.15 on 4 and 1375 DF,  p-value: 0.3311

#> lm_bmi <- lm(as.formula(paste(feature, " ~ ck7bmi")), data = gs1)
#>   summary(lm_bmi)

#Call:
#lm(formula = as.formula(paste(feature, " ~ ck7bmi")), data = gs1)

#Residuals:
#Min        1Q    Median        3Q       Max 
#-0.103509 -0.025302 -0.004086  0.021153  0.175783 

#Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 2.863e-01  2.715e-03 105.450   <2e-16 ***
#ck7bmi      1.775e-04  9.554e-05   1.858   0.0634 .  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.03477 on 1378 degrees of freedom
#(41 observations deleted due to missingness)
#Multiple R-squared:  0.002499,	Adjusted R-squared:  0.001775 
#F-statistic: 3.452 on 1 and 1378 DF,  p-value: 0.06339



#SCATTERPLOT FOR SEX, RACE, INCOME LEVEL; This is not a continuous variable.
#BMI is the only continuous variable and I have scatterplots for that. 
#Entr_mean


#SCATTERPLOT SUMMARIES OF THE RELATIONSHIPS BETWEEN THE LE8 SCORES AND GRAYSCALE FEATURES. 
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/LE8scatterplotsummaries.060425.pdf")

#Entr_mean vs LE8 scores lm; 
#This is significantly different but the effect size is small and negatively associated; left is y and right is x.
#Multiple R-squared:  0.006069,	Adjusted R-squared:  0.005218 
#F-statistic: 7.126 on 1 and 1167 DF,  p-value: 0.007702

ggplot(gs, aes(x = Entr_Mean, y = le8_no_slp_score)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs LE8 Scores",
    y = "LE8 Scores",
    x = "Entropy Mean"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs$Entr_Mean ~ gs$le8_no_slp_score))

#Median_Mean vs LE8 scores lm 
#Multiple R-squared:  0.07982,	Adjusted R-squared:  0.07903 
#F-statistic: 101.2 on 1 and 1167 DF,  p-value: < 2.2e-16
ggplot(gs, aes(x = Median_Mean, y = le8_no_slp_score)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+ 
  labs(
    title = "Median Mean vs LE8 Scores",
    y = "LE8 Scores",
    x = "Median Mean"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  )

summary (lm (formula = gs$Median_Mean ~ gs$le8_no_slp_score))

#Gldm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0005928,	Adjusted R-squared:  -0.0002636 
#F-statistic: 0.6923 on 1 and 1167 DF,  p-value: 0.4056
ggplot(gs, aes(x = Gldm_Mean, y = le8_no_slp_score)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+ 
  labs(
    title = "GLDM Mean vs LE8 Scores",
    y = "LE8 Scores",
    x = "GLDM Mean"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  )

summary (lm (formula = gs$Gldm_Mean ~ gs$le8_no_slp_score))

#SgldAsm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.002415,	Adjusted R-squared:  0.00156 
#F-statistic: 2.825 on 1 and 1167 DF,  p-value: 0.09308

ggplot(gs, aes(x = SgldAsm_Mean, y = le8_no_slp_score)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+ 
  labs(
    title = "SGLD-ASM Mean vs LE8 Scores",
    y = "LE8 Scores",
    x = "SGLD-ASM Mean"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  )

summary (lm (formula = gs$SgldAsm_Mean ~ gs$le8_no_slp_score))

#SgldHom_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0003411,	Adjusted R-squared:  -0.0005155 
#F-statistic: 0.3982 on 1 and 1167 DF,  p-value: 0.5281

ggplot(gs, aes(x = SgldHom_Mean, y = le8_no_slp_score)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+ 
  labs(
    title = "SGLD-HOM Mean vs LE8 Scores",
    y = "LE8 Scores",
    x = "SGLD-HOM Mean"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  )

summary (lm (formula = gs$SgldHom_Mean ~ gs$le8_no_slp_score))

dev.off()

#SCATTERPLOT SUMMARIES OF THE RELATIONSHIPS BETWEEN THE DNAm AGE AND GRAYSCALE FEATURES. 

#ADD DNAm AGE TO GS DATASET for each of 6 desired clocks (k5= age 9; k6= age15)
#Clocks: Horvath Pan-Tissue (k5mk_horvath; k5me_horvath), 
#Horvath Skin and Blood (k5mk_skinblood; k5me_skinblood), 
#DNAmPhenoAge(k5mk_phenoage;k5me_phenoage), 
#DNAmGrimAge (k5mk_grim; k5me_grim), 
#DunedinPoAm (k5mk_poam38; k5me_poam38),
#DunedinPACE (k5mk_poam45; k5me_poam45),

intvar2<-"CVID"
intvar2<-c(intvar2,names(biomarker)[grep("k5mk_horvath",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k5me_horvath",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k6mk_horvath",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k6me_horvath",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k5mk_skinblood",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k5me_skinblood",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k6mk_skinblood",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k6me_skinblood",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k5mk_phenoage",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k5me_phenoage",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k6mk_phenoage",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k6me_phenoage",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k5mk_grim",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k5me_grim",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k6mk_grim",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k6me_grim",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k5mk_poam38",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k5me_poam38",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k6mk_poam38",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k6me_poam38",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k5mk_poam45",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k5me_poam45",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k6mk_poam45",names(biomarker))])
intvar2<-c(intvar2,names(biomarker)[grep("k6me_poam45",names(biomarker))])


#create dataframe isolating desired clocks
gs2<- biomarker [,intvar2] 



#I need to put entrmeans and biomarkers in the same dataframe. 
gs_joined <- left_join (gs1, gs2, by = "CVID")

write.csv(gs_joined, "gs_joined.csv")


#HORVATH PAN-TISSUE
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/HorvathPan-Tissue")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/HorvathPan-Tissue/k5mk_horvath060625.pdf")

#Entr_mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.01443,	Adjusted R-squared:  0.01186 
#F-statistic:  5.62 on 1 and 384 DF,  p-value: 0.01825

ggplot(gs_joined, aes(y = Entr_Mean, x = k5mk_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs Horvath Pan-Tissue",
    y = "Entropy Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k5mk_horvath))

#Median_Mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.0004728,	Adjusted R-squared:  -0.00213 
#F-statistic: 0.1816 on 1 and 384 DF,  p-value: 0.6702

ggplot(gs_joined, aes(y = Median_Mean, x = k5mk_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs Horvath Pan-Tissue",
    y = "Median Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k5mk_horvath))

#Gldm_Mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.001589,	Adjusted R-squared:  -0.001011 
#F-statistic: 0.6113 on 1 and 384 DF,  p-value: 0.4348

ggplot(gs_joined, aes(y = Gldm_Mean, x = k5mk_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs Horvath Pan-Tissue",
    y = "GLDM Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k5mk_horvath))

#SgldASM_Mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.02064,	Adjusted R-squared:  0.01809 
#F-statistic: 8.094 on 1 and 384 DF,  p-value: 0.00468

ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k5mk_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs Horvath Pan-Tissue",
    y = "SGLD-ASM Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k5mk_horvath))

#SgldHOM_Mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.01541,	Adjusted R-squared:  0.01285 
#F-statistic: 6.011 on 1 and 384 DF,  p-value: 0.01466

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k5mk_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs Horvath Pan-Tissue",
    y = "Median Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k5mk_horvath))
dev.off()

dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/HorvathPan-Tissue")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/HorvathPan-Tissue/k5me_horvath060625.pdf")

#Entr_mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.006225,	Adjusted R-squared:  0.004213 
#F-statistic: 3.094 on 1 and 494 DF,  p-value: 0.07919

ggplot(gs_joined, aes(y = Entr_Mean, x = k5me_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs Horvath Pan-Tissue",
    y = "Entropy Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k5me_horvath))

#Median_Mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.002755,	Adjusted R-squared:  0.0007362 
#F-statistic: 1.365 on 1 and 494 DF,  p-value: 0.2433

ggplot(gs_joined, aes(y = Median_Mean, x = k5me_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs Horvath Pan-Tissue",
    y = "Median Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k5me_horvath))

#Gldm_Mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  3.555e-05,	Adjusted R-squared:  -0.001989 
#F-statistic: 0.01756 on 1 and 494 DF,  p-value: 0.8946

ggplot(gs_joined, aes(y = Gldm_Mean, x = k5me_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs Horvath Pan-Tissue",
    y = "GLDM Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k5me_horvath))

#SgldASM_Mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.001624,	Adjusted R-squared:  -0.000397 
#F-statistic: 0.8036 on 1 and 494 DF,  p-value: 0.3705

ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k5me_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs Horvath Pan-Tissue",
    y = "SGLD-ASM Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k5me_horvath))

#SgldHOM_Mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.0001842,	Adjusted R-squared:  -0.00184 
#F-statistic: 0.09101 on 1 and 494 DF,  p-value: 0.763


ggplot(gs_joined, aes(y = SgldHom_Mean, x = k5me_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs Horvath Pan-Tissue",
    y = "SGLD-HOM Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k5me_horvath))
dev.off()

dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/HorvathPan-Tissue")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/HorvathPan-Tissue/k6mk_horvath060625.pdf")

#Entr_mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.000546,	Adjusted R-squared:  -0.001946 
#F-statistic: 0.219 on 1 and 401 DF,  p-value: 0.64

ggplot(gs_joined, aes(y = Entr_Mean, x = k6mk_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs Horvath Pan-Tissue",
    y = "Entropy Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k6mk_horvath))

#Median_Mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.005645,	Adjusted R-squared:  0.003165 
#F-statistic: 2.276 on 1 and 401 DF,  p-value: 0.1321

ggplot(gs_joined, aes(y = Median_Mean, x = k6mk_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs Horvath Pan-Tissue",
    y = "Median Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k6mk_horvath))

#Gldm_Mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.002548,	Adjusted R-squared:  6.044e-05 
#F-statistic: 1.024 on 1 and 401 DF,  p-value: 0.3121

ggplot(gs_joined, aes(y = Gldm_Mean, x = k6mk_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs Horvath Pan-Tissue",
    y = "GLDM Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k6mk_horvath))

#SgldASM_Mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.002376,	Adjusted R-squared:  -0.0001117 
#F-statistic: 0.9551 on 1 and 401 DF,  p-value: 0.329

ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k6mk_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs Horvath Pan-Tissue",
    y = "SGLD-ASM Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k6mk_horvath))

#SgldHOM_Mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.001357,	Adjusted R-squared:  -0.001134 
#F-statistic: 0.5447 on 1 and 401 DF,  p-value: 0.4609

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k6mk_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs Horvath Pan-Tissue",
    y = "SGLD-HOM Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k6mk_horvath))
dev.off()

dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/HorvathPan-Tissue")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/HorvathPan-Tissue/k6me_horvath060625.pdf")

#Entr_mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.01054,	Adjusted R-squared:  0.008517 
#F-statistic: 5.201 on 1 and 488 DF,  p-value: 0.02301

ggplot(gs_joined, aes(y = Entr_Mean, x = k6me_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs Horvath Pan-Tissue",
    y = "Entropy Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k6me_horvath))

#Median_Mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.005742,	Adjusted R-squared:  0.003705 
#F-statistic: 2.818 on 1 and 488 DF,  p-value: 0.09382

ggplot(gs_joined, aes(y = Median_Mean, x = k6me_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs Horvath Pan-Tissue",
    y = "Median Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k6me_horvath))

#Gldm_Mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.0004593,	Adjusted R-squared:  -0.001589 
#F-statistic: 0.2242 on 1 and 488 DF,  p-value: 0.636

ggplot(gs_joined, aes(y = Gldm_Mean, x = k6me_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs Horvath Pan-Tissue",
    y = "GLDM Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k6me_horvath))

#SgldASM_Mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.001297,	Adjusted R-squared:  -0.0007491 
#F-statistic: 0.634 on 1 and 488 DF,  p-value: 0.4263


ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k6me_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs Horvath Pan-Tissue",
    y = "SGLD-ASM Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k6me_horvath))

#SgldHOM_Mean vs Horvath Pan-Tissue lm; 
#Multiple R-squared:  0.0002783,	Adjusted R-squared:  -0.00177 
#F-statistic: 0.1359 on 1 and 488 DF,  p-value: 0.7126

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k6me_horvath)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs Horvath Pan-Tissue",
    y = "SGLD-HOM Mean",
    x = "Horvath Pan-Tissue"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k6me_horvath))
dev.off()


#HORVATH SKIN AND BLOOD

dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/HorvathSkinandBlood")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/HorvathSkinandBlood/k5mk_skinblood060625.pdf")

#Entr_mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.008156,	Adjusted R-squared:  0.005573 
#F-statistic: 3.158 on 1 and 384 DF,  p-value: 0.07636

ggplot(gs_joined, aes(y = Entr_Mean, x = k5mk_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs Horvath Skin and Blood",
    y = "Entropy Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k5mk_skinblood))

#Median_Mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.002748,	Adjusted R-squared:  0.0001514 
#F-statistic: 1.058 on 1 and 384 DF,  p-value: 0.3043


ggplot(gs_joined, aes(y = Median_Mean, x = k5mk_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs Horvath Skin and Blood",
    y = "Median Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k5mk_skinblood))

#Gldm_Mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.001262,	Adjusted R-squared:  -0.001339 
#F-statistic: 0.4852 on 1 and 384 DF,  p-value: 0.4865

ggplot(gs_joined, aes(y = Gldm_Mean, x = k5mk_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs Horvath Skin and Blood",
    y = "GLDM Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k5mk_skinblood))

#SgldASM_Mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.01546,	Adjusted R-squared:  0.0129 
#F-statistic:  6.03 on 1 and 384 DF,  p-value: 0.0145

ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k5mk_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs Horvath Skin and Blood",
    y = "SGLD-ASM Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k5mk_skinblood))

#SgldHOM_Mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.006658,	Adjusted R-squared:  0.004071 
#F-statistic: 2.574 on 1 and 384 DF,  p-value: 0.1095

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k5mk_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs Horvath Skin and Blood",
    y = "Median Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k5mk_skinblood))
dev.off()

dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/HorvathSkinandBlood")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/HorvathSkinandBlood/k5me_skinblood060625.pdf")

#Entr_mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.009092,	Adjusted R-squared:  0.007086 
#F-statistic: 4.533 on 1 and 494 DF,  p-value: 0.03374

ggplot(gs_joined, aes(y = Entr_Mean, x = k5me_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs Horvath Skin and Blood",
    y = "Entropy Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k5me_skinblood))

#Median_Mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.001826,	Adjusted R-squared:  -0.000195 
#F-statistic: 0.9035 on 1 and 494 DF,  p-value: 0.3423


ggplot(gs_joined, aes(y = Median_Mean, x = k5me_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs Horvath Skin and Blood",
    y = "Median Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k5me_skinblood))

#Gldm_Mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.001659,	Adjusted R-squared:  -0.0003619 
#F-statistic: 0.8209 on 1 and 494 DF,  p-value: 0.3654

ggplot(gs_joined, aes(y = Gldm_Mean, x = k5me_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs Horvath Skin and Blood",
    y = "GLDM Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k5me_skinblood))

#SgldASM_Mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.00117,	Adjusted R-squared:  -0.0008516 
#F-statistic: 0.5788 on 1 and 494 DF,  p-value: 0.4471

ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k5me_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs Horvath Skin and Blood",
    y = "SGLD-ASM Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k5me_skinblood))

#SgldHOM_Mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  8.699e-07,	Adjusted R-squared:  -0.002023 
#F-statistic: 0.0004297 on 1 and 494 DF,  p-value: 0.9835

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k5me_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs Horvath Skin and Blood",
    y = "Median Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k5me_skinblood))
dev.off()

dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/HorvathSkinandBlood")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/HorvathSkinandBlood/k6mk_skinblood060625.pdf")

#Entr_mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  7.251e-05,	Adjusted R-squared:  -0.002421 
#F-statistic: 0.02908 on 1 and 401 DF,  p-value: 0.8647

ggplot(gs_joined, aes(y = Entr_Mean, x = k6mk_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs Horvath Skin and Blood",
    y = "Entropy Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k6mk_skinblood))

#Median_Mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.001519,	Adjusted R-squared:  -0.0009714 
#F-statistic: 0.6099 on 1 and 401 DF,  p-value: 0.4353


ggplot(gs_joined, aes(y = Median_Mean, x = k6mk_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs Horvath Skin and Blood",
    y = "Median Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k6mk_skinblood))

#Gldm_Mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  6.556e-05,	Adjusted R-squared:  -0.002428 
#F-statistic: 0.02629 on 1 and 401 DF,  p-value: 0.8713

ggplot(gs_joined, aes(y = Gldm_Mean, x = k6mk_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs Horvath Skin and Blood",
    y = "GLDM Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k6mk_skinblood))

#SgldASM_Mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.0003404,	Adjusted R-squared:  -0.002152 
#F-statistic: 0.1366 on 1 and 401 DF,  p-value: 0.7119

ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k6mk_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs Horvath Skin and Blood",
    y = "SGLD-ASM Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k6mk_skinblood))

#SgldHOM_Mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.0004537,	Adjusted R-squared:  -0.002039 
#F-statistic: 0.182 on 1 and 401 DF,  p-value: 0.6699


ggplot(gs_joined, aes(y = SgldHom_Mean, x = k6mk_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs Horvath Skin and Blood",
    y = "Median Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k6mk_skinblood))
dev.off()

dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/HorvathSkinandBlood")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/HorvathSkinandBlood/k6me_skinblood060625.pdf")

#Entr_mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.007167,	Adjusted R-squared:  0.005132 
#F-statistic: 3.523 on 1 and 488 DF,  p-value: 0.06113

ggplot(gs_joined, aes(y = Entr_Mean, x = k6me_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs Horvath Skin and Blood",
    y = "Entropy Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k6me_skinblood))

#Median_Mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.0003882,	Adjusted R-squared:  -0.00166 
#F-statistic: 0.1895 on 1 and 488 DF,  p-value: 0.6635


ggplot(gs_joined, aes(y = Median_Mean, x = k6me_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs Horvath Skin and Blood",
    y = "Median Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k6me_skinblood))

#Gldm_Mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.0003234,	Adjusted R-squared:  -0.001725 
#F-statistic: 0.1579 on 1 and 488 DF,  p-value: 0.6913

ggplot(gs_joined, aes(y = Gldm_Mean, x = k6me_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs Horvath Skin and Blood",
    y = "GLDM Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k6me_skinblood))

#SgldASM_Mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.003601,	Adjusted R-squared:  0.001559 
#F-statistic: 1.764 on 1 and 488 DF,  p-value: 0.1848

ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k6me_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs Horvath Skin and Blood",
    y = "SGLD-ASM Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k6me_skinblood))

#SgldHOM_Mean vs Horvath Skin and Blood lm; 
#Multiple R-squared:  0.0005088,	Adjusted R-squared:  -0.001539 
#F-statistic: 0.2484 on 1 and 488 DF,  p-value: 0.6184

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k6me_skinblood)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs Horvath Skin and Blood",
    y = "Median Mean",
    x = "Horvath Skin and Blood"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k6me_skinblood))
dev.off()


#DNAmPhenoAge (Epigenetic Age of Phenotypic Age)

dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DNAmPhenoAge")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DNAmPhenoAge/k5mk_phenoage060625.pdf")

#Entr_mean vs DNAmPhenoAge lm; 
#Multiple R-squared:  0.004929,	Adjusted R-squared:  0.002338 
#F-statistic: 1.902 on 1 and 384 DF,  p-value: 0.1686

ggplot(gs_joined, aes(y = Entr_Mean, x = k5mk_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs DNAmPhenoAge",
    y = "Entropy Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k5mk_phenoage))

#Median_Mean vs DNAmPhenoAge lm; 
#Multiple R-squared:  0.001701,	Adjusted R-squared:  -0.0008985 
#F-statistic: 0.6544 on 1 and 384 DF,  p-value: 0.4191


ggplot(gs_joined, aes(y = Median_Mean, x = k5mk_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs DNAmPhenoAge",
    y = "Median Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k5mk_phenoage))

#Gldm_Mean vs DNAmPhenoAge lm; 
#Multiple R-squared:  1.928e-05,	Adjusted R-squared:  -0.002585 
#F-statistic: 0.007403 on 1 and 384 DF,  p-value: 0.9315

ggplot(gs_joined, aes(y = Gldm_Mean, x = k5mk_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs DNAmPhenoAge",
    y = "GLDM Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k5mk_phenoage))

#SgldASM_Mean vs DNAmPhenoAged lm; 
#Multiple R-squared:  0.006093,	Adjusted R-squared:  0.003505 
#F-statistic: 2.354 on 1 and 384 DF,  p-value: 0.1258


ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k5mk_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs DNAmPhenoAge",
    y = "SGLD-ASM Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k5mk_phenoage))

#SgldHOM_Mean vs DNAmPhenoAge lm; 
#Multiple R-squared:  0.00375,	Adjusted R-squared:  0.001156 
#F-statistic: 1.446 on 1 and 384 DF,  p-value: 0.23

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k5mk_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs DNAmPhenoAge",
    y = "Median Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k5mk_phenoage))
dev.off()


dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DNAmPhenoAge")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DNAmPhenoAge/k5me_phenoage060625.pdf")

#Entr_mean vs DNAmPhenoAge lm; 
#Multiple R-squared:  0.0005067,	Adjusted R-squared:  -0.001517 
#F-statistic: 0.2504 on 1 and 494 DF,  p-value: 0.617

ggplot(gs_joined, aes(y = Entr_Mean, x = k5me_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs DNAmPhenoAge",
    y = "Entropy Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k5me_phenoage))

#Median_Mean vs DNAmPhenoAge lm; 
#Multiple R-squared:  0.01158,	Adjusted R-squared:  0.009577 
#F-statistic: 5.787 on 1 and 494 DF,  p-value: 0.01652


ggplot(gs_joined, aes(y = Median_Mean, x = k5me_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs DNAmPhenoAge",
    y = "Median Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k5me_phenoage))

#Gldm_Mean vs DNAmPhenoAge lm; 
#Multiple R-squared:  0.0001013,	Adjusted R-squared:  -0.001923 
#F-statistic: 0.05004 on 1 and 494 DF,  p-value: 0.8231

ggplot(gs_joined, aes(y = Gldm_Mean, x = k5me_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs DNAmPhenoAge",
    y = "GLDM Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k5me_phenoage))

#SgldASM_Mean vs DNAmPhenoAged lm; 
#Multiple R-squared:  4.266e-05,	Adjusted R-squared:  -0.001982 
#F-statistic: 0.02108 on 1 and 494 DF,  p-value: 0.8846


ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k5me_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs DNAmPhenoAge",
    y = "SGLD-ASM Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k5me_phenoage))

#SgldHOM_Mean vs DNAmPhenoAge lm; 
#Multiple R-squared:  1.876e-05,	Adjusted R-squared:  -0.002005 
#F-statistic: 0.009269 on 1 and 494 DF,  p-value: 0.9233

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k5me_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs DNAmPhenoAge",
    y = "Median Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k5me_phenoage))
dev.off()


dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DNAmPhenoAge")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DNAmPhenoAge/k6mk_phenoage060625.pdf")

#Entr_mean vs DNAmPhenoAge lm; 
#Multiple R-squared:  0.003085,	Adjusted R-squared:  0.0005985 
#F-statistic: 1.241 on 1 and 401 DF,  p-value: 0.266

ggplot(gs_joined, aes(y = Entr_Mean, x = k6mk_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs DNAmPhenoAge",
    y = "Entropy Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k6mk_phenoage))

#Median_Mean vs DNAmPhenoAge lm; 
#Multiple R-squared:  0.005863,	Adjusted R-squared:  0.003384 
#F-statistic: 2.365 on 1 and 401 DF,  p-value: 0.1249

ggplot(gs_joined, aes(y = Median_Mean, x = k6mk_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs DNAmPhenoAge",
    y = "Median Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k6mk_phenoage))

#Gldm_Mean vs DNAmPhenoAge lm; 
#Multiple R-squared:  0.0007145,	Adjusted R-squared:  -0.001777 
#F-statistic: 0.2867 on 1 and 401 DF,  p-value: 0.5926


ggplot(gs_joined, aes(y = Gldm_Mean, x = k6mk_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs DNAmPhenoAge",
    y = "GLDM Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k6mk_phenoage))

#SgldASM_Mean vs DNAmPhenoAged lm; 
#Multiple R-squared:  0.003035,	Adjusted R-squared:  0.0005489 
#F-statistic: 1.221 on 1 and 401 DF,  p-value: 0.2699


ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k6mk_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs DNAmPhenoAge",
    y = "SGLD-ASM Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k6mk_phenoage))

#SgldHOM_Mean vs DNAmPhenoAge lm; 
#Multiple R-squared:  0.005458,	Adjusted R-squared:  0.002978 
#F-statistic: 2.201 on 1 and 401 DF,  p-value: 0.1387

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k6mk_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs DNAmPhenoAge",
    y = "Median Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k6mk_phenoage))
dev.off()


dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DNAmPhenoAge")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DNAmPhenoAge/k6me_phenoage060625.pdf")

#Entr_mean vs DNAmPhenoAge lm; 
#Multiple R-squared:  0.002055,	Adjusted R-squared:  1.041e-05 
#F-statistic: 1.005 on 1 and 488 DF,  p-value: 0.3166

ggplot(gs_joined, aes(y = Entr_Mean, x = k6me_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs DNAmPhenoAge",
    y = "Entropy Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k6me_phenoage))

#Median_Mean vs DNAmPhenoAge lm; 
#Multiple R-squared:  0.01911,	Adjusted R-squared:  0.0171 
#F-statistic: 9.509 on 1 and 488 DF,  p-value: 0.00216

ggplot(gs_joined, aes(y = Median_Mean, x = k6me_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs DNAmPhenoAge",
    y = "Median Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k6me_phenoage))

#Gldm_Mean vs DNAmPhenoAge lm; 
#Multiple R-squared:  0.000259,	Adjusted R-squared:  -0.00179 
#F-statistic: 0.1264 on 1 and 488 DF,  p-value: 0.7223


ggplot(gs_joined, aes(y = Gldm_Mean, x = k6me_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs DNAmPhenoAge",
    y = "GLDM Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k6me_phenoage))

#SgldASM_Mean vs DNAmPhenoAged lm; 
#Multiple R-squared:  1.568e-06,	Adjusted R-squared:  -0.002048 
#F-statistic: 0.0007649 on 1 and 488 DF,  p-value: 0.9779


ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k6me_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs DNAmPhenoAge",
    y = "SGLD-ASM Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k6me_phenoage))

#SgldHOM_Mean vs DNAmPhenoAge lm; 
#Multiple R-squared:  0.001753,	Adjusted R-squared:  -0.000293 
#F-statistic: 0.8568 on 1 and 488 DF,  p-value: 0.3551

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k6me_phenoage)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs DNAmPhenoAge",
    y = "Median Mean",
    x = "DNAmPhenoAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k6me_phenoage))
dev.off()

#DNAmGrimAge
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DNAmGrimAge")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DNAmGrimAge/k5mk_grim060625.pdf")

#Entr_mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.0007884,	Adjusted R-squared:  -0.001814 
#F-statistic: 0.303 on 1 and 384 DF,  p-value: 0.5823

ggplot(gs_joined, aes(y = Entr_Mean, x = k5mk_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs DNAmGrimAge",
    y = "Entropy Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k5mk_grim))

#Median_Mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.0003772,	Adjusted R-squared:  -0.002226 
#F-statistic: 0.1449 on 1 and 384 DF,  p-value: 0.7037

ggplot(gs_joined, aes(y = Median_Mean, x = k5mk_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs DNAmGrimAge",
    y = "Median Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k5mk_grim))

#Gldm_Mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.0005914,	Adjusted R-squared:  -0.002011 
#F-statistic: 0.2272 on 1 and 384 DF,  p-value: 0.6338


ggplot(gs_joined, aes(y = Gldm_Mean, x = k5mk_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs DNAmGrimAge",
    y = "GLDM Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k5mk_grim))

#SgldASM_Mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.0004617,	Adjusted R-squared:  -0.002141 
#F-statistic: 0.1774 on 1 and 384 DF,  p-value: 0.6739


ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k5mk_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs DNAmGrimAge",
    y = "SGLD-ASM Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k5mk_grim))

#SgldHOM_Mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.005696,	Adjusted R-squared:  0.003106 
#F-statistic:   2.2 on 1 and 384 DF,  p-value: 0.1389

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k5mk_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs DNAmGrimAge",
    y = "Median Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k5mk_grim))
dev.off()


dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DNAmGrimAge")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DNAmGrimAge/k5me_grim060625.pdf")

#Entr_mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.00686,	Adjusted R-squared:  0.00485 
#F-statistic: 3.412 on 1 and 494 DF,  p-value: 0.06531

ggplot(gs_joined, aes(y = Entr_Mean, x = k5me_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs DNAmGrimAge",
    y = "Entropy Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k5me_grim))

#Median_Mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.0003451,	Adjusted R-squared:  -0.001679 
#F-statistic: 0.1705 on 1 and 494 DF,  p-value: 0.6798

ggplot(gs_joined, aes(y = Median_Mean, x = k5me_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs DNAmGrimAge",
    y = "Median Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k5me_grim))

#Gldm_Mean vs DNAmGrimAge lm; 
#Multiple R-squared:  3.474e-05,	Adjusted R-squared:  -0.001989 
#F-statistic: 0.01716 on 1 and 494 DF,  p-value: 0.8958

ggplot(gs_joined, aes(y = Gldm_Mean, x = k5me_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs DNAmGrimAge",
    y = "GLDM Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k5me_grim))

#SgldASM_Mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.006034,	Adjusted R-squared:  0.004022 
#F-statistic: 2.999 on 1 and 494 DF,  p-value: 0.08395


ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k5me_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs DNAmGrimAge",
    y = "SGLD-ASM Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k5me_grim))

#SgldHOM_Mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.005466,	Adjusted R-squared:  0.003453 
#F-statistic: 2.715 on 1 and 494 DF,  p-value: 0.1

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k5me_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs DNAmGrimAge",
    y = "Median Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k5me_grim))
dev.off()

dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DNAmGrimAge")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DNAmGrimAge/k6mk_grim060625.pdf")

#Entr_mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.01051,	Adjusted R-squared:  0.008041 
#F-statistic: 4.259 on 1 and 401 DF,  p-value: 0.0397

ggplot(gs_joined, aes(y = Entr_Mean, x = k6mk_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs DNAmGrimAge",
    y = "Entropy Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k6mk_grim))

#Median_Mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.001835,	Adjusted R-squared:  -0.0006546 
#F-statistic: 0.737 on 1 and 401 DF,  p-value: 0.3911

ggplot(gs_joined, aes(y = Median_Mean, x = k6mk_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs DNAmGrimAge",
    y = "Median Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k6mk_grim))

#Gldm_Mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.002213,	Adjusted R-squared:  -0.0002748 
#F-statistic: 0.8896 on 1 and 401 DF,  p-value: 0.3462

ggplot(gs_joined, aes(y = Gldm_Mean, x = k6mk_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs DNAmGrimAge",
    y = "GLDM Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k6mk_grim))

#SgldASM_Mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.009747,	Adjusted R-squared:  0.007278 
#F-statistic: 3.947 on 1 and 401 DF,  p-value: 0.04763


ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k6mk_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs DNAmGrimAge",
    y = "SGLD-ASM Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k6mk_grim))

#SgldHOM_Mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.004033,	Adjusted R-squared:  0.00155 
#F-statistic: 1.624 on 1 and 401 DF,  p-value: 0.2033

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k6mk_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs DNAmGrimAge",
    y = "Median Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k6mk_grim))
dev.off()

dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DNAmGrimAge")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DNAmGrimAge/k6me_grim060625.pdf")

#Entr_mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.01134,	Adjusted R-squared:  0.009319 
#F-statistic:   5.6 on 1 and 488 DF,  p-value: 0.01835

ggplot(gs_joined, aes(y = Entr_Mean, x = k6me_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs DNAmGrimAge",
    y = "Entropy Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k6me_grim))

#Median_Mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.001167,	Adjusted R-squared:  -0.0008798 
#F-statistic: 0.5702 on 1 and 488 DF,  p-value: 0.4506

ggplot(gs_joined, aes(y = Median_Mean, x = k6me_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs DNAmGrimAge",
    y = "Median Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k6me_grim))

#Gldm_Mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.0005974,	Adjusted R-squared:  -0.001451 
#F-statistic: 0.2917 on 1 and 488 DF,  p-value: 0.5894

ggplot(gs_joined, aes(y = Gldm_Mean, x = k6me_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs DNAmGrimAge",
    y = "GLDM Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k6me_grim))

#SgldASM_Mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.01216,	Adjusted R-squared:  0.01014 
#F-statistic: 6.009 on 1 and 488 DF,  p-value: 0.01458


ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k6me_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs DNAmGrimAge",
    y = "SGLD-ASM Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k6me_grim))

#SgldHOM_Mean vs DNAmGrimAge lm; 
#Multiple R-squared:  0.00898,	Adjusted R-squared:  0.00695 
#F-statistic: 4.422 on 1 and 488 DF,  p-value: 0.03599

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k6me_grim)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs DNAmGrimAge",
    y = "Median Mean",
    x = "DNAmGrimAge"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k6me_grim))
dev.off()


#DunedinPoAm

dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DunedinPoAm")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DunedinPoAm/k5mk_poam38060625.pdf")

#Entr_mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.007985,	Adjusted R-squared:  0.005402 
#F-statistic: 3.091 on 1 and 384 DF,  p-value: 0.07952

ggplot(gs_joined, aes(y = Entr_Mean, x = k5mk_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs DunedinPoAm",
    y = "Entropy Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k5mk_poam38))

#Median_Mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.008456,	Adjusted R-squared:  0.005874 
#F-statistic: 3.275 on 1 and 384 DF,  p-value: 0.07113

ggplot(gs_joined, aes(y = Median_Mean, x = k5mk_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs DunedinPoAm",
    y = "Median Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k5mk_poam38))

#Gldm_Mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.0002985,	Adjusted R-squared:  -0.002305 
#F-statistic: 0.1146 on 1 and 384 DF,  p-value: 0.7351

ggplot(gs_joined, aes(y = Gldm_Mean, x = k5mk_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs DunedinPoAm",
    y = "GLDM Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k5mk_poam38))

#SgldASM_Mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.01144,	Adjusted R-squared:  0.008867 
#F-statistic: 4.444 on 1 and 384 DF,  p-value: 0.03566


ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k5mk_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs DunedinPoAm",
    y = "SGLD-ASM Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k5mk_poam38))

#SgldHOM_Mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.006989,	Adjusted R-squared:  0.004403 
#F-statistic: 2.703 on 1 and 384 DF,  p-value: 0.101

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k5mk_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs DunedinPoAm",
    y = "Median Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k5mk_poam38))
dev.off()

dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DunedinPoAm")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DunedinPoAm/k5me_poam38060625.pdf")

#Entr_mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.0001031,	Adjusted R-squared:  -0.001921 
#F-statistic: 0.05093 on 1 and 494 DF,  p-value: 0.8216

ggplot(gs_joined, aes(y = Entr_Mean, x = k5me_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs DunedinPoAm",
    y = "Entropy Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k5me_poam38))

#Median_Mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.007267,	Adjusted R-squared:  0.005258 
#F-statistic: 3.616 on 1 and 494 DF,  p-value: 0.0578

ggplot(gs_joined, aes(y = Median_Mean, x = k5me_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs DunedinPoAm",
    y = "Median Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k5me_poam38))

#Gldm_Mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.0001332,	Adjusted R-squared:  -0.001891 
#F-statistic: 0.06583 on 1 and 494 DF,  p-value: 0.7976

ggplot(gs_joined, aes(y = Gldm_Mean, x = k5me_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs DunedinPoAm",
    y = "GLDM Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k5me_poam38))

#SgldASM_Mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.0004794,	Adjusted R-squared:  -0.001544 
#F-statistic: 0.2369 on 1 and 494 DF,  p-value: 0.6266


ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k5me_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs DunedinPoAm",
    y = "SGLD-ASM Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k5me_poam38))

#SgldHOM_Mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.001151,	Adjusted R-squared:  -0.0008709 
#F-statistic: 0.5693 on 1 and 494 DF,  p-value: 0.4509

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k5me_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs DunedinPoAm",
    y = "Median Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k5me_poam38))
dev.off()

dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DunedinPoAm")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DunedinPoAm/k6mk_poam38060625.pdf")

#Entr_mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.001569,	Adjusted R-squared:  -0.0009211 
#F-statistic:  0.63 on 1 and 401 DF,  p-value: 0.4278

ggplot(gs_joined, aes(y = Entr_Mean, x = k6mk_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs DunedinPoAm",
    y = "Entropy Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k6mk_poam38))

#Median_Mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.01271,	Adjusted R-squared:  0.01025 
#F-statistic: 5.162 on 1 and 401 DF,  p-value: 0.02362

ggplot(gs_joined, aes(y = Median_Mean, x = k6mk_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs DunedinPoAm",
    y = "Median Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k6mk_poam38))

#Gldm_Mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.0004094,	Adjusted R-squared:  -0.002083 
#F-statistic: 0.1642 on 1 and 401 DF,  p-value: 0.6855

ggplot(gs_joined, aes(y = Gldm_Mean, x = k6mk_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs DunedinPoAm",
    y = "GLDM Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k6mk_poam38))

#SgldASM_Mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.006761,	Adjusted R-squared:  0.004284 
#F-statistic:  2.73 on 1 and 401 DF,  p-value: 0.09928


ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k6mk_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs DunedinPoAm",
    y = "SGLD-ASM Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k6mk_poam38))

#SgldHOM_Mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.006673,	Adjusted R-squared:  0.004195 
#F-statistic: 2.694 on 1 and 401 DF,  p-value: 0.1015

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k6mk_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs DunedinPoAm",
    y = "Median Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k6mk_poam38))
dev.off()

dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DunedinPoAm")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DunedinPoAm/k6me_poam38060625.pdf")

#Entr_mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.002064,	Adjusted R-squared:  1.884e-05 
#F-statistic: 1.009 on 1 and 488 DF,  p-value: 0.3156

ggplot(gs_joined, aes(y = Entr_Mean, x = k6me_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs DunedinPoAm",
    y = "Entropy Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k6me_poam38))

#Median_Mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.01641,	Adjusted R-squared:  0.0144 
#F-statistic: 8.143 on 1 and 488 DF,  p-value: 0.004506

ggplot(gs_joined, aes(y = Median_Mean, x = k6me_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs DunedinPoAm",
    y = "Median Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k6me_poam38))

#Gldm_Mean vs DunedinPoAm lm; 
#Multiple R-squared:  5.597e-07,	Adjusted R-squared:  -0.002049 
#F-statistic: 0.0002731 on 1 and 488 DF,  p-value: 0.9868

ggplot(gs_joined, aes(y = Gldm_Mean, x = k6me_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs DunedinPoAm",
    y = "GLDM Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k6me_poam38))

#SgldASM_Mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.0001619,	Adjusted R-squared:  -0.001887 
#F-statistic: 0.07903 on 1 and 488 DF,  p-value: 0.7787

ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k6me_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs DunedinPoAm",
    y = "SGLD-ASM Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k6me_poam38))

#SgldHOM_Mean vs DunedinPoAm lm; 
#Multiple R-squared:  0.0004279,	Adjusted R-squared:  -0.00162 
#F-statistic: 0.2089 on 1 and 488 DF,  p-value: 0.6478

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k6me_poam38)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs DunedinPoAm",
    y = "Median Mean",
    x = "DunedinPoAm"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k6me_poam38))
dev.off()


#DunedinPACE

dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DunedinPACE")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DunedinPACE/k5mk_poam45060625.pdf")

#Entr_mean vs DunedinPACE lm; 
#MMultiple R-squared:  0.003211,	Adjusted R-squared:  0.0006152 
#F-statistic: 1.237 on 1 and 384 DF,  p-value: 0.2667

ggplot(gs_joined, aes(y = Entr_Mean, x = k5mk_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs DunedinPACE",
    y = "Entropy Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k5mk_poam45))

#Median_Mean vs DunedinPACE lm; 
#Multiple R-squared:  5.265e-06,	Adjusted R-squared:  -0.002599 
#F-statistic: 0.002022 on 1 and 384 DF,  p-value: 0.9642

ggplot(gs_joined, aes(y = Median_Mean, x = k5mk_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs DunedinPACE",
    y = "Median Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k5mk_poam45))

#Gldm_Mean vs DunedinPACE lm; 
#Multiple R-squared:  0.0002293,	Adjusted R-squared:  -0.002374 
#F-statistic: 0.08807 on 1 and 384 DF,  p-value: 0.7668

ggplot(gs_joined, aes(y = Gldm_Mean, x = k5mk_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs DunedinPACE",
    y = "GLDM Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k5mk_poam45))

#SgldASM_Mean vs DunedinPACE lm; 
#Multiple R-squared:  0.001071,	Adjusted R-squared:  -0.001531 
#F-statistic: 0.4116 on 1 and 384 DF,  p-value: 0.5215

ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k5mk_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs DunedinPACE",
    y = "SGLD-ASM Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k5mk_poam45))

#SgldHOM_Mean vs DunedinPACE lm; 
#Multiple R-squared:  0.004698,	Adjusted R-squared:  0.002106 
#F-statistic: 1.812 on 1 and 384 DF,  p-value: 0.179

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k5mk_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs DunedinPACE",
    y = "Median Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k5mk_poam45))
dev.off()


dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DunedinPACE")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DunedinPACE/k5me_poam45060625.pdf")

#Entr_mean vs DunedinPACE lm; 
#Multiple R-squared:  0.000106,	Adjusted R-squared:  -0.001918 
#F-statistic: 0.05237 on 1 and 494 DF,  p-value: 0.8191

ggplot(gs_joined, aes(y = Entr_Mean, x = k5me_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs DunedinPACE",
    y = "Entropy Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k5me_poam45))

#Median_Mean vs DunedinPACE lm; 
#Multiple R-squared:  0.007367,	Adjusted R-squared:  0.005358 
#F-statistic: 3.666 on 1 and 494 DF,  p-value: 0.0561

ggplot(gs_joined, aes(y = Median_Mean, x = k5me_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs DunedinPACE",
    y = "Median Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k5me_poam45))

#Gldm_Mean vs DunedinPACE lm; 
#Multiple R-squared:  3.026e-05,	Adjusted R-squared:  -0.001994 
#F-statistic: 0.01495 on 1 and 494 DF,  p-value: 0.9027

ggplot(gs_joined, aes(y = Gldm_Mean, x = k5me_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs DunedinPACE",
    y = "GLDM Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k5me_poam45))

#SgldASM_Mean vs DunedinPACE lm; 
#Multiple R-squared:  0.001135,	Adjusted R-squared:  -0.000887 
#F-statistic: 0.5613 on 1 and 494 DF,  p-value: 0.4541

ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k5me_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs DunedinPACE",
    y = "SGLD-ASM Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k5me_poam45))

#SgldHOM_Mean vs DunedinPACE lm; 
#Multiple R-squared:  0.0004021,	Adjusted R-squared:  -0.001621 
#F-statistic: 0.1987 on 1 and 494 DF,  p-value: 0.656

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k5me_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs DunedinPACE",
    y = "Median Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k5me_poam45))
dev.off()


dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DunedinPACE")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DunedinPACE/k6mk_poam45060625.pdf")

#Entr_mean vs DunedinPACE lm; 
#Multiple R-squared:  0.01432,	Adjusted R-squared:  0.01187 
#F-statistic: 5.827 on 1 and 401 DF,  p-value: 0.01623

ggplot(gs_joined, aes(y = Entr_Mean, x = k6mk_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs DunedinPACE",
    y = "Entropy Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k6mk_poam45))

#Median_Mean vs DunedinPACE lm; 
#Multiple R-squared:  0.005084,	Adjusted R-squared:  0.002603 
#F-statistic: 2.049 on 1 and 401 DF,  p-value: 0.1531

ggplot(gs_joined, aes(y = Median_Mean, x = k6mk_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs DunedinPACE",
    y = "Median Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k6mk_poam45))

#Gldm_Mean vs DunedinPACE lm; 
#Multiple R-squared:  0.0002342,	Adjusted R-squared:  -0.002259 
#F-statistic: 0.09393 on 1 and 401 DF,  p-value: 0.7594

ggplot(gs_joined, aes(y = Gldm_Mean, x = k6mk_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs DunedinPACE",
    y = "GLDM Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k6mk_poam45))

#SgldASM_Mean vs DunedinPACE lm; 
#Multiple R-squared:  0.01351,	Adjusted R-squared:  0.01105 
#F-statistic: 5.492 on 1 and 401 DF,  p-value: 0.0196

ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k6mk_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs DunedinPACE",
    y = "SGLD-ASM Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k6mk_poam45))

#SgldHOM_Mean vs DunedinPACE lm; 
#Multiple R-squared:  0.01199,	Adjusted R-squared:  0.009529 
#F-statistic: 4.868 on 1 and 401 DF,  p-value: 0.02793

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k6mk_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs DunedinPACE",
    y = "Median Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k6mk_poam45))
dev.off()

dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DunedinPACE")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/DNAmscatterplotsummaries/DunedinPACE/k6me_poam45060625.pdf")

#Entr_mean vs DunedinPACE lm; 
#Multiple R-squared:  0.001068,	Adjusted R-squared:  -0.0009788 
#F-statistic: 0.5218 on 1 and 488 DF,  p-value: 0.4704

ggplot(gs_joined, aes(y = Entr_Mean, x = k6me_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs DunedinPACE",
    y = "Entropy Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Entr_Mean ~ gs_joined$k6me_poam45))

#Median_Mean vs DunedinPACE lm; 
#Multiple R-squared:  0.008868,	Adjusted R-squared:  0.006837 
#F-statistic: 4.366 on 1 and 488 DF,  p-value: 0.03717

ggplot(gs_joined, aes(y = Median_Mean, x = k6me_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Median Mean vs DunedinPACE",
    y = "Median Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Median_Mean ~ gs_joined$k6me_poam45))

#Gldm_Mean vs DunedinPACE lm; 
#Multiple R-squared:  0.0007168,	Adjusted R-squared:  -0.001331 
#F-statistic:  0.35 on 1 and 488 DF,  p-value: 0.5544

ggplot(gs_joined, aes(y = Gldm_Mean, x = k6me_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "GLDM Mean vs DunedinPACE",
    y = "GLDM Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$Gldm_Mean ~ gs_joined$k6me_poam45))

#SgldASM_Mean vs DunedinPACE lm; 
#Multiple R-squared:  0.003682,	Adjusted R-squared:  0.001641 
#F-statistic: 1.804 on 1 and 488 DF,  p-value: 0.1799

ggplot(gs_joined, aes(y = SgldAsm_Mean, x = k6me_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-ASM Mean vs DunedinPACE",
    y = "SGLD-ASM Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldAsm_Mean ~ gs_joined$k6me_poam45))

#SgldHOM_Mean vs DunedinPACE lm; 
#Multiple R-squared:  0.001129,	Adjusted R-squared:  -0.000918 
#F-statistic: 0.5515 on 1 and 488 DF,  p-value: 0.4581

ggplot(gs_joined, aes(y = SgldHom_Mean, x = k6me_poam45)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "SGLD-HOM Mean vs DunedinPACE",
    y = "Median Mean",
    x = "DunedinPACE"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_joined$SgldHom_Mean ~ gs_joined$k6me_poam45))
dev.off()

setwd("/drives/drive1/ff_chaya/Angel/bin/")
#read.csv("gs_joined.csv")
gs_joined <- read.csv("gs_joined.csv")

library(tibble)

install.packages('pheatmap',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("pheatmap")

install.packages('rbind',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("rbind")

install.packages('data.table',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("data.table")

install.packages('dplyr',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("dplyr")

install.packages('gtable',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("gtable")

install.packages('broom',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("broom")


le8_with_sleep <- read.csv("/drives/drive1/ff_chaya/original_data/CVH_scoreOnly.csv")

gs_newjoined <- left_join (gs_joined, le8_with_sleep, by = "CVID")

gs_newjoined <- read.csv("gs_newjoined.csv")

write.csv(gs_newjoined, "gs_newjoined.csv")

#List of Additional LE8 performed: 
#diet_le8.y, pa_le8.y, smoker_le8.y, 
#pa_le8.y, glu_le8.y, bp_le8.y, nonhdl_le8.y, 
#bmi_le8.y, le8_w_slp, slp_le8

#Make a Table of Multiple R-squared, Adjusted R-squared, F-statistic, and p-value
#grayscale vs LE8 (GS); DNAmage vs grayscale(GS_Joined)
Le8s<- c("diet_le8.y", "pa_le8.y", "smoker_le8.y", 
         "pa_le8.y", "glu_le8.y", "bp_le8.y", "nonhdl_le8.y", "bmi_le8.y", 
         "le8_w_slp", "slp_le8")
features <- c( "Entr_Mean", "Median_Mean", "Gldm_Mean", "SgldAsm_Mean", "SgldHom_Mean")
DNAmages <- c("k5mk_horvath", "k5me_horvath", "k6mk_horvath", "k6me_horvath", "k5mk_skinblood", 
              "k5me_skinblood", "k6mk_skinblood", "k6me_skinblood", "k5mk_phenoage", "k5me_phenoage", 
              "k6mk_phenoage", "k6me_phenoage", "k6me_phenoage", "k5mk_grim", "k5me_grim", "k6mk_grim", 
              "k6me_grim", "k5mk_poam38", "k5me_poam38", "k6mk_poam38", "k6me_poam38", "k5mk_poam45", 
              "k5me_poam45", "k5me_poam45", "k6mk_poam45", "k6me_poam45")
results_list <- list()

fit1 <- lm(Entr_Mean~le8_no_slp_score, gs_joined)
fit2 <- lm(Median_Mean~le8_no_slp_score, gs_joined)
fit3 <- lm(Gldm_Mean~le8_no_slp_score, gs_joined)
fit4 <- lm(SgldAsm_Mean~le8_no_slp_score, gs_joined)
fit5 <- lm(SgldHom_Mean~le8_no_slp_score, gs_joined)

lm_Le8subscoresvsEntr_Mean <- list()
lm_Le8subscoresvsMedian_Mean<- list()
lm_Le8subscoresvsGldm_Mean <- list()
lm_Le8subscoresvsSgldAsm_Mean <- list()
lm_Le8subscoresvsSgldHom_Mean <- list()
#LE8 and GRAYSCALE FEATURES 
for (Le8 in Le8s) {
  #lm for Entr_Mean
  lm_Le8subscoresvsEntr_Mean[[Le8]]<- lm(as.formula(paste(Le8, " ~ Entr_Mean")), data = gs_newjoined)
  summary(lm_Le8subscoresvsEntr_Mean)
  
  #lm for Median_Mean
  lm_Le8subscoresvsMedian_Mean[[Le8]]<- lm(as.formula(paste(Le8, " ~ Median_Mean")), data = gs_newjoined)
  summary(lm_Le8subscoresvsMedian_Mean)
  
  #lm for Gldm_Mean
  lm_Le8subscoresvsGldm_Mean[[Le8]]<- lm(as.formula(paste(Le8, " ~ Gldm_Mean")), data = gs_newjoined)
  summary(lm_Le8subscoresvsGldm_Mean)
  
  #lm for SgldAsm_Mean
  lm_Le8subscoresvsSgldAsm_Mean[[Le8]]<- lm(as.formula(paste(Le8, " ~ SgldAsm_Mean")), data = gs_newjoined)
  summary(lm_Le8subscoresvsSgldAsm_Mean)
  
  #lm for SgldHom_Mean
  lm_Le8subscoresvsSgldHom_Mean[[Le8]]<- lm(as.formula(paste(Le8, " ~ SgldHom_Mean")), data = gs_newjoined)
  summary(lm_Le8subscoresvsSgldHom_Mean)
  
  
}


lm_DNAmvsEntr_Mean <- list()
lm_DNAmvsMedian_Mean<- list()
lm_DNAmvsGldm_Mean <- list()
lm_DNAmvsSgldAsm_Mean <- list()
lm_DNAmvsSgldHom_Mean <- list()
for (DNAmage in DNAmages) {
  #lm for Entr_Mean
  lm_DNAmvsEntr_Mean[[DNAmage]]<- lm(as.formula(paste(DNAmage, " ~ Entr_Mean")), data = gs_joined)
  summary(lm_DNAmvsEntr_Mean)
  
  #lm for Median_Mean
  lm_DNAmvsMedian_Mean[[DNAmage]]<- lm(as.formula(paste(DNAmage, " ~ Median_Mean")), data = gs_joined)
  summary(lm_DNAmvsMedian_Mean)
  
  #lm for Gldm_Mean
  lm_DNAmvsGldm_Mean[[DNAmage]]<- lm(as.formula(paste(DNAmage, " ~ Gldm_Mean")), data = gs_joined)
  summary(lm_DNAmvsGldm_Mean)
  
  #lm for SgldAsm_Mean
  lm_DNAmvsSgldAsm_Mean[[DNAmage]]<- lm(as.formula(paste(DNAmage, " ~ SgldAsm_Mean")), data = gs_joined)
  summary(lm_DNAmvsSgldAsm_Mean)
  
  #lm for SgldHom_Mean
  lm_DNAmvsSgldHom_Mean[[DNAmage]]<- lm(as.formula(paste(DNAmage, " ~ SgldHom_Mean")), data = gs_joined)
  summary(lm_DNAmvsSgldHom_Mean)
  
  
  
  #Store tidy summaries
  results_list [[DNAmage]] <- list (
    LM_Entr_Mean = tidy(lm_DNAmvsEntr_Mean), 
    LM_Median_Mean = tidy(lm_DNAmvsMedian_Mean), 
    LM_Gldm_Mean = tidy(lm_DNAmvsGldm_Mean), 
    LM_SgldAsm_Mean = tidy(lm_DNAmvsSgldAsm_Mean), 
    LM_SgldHom_Mean = tidy(lm_DNAmvsSgldHom_Mean), 
    LM_Entr_Mean = glance(lm_DNAmvsEntr_Mean), 
    LM_Median_Mean = glance(lm_DNAmvsMedian_Mean), 
    LM_Gldm_Mean = glance(lm_DNAmvsGldm_Mean), 
    LM_SgldAsm_Mean = glance(lm_DNAmvsSgldAsm_Mean), 
    LM_SgldHom_Mean = glance(lm_DNAmvsSgldHom_Mean) 
  )
  
}



print(results_list)

linearmodelslist <- list(fit1,fit2, fit3, fit4, fit5) 
#lm_DNAmvsEntr_Mean, lm_DNAmvsMedian_Mean, lm_DNAmvsGldm_Mean, 
#lm_DNAmvsSgldAsm_Mean, lm_DNAmvsSgldHom_Mean)


model_summaries <- lapply(linearmodelslist, function(mod){
  summary_mod<- summary(mod)
  data.frame( 
    Model = deparse(formula(mod)), 
    R_squared = summary_mod$r.squared, 
    Adj_R_Squared = summary_mod$adj.r.squared, 
    F_Statistic = summary_mod$fstatistic[1], 
    p_value = pf (summary_mod$fstatistic[1], 
                  summary_mod$fstatistic[2], 
                  summary_mod$fstatistic[3], 
                  lower.tail = F)
  )
})

model_summaries1 <- lapply(lm_DNAmvsEntr_Mean, function(mod){
  summary_mod<- summary(mod)
  data.frame( 
    Model = deparse(formula(mod)), 
    R_squared = summary_mod$r.squared, 
    Adj_R_Squared = summary_mod$adj.r.squared, 
    F_Statistic = summary_mod$fstatistic[1], 
    p_value = pf (summary_mod$fstatistic[1], 
                  summary_mod$fstatistic[2], 
                  summary_mod$fstatistic[3], 
                  lower.tail = F)
  )
})

model_summaries2 <- lapply(lm_DNAmvsMedian_Mean, function(mod){
  summary_mod<- summary(mod)
  data.frame( 
    Model = deparse(formula(mod)), 
    R_squared = summary_mod$r.squared, 
    Adj_R_Squared = summary_mod$adj.r.squared, 
    F_Statistic = summary_mod$fstatistic[1], 
    p_value = pf (summary_mod$fstatistic[1], 
                  summary_mod$fstatistic[2], 
                  summary_mod$fstatistic[3], 
                  lower.tail = F)
  )
})

model_summaries3 <- lapply(lm_DNAmvsGldm_Mean, function(mod){
  summary_mod<- summary(mod)
  data.frame( 
    Model = deparse(formula(mod)), 
    R_squared = summary_mod$r.squared, 
    Adj_R_Squared = summary_mod$adj.r.squared, 
    F_Statistic = summary_mod$fstatistic[1], 
    p_value = pf (summary_mod$fstatistic[1], 
                  summary_mod$fstatistic[2], 
                  summary_mod$fstatistic[3], 
                  lower.tail = F)
  )
})


model_summaries4 <- lapply(lm_DNAmvsSgldAsm_Mean, function(mod){
  summary_mod<- summary(mod)
  data.frame( 
    Model = deparse(formula(mod)), 
    R_squared = summary_mod$r.squared, 
    Adj_R_Squared = summary_mod$adj.r.squared, 
    F_Statistic = summary_mod$fstatistic[1], 
    p_value = pf (summary_mod$fstatistic[1], 
                  summary_mod$fstatistic[2], 
                  summary_mod$fstatistic[3], 
                  lower.tail = F)
  )
})

model_summaries5 <- lapply(lm_DNAmvsSgldHom_Mean, function(mod){
  summary_mod<- summary(mod)
  data.frame( 
    Model = deparse(formula(mod)), 
    R_squared = summary_mod$r.squared, 
    Adj_R_Squared = summary_mod$adj.r.squared, 
    F_Statistic = summary_mod$fstatistic[1], 
    p_value = pf (summary_mod$fstatistic[1], 
                  summary_mod$fstatistic[2], 
                  summary_mod$fstatistic[3], 
                  lower.tail = F)
  )
})

#Addition of LM LE8 Subscores
#lm_Le8subscoresvsEntr_Mean <- list()
#lm_Le8subscoresvsMedian_Mean<- list()
#lm_Le8subscoresvsGldm_Mean <- list()
#lm_Le8subscoresvsSgldAsm_Mean <- list()
#lm_Le8subscoresvsSgldHom_Mean <- list()
model_summaries6 <- lapply(lm_Le8subscoresvsEntr_Mean, function(mod){
  summary_mod<- summary(mod)
  data.frame( 
    Model = deparse(formula(mod)), 
    R_squared = summary_mod$r.squared, 
    Adj_R_Squared = summary_mod$adj.r.squared, 
    F_Statistic = summary_mod$fstatistic[1], 
    p_value = pf (summary_mod$fstatistic[1], 
                  summary_mod$fstatistic[2], 
                  summary_mod$fstatistic[3], 
                  lower.tail = F)
  )
})

model_summaries7 <- lapply(lm_Le8subscoresvsMedian_Mean, function(mod){
  summary_mod<- summary(mod)
  data.frame( 
    Model = deparse(formula(mod)), 
    R_squared = summary_mod$r.squared, 
    Adj_R_Squared = summary_mod$adj.r.squared, 
    F_Statistic = summary_mod$fstatistic[1], 
    p_value = pf (summary_mod$fstatistic[1], 
                  summary_mod$fstatistic[2], 
                  summary_mod$fstatistic[3], 
                  lower.tail = F)
  )
})

model_summaries8 <- lapply(lm_Le8subscoresvsGldm_Mean, function(mod){
  summary_mod<- summary(mod)
  data.frame( 
    Model = deparse(formula(mod)), 
    R_squared = summary_mod$r.squared, 
    Adj_R_Squared = summary_mod$adj.r.squared, 
    F_Statistic = summary_mod$fstatistic[1], 
    p_value = pf (summary_mod$fstatistic[1], 
                  summary_mod$fstatistic[2], 
                  summary_mod$fstatistic[3], 
                  lower.tail = F)
  )
})


model_summaries9 <- lapply(lm_Le8subscoresvsSgldAsm_Mean, function(mod){
  summary_mod<- summary(mod)
  data.frame( 
    Model = deparse(formula(mod)), 
    R_squared = summary_mod$r.squared, 
    Adj_R_Squared = summary_mod$adj.r.squared, 
    F_Statistic = summary_mod$fstatistic[1], 
    p_value = pf (summary_mod$fstatistic[1], 
                  summary_mod$fstatistic[2], 
                  summary_mod$fstatistic[3], 
                  lower.tail = F)
  )
})

model_summaries10 <- lapply(lm_Le8subscoresvsSgldHom_Mean, function(mod){
  summary_mod<- summary(mod)
  data.frame( 
    Model = deparse(formula(mod)), 
    R_squared = summary_mod$r.squared, 
    Adj_R_Squared = summary_mod$adj.r.squared, 
    F_Statistic = summary_mod$fstatistic[1], 
    p_value = pf (summary_mod$fstatistic[1], 
                  summary_mod$fstatistic[2], 
                  summary_mod$fstatistic[3], 
                  lower.tail = F)
  )
})

#LE8 vs Grayscale
results_df <- do.call(rbind, model_summaries)
#lm_DNAmvsEntr_Mean
results_df1 <- do.call(rbind, model_summaries1)
#lm_DNAmvsMedian_Mean
results_df2 <- do.call(rbind, model_summaries2)
#lm_DNAmvsGldm_Mean
results_df3 <- do.call(rbind, model_summaries3)
#lm_DNAmvsSgldAsm_Mean
results_df4 <- do.call(rbind, model_summaries4)
#lm_DNAmvsSgldHom_Mean
results_df5 <- do.call(rbind, model_summaries5)
#lm_Le8subscoresvsEntr_Mean
results_df6 <- do.call(rbind, model_summaries6)
#lm_Le8subscoresvsMedian_Mean
results_df7 <- do.call(rbind, model_summaries7)
#lm_Le8subscoresvsGldm_Mean
results_df8 <- do.call(rbind, model_summaries8)
#lm_Le8subscoresvsSgldAsm_Mean
results_df9 <- do.call(rbind, model_summaries9)
#lm_Le8subscoresvsSgldHom_Mean
results_df10 <- do.call(rbind, model_summaries10)

#Combine Datasets
comb1 <- combine(results_df, results_df1)
comb2 <- combine(results_df2, results_df3)
comb3 <- combine(results_df4, results_df5)
comb4 <- combine(comb1,comb2)
comb8<- combine(comb4, comb3)
comb5 <- combine(results_df6, results_df7)
comb6 <- combine(results_df8, results_df9)
comb7 <- combine(comb5, comb6)
comb9<- combine(comb8, comb7)
combfinal <- combine(comb9, results_df10)

#This is the total dataset of all the Rsquared, 
#Adjusted Rsquared, Fstatistic, and pvalues in 
#table format for each linear model. 
results_total <- combine(comb4, comb3)


#Save as CSV
write.csv(results_total, "results_total.csv")
#Pick out p-value -02 or less
#Any notable conclusions

#MAKE A HEATMAP TO COMPARE DNAmAge (pheatmap)
#Grayscale vs LE8 + DNAmAge

library(pheatmap)
library(RColorBrewer)
library(dplyr)
library(corrplot)

#Define Column Groups
features <- c( "Entr_Mean", "Median_Mean", "Gldm_Mean", "SgldAsm_Mean", "SgldHom_Mean")
DNAmages <- c("k5mk_horvath", "k5me_horvath", "k6mk_horvath", "k6me_horvath", "k5mk_skinblood", 
              "k5me_skinblood", "k6mk_skinblood", "k6me_skinblood", "k5mk_phenoage", "k5me_phenoage", 
              "k6mk_phenoage", "k6me_phenoage", "k5mk_grim", "k5me_grim", "k6mk_grim", 
              "k6me_grim", "k5mk_poam38", "k5me_poam38", "k6mk_poam38", "k6me_poam38", "k5mk_poam45", 
              "k5me_poam45", "k6mk_poam45", "k6me_poam45")
LE8col <- c("le8_no_slp_score")
covariates<- c("ff_cm1bsex", "ff_cm1povca", "ck7bmi", "ff_cm1ethrace")

covariatescontinuous <- c("ck7bmi")
#Combine Selected Columns
selected_cols <- c(features, DNAmages, LE8col)
data_subset <- gs_joined[, selected_cols]

#combine selected columns with covariates
selected_cols1 <- c(features, DNAmages, LE8col, covariatescontinuous)
data_subset1 <- gs_joined [, selected_cols1]

#Compute correlation matrix (Pearson)
cor_matrix <- cor(data_subset, use = "pairwise.complete.obs", method = "pearson")

#Compute correlation matrix with covariates
cor_matrix1 <- cor(data_subset1, use = "pairwise.complete.obs", method = "pearson")

#Basic Heatmap with Grayscale, Methylation Age, and LE8
pheatmap(cor_matrix, 
         color = colorRampPalette(c("blue", "white", "red"))(100), 
         cluster_rows = T,
         cluster_cols = T,
         main = "Correlation Heatmap: Grayscale, Methylation Age, and LE8")

#Basic Heatmap with all including covariates
pheatmap(cor_matrix1, 
         color = colorRampPalette(c("blue", "white", "red"))(100), 
         cluster_rows = T,
         cluster_cols = T,
         main = "Correlation Heatmap: Grayscale, Methylation Age, LE8, BMI")


#Heatmap to compare only Grayscale features vs methylation + LE8
cor_focus <- cor(data_subset[features], data_subset[c(DNAmages, LE8col)],
                 use = "pairwise.complete.obs", method = "pearson")

#Heatmap of just that sub-matrix
pheatmap(cor_focus, 
         cluster_rows = T,
         cluster_cols = T,
         color = colorRampPalette(c("blue", "white", "red"))(100), 
         main = "Grayscale Features vs Methylation Clocks & LE8")


#Analysis LE8 with sleep (overall)
#SCATTERPLOT SUMMARIES OF THE RELATIONSHIPS BETWEEN THE LE8 w/ sleep SCORES AND GRAYSCALE FEATURES. 
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/LE8scatterplots_w_sleepummaries.070925.pdf")

#Entr_mean vs LE8 scores lm; 
#This is significantly different but the effect size is small and negatively associated; left is y and right is x.

ggplot(gs_newjoined, aes(x = Entr_Mean, y = le8_w_slp)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+
  labs(
    title = "Entropy Mean vs LE8 Scores",
    y = "LE8 Scores",
    x = "Entropy Mean"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  ) 

summary (lm (formula = gs_newjoined$Entr_Mean ~ gs_newjoined$le8_w_slp))

#Median_Mean vs LE8 scores lm 
#Multiple R-squared:  0.07982,	Adjusted R-squared:  0.07903 
#F-statistic: 101.2 on 1 and 1167 DF,  p-value: < 2.2e-16
ggplot(gs_newjoined, aes(x = Median_Mean, y = le8_w_slp)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+ 
  labs(
    title = "Median Mean vs LE8 Scores",
    y = "LE8 Scores",
    x = "Median Mean"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  )

summary (lm (formula = gs_newjoined$Median_Mean ~ gs_newjoined$le8_w_slp))

#Gldm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0005928,	Adjusted R-squared:  -0.0002636 
#F-statistic: 0.6923 on 1 and 1167 DF,  p-value: 0.4056
ggplot(gs_newjoined, aes(x = Gldm_Mean, y = le8_w_slp)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+ 
  labs(
    title = "GLDM Mean vs LE8 Scores",
    y = "LE8 Scores",
    x = "GLDM Mean"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  )

summary (lm (formula = gs_newjoined$Gldm_Mean ~ gs_newjoined$le8_w_slp))

#SgldAsm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.002415,	Adjusted R-squared:  0.00156 
#F-statistic: 2.825 on 1 and 1167 DF,  p-value: 0.09308

ggplot(gs_newjoined, aes(x = SgldAsm_Mean, y = le8_w_slp)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+ 
  labs(
    title = "SGLD-ASM Mean vs LE8 Scores",
    y = "LE8 Scores",
    x = "SGLD-ASM Mean"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  )

summary (lm (formula = gs_newjoined$SgldAsm_Mean ~ gs_newjoined$le8_w_slp))

#SgldHom_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0003411,	Adjusted R-squared:  -0.0005155 
#F-statistic: 0.3982 on 1 and 1167 DF,  p-value: 0.5281

ggplot(gs_newjoined, aes(x = SgldHom_Mean, y = le8_w_slp)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+ 
  labs(
    title = "SGLD-HOM Mean vs LE8 Scores",
    y = "LE8 Scores",
    x = "SGLD-HOM Mean"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  )

summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$le8_w_slp))

dev.off()

library(ggplot2)
install.packages('ggsignif',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library(ggsignif)


#Analysis LE8 subscores
#BMI_LE8
#SCATTERPLOT SUMMARIES OF THE RELATIONSHIPS BETWEEN THE BMI_LE8 SCORES AND GRAYSCALE FEATURES. 
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/LE8scatterplotBMIsummaries.070925.pdf")

#Entr_mean vs LE8 scores lm; 
#This is significantly different but the effect size is small and negatively associated; left is y and right is x.

boxplot(Entr_Mean ~ bmi_le8.y, data = gs_newjoined, 
        main = "Association between Average Vascular Wall Entropy and BMI Subscore of LE8", 
        xlab = "BMI Subscore of LE8",
        ylab = "Average Vascular Wall Entropy")
bmile8 <- lm(Entr_Mean ~ bmi_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bmi_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bmi_le8.y = x)), col = "red", lty = 8) 
abline(bmile8, col = "red")



#Median_Mean vs LE8 scores lm 
#Multiple R-squared:  0.07982,	Adjusted R-squared:  0.07903 
#F-statistic: 101.2 on 1 and 1167 DF,  p-value: < 2.2e-16
boxplot(Median_Mean ~ bmi_le8.y, data = gs_newjoined, 
        main = "Association between Grayscale Median and BMI Subscore of LE8", 
        xlab = "BMI Subscore of LE8",
        ylab = "Grayscale Median")
bmile8 <- lm(Median_Mean ~ bmi_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bmi_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bmi_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Median_Mean ~ gs_newjoined$bmi_le8.y))

#Gldm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0005928,	Adjusted R-squared:  -0.0002636 
#F-statistic: 0.6923 on 1 and 1167 DF,  p-value: 0.4056
boxplot(Gldm_Mean ~ bmi_le8.y, data = gs_newjoined, 
        main = "Association between GLDM and BMI Subscore of LE8", 
        xlab = "BMI Subscore of LE8",
        ylab = "Average GLDM")
bmile8 <- lm(Gldm_Mean ~ bmi_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bmi_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bmi_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Gldm_Mean ~ gs_newjoined$bmi_le8.y))

#SgldAsm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.002415,	Adjusted R-squared:  0.00156 
#F-statistic: 2.825 on 1 and 1167 DF,  p-value: 0.09308

boxplot(SgldAsm_Mean ~ bmi_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and BMI Subscore of LE8", 
        xlab = "BMI Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldAsm_Mean ~ bmi_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bmi_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bmi_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$SgldAsm_Mean ~ gs_newjoined$bmi_le8.y))

#SgldHom_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0003411,	Adjusted R-squared:  -0.0005155 
#F-statistic: 0.3982 on 1 and 1167 DF,  p-value: 0.5281

boxplot(SgldHom_Mean ~ bmi_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and BMI Subscore of LE8", 
        xlab = "BMI Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldHom_Mean ~ bmi_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bmi_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bmi_le8.y = x)), col = "red", lty = 8)

summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$bmi_le8.y))

dev.off()

#nonhdl_le8.y
#SCATTERPLOT SUMMARIES OF THE RELATIONSHIPS BETWEEN THE nonhdl_le8 SCORES AND GRAYSCALE FEATURES. 
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/LE8scatterplotNONHDLsummaries.070925.pdf")

boxplot(Entr_Mean ~ nonhdl_le8.y, data = gs_newjoined, 
        main = "Association between Average Vascular Wall Entropy and Non-HDL Cholesterol Subscore of LE8", 
        xlab = "Non-HDL Cholesterol Subscore of LE8",
        ylab = "Average Vascular Wall Entropy")
bmile8 <- lm(Entr_Mean ~ nonhdl_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$nonhdl_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(nonhdl_le8.y = x)), col = "red", lty = 8) 

#Median_Mean vs LE8 scores lm 
#Multiple R-squared:  0.07982,	Adjusted R-squared:  0.07903 
#F-statistic: 101.2 on 1 and 1167 DF,  p-value: < 2.2e-16
boxplot(Median_Mean ~ nonhdl_le8.y, data = gs_newjoined, 
        main = "Association between Grayscale Median and Non-HDL Cholesterol Subscore of LE8", 
        xlab = "Non-HDL Cholesterol Subscore of LE8",
        ylab = "Grayscale Median")
bmile8 <- lm(Median_Mean ~ nonhdl_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$nonhdl_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(nonhdl_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Median_Mean ~ gs_newjoined$nonhdl_le8.y))

#Gldm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0005928,	Adjusted R-squared:  -0.0002636 
#F-statistic: 0.6923 on 1 and 1167 DF,  p-value: 0.4056
boxplot(Gldm_Mean ~ nonhdl_le8.y, data = gs_newjoined, 
        main = "Association between GLDM and Non-HDL Cholesterol Subscore of LE8", 
        xlab = "Non-HDL Cholesterol Subscore of LE8",
        ylab = "Average GLDM")
bmile8 <- lm(Gldm_Mean ~ nonhdl_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$nonhdl_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(nonhdl_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Gldm_Mean ~ gs_newjoined$nonhdl_le8.y))

#SgldAsm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.002415,	Adjusted R-squared:  0.00156 
#F-statistic: 2.825 on 1 and 1167 DF,  p-value: 0.09308

boxplot(SgldAsm_Mean ~ nonhdl_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Non-HDL Cholesterol Subscore of LE8", 
        xlab = "Non-HDL Cholesterol Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldAsm_Mean ~ nonhdl_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$nonhdl_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(nonhdl_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$SgldAsm_Mean ~ gs_newjoined$nonhdl_le8.y))

#SgldHom_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0003411,	Adjusted R-squared:  -0.0005155 
#F-statistic: 0.3982 on 1 and 1167 DF,  p-value: 0.5281

boxplot(SgldHom_Mean ~ nonhdl_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Non-HDL Cholesterol Subscore of LE8", 
        xlab = "Non-HDL Cholesterol Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldHom_Mean ~ nonhdl_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$nonhdl_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(nonhdl_le8.y = x)), col = "red", lty = 8)

summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$nonhdl_le8.y))

dev.off()

#bp_le8.y
#SCATTERPLOT SUMMARIES OF THE RELATIONSHIPS BETWEEN THE bp_le8 SCORES AND GRAYSCALE FEATURES. 
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/LE8scatterplotBPLE8summaries.070925.pdf")

boxplot(Entr_Mean ~ bp_le8.y, data = gs_newjoined, 
        main = "Association between Average Vascular Wall Entropy and Blood Pressure Subscore of LE8", 
        xlab = "Blood Pressure Subscore of LE8",
        ylab = "Average Vascular Wall Entropy")
bmile8 <- lm(Entr_Mean ~ bp_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bp_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bp_le8.y = x)), col = "red", lty = 8) 

#Median_Mean vs LE8 scores lm 
#Multiple R-squared:  0.07982,	Adjusted R-squared:  0.07903 
#F-statistic: 101.2 on 1 and 1167 DF,  p-value: < 2.2e-16
boxplot(Median_Mean ~ bp_le8.y, data = gs_newjoined, 
        main = "Association between Grayscale Median and Blood Pressure Subscore of LE8", 
        xlab = "Blood Pressure Subscore of LE8",
        ylab = "Grayscale Median")
bmile8 <- lm(Median_Mean ~ bp_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bp_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bp_le8.y = x)), col = "red", lty = 8) 

abline(bmile8, col = "red")
summary (lm (formula = gs_newjoined$Median_Mean ~ gs_newjoined$bp_le8.y))

#Gldm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0005928,	Adjusted R-squared:  -0.0002636 
#F-statistic: 0.6923 on 1 and 1167 DF,  p-value: 0.4056
boxplot(Gldm_Mean ~ bp_le8.y, data = gs_newjoined, 
        main = "Association between GLDM and Blood Pressure Subscore of LE8", 
        xlab = "Blood Pressure Subscore of LE8",
        ylab = "Average GLDM")
bmile8 <- lm(Gldm_Mean ~ bp_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bp_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bp_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Gldm_Mean ~ gs_newjoined$bp_le8.y))

#SgldAsm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.002415,	Adjusted R-squared:  0.00156 
#F-statistic: 2.825 on 1 and 1167 DF,  p-value: 0.09308

boxplot(SgldAsm_Mean ~ bp_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Blood Pressure Subscore of LE8", 
        xlab = "Blood Pressure Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldAsm_Mean ~ bp_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bp_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bp_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$SgldAsm_Mean ~ gs_newjoined$bp_le8.y))

#SgldHom_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0003411,	Adjusted R-squared:  -0.0005155 
#F-statistic: 0.3982 on 1 and 1167 DF,  p-value: 0.5281

boxplot(SgldHom_Mean ~ bp_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Blood Pressure Subscore of LE8", 
        xlab = "Blood Pressure Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldHom_Mean ~ bp_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bp_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bp_le8.y = x)), col = "red", lty = 8)

summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$bp_le8.y))

dev.off()

#glu_le8.y
#SCATTERPLOT SUMMARIES OF THE RELATIONSHIPS BETWEEN THE bp_le8 SCORES AND GRAYSCALE FEATURES. 
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/LE8scatterplotGLULE8summaries.070925.pdf")


boxplot(Entr_Mean ~ glu_le8.y, data = gs_newjoined, 
        main = "Association between Average Vascular Wall Entropy and Glucose Subscore of LE8", 
        xlab = "Glucose Subscore of LE8",
        ylab = "Average Vascular Wall Entropy")
bmile8 <- lm(Entr_Mean ~ glu_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$glu_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(glu_le8.y = x)), col = "red", lty = 8) 

#Median_Mean vs LE8 scores lm 
#Multiple R-squared:  0.07982,	Adjusted R-squared:  0.07903 
#F-statistic: 101.2 on 1 and 1167 DF,  p-value: < 2.2e-16
boxplot(Median_Mean ~ glu_le8.y, data = gs_newjoined, 
        main = "Association between Grayscale Median and Glucose Subscore of LE8", 
        xlab = "Glucose Subscore of LE8",
        ylab = "Grayscale Median")
bmile8 <- lm(Median_Mean ~ glu_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$glu_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(glu_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Median_Mean ~ gs_newjoined$glu_le8.y))

#Gldm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0005928,	Adjusted R-squared:  -0.0002636 
#F-statistic: 0.6923 on 1 and 1167 DF,  p-value: 0.4056
boxplot(Gldm_Mean ~ glu_le8.y, data = gs_newjoined, 
        main = "Association between GLDM and Glucose Subscore of LE8", 
        xlab = "Glucose Subscore of LE8",
        ylab = "Average GLDM")
bmile8 <- lm(Gldm_Mean ~ glu_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$glu_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(glu_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Gldm_Mean ~ gs_newjoined$glu_le8.y))

#SgldAsm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.002415,	Adjusted R-squared:  0.00156 
#F-statistic: 2.825 on 1 and 1167 DF,  p-value: 0.09308

boxplot(SgldAsm_Mean ~ glu_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Glucose Subscore of LE8", 
        xlab = "Glucose Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldAsm_Mean ~ glu_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$glu_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(glu_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$SgldAsm_Mean ~ gs_newjoined$glu_le8.y))

#SgldHom_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0003411,	Adjusted R-squared:  -0.0005155 
#F-statistic: 0.3982 on 1 and 1167 DF,  p-value: 0.5281

boxplot(SgldHom_Mean ~ glu_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Glucose Subscore of LE8", 
        xlab = "Glucose Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldHom_Mean ~ glu_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$glu_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(glu_le8.y = x)), col = "red", lty = 8)

summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$glu_le8.y))

dev.off()

#smoker_le8
#SCATTERPLOT SUMMARIES OF THE RELATIONSHIPS BETWEEN THE smoker_le8 SCORES AND GRAYSCALE FEATURES. 
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/LE8scatterplotSMOKERLE8summaries.070925.pdf")

boxplot(Entr_Mean ~ smoker_le8.y, data = gs_newjoined, 
        main = "Association between Average Vascular Wall Entropy and Smoker Subscore of LE8", 
        xlab = "Smoker Subscore of LE8",
        ylab = "Average Vascular Wall Entropy")
bmile8 <- lm(Entr_Mean ~ smoker_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$smoker_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(smoker_le8.y = x)), col = "red", lty = 8) 
summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$smoker_le8.y))

#Median_Mean vs LE8 scores lm 
#Multiple R-squared:  0.07982,	Adjusted R-squared:  0.07903 
#F-statistic: 101.2 on 1 and 1167 DF,  p-value: < 2.2e-16
boxplot(Median_Mean ~ smoker_le8.y, data = gs_newjoined, 
        main = "Association between Grayscale Median and Smoker Subscore of LE8", 
        xlab = "Smoker Subscore of LE8",
        ylab = "Grayscale Median")
bmile8 <- lm(Median_Mean ~ smoker_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$smoker_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(smoker_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Median_Mean ~ gs_newjoined$smoker_le8.y))

#Gldm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0005928,	Adjusted R-squared:  -0.0002636 
#F-statistic: 0.6923 on 1 and 1167 DF,  p-value: 0.4056
boxplot(Gldm_Mean ~ smoker_le8.y, data = gs_newjoined, 
        main = "Association between GLDM and Smoker Subscore of LE8", 
        xlab = "Smoker Subscore of LE8",
        ylab = "Average GLDM")
bmile8 <- lm(Gldm_Mean ~ smoker_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$smoker_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(smoker_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Gldm_Mean ~ gs_newjoined$smoker_le8.y))

#SgldAsm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.002415,	Adjusted R-squared:  0.00156 
#F-statistic: 2.825 on 1 and 1167 DF,  p-value: 0.09308

boxplot(SgldAsm_Mean ~ smoker_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Smoker Subscore of LE8", 
        xlab = "Smoker Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldAsm_Mean ~ smoker_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$smoker_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(smoker_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$SgldAsm_Mean ~ gs_newjoined$smoker_le8.y))

#SgldHom_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0003411,	Adjusted R-squared:  -0.0005155 
#F-statistic: 0.3982 on 1 and 1167 DF,  p-value: 0.5281

boxplot(SgldHom_Mean ~ smoker_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Smoker Subscore of LE8", 
        xlab = "Smoker Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldHom_Mean ~ smoker_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$smoker_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(smoker_le8.y = x)), col = "red", lty = 8)

summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$smoker_le8.y))

dev.off()

#pa_le8.y
#SCATTERPLOT SUMMARIES OF THE RELATIONSHIPS BETWEEN THE pa_le8 SCORES AND GRAYSCALE FEATURES. 
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/LE8scatterplotPALE8summaries.070925.pdf")

boxplot(Entr_Mean ~ pa_le8.y, data = gs_newjoined, 
        main = "Association between Average Vascular Wall Entropy and Physical Activity Subscore of LE8", 
        xlab = "Physical Activity Subscore of LE8",
        ylab = "Average Vascular Wall Entropy")
bmile8 <- lm(Entr_Mean ~ pa_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$pa_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(pa_le8.y = x)), col = "red", lty = 8) 
summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$pa_le8.y))

#Median_Mean vs LE8 scores lm 
#Multiple R-squared:  0.07982,	Adjusted R-squared:  0.07903 
#F-statistic: 101.2 on 1 and 1167 DF,  p-value: < 2.2e-16
boxplot(Median_Mean ~ pa_le8.y, data = gs_newjoined, 
        main = "Association between Grayscale Median and Physical Activity Subscore of LE8", 
        xlab = "Physical Activity Subscore of LE8",
        ylab = "Grayscale Median")
bmile8 <- lm(Median_Mean ~ pa_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$pa_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(pa_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Median_Mean ~ gs_newjoined$pa_le8.y))

#Gldm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0005928,	Adjusted R-squared:  -0.0002636 
#F-statistic: 0.6923 on 1 and 1167 DF,  p-value: 0.4056
boxplot(Gldm_Mean ~ pa_le8.y, data = gs_newjoined, 
        main = "Association between GLDM and Physical Activity Subscore of LE8", 
        xlab = "Physical Activity Subscore of LE8",
        ylab = "Average GLDM")
bmile8 <- lm(Gldm_Mean ~ pa_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$pa_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(pa_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Gldm_Mean ~ gs_newjoined$pa_le8.y))

#SgldAsm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.002415,	Adjusted R-squared:  0.00156 
#F-statistic: 2.825 on 1 and 1167 DF,  p-value: 0.09308

boxplot(SgldAsm_Mean ~ pa_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Physical Activity Subscore of LE8", 
        xlab = "Physical Activity Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldAsm_Mean ~ pa_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$pa_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(pa_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$SgldAsm_Mean ~ gs_newjoined$pa_le8.y))

#SgldHom_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0003411,	Adjusted R-squared:  -0.0005155 
#F-statistic: 0.3982 on 1 and 1167 DF,  p-value: 0.5281

boxplot(SgldHom_Mean ~ pa_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Physical Activity Subscore of LE8", 
        xlab = "Physical Activity Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldHom_Mean ~ pa_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$pa_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(pa_le8.y = x)), col = "red", lty = 8)

summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$pa_le8.y))

dev.off()

#diet_le8
#SCATTERPLOT SUMMARIES OF THE RELATIONSHIPS BETWEEN THE diet_le8 SCORES AND GRAYSCALE FEATURES. 
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/LE8scatterplotDIETLE8summaries.070925.pdf")

boxplot(Entr_Mean ~ diet_le8.y, data = gs_newjoined, 
        main = "Association between Average Vascular Wall Entropy and Diet Subscore of LE8", 
        xlab = "Diet Subscore of LE8",
        ylab = "Average Vascular Wall Entropy")
bmile8 <- lm(Entr_Mean ~ diet_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$diet_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(diet_le8.y = x)), col = "red", lty = 8) 
summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$diet_le8.y))

#Median_Mean vs LE8 scores lm 
#Multiple R-squared:  0.07982,	Adjusted R-squared:  0.07903 
#F-statistic: 101.2 on 1 and 1167 DF,  p-value: < 2.2e-16
boxplot(Median_Mean ~ diet_le8.y, data = gs_newjoined, 
        main = "Association between Grayscale Median and Diet Subscore of LE8", 
        xlab = "Diet Subscore of LE8",
        ylab = "Grayscale Median")
bmile8 <- lm(Median_Mean ~ diet_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$diet_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(diet_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Median_Mean ~ gs_newjoined$diet_le8.y))

#Gldm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0005928,	Adjusted R-squared:  -0.0002636 
#F-statistic: 0.6923 on 1 and 1167 DF,  p-value: 0.4056
boxplot(Gldm_Mean ~ diet_le8.y, data = gs_newjoined, 
        main = "Association between GLDM and Diet Subscore of LE8", 
        xlab = "Diet Subscore of LE8",
        ylab = "Average GLDM")
bmile8 <- lm(Gldm_Mean ~ diet_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$diet_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(diet_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Gldm_Mean ~ gs_newjoined$diet_le8.y))

#SgldAsm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.002415,	Adjusted R-squared:  0.00156 
#F-statistic: 2.825 on 1 and 1167 DF,  p-value: 0.09308

boxplot(SgldAsm_Mean ~ diet_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Diet Subscore of LE8", 
        xlab = "Diet Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldAsm_Mean ~ diet_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$diet_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(diet_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$SgldAsm_Mean ~ gs_newjoined$diet_le8.y))

#SgldHom_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0003411,	Adjusted R-squared:  -0.0005155 
#F-statistic: 0.3982 on 1 and 1167 DF,  p-value: 0.5281

boxplot(SgldHom_Mean ~ diet_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Diet Subscore of LE8", 
        xlab = "Diet Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldHom_Mean ~ diet_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$diet_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(diet_le8.y = x)), col = "red", lty = 8)

summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$diet_le8.y))

dev.off()

bp_le8.y
#SCATTERPLOT SUMMARIES OF THE RELATIONSHIPS BETWEEN THE bp_le8 SCORES AND GRAYSCALE FEATURES. 
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/LE8scatterplotBPLE8summaries.070925.pdf")

boxplot(Entr_Mean ~ bp_le8.y, data = gs_newjoined, 
        main = "Association between Average Vascular Wall Entropy and Blood Pressure Subscore of LE8", 
        xlab = "Blood Pressure Subscore of LE8",
        ylab = "Average Vascular Wall Entropy")
bmile8 <- lm(Entr_Mean ~ bp_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bp_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bp_le8.y = x)), col = "red", lty = 8) 
summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$bp_le8.y))

#Median_Mean vs LE8 scores lm 
#Multiple R-squared:  0.07982,	Adjusted R-squared:  0.07903 
#F-statistic: 101.2 on 1 and 1167 DF,  p-value: < 2.2e-16
boxplot(Median_Mean ~ bp_le8.y, data = gs_newjoined, 
        main = "Association between Grayscale Median and Blood Pressure Subscore of LE8", 
        xlab = "Blood Pressure Subscore of LE8",
        ylab = "Grayscale Median")
bmile8 <- lm(Median_Mean ~ bp_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bp_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bp_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Median_Mean ~ gs_newjoined$bp_le8.y))

#Gldm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0005928,	Adjusted R-squared:  -0.0002636 
#F-statistic: 0.6923 on 1 and 1167 DF,  p-value: 0.4056
boxplot(Gldm_Mean ~ bp_le8.y, data = gs_newjoined, 
        main = "Association between GLDM and Blood Pressure Subscore of LE8", 
        xlab = "Blood Pressure Subscore of LE8",
        ylab = "Average GLDM")
bmile8 <- lm(Gldm_Mean ~ bp_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bp_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bp_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Gldm_Mean ~ gs_newjoined$bp_le8.y))

#SgldAsm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.002415,	Adjusted R-squared:  0.00156 
#F-statistic: 2.825 on 1 and 1167 DF,  p-value: 0.09308

boxplot(SgldAsm_Mean ~ bp_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Blood Pressure Subscore of LE8", 
        xlab = "Blood Pressure Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldAsm_Mean ~ bp_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bp_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bp_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$SgldAsm_Mean ~ gs_newjoined$bp_le8.y))

#SgldHom_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0003411,	Adjusted R-squared:  -0.0005155 
#F-statistic: 0.3982 on 1 and 1167 DF,  p-value: 0.5281

boxplot(SgldHom_Mean ~ bp_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Blood Pressure Subscore of LE8", 
        xlab = "Blood Pressure Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldHom_Mean ~ bp_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bp_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bp_le8.y = x)), col = "red", lty = 8)

summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$bp_le8.y))

dev.off()

#diet_le8
#SCATTERPLOT SUMMARIES OF THE RELATIONSHIPS BETWEEN THE diet_le8 SCORES AND GRAYSCALE FEATURES. 
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/LE8scatterplotDIETLE8summaries.070925.pdf")

boxplot(Entr_Mean ~ diet_le8.y, data = gs_newjoined, 
        main = "Association between Average Vascular Wall Entropy and Diet Subscore of LE8", 
        xlab = "Diet Subscore of LE8",
        ylab = "Average Vascular Wall Entropy")
bmile8 <- lm(Entr_Mean ~ diet_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$diet_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(diet_le8.y = x)), col = "red", lty = 8) 
summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$diet_le8.y))

#Median_Mean vs LE8 scores lm 
#Multiple R-squared:  0.07982,	Adjusted R-squared:  0.07903 
#F-statistic: 101.2 on 1 and 1167 DF,  p-value: < 2.2e-16
boxplot(Median_Mean ~ diet_le8.y, data = gs_newjoined, 
        main = "Association between Grayscale Median and Diet Subscore of LE8", 
        xlab = "Diet Subscore of LE8",
        ylab = "Grayscale Median")
bmile8 <- lm(Median_Mean ~ diet_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$diet_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(diet_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Median_Mean ~ gs_newjoined$diet_le8.y))

#Gldm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0005928,	Adjusted R-squared:  -0.0002636 
#F-statistic: 0.6923 on 1 and 1167 DF,  p-value: 0.4056
boxplot(Gldm_Mean ~ diet_le8.y, data = gs_newjoined, 
        main = "Association between GLDM and Diet Subscore of LE8", 
        xlab = "Diet Subscore of LE8",
        ylab = "Average GLDM")
bmile8 <- lm(Gldm_Mean ~ diet_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$diet_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(diet_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Gldm_Mean ~ gs_newjoined$diet_le8.y))

#SgldAsm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.002415,	Adjusted R-squared:  0.00156 
#F-statistic: 2.825 on 1 and 1167 DF,  p-value: 0.09308

boxplot(SgldAsm_Mean ~ diet_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Diet Subscore of LE8", 
        xlab = "Diet Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldAsm_Mean ~ diet_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$diet_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(diet_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$SgldAsm_Mean ~ gs_newjoined$diet_le8.y))

#SgldHom_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0003411,	Adjusted R-squared:  -0.0005155 
#F-statistic: 0.3982 on 1 and 1167 DF,  p-value: 0.5281

boxplot(SgldHom_Mean ~ diet_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Diet Subscore of LE8", 
        xlab = "Diet Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldHom_Mean ~ diet_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$diet_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(diet_le8.y = x)), col = "red", lty = 8)

summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$diet_le8.y))

dev.off()

#nonhdl_le8.y
#SCATTERPLOT SUMMARIES OF THE RELATIONSHIPS BETWEEN THE bp_le8 SCORES AND GRAYSCALE FEATURES. 
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/LE8scatterplotNONHDLLE8summaries.070925.pdf")

boxplot(Entr_Mean ~ nonhdl_le8.y, data = gs_newjoined, 
        main = "Association between Average Vascular Wall Entropy and NON-HDL Subscore of LE8", 
        xlab = "NON-HDL Subscore of LE8",
        ylab = "Average Vascular Wall Entropy")
bmile8 <- lm(Entr_Mean ~ nonhdl_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$nonhdl_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(nonhdl_le8.y = x)), col = "red", lty = 8) 
summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$nonhdl_le8.y))

#Median_Mean vs LE8 scores lm 
#Multiple R-squared:  0.07982,	Adjusted R-squared:  0.07903 
#F-statistic: 101.2 on 1 and 1167 DF,  p-value: < 2.2e-16
boxplot(Median_Mean ~ nonhdl_le8.y, data = gs_newjoined, 
        main = "Association between Grayscale Median and NON-HDL Subscore of LE8", 
        xlab = "NON-HDL Subscore of LE8",
        ylab = "Grayscale Median")
bmile8 <- lm(Median_Mean ~ nonhdl_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$nonhdl_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(nonhdl_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Median_Mean ~ gs_newjoined$nonhdl_le8.y))

#Gldm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0005928,	Adjusted R-squared:  -0.0002636 
#F-statistic: 0.6923 on 1 and 1167 DF,  p-value: 0.4056
boxplot(Gldm_Mean ~ nonhdl_le8.y, data = gs_newjoined, 
        main = "Association between GLDM and NON-HDL Subscore of LE8", 
        xlab = "NON-HDL Subscore of LE8",
        ylab = "Average GLDM")
bmile8 <- lm(Gldm_Mean ~ nonhdl_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$nonhdl_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(nonhdl_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Gldm_Mean ~ gs_newjoined$nonhdl_le8.y))

#SgldAsm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.002415,	Adjusted R-squared:  0.00156 
#F-statistic: 2.825 on 1 and 1167 DF,  p-value: 0.09308

boxplot(SgldAsm_Mean ~ nonhdl_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and NON-HDL Subscore of LE8", 
        xlab = "NON-HDL Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldAsm_Mean ~ nonhdl_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$nonhdl_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(nonhdl_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$SgldAsm_Mean ~ gs_newjoined$nonhdl_le8.y))

#SgldHom_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0003411,	Adjusted R-squared:  -0.0005155 
#F-statistic: 0.3982 on 1 and 1167 DF,  p-value: 0.5281

boxplot(SgldHom_Mean ~ nonhdl_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and NON-HDL Subscore of LE8", 
        xlab = "NON-HDL Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldHom_Mean ~ nonhdl_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$nonhdl_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(nonhdl_le8.y = x)), col = "red", lty = 8)

summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$nonhdl_le8.y))

dev.off()

#bmi_le8.y
#SCATTERPLOT SUMMARIES OF THE RELATIONSHIPS BETWEEN THE bmi_le8.y SCORES AND GRAYSCALE FEATURES. 
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/LE8scatterplotBMILE8summaries.070925.pdf")

boxplot(Entr_Mean ~ bmi_le8.y, data = gs_newjoined, 
        main = "Association between Average Vascular Wall Entropy and BMI Subscore of LE8", 
        xlab = "BMI Subscore of LE8",
        ylab = "Average Vascular Wall Entropy")
bmile8 <- lm(Entr_Mean ~ bmi_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bmi_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bmi_le8.y = x)), col = "red", lty = 8) 
summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$bmi_le8.y))

#Median_Mean vs LE8 scores lm 
#Multiple R-squared:  0.07982,	Adjusted R-squared:  0.07903 
#F-statistic: 101.2 on 1 and 1167 DF,  p-value: < 2.2e-16
boxplot(Median_Mean ~ bmi_le8.y, data = gs_newjoined, 
        main = "Association between Grayscale Median and BMI Subscore of LE8", 
        xlab = "BMI Subscore of LE8",
        ylab = "Grayscale Median")
bmile8 <- lm(Median_Mean ~ bmi_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bmi_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bmi_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Median_Mean ~ gs_newjoined$bmi_le8.y))

#Gldm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0005928,	Adjusted R-squared:  -0.0002636 
#F-statistic: 0.6923 on 1 and 1167 DF,  p-value: 0.4056
boxplot(Gldm_Mean ~ bmi_le8.y, data = gs_newjoined, 
        main = "Association between GLDM and BMI Subscore of LE8", 
        xlab = "BMI Subscore of LE8",
        ylab = "Average GLDM")
bmile8 <- lm(Gldm_Mean ~ bmi_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bmi_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bmi_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Gldm_Mean ~ gs_newjoined$bmi_le8.y))

#SgldAsm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.002415,	Adjusted R-squared:  0.00156 
#F-statistic: 2.825 on 1 and 1167 DF,  p-value: 0.09308

boxplot(SgldAsm_Mean ~ bmi_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and BMI Subscore of LE8", 
        xlab = "BMI Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldAsm_Mean ~ bmi_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bmi_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bmi_le8.y = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$SgldAsm_Mean ~ gs_newjoined$bmi_le8.y))

#SgldHom_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0003411,	Adjusted R-squared:  -0.0005155 
#F-statistic: 0.3982 on 1 and 1167 DF,  p-value: 0.5281

boxplot(SgldHom_Mean ~ bmi_le8.y, data = gs_newjoined, 
        main = "Association between SGLD-ASM and BMI Subscore of LE8", 
        xlab = "BMI Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldHom_Mean ~ bmi_le8.y, data = gs_newjoined)
x = sort(unique(gs_newjoined$bmi_le8.y))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(bmi_le8.y = x)), col = "red", lty = 8)

summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$bmi_le8.y))

dev.off()

#slp_le8
#SCATTERPLOT SUMMARIES OF THE RELATIONSHIPS BETWEEN THE slp_le8 SCORES AND GRAYSCALE FEATURES. 
dir.create("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/")
pdf("/drives/drive1/ff_chaya/Angel/bin/PrelimFindings/SLPsubscoreLE8summaries.070925.pdf")

boxplot(Entr_Mean ~ slp_le8, data = gs_newjoined, 
        main = "Association between Average Vascular Wall Entropy and Sleep Subscore of LE8", 
        xlab = "Sleep Subscore of LE8",
        ylab = "Average Vascular Wall Entropy")
bmile8 <- lm(Entr_Mean ~ slp_le8, data = gs_newjoined)
x = sort(unique(gs_newjoined$slp_le8))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(slp_le8 = x)), col = "red", lty = 8) 
summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$slp_le8))

#Median_Mean vs LE8 scores lm 
#Multiple R-squared:  0.07982,	Adjusted R-squared:  0.07903 
#F-statistic: 101.2 on 1 and 1167 DF,  p-value: < 2.2e-16
boxplot(Median_Mean ~ slp_le8, data = gs_newjoined, 
        main = "Association between Grayscale Median and Sleep Subscore of LE8", 
        xlab = "Sleep Subscore of LE8",
        ylab = "Grayscale Median")
bmile8 <- lm(Median_Mean ~ slp_le8, data = gs_newjoined)
x = sort(unique(gs_newjoined$slp_le8))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(slp_le8 = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Median_Mean ~ gs_newjoined$slp_le8))

#Gldm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0005928,	Adjusted R-squared:  -0.0002636 
#F-statistic: 0.6923 on 1 and 1167 DF,  p-value: 0.4056
boxplot(Gldm_Mean ~ slp_le8, data = gs_newjoined, 
        main = "Association between GLDM and Sleep Subscore of LE8", 
        xlab = "Sleep Subscore of LE8",
        ylab = "Average GLDM")
bmile8 <- lm(Gldm_Mean ~ slp_le8, data = gs_newjoined)
x = sort(unique(gs_newjoined$slp_le8))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(slp_le8 = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$Gldm_Mean ~ gs_newjoined$slp_le8))

#SgldAsm_Mean vs LE8 scores lm 
#Multiple R-squared:  0.002415,	Adjusted R-squared:  0.00156 
#F-statistic: 2.825 on 1 and 1167 DF,  p-value: 0.09308

boxplot(SgldAsm_Mean ~ slp_le8, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Sleep Subscore of LE8", 
        xlab = "Sleep Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldAsm_Mean ~ slp_le8, data = gs_newjoined)
x = sort(unique(gs_newjoined$slp_le8))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(slp_le8 = x)), col = "red", lty = 8) 

summary (lm (formula = gs_newjoined$SgldAsm_Mean ~ gs_newjoined$slp_le8))

#SgldHom_Mean vs LE8 scores lm 
#Multiple R-squared:  0.0003411,	Adjusted R-squared:  -0.0005155 
#F-statistic: 0.3982 on 1 and 1167 DF,  p-value: 0.5281

boxplot(SgldHom_Mean ~ slp_le8, data = gs_newjoined, 
        main = "Association between SGLD-ASM and Sleep Subscore of LE8", 
        xlab = "Sleep Subscore of LE8",
        ylab = "Average SGLD-ASM")
bmile8 <- lm(SgldHom_Mean ~ slp_le8, data = gs_newjoined)
x = sort(unique(gs_newjoined$slp_le8))
lines(x=1:length(x), 
      y = predict(bmile8, data.frame(slp_le8 = x)), col = "red", lty = 8)

summary (lm (formula = gs_newjoined$SgldHom_Mean ~ gs_newjoined$slp_le8))

dev.off()

#List of Additional LE8 performed: 
#diet_le8.y, pa_le8.y, smoker_le8.y, 
#pa_le8.y, glu_le8.y, bp_le8.y, nonhdl_le8.y, 
#bmi_le8.y, le8_w_slp, slp_le8

summary(le8_with_sleep$bmi_le8)

#CUBIC SPLINE CURVE;(LISA)
library(ggplot2)
library(broom)
library(tidyr)

library(dplyr)
install.packages('mgcv',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
.libPaths("/drives/drive1/home/an9206/R/x86_64-redhat-linux-gnu-library/4.5")

library(moments)
library(mgcv)

ggplot(gs_nona2, aes(x = Median_Mean))+
  geom_histogram()+ 
  theme_minimal()
#Center the data by subtracting what it's centered on. (Make the data normally distributed (normalize it); shouldn't use lm)
#Calculate the mean of your variable and subtract the mean. 
shapiro.test(gs_nona2$Entr_Mean)

# Remove NAs - # note, you can probably impute some of the le8 scores if necessary; also, I used gs - don't know if you need to use gs1
gs_nona<-gs_newjoined[complete.cases(gs_newjoined$Median_Mean),]
gs_nona2<-gs_nona[complete.cases(gs_nona$le8_w_slp),]
# Check skewness - should be close to zero
skewness(gs_nona2$le8_w_slp)
# rename for clarity
df<-gs_nona2



# PS model
# B-spline Curve
modelps<-gam(le8_w_slp~s(Median_Mean, bs="ps",k=10),data=df)
#Cubic Spline Curve
modelcr<-gam(le8_w_slp~s(Median_Mean, bs="cr",k=10),data=df)

?gam
# check model
gam.check(modelcr)
summary(modelcr)

# check linear model
lm_model<-lm(le8_w_slp~Median_Mean,data=df)
summary(lm_model)

# Check AIC; check the cubic and b-spline with the linear model 
AIC(modelcr,lm_model)
AIC(modelps,modelcr)

# Note the p-value is not significant, indicating not much improvement and AIC is similar to linear model over the linear model

#Sequence for smooth term prediction
x_seq<-seq(min(df$Median_Mean),max(df$Median_Mean),length.out=length(df$Median_Mean))

# Predict partial smooth term and standard errors
partial_pred <-predict(modelcr, newdata=data.frame(Median_Mean=x_seq),type="terms",terms="s(Median_Mean)",se.fit=T)
fit<-partial_pred$fit[,"s(Median_Mean)"]
se <- partial_pred$se.fit[,"s(Median_Mean)"]

# Create dataframe for plotting
plot_df<-data.frame(
  x=x_seq,
  fit=fit,
  upper=fit+2 *se,
  lower=fit - 2*se
)


# Get the smooth object from the model
smooth_obj<-modelcr$smooth[[1]]
smooth_obj

# Extract the knots
knots<-smooth_obj$knots
x_range<-range(df$Median_Mean)
knots_in_range<-knots[knots >= x_range[1] & knots <= x_range[2]]
knot_df<-data.frame(Median_Mean=knots_in_range)
knot_df$y_knot_pos<-min(plot_df$lower) -0.05 * diff(range(plot_df$fit))
knot_df$fit<-NA
knot_pred<-predict(modelcr, newdata=knot_df,type="terms",terms="s(Median_Mean)")
knot_df$fit<-knot_pred[,"s(Median_Mean)"]
#y_knot_pos<-min(plot_df$fit) - 0.1*diff(range(plot_df$fit))
#x_range<-range(x)
#knots_in_range<-knots[knots >= x_range[1] & knots <= x_range[2]]

#knot_df<-data.frame(x=knots_in_range)
#knot_df$fit<-predict(model_ps,newdata=knot_df)


ggplot(plot_df,aes(x=x,y=fit))+
  geom_line(color="blue",size=1) +
  geom_line(aes(y=upper),linetype="dotted",color="blue")+
  geom_line(aes(y=lower),linetype="dotted",color="blue")+
  #geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3,fill="lightblue") +
  geom_point(data=knot_df,aes(x= Median_Mean,y=fit),color="red",shape=15,size=3) +
  labs(x="Grayscale Median (GSM)",y="Partial Effect on Total LE8 Score with Sleep") + 
  theme_minimal()+ 
  theme(axis.title = element_text (size = 28), axis.text = element_text (size = 20), 
        plot.title = element_text(size = 30,hjust = 0.5, face = "bold"))+ 
  ggtitle("Predicting LE8 As a Measure of Grayscale Median (GSM)")

#List of Additional LE8 performed: 
#diet_le8.y, pa_le8.y, smoker_le8.y, 
#glu_le8.y, bp_le8.y, nonhdl_le8.y, 
#bmi_le8.y, le8_w_slp, slp_le8

#Center the data by subtracting what it's centered on. 
#(Make the data normally distributed (normalize it); shouldn't use lm)
#Calculate the mean of your variable and subtract the mean. 
shapiro.test(gs_newjoined$Entr_Mean) 
#Pvalue = 2.73*10^11; lower than 0.1 deviates from normal distribution. 
#Wvalue = 0.98385; closer to 1 indicates better fit. lower mean deviates from normality. 
shapiro.test(gs_newjoined$Median_Mean) #p =.72 , w = 0.999
#shapiro.test(gs_newjoined$Gldm_Mean) #p= .532*e^13 ; w = 0.9803
#shapiro.test(gs_newjoined$SgldAsm_Mean) #p= 2*e^16 ; w = 0.8677
#shapiro.test(gs_newjoined$SgldHom_Mean) #p= 412*e^-14; w = 0.97615
shapiro.test(gs_newjoined$diet_le8.y) #p < 2.2*e^-16; w = 0.88871
shapiro.test(gs_newjoined$pa_le8.y) #p < 2.2*e^-16; w = 0.66294
shapiro.test(gs_newjoined$smoker_le8.y) #p < 2.2e^-16; w = 0.72176
shapiro.test(gs_newjoined$glu_le8.y) #p < 2.2e^-16; w = 0.39001
shapiro.test(gs_newjoined$bp_le8.y) #p < 2.2e^-16; w = 0.70558
shapiro.test(gs_newjoined$nonhdl_le8.y) #p = 0.63807; w = 0.63807
shapiro.test(gs_newjoined$bmi_le8.y) #p = 0.82636; w = 0.82636
shapiro.test(gs_newjoined$le8_w_slp) #p = 6.7644e^05; w = 0.9936
shapiro.test(gs_newjoined$slp_le8) #p < 2.2e^-16; w = 0.77
#shapiro.test(gs_newjoined$le8_no_slp_score.y) #p = 2.07e^-05; w = 0.99304

#Median_Mean shapiro test shows a p-value greater than 0.1 and a w statistic closer to 1, 
#which indicates a better fit (0.999)
install.packages('normalize',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("normalize")

#install.packages
#Normalize the data
gs_newjoined|>
  select(diet_le8.y, pa_le8.y, smoker_le8.y, nonhdl_le8.y, glu_le8.y, bp_le8.y, le8_w_slp, slp_le8)|>
  mutate(
    diet_le8.y = log(diet_le8.y, na.rm = T),
    pa_le8.y = log(pa_le8.y, na.rm = T),
    smoker_le8.y = log(smoker_le8.y, na.rm = T),
    nonhdl_le8.y = log(nonhdl_le8.y, na.rm = T),
    glu_le8.y = log(glu_le8.y, na.rm = T),
    bp_le8.y = log(bp_le8.y, na.rm = T),
    le8_w_slp = log(le8_w_slp, na.rm = T),
    slp_le8 = log(slp_le8, na.rm = T))

test<- sqrt(na.omit(gs_newjoined$le8_w_slp))

ggplot(gs_newjoined, aes(x = le8_no_slp_score.y))+
  geom_histogram()+ 
  theme_minimal()

ggplot(gs_newjoined, aes(x = test))+
  geom_histogram()+ 
  theme_minimal()


#Center the data by subtracting what it's centered on. (Make the data normally distributed (normalize it); shouldn't use lm)
#Calculate the mean of your variable and subtract the mean. 
shapiro.test(test)

#List of Additional LE8 performed: 
#diet_le8.y, pa_le8.y, smoker_le8.y, 
#glu_le8.y, bp_le8.y, nonhdl_le8.y, 
#bmi_le8.y, le8_w_slp, slp_le8

#Heteroskedasticaticity Check- test for equal variances for all Median Mean vs Grayscale
model1<- lm(Median_Mean ~ slp_le8, data = gs_newjoined) 
summary(model1)
ncvTest(model1) #p value is high, which indicates no problem of unequal variance. 
install.packages('car',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("car")
library("dplyr")
model2<- lm(Median_Mean ~ diet_le8.y, data = gs_newjoined) 
model3<- lm(Median_Mean ~ pa_le8.y, data = gs_newjoined) 
model4<- lm(Median_Mean ~ smoker_le8.y, data = gs_newjoined) 
model5<- lm(Median_Mean ~ glu_le8.y, data = gs_newjoined) 
model6<- lm(Median_Mean ~ bp_le8.y, data = gs_newjoined) 
model7<- lm(Median_Mean ~ nonhdl_le8.y, data = gs_newjoined) 
model8<- lm(Median_Mean ~ bmi_le8.y, data = gs_newjoined) 
model9<- lm(Median_Mean ~ le8_w_slp, data = gs_newjoined) 
model10<- lm(Median_Mean ~ le8_no_slp_score.y, data = gs_newjoined) 
#pvalue of lower than 0.05
ncvTest(model2) #p = 0.62 does not meet the threshold, so no unequal variance
ncvTest(model3) #p = 0.69 does not meet the threshold, so no unequal variance
ncvTest(model4) #p = 0.1368 does not meet the threshold, so no unequal variance
ncvTest(model5) #p = 0.272 does not meet the threshold, so no unequal variance
ncvTest(model6) #p = 0.1151 does not meet the threshold, so no unequal variance
ncvTest(model7) #p = 0.27561 does not meet the threshold, so no unequal variance
ncvTest(model8) #p = 0.49568 does not meet the threshold, so no unequal variance
ncvTest(model9) #p = 0.45505 does not meet the threshold, so no unequal variance
ncvTest(model10) #p = 0.45505 does not meet the threshold, so no unequal variance

plot(model1)
plot(model2)
plot(model3)
plot(model4)
plot(model5)
plot(model6)
plot(model7)
plot(model8)
plot(model9)
plot(model10)

#LS7
LS7 <- readRDS ("/drives/drive1/ff_chaya/R_versions/FFS_LE8_LS7_all_mergedFFallwvs_Y22sh_CIMT2/FFS_LE8_LS7_all_mergedFFallwvs_Y22sh_CIMT2_datafile.RDS")
LS7_var_labels <- read.table ("/drives/drive1/ff_chaya/R_versions/FFS_LE8_LS7_all_mergedFFallwvs_Y22sh_CIMT2/FFS_LE8_LS7_all_mergedFFallwvs_Y22sh_CIMT2_var_labels.txt", 
                              sep = "\t")

hist(LS7$uvm_sgl)

summary(LS7$uvm_sgl)


#Meeting today:
#Ask about Shapiro Test for the data

#Ask about the cubic spline curves: 
#How to perform them. 
#Show the sample code
#Explain code and show her the draft and ask about interpretation. 
#Do ANOVA instead 

#Multivariate linear models

setwd("/drives/drive1/ff_chaya/Angel/bin/")
#read.csv("gs_joined.csv")
gs_joined <- read.csv("gs_joined.csv")

library(tibble)

install.packages('pheatmap',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("pheatmap")

install.packages('rbind',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("rbind")

install.packages('data.table',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("data.table")

install.packages('dplyr',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("dplyr")

install.packages('gtable',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("gtable")

install.packages('broom',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library("broom")


le8_with_sleep <- read.csv("/drives/drive1/ff_chaya/original_data/CVH_scoreOnly.csv")

gs_newjoined <- left_join (gs_joined, le8_with_sleep, by = "CVID")

gs_newjoined <- read.csv("gs_newjoined.csv")

write.csv(gs_newjoined, "gs_newjoined.csv")# Remove NAs - # note, you can probably impute some of the le8 scores if necessary; also, I used gs - don't know if you need to use gs1

Le8s<- c("diet_le8.y", "pa_le8.y", "smoker_le8.y", 
         "pa_le8.y", "glu_le8.y", "bp_le8.y", "nonhdl_le8.y", "bmi_le8.y", 
         "le8_w_slp", "slp_le8")
features <- c( "Entr_Mean", "Median_Mean", "Gldm_Mean", "SgldAsm_Mean", "SgldHom_Mean")
DNAmages <- c("k5mk_horvath", "k5me_horvath", "k6mk_horvath", "k6me_horvath", "k5mk_skinblood", 
              "k5me_skinblood", "k6mk_skinblood", "k6me_skinblood", "k5mk_phenoage", "k5me_phenoage", 
              "k6mk_phenoage", "k6me_phenoage", "k6me_phenoage", "k5mk_grim", "k5me_grim", "k6mk_grim", 
              "k6me_grim", "k5mk_poam38", "k5me_poam38", "k6mk_poam38", "k6me_poam38", "k5mk_poam45", 
              "k5me_poam45", "k5me_poam45", "k6mk_poam45", "k6me_poam45")


#CUBIC SPLINE CURVE
library(ggplot2)
library(broom)
library(tidyr)

library(dplyr)
install.packages('mgcv',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
.libPaths("/drives/drive1/home/an9206/R/x86_64-redhat-linux-gnu-library/4.5")
install.packages('nlme',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library(nlme)
library(moments)
library(mgcv)

gs_nona<-gs_newjoined[complete.cases(gs_newjoined$Median_Mean),]
gs_nona2<-gs_nona[complete.cases(gs_nona$le8_w_slp),]
# Check skewness - should be close to zero
skewness(gs_nona2$le8_w_slp)
# rename for clarity
df<-gs_nona2

# PS model
# B-spline Curve
modelps<-gam(le8_w_slp~s(Median_Mean, bs="ps",k=10)+ 
               ff_cm1ethrace + ff_cm1bsex + ff_cm1povca + ck7bmi,
             data=df, 
             method = "REML"
)
#Cubic Spline Curve
modelcr<-gam(le8_w_slp~s(Median_Mean, bs="cr",k=10),data=df)

?gam
# check model
gam.check(modelps)
summary(modelps)

# check linear model
lm_model<-lm(le8_w_slp~Median_Mean+ 
               ff_cm1ethrace + ff_cm1bsex + ff_cm1povca + ck7bmi,
             data=df
)
summary(lm_model)

# Check AIC; check the cubic and b-spline with the linear model 
AIC(modelcr,lm_model)
AIC(modelps,modelcr)

# Note the p-value is not significant, indicating not much improvement and AIC is similar to linear model over the linear model

#Sequence for smooth term prediction
x_seq<-seq(min(df$Median_Mean, na.rm = T),max(df$Median_Mean, na.rm = T),
           length.out=length(df$Median_Mean))

ref_race <- names(sort(table(df$ff_cm1ethrace), decreasing = T))[1]
ref_sex <- names (sort(table(df$ff_cm1bsex), decreasing = T)) [1]
ref_pov <- names (sort(table(df$ff_cm1povca), decreasing = T)) [1]
ref_bmi <- mean (df$ck7bmi, na.rm = T)

newdata <- data.frame (
  Median_Mean = x_seq, 
  ff_cm1ethrace = factor (ref_race, levels = levels (df$ff_cm1ethrace)), 
  ff_cm1bsex = factor (ref_sex, levels = levels (df$ ff_cm1bsex)), 
  ff_cm1povca = factor (ref_pov, levels = levels (df$ff_cm1povca)), 
  ck7bmi = ref_bmi
)

smooth_lab <- modelps$smooth[[1]]$label
smooth_lab

# Predict partial smooth term and standard errors
partial_pred <-predict(modelps, newdata= newdata,type="terms",
                       terms="s(Median_Mean)",se.fit=TRUE)
colnames(partial_pred$fit)
smooth_col <- grep ("^s\\(", colnames (partial_pred$fit), value = T) [1]

fit<-as.numeric (partial_pred$fit[,smooth_col])
se <- as.numeric(partial_pred$se.fit[,smooth_col])

# Create dataframe for plotting
plot_df<-data.frame(
  x=x_seq,
  fit=fit,
  upper=fit+2 *se,
  lower=fit - 2*se
)

q_vals <- quantile(df$Median_Mean, probs = seq (0.1, 0.9, by = 0.1), na.rm = T)

# Get the smooth object from the model
#smooth_obj<-modelps$smooth[[1]]
#smooth_obj

# Extract the knots
#knots<-smooth_obj$knots
#x_range<-range(df$Median_Mean)
#knots_in_range<-knots[knots >= x_range[1] & knots <= x_range[2]]

#knot_df<-data.frame(Median_Mean=knots_in_range)
#knot_df$y_knot_pos<-min(plot_df$lower) -0.05 * diff(range(plot_df$fit))
#knot_df$fit<-NA
knot_df<- data.frame (
  Median_Mean = as.numeric (q_vals), 
  ff_cm1ethrace = factor (ref_race, levels = levels (df$ff_cm1ethrace)), 
  ff_cm1bsex = factor (ref_sex, levels = levels (df$ff_cm1bsex)), 
  ff_cm1povca = factor (ref_pov, levels = levels (df$ff_cm1povca)), 
  ck7bmi = ref_bmi
)

knot_pred<-predict(modelps, newdata=knot_df,type="terms",terms="s(Median_Mean)")

smooth_col2 <- grep ("^s\\(", colnames(knot_pred), value = T)[1]

knot_df$fit<- as.numeric(knot_pred[,smooth_lab])
#y_knot_pos<-min(plot_df$fit) - 0.1*diff(range(plot_df$fit))
#x_range<-range(x)
#knots_in_range<-knots[knots >= x_range[1] & knots <= x_range[2]]

#knot_df<-data.frame(x=knots_in_range)
#knot_df$fit<-predict(model_ps,newdata=knot_df)

ggplot(plot_df,aes(x=x,y=fit))+
  geom_line(color="blue",size=1) +
  geom_line(aes(y=upper),linetype="dotted",color="blue")+
  geom_line(aes(y=lower),linetype="dotted",color="blue")+
  #geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3,fill="lightblue") +
  geom_point(data=knot_df,aes(x= Median_Mean,y=fit),color="red",shape=15,size=3) +
  labs(x="Grayscale Median (GSM)",y="Adjusted Partial Effect on LE8") + 
  theme_minimal()+ 
  theme(axis.title = element_text (size = 28), axis.text = element_text (size = 20), 
        plot.title = element_text(size = 30,hjust = 0.5, face = "bold"))+ 
  ggtitle("Multivariable GAM Adjusted for BMI, Race/Ethnicity, Income Level, and Sex")






ggplot(gs_newjoined, aes(x = Median_Mean, y = le8_w_slp)) + 
  geom_point() + theme_bw() + stat_smooth(method = 'lm', se = T)+ 
  labs(
    title = "Median Mean vs LE8 Scores",
    y = "LE8 Scores",
    x = "Median Mean"
  ) + 
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
  )














gs_nona<-gs_newjoined[complete.cases(gs_newjoined$Entr_Mean),]
gs_nona2<-gs_nona[complete.cases(gs_nona$le8_w_slp),]
# Check skewness - should be close to zero
skewness(gs_nona2$le8_w_slp)
# rename for clarity
df<-gs_nona2

# PS model
# B-spline Curve
modelps<-gam(le8_w_slp~s(Entr_Mean, bs="ps",k=10),data=df)
#Cubic Spline Curve
modelcr<-gam(le8_w_slp~s(Entr_Mean, bs="cr",k=10),data=df)

?gam.check
# check model
gam.check(modelps)
summary(modelps)

# check linear model
lm_model<-lm(le8_w_slp~Entr_Mean,data=df)
summary(lm_model)

# Check AIC; check the cubic and b-spline with the linear model 
AIC(modelcr,lm_model)
AIC(modelps,modelcr)

# Note the p-value is not significant, indicating not much improvement and AIC is similar to linear model over the linear model

#Sequence for smooth term prediction
x_seq<-seq(min(df$Entr_Mean),max(df$Entr_Mean),length.out=length(df$Entr_Mean))

# Predict partial smooth term and standard errors
partial_pred <-predict(modelps, newdata=data.frame(Entr_Mean=x_seq),type="terms",terms="s(Entr_Mean)",se.fit=T)
fit<-partial_pred$fit[,"s(Entr_Mean)"]
se <- partial_pred$se.fit[,"s(Entr_Mean)"]

# Create dataframe for plotting
plot_df<-data.frame(
  x=x_seq,
  fit=fit,
  upper=fit+2 *se,
  lower=fit - 2*se
)


# Get the smooth object from the model
smooth_obj<-modelps$smooth[[1]]
smooth_obj

# Extract the knots
knots<-smooth_obj$knots
x_range<-range(df$Entr_Mean)
knots_in_range<-knots[knots >= x_range[1] & knots <= x_range[2]]
knot_df<-data.frame(Entr_Mean=knots_in_range)
#knot_df$y_knot_pos<-min(plot_df$lower) -0.05 * diff(range(plot_df$fit))
#Line 114 does not run.
knot_df$y_knot_pos<-min(plot_df$lower) -0.05 * diff(range(plot_df$fit))
knot_df$fit<-NA
knot_pred<-predict(modelps, newdata=knot_df,type="terms",terms="s(Entr_Mean)")
knot_df$fit<-knot_pred[,"s(Entr_Mean)"]
#y_knot_pos<-min(plot_df$fit) - 0.1*diff(range(plot_df$fit))
#x_range<-range(x)
#knots_in_range<-knots[knots >= x_range[1] & knots <= x_range[2]]

#knot_df<-data.frame(x=knots_in_range)
#knot_df$fit<-predict(model_ps,newdata=knot_df)


ggplot(plot_df,aes(x=x,y=fit))+
  geom_line(color="blue",size=1) +
  geom_line(aes(y=upper),linetype="dotted",color="blue")+
  geom_line(aes(y=lower),linetype="dotted",color="blue")+
  #geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3,fill="lightblue") +
  geom_point(data=knot_df,aes(x= Entr_Mean,y=fit),color="red",shape=15,size=3) +
  labs(x="Entropy Mean",y="Partial Effect on Total LE8 Score with Sleep") + 
  theme_minimal()+ 
  theme(axis.title = element_text (size = 28), axis.text = element_text (size = 20), 
        plot.title = element_text(size = 30,hjust = 0.5, face = "bold"))+ 
  ggtitle("Predicting LE8 As a Measure of Entropy Mean")

DNAmages <- c("k5mk_horvath", "k5me_horvath", "k6mk_horvath", "k6me_horvath", "k5mk_skinblood", 
              "k5me_skinblood", "k6mk_skinblood", "k6me_skinblood", "k5mk_phenoage", "k5me_phenoage", 
              "k6mk_phenoage", "k6me_phenoage", "k6me_phenoage", "k5mk_grim", "k5me_grim", "k6mk_grim", 
              "k6me_grim", "k5mk_poam38", "k5me_poam38", "k6mk_poam38", "k6me_poam38", "k5mk_poam45", 
              "k5me_poam45", "k5me_poam45", "k6mk_poam45", "k6me_poam45")

gs_nona<-gs_newjoined[complete.cases(gs_newjoined$k5me_horvath),]
gs_nona2<-gs_nona[complete.cases(gs_nona$Median_Mean),]
# Check skewness - should be close to zero
skewness(gs_nona2$Median_Mean)
# rename for clarity
df<-gs_nona2

# PS model
# B-spline Curve
modelps<-gam(Median_Mean~s(k5me_horvath, bs="ps",k=10),data=df)
#Cubic Spline Curve
modelcr<-gam(Median_Mean~s(k5me_horvath, bs="cr",k=10),data=df)

?gam.check
# check model
gam.check(modelps)
summary(modelps)

# check linear model
lm_model<-lm(Median_Mean~k5me_horvath,data=df)
summary(lm_model)

# Check AIC; check the cubic and b-spline with the linear model 
AIC(modelcr,lm_model)
AIC(modelps,modelcr)

# Note the p-value is not significant, indicating not much improvement and AIC is similar to linear model over the linear model

#Sequence for smooth term prediction
x_seq<-seq(min(df$k5me_horvath),max(df$k5me_horvath),length.out=length(df$k5me_horvath))

# Predict partial smooth term and standard errors
partial_pred <-predict(modelps, newdata=data.frame(k5me_horvath=x_seq),type="terms",terms="s(k5me_horvath)",se.fit=T)
fit<-partial_pred$fit[,"s(k5me_horvath)"]
se <- partial_pred$se.fit[,"s(k5me_horvath)"]

# Create dataframe for plotting
plot_df<-data.frame(
  x=x_seq,
  fit=fit,
  upper=fit+2 *se,
  lower=fit - 2*se
)


# Get the smooth object from the model
smooth_obj<-modelps$smooth[[1]]
smooth_obj

# Extract the knots
knots<-smooth_obj$knots
x_range<-range(df$k5me_horvath)
knots_in_range<-knots[knots >= x_range[1] & knots <= x_range[2]]
knot_df<-data.frame(k5me_horvath=knots_in_range)
#knot_df$y_knot_pos<-min(plot_df$lower) -0.05 * diff(range(plot_df$fit))
#Line 114 does not run.
knot_df$y_knot_pos<-min(plot_df$lower) -0.05 * diff(range(plot_df$fit))
knot_df$fit<-NA
knot_pred<-predict(modelps, newdata=knot_df,type="terms",terms="s(k5me_horvath)")
knot_df$fit<-knot_pred[,"s(k5me_horvath)"]
#y_knot_pos<-min(plot_df$fit) - 0.1*diff(range(plot_df$fit))
#x_range<-range(x)
#knots_in_range<-knots[knots >= x_range[1] & knots <= x_range[2]]

#knot_df<-data.frame(x=knots_in_range)
#knot_df$fit<-predict(model_ps,newdata=knot_df)


ggplot(plot_df,aes(x=x,y=fit))+
  geom_line(color="blue",size=1) +
  geom_line(aes(y=upper),linetype="dotted",color="blue")+
  geom_line(aes(y=lower),linetype="dotted",color="blue")+
  #geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3,fill="lightblue") +
  geom_point(data=knot_df,aes(x= k5me_horvath,y=fit),color="red",shape=15,size=3) +
  labs(x="Horvath Pan-Tissue Estimator",y="Partial Effect on Grayscale Median") + 
  theme_minimal()+ 
  theme(axis.title = element_text (size = 28), axis.text = element_text (size = 20), 
        plot.title = element_text(size = 30,hjust = 0.5, face = "bold"))+ 
  ggtitle("Predicting Grayscale Median (GSM) As a Measure of DNAmAge Horvath Pan-Tissue Estimator")


# Set library path - add the path between the quotes.
.libPaths("/drives/drive1/ff_chaya/Angel/bin/")
# Install the following packages as needed

install.packages('gratia',contriburl='file:///mnt/modules/cran/src/contrib',type='source')
library(gratia)
library(ggplot2)
library(broom)
library(tidyr)
library(dplyr)
library(moments)
library(mgcv)

df<-readRDS("/drives/drive1/ff_chaya/Angel/bin/cubic_spline_code/df_LS_012826.rds")

##### Cubic Spline Function
cubic_spline_fun<-function(x,y,covar=NULL,k=10,xlabel=x,ylabel=y,plotlabel="Y vs X"){
  rhs<-paste0("s(",x,",bs='ps', k=",k,")")
  if(!is.null(covar)){
    rhs<-paste(rhs, paste(covar,collapse=" + "), sep=" + ")
  }
  # Build formula
  form<-as.formula(paste(y,"~",rhs))
  # fit model
  modelps<-gam(form,data=df)
  
  #Extract the sequence for smooth term prediction
  xmin<-min(df[[x]])
  xmax<-max(df[[x]])
  #x_seq<-seq(xmin,xmax,length.out=length(df[[x]]))
  x_seq<-seq(xmin,xmax,length.out=200)
  # Predict
  newdata<-data.frame(x_seq)
  names(newdata)<-x
  
  # Add covariates at mean/reference level
  if(!is.null(covar)){
    for(v in covar) {
      if(is.numeric(df[[v]])){
        newdata[[v]]<-mean(df[[v]],na.rm=TRUE)
      } else {
        newdata[[v]] <-levels(df[[v]])[1]
      }
    }
  }
  partial_pred <-predict(
    modelcr,
    newdata=newdata,type="response",se.fit=T)
  fit<-partial_pred$fit
  se <- partial_pred$se.fit
  # Create dataframe for plotting
  plot_df<-data.frame(
    x=x_seq,
    fit=fit,
    upper=fit+2 *se,
    lower=fit - 2*se
  )
  
  csplot<-ggplot(plot_df,aes(x=x,y=fit))+
    geom_line(color="blue",size=1) +
    geom_line(aes(y=upper),linetype="dotted",color="blue")+
    geom_line(aes(y=lower),linetype="dotted",color="blue")+
    #geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3,fill="lightblue") +
    #geom_point(data=knot_df,aes(x= Median_Mean,y=fit),color="red",shape=15,size=3) +
    labs(x=xlabel,y=paste0("Partial Effect On ",ylabel)) + 
    theme_minimal()+ 
    theme(axis.title = element_text (size = 22), axis.text = element_text (size = 20), 
          plot.title = element_text(size = 28,hjust = 0.5, face = "bold"))+ 
    ggtitle(plotlabel)
  return(csplot)
}

# Examples:
cubic_spline_fun(x="Median_Mean",y="le8_w_slp",
                 covar = c("ff_cm1ethrace"),plotlabel = "LE8 vs Grayscale Median-Mean adjusted for Sex")
cubic_spline_fun(x="Median_Mean",y="le8_w_slp",
                 covar = c("ff_cm1ethrace"),plotlabel =  "Grayscale Median-Mean adjusted for Race vs LE8",
                 xlabel = "Grayscale Median Mean",ylabel="LE8")



#ff_cm1ethrace = factor (ref_race, levels = levels (df$ff_cm1ethrace)), 
#ff_cm1bsex = factor (ref_sex, levels = levels (df$ff_cm1bsex)), 
#ff_cm1povca = factor (ref_pov, levels = levels (df$ff_cm1povca)), 
#ck7bmi = ref_bmi








gs_nona<-gs_newjoined[complete.cases(gs_newjoined$Median_Mean),]
gs_nona2<-gs_nona[complete.cases(gs_nona$le8_w_slp),]
# Check skewness - should be close to zero
skewness(gs_nona2$le8_w_slp)
# rename for clarity
df<-gs_nona2

# PS model
# B-spline Curve
modelps<-gam(le8_w_slp~s(Median_Mean, bs="ps",k=10)+ 
               ff_cm1ethrace + ff_cm1bsex + ff_cm1povca + ck7bmi,
             data=df, 
             method = "REML"
)
#Cubic Spline Curve
modelcr<-gam(le8_w_slp~s(Median_Mean, bs="cr",k=10),data=df)

?gam
# check model
gam.check(modelps)
summary(modelps)

# check linear model
lm_model<-lm(le8_w_slp~Median_Mean+ 
               ff_cm1ethrace + ff_cm1bsex + ff_cm1povca + ck7bmi,
             data=df
)
summary(lm_model)

# Check AIC; check the cubic and b-spline with the linear model 
AIC(modelcr,lm_model)
AIC(modelps,modelcr)

# Note the p-value is not significant, indicating not much improvement and AIC is similar to linear model over the linear model

#Sequence for smooth term prediction

pd<- plot (modelps, select = 1, se = T, shade = F, rug = F, seWithMean = F)

smooth_df <- data.frame (
  x = pd[[1]]$x, 
  fit = pd [[1]]$fit, 
  se = pd[[1]]$se
)

smooth_df$upper <- smooth_df$fit +2 * smooth_df$se
smooth_df$lower <- smooth_df$fit - 2 * smooth_df$se

q_vals <- quantile(df$Median_Mean, probs = seq (0.1, 0.9, by = 0.1), na.rm = T)

knot_df<- data.frame (Median_Mean = as.numeric (q_vals))
knot_df$fit <- approx (
  x = smooth_df$x, 
  y = smooth_df$fit, 
  xout = knot_df$Median_Mean
)$y

ggplot(plot_df,aes(x=x,y=fit))+
  geom_line(color="blue",size=1) +
  geom_line(aes(y=upper),linetype="dotted",color="blue")+
  geom_line(aes(y=lower),linetype="dotted",color="blue")+
  #geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3,fill="lightblue") +
  geom_point(data=knot_df,aes(x= Median_Mean,y=fit),color="red",shape=15,size=3) +
  labs(x="Grayscale Median (GSM)",y="Adjusted Partial Effect on LE8") + 
  theme_minimal()+ 
  theme(axis.title = element_text (size = 28), axis.text = element_text (size = 20), 
        plot.title = element_text(size = 30,hjust = 0.5, face = "bold"))+ 
  ggtitle("Multivariable GAM Adjusted for BMI, Race/Ethnicity, Income Level, and Sex")




DNAmages <- c("k5mk_horvath", "k5me_horvath", "k6mk_horvath", "k6me_horvath", "k5mk_skinblood", 
              "k5me_skinblood", "k6mk_skinblood", "k6me_skinblood", "k5mk_phenoage", "k5me_phenoage", 
              "k6mk_phenoage", "k6me_phenoage", "k6me_phenoage", "k5mk_grim", "k5me_grim", "k6mk_grim", 
              "k6me_grim", "k5mk_poam38", "k5me_poam38", "k6mk_poam38", "k6me_poam38", "k5mk_poam45", 
              "k5me_poam45", "k5me_poam45", "k6mk_poam45", "k6me_poam45")

lm <- lm(gs_newjoined$le8_w_slp ~ gs_newjoined$k6mk_phenoage)
summary (lm)
