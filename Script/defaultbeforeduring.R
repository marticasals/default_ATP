df_ATP <- readRDS("C:/Users/Victoria/Desktop/Master Bioestadistica/TFM/Default/df_ATP.rds")
library(readxl)
library(openxlsx)
library(dplyr)
library(rlang) 
library(nortest)
library(SmartEDA)
library(ggplot2)
library(compareGroups)
library(epiR)
library(tidyr)

df_ATP$Default_Before <- ifelse(is.na(df_ATP$Default_Type) | df_ATP$Default_Type != "Antes", "NO", "YES")
df_ATP$Default_During <- ifelse(is.na(df_ATP$Default_Type) | df_ATP$Default_Type != "Durante", "NO", "YES")


##Exploratory analysis.
##Analysis of the normality. 
qqnorm(df_ATP$winner_age, main = "QQ Plot of Winner Age")
qqline(df_ATP$winner_age)
lillie.test(x =  df_ATP$winner_age)

qqnorm(df_ATP$loser_age, main = "QQ Plot of Loser Age")
qqline(df_ATP$loser_age)
lillie.test(x =  df_ATP$loser_age)

qqnorm(df_ATP$dif_age, main = "QQ Plot of Age Difference")
qqline(df_ATP$dif_age)
lillie.test(x =  df_ATP$dif_age)

qqnorm(df_ATP$winner_rank, main = "QQ Plot of Winner Rank")
qqline(df_ATP$winner_rank)
lillie.test(x =  df_ATP$winner_rank)

qqnorm(df_ATP$loser_rank, main = "QQ Plot of Loser Rank")
qqline(df_ATP$loser_rank)
lillie.test(x =  df_ATP$loser_rank)

qqnorm(df_ATP$dif_rank, main = "QQ Plot of Rank Difference")
qqline(df_ATP$dif_rank)
lillie.test(x =  df_ATP$dif_rank)

qqnorm(df_ATP$games, main = "QQ Plot of Games")
qqline(df_ATP$games)
lillie.test(x =  df_ATP$games)

#Creation of an exploratory report.
ExpReport(df_ATP, Template = NULL, Target = NULL, label = NULL, theme = "Default", op_file = "report.html", op_dir = getwd(), sc = NULL, sn = NULL, Rc = NULL)

Summary_Cat<- ExpCTable(df_ATP, Target = NULL, margin = 1, clim = 10, nlim = 10, round = 2, bin = 3, per = FALSE, weight = NULL)
write.xlsx(Summary_Cat, "C:/Users/Victoria/Desktop/Master Bioestadistica/TFM/Default/Summary_Cat.csv", rowNames = FALSE)

Summary_Num<-ExpNumStat(df_ATP,by="A",gp=NULL,Qnt=c(0.25,0.75),MesofShape=2,Outlier=TRUE,round=2)
write.xlsx(Summary_Num, "C:/Users/Victoria/Desktop/Master Bioestadistica/TFM/Default/Summary_Num.csv", rowNames = FALSE)

selected_columns <- c("Vname", "mean", "median", "SD", "IQR","25%","75%")

Summary_numeric <- Summary_Num[, selected_columns]
write.xlsx(Summary_numeric, "C:/Users/Victoria/Desktop/Master Bioestadistica/TFM/Default/Summary_numeric.csv", rowNames = FALSE)

##Epidemiological analysis. 
#Calculation of PI and 95%CI
prop_default <- sum(df_ATP$Default == "YES") / nrow(df_ATP)
ci_default <- prop.test(sum(df_ATP$Default == "YES"), nrow(df_ATP))$conf.int
prop_per_1000_default <- prop_default * 1000
ci_per_1000_default <- prop.test(sum(df_ATP$Default == "YES"), nrow(df_ATP), conf.level = 0.95)$conf.int * 1000
defaults_before_yes <- sum(df_ATP$Default_Before == "YES", na.rm = TRUE)
defaults_during_yes <- sum(df_ATP$Default_During == "YES", na.rm = TRUE)
total_matches_before <- nrow(df_ATP) - defaults_during_yes
total_matches_during <- nrow(df_ATP) - defaults_before_yes
prop_per_1000_default_before <- (defaults_before_yes / total_matches_before) * 1000
prop_per_1000_default_during <- (defaults_during_yes / total_matches_during) * 1000
ci_default_before <- epi.conf(cbind(defaults_before_yes, total_matches_before - defaults_before_yes), ctype = "inc.rate", method = "exact")
ci_default_during <- epi.conf(cbind(defaults_during_yes, total_matches_during - defaults_during_yes), ctype = "inc.rate", method = "exact")
ci_per_1000_default_before_lower <- ci_default_before$lower * 1000
ci_per_1000_default_before_upper <- ci_default_before$upper * 1000
ci_per_1000_default_during_lower <- ci_default_during$lower * 1000
ci_per_1000_default_during_upper <- ci_default_during$upper * 1000
cat("Incidence proportion of Default per 1000 matches:", prop_per_1000_default, "%\n")
cat("Confidence interval for Default per 1000 matches:", ci_per_1000_default[1], "-", ci_per_1000_default[2], "%\n")
cat("Incidence proportion of Default:", prop_default, "\n")
cat("Confidence interval for Default:", ci_default[1], "-", ci_default[2], "\n")
cat("Incidence Proportion of Defaults 'Before' per 1000 matches:", prop_per_1000_default_before, "\n")
cat("Confidence interval for Defaults 'Before' per 1000 matches:", ci_per_1000_default_before_lower, "-", ci_per_1000_default_before_upper, "\n")
cat("Incidence Proportion of Defaults 'During' per 1000 matches:", prop_per_1000_default_during, "\n")
cat("Confidence interval for Defaults 'During' per 1000 matches:", ci_per_1000_default_during_lower, "-", ci_per_1000_default_during_upper, "\n")


#Analysis of the time trend of the Default.

default_cases_by_year <- df_ATP %>%
  group_by(year) %>%
  summarize(
    Total_Matches = n(),
    Defaults_Before = sum(Default_Before == "YES", na.rm = TRUE),
    Defaults_During = sum(Default_During == "YES", na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Total_Matches_Before = Total_Matches - Defaults_During,
    Total_Matches_During = Total_Matches - Defaults_Before,
    Incidence_Proportion_Before = (Defaults_Before / Total_Matches_Before) * 1000,
    Incidence_Proportion_During = (Defaults_During / Total_Matches_During) * 1000
  )

default_cases_by_year_long <- default_cases_by_year %>%
  select(year, Incidence_Proportion_Before, Incidence_Proportion_During) %>%
  pivot_longer(cols = c("Incidence_Proportion_Before", "Incidence_Proportion_During"), names_to = "Default_Type", values_to = "Incidence_Proportion")


ggplot(default_cases_by_year_long, aes(x = year, y = Incidence_Proportion, color = Default_Type)) +
  geom_line() +
  geom_smooth(method = "loess", se = TRUE) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(default_cases_by_year_long$year), max(default_cases_by_year_long$year), by = 2)) +
  scale_y_continuous(breaks = seq(0, max(default_cases_by_year_long$Incidence_Proportion, na.rm = TRUE), by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Incidence Proportion per 1000 Matches", x = "Year", color = "Default Type")

##Bivariate analysis.

#Summary of covariates by groups
summary_default_before <- compareGroups(Default_Before ~ tourney_level + surface + round + sets + games + winner_hand + loser_hand + winner_age + loser_age + dif_age + winner_rank + loser_rank + dif_rank, df_ATP, byrow = TRUE)
df_summary_default_before <- createTable(summary_default_before)
export2word(df_summary_default_before, file = 'summary_default_before.docx')

calculate_stats <- function(data, var_name) {
  data %>%
    group_by(Default_Before) %>%
    summarise(
      Median = median(!!sym(var_name), na.rm = TRUE),
      IQR = IQR(!!sym(var_name), na.rm = TRUE),
      Q1 = quantile(!!sym(var_name), 0.25, na.rm = TRUE),
      Q3 = quantile(!!sym(var_name), 0.75, na.rm = TRUE)
    ) %>%
    mutate(Variable = var_name)  
}

all_stats_default_before <- lapply(numeric_vars, function(var) calculate_stats(df_ATP, var)) %>%
  bind_rows()

print(all_stats_default_before)


summary_default_during <- compareGroups(Default_During ~ tourney_level + surface + round + sets + games + winner_hand + loser_hand + winner_age + loser_age + dif_age + winner_rank + loser_rank + dif_rank, df_ATP, byrow = TRUE)
df_summary_default_during <- createTable(summary_default_during)
export2word(df_summary_default_during, file = 'summary_default_during.docx')

calculate_stats <- function(data, var_name) {
  data %>%
    group_by(Default_During) %>%
    summarise(
      Median = median(!!sym(var_name), na.rm = TRUE),
      IQR = IQR(!!sym(var_name), na.rm = TRUE),
      Q1 = quantile(!!sym(var_name), 0.25, na.rm = TRUE),
      Q3 = quantile(!!sym(var_name), 0.75, na.rm = TRUE)
    ) %>%
    mutate(Variable = var_name)  
}

all_stats_default_during <- lapply(numeric_vars, function(var) calculate_stats(df_ATP, var)) %>%
  bind_rows()

print(all_stats_default_during)

#Epidemiological analysis by groups 

tabla_tourney_Default_Before <- table(df_ATP$tourney_level, df_ATP$Default_Before)
tabla_surface_Default_Before <- table(df_ATP$surface, df_ATP$Default_Before)
tabla_sets_Default_Before <- table(df_ATP$sets, df_ATP$Default_Before)
tabla_round_Default_Before <- table(df_ATP$round, df_ATP$Default_Before)

# Tourney level and def for Default_Before
Valores_M_GS_Before <- c(tabla_tourney_Default_Before["Masters", "YES"], tabla_tourney_Default_Before["Masters", "NO"], tabla_tourney_Default_Before["Grand Slams", "YES"], tabla_tourney_Default_Before["Grand Slams", "NO"])
epi.2by2(dat = Valores_M_GS_Before, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

Valores_250_500_GS_Before <- c(tabla_tourney_Default_Before["250 or 500", "YES"], tabla_tourney_Default_Before["250 or 500", "NO"], tabla_tourney_Default_Before["Grand Slams", "YES"], tabla_tourney_Default_Before["Grand Slams", "NO"])
epi.2by2(dat = Valores_250_500_GS_Before, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

Valores_TF_GS_Before <- c(tabla_tourney_Default_Before["Tour Finals", "YES"], tabla_tourney_Default_Before["Tour Finals", "NO"], tabla_tourney_Default_Before["Grand Slams", "YES"], tabla_tourney_Default_Before["Grand Slams", "NO"])
epi.2by2(dat = Valores_TF_GS_Before, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Surface and def for Default_Before
Valores_G_C_Before <- c(tabla_surface_Default_Before["Grass", "YES"], tabla_surface_Default_Before["Grass", "NO"], tabla_surface_Default_Before["Clay", "YES"], tabla_surface_Default_Before["Clay", "NO"])
epi.2by2(dat = Valores_G_C_Before, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

Valores_H_C_Before <- c(tabla_surface_Default_Before["Hard", "YES"], tabla_surface_Default_Before["Hard", "NO"], tabla_surface_Default_Before["Clay", "YES"], tabla_surface_Default_Before["Clay", "NO"])
epi.2by2(dat = Valores_H_C_Before, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

Valores_C_C_Before <- c(tabla_surface_Default_Before["Carpet", "YES"], tabla_surface_Default_Before["Carpet", "NO"], tabla_surface_Default_Before["Clay", "YES"], tabla_surface_Default_Before["Clay", "NO"])
epi.2by2(dat = Valores_C_C_Before, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Sets and def for Default_Before
epi.2by2(dat = tabla_sets_Default_Before, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Round and def for Default_Before
Valores_F_Q_Before <- c(tabla_round_Default_Before["Final", "YES"], tabla_round_Default_Before["Final", "NO"], tabla_round_Default_Before["Qualifying", "YES"], tabla_round_Default_Before["Qualifying", "NO"])
epi.2by2(dat = Valores_F_Q_Before, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

Valores_P_Q_Before <- c(tabla_round_Default_Before["Preliminary", "YES"], tabla_round_Default_Before["Preliminary", "NO"], tabla_round_Default_Before["Qualifying", "YES"], tabla_round_Default_Before["Qualifying", "NO"])
epi.2by2(dat = Valores_P_Q_Before, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")


tabla_tourney_Default_During <- table(df_ATP$tourney_level, df_ATP$Default_During)
tabla_surface_Default_During <- table(df_ATP$surface, df_ATP$Default_During)
tabla_sets_Default_During <- table(df_ATP$sets, df_ATP$Default_During)
tabla_round_Default_During <- table(df_ATP$round, df_ATP$Default_During)

# Tourney level and def for Default_During
Valores_M_GS_During <- c(tabla_tourney_Default_During["Masters", "YES"], tabla_tourney_Default_During["Masters", "NO"], tabla_tourney_Default_During["Grand Slams", "YES"], tabla_tourney_Default_During["Grand Slams", "NO"])
epi.2by2(dat = Valores_M_GS_During, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

Valores_250_500_GS_During <- c(tabla_tourney_Default_During["250 or 500", "YES"], tabla_tourney_Default_During["250 or 500", "NO"], tabla_tourney_Default_During["Grand Slams", "YES"], tabla_tourney_Default_During["Grand Slams", "NO"])
epi.2by2(dat = Valores_250_500_GS_During, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

Valores_TF_GS_During <- c(tabla_tourney_Default_During["Tour Finals", "YES"], tabla_tourney_Default_During["Tour Finals", "NO"], tabla_tourney_Default_During["Grand Slams", "YES"], tabla_tourney_Default_During["Grand Slams", "NO"])
epi.2by2(dat = Valores_TF_GS_During, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Surface and def for Default_During
Valores_G_C_During <- c(tabla_surface_Default_During["Grass", "YES"], tabla_surface_Default_During["Grass", "NO"], tabla_surface_Default_During["Clay", "YES"], tabla_surface_Default_During["Clay", "NO"])
epi.2by2(dat = Valores_G_C_During, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

Valores_H_C_During <- c(tabla_surface_Default_During["Hard", "YES"], tabla_surface_Default_During["Hard", "NO"], tabla_surface_Default_During["Clay", "YES"], tabla_surface_Default_During["Clay", "NO"])
epi.2by2(dat = Valores_H_C_During, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

Valores_C_C_During <- c(tabla_surface_Default_During["Carpet", "YES"], tabla_surface_Default_During["Carpet", "NO"], tabla_surface_Default_During["Clay", "YES"], tabla_surface_Default_During["Clay", "NO"])
epi.2by2(dat = Valores_C_C_During, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Sets and def for Default_During
epi.2by2(dat = tabla_sets_Default_During, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

# Round and def for Default_During
Valores_F_Q_During <- c(tabla_round_Default_During["Final", "YES"], tabla_round_Default_During["Final", "NO"], tabla_round_Default_During["Qualifying", "YES"], tabla_round_Default_During["Qualifying", "NO"])
epi.2by2(dat = Valores_F_Q_During, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

Valores_P_Q_During <- c(tabla_round_Default_During["Preliminary", "YES"], tabla_round_Default_During["Preliminary", "NO"], tabla_round_Default_During["Qualifying", "YES"], tabla_round_Default_During["Qualifying", "NO"])
epi.2by2(dat = Valores_P_Q_During, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

