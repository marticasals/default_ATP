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
cat("Incidence proportion of Default per 1000 matches:", prop_per_1000_default, "%\n")
cat("Confidence interval for Default per 1000 matches:", ci_per_1000_default[1], "-", ci_per_1000_default[2], "%\n")
cat("Incidence proportion of Default:", prop_default, "\n")
cat("Confidence interval for Default:", ci_default[1], "-", ci_default[2], "\n")
df_ATP$Default_Before <- ifelse(is.na(df_ATP$Default_Type) | df_ATP$Default_Type != "Before", "NO", "YES")
df_ATP$Default_During <- ifelse(is.na(df_ATP$Default_Type) | df_ATP$Default_Type != "During", "NO", "YES")

defaults_before_yes <- sum(df_ATP$Default_Before == "YES")
total_matches <- nrow(df_ATP)
prop_default_before <- defaults_before_yes / total_matches
ci_default_before <- prop.test(defaults_before_yes, total_matches)$conf.int
prop_per_1000_default_before <- prop_default_before * 1000
ci_per_1000_default_before <- ci_default_before * 1000

cat("Incidence Proportion of Defaults 'Before' per 1000 matches:", prop_per_1000_default_before, "\n")
cat("Confidence interval for Defaults 'Before' per 1000 matches:", ci_per_1000_default_before[1], "-", ci_per_1000_default_before[2], "\n")

defaults_during_yes <- sum(df_ATP$Default_During == "YES")
prop_default_during <- defaults_during_yes / total_matches
ci_default_during <- prop.test(defaults_during_yes, total_matches)$conf.int
prop_per_1000_default_during <- prop_default_during * 1000
ci_per_1000_default_during <- ci_default_during * 1000

cat("Incidence Proportion of defaults 'During' per 1000 matches:", prop_per_1000_default_during, "\n")
cat("Confidence interval for defaults 'During' per 1000 matches:", ci_per_1000_default_during[1], "-", ci_per_1000_default_during[2], "\n")

#Analysis of the time trend of the Default.
default_cases_by_year <- df_ATP %>%
  group_by(year) %>%
  summarize(
    Total_Matches = n(),
    Defaults = sum(Default == "YES", na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(Incidence_Proportion = (Defaults / Total_Matches) * 1000)

ggplot(default_cases_by_year, aes(x = year, y = Incidence_Proportion)) +
  geom_line() +  
  geom_smooth(method = "loess", se = TRUE, color = "blue") +  
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(default_cases_by_year$year), max(default_cases_by_year$year), by = 2)) +  
  scale_y_continuous(breaks = seq(0, max(default_cases_by_year$Incidence_Proportion), by = 1)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  labs(y = "Incidence Proportion per 1000 Matches",
       x = "Year")

##Bivariate analysis.

#Summary of covariates by groups
summary_default<-compareGroups(Default~tourney_level+surface+round+sets+games+winner_hand+loser_hand+winner_age+loser_age+dif_age+winner_rank+loser_rank+dif_rank, df_ATP, byrow = TRUE)
df_summary_default<- createTable(summary_default)
export2word(df_summary_default, file='summary_default.docx')

numeric_vars <- c("winner_age", "loser_age", "dif_age", "winner_rank", "loser_rank", "dif_rank")

calculate_stats <- function(data, var_name) {
  data %>%
    group_by(Default) %>%
    summarise(
      Median = median(!!sym(var_name), na.rm = TRUE),
      IQR = IQR(!!sym(var_name), na.rm = TRUE),
      Q1 = quantile(!!sym(var_name), 0.25, na.rm = TRUE),
      Q3 = quantile(!!sym(var_name), 0.75, na.rm = TRUE)
    ) %>%
    mutate(Variable = var_name)  
}

all_stats <- lapply(numeric_vars, function(var) calculate_stats(df_ATP, var)) %>%
  bind_rows()

print(all_stats)

#Epidemiological analysis by groups 

#Tourney level and def
tabla_tourney_Default <- table(df_ATP$tourney_level, df_ATP$Default)
Valores_M_GS <- c(tabla_tourney_Default["Masters", "YES"], tabla_tourney_Default["Masters", "NO"], tabla_tourney_Default["Grand Slams", "YES"], tabla_tourney_Default["Grand Slams", "NO"])
epi.2by2(dat = Valores_M_GS, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

Valores_250.500_GS <- c(tabla_tourney_Default["250 or 500", "YES"], tabla_tourney_Default["250 or 500", "NO"], tabla_tourney_Default["Grand Slams", "YES"], tabla_tourney_Default["Grand Slams", "NO"])
epi.2by2(dat = Valores_250.500_GS, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

Valores_TF_GS<- c(tabla_tourney_Default["Tour Finals", "YES"], tabla_tourney_Default["Tour Finals", "NO"], tabla_tourney_Default["Grand Slams", "YES"], tabla_tourney_Default["Grand Slams", "NO"])
epi.2by2(dat = Valores_TF_GS, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

#Surface and def
(tabla_surface_Default <- table(df_ATP$surface, df_ATP$Default))

Valores_G_C<- c(tabla_surface_Default["Grass", "YES"], tabla_surface_Default["Grass", "NO"], tabla_surface_Default["Clay", "YES"], tabla_surface_Default["Clay", "NO"])

epi.2by2(dat = Valores_G_C, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

Valores_H_C<- c(tabla_surface_Default["Hard", "YES"], tabla_surface_Default["Hard", "NO"], tabla_surface_Default["Clay", "YES"], tabla_surface_Default["Clay", "NO"])

epi.2by2(dat = Valores_H_C, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

Valores_C_C<- c( tabla_surface_Default["Carpet", "YES"], tabla_surface_Default["Carpet", "NO"], tabla_surface_Default["Clay", "YES"], tabla_surface_Default["Clay", "NO"])

epi.2by2(dat = Valores_C_C, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

#Sets and def
(tabla_sets_Default <- table(df_ATP$sets, df_ATP$Default))

epi.2by2(dat = tabla_sets_Default, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

#Round and def
(tabla_round_Default <- table(df_ATP$round, df_ATP$Default))

Valores_F_Q<- c(
  tabla_round_Default["Final", "YES"],
  tabla_round_Default["Final", "NO"],
  tabla_round_Default["Qualifying", "YES"],
  tabla_round_Default["Qualifying", "NO"]
)

epi.2by2(dat = Valores_F_Q, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")

Valores_P_Q<- c(
  tabla_round_Default["Preliminary", "YES"],
  tabla_round_Default["Preliminary", "NO"],
  tabla_round_Default["Qualifying", "YES"],
  tabla_round_Default["Qualifying", "NO"]
)

epi.2by2(dat = Valores_P_Q, method = "cohort.count", digits = 2,
         conf.level = 0.95, units = 1000, interpret = FALSE, outcome = "as.columns")


