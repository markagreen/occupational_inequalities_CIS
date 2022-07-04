#################################
### Occupational Inequalities ###
####### England summary #########
#################################

# Purpose: To investigate the drivers of inequalities in COVID-19 transmission within Hackney.

# Set libraries folder
lib_base="P:\\Working\\Libraries"
assign(".lib.loc", lib_base, envir = environment(.libPaths))
rm(lib_base)

# Libraries
library(readstata13)
library(data.table)
library(ggplot2)
library(tidyr)
library(lme4)

# Set options
options(scipen = 999) # Stop printing decimal places as e+10 etc


# 1. Load and tidy data #


# Load COVID Infection Survey
cis <- data.table(read.dta13("//vmlfps04v/DataStore2$/COVID-19 Infection Survey, Monthly/CIS_Extract_2021_02_03/Extract_20210203.dta")) # Load
cis$visit_date[cis$visit_date == "2020-01-03"] <- "2021-01-03" # Correct erroneous data (so same as sample taken date)

# Subset data
cis <- cis[cis$visit_date >= "2020-08-01" & cis$visit_date < "2021-02-01"] # 1st August 2020 to 31st January 2021
cis <- cis[cis$age_at_visit >= 18] # Adults only

# Create outcome variable for analysis
cis$positive <- NA 
cis$positive[cis$result_mk == "Positive"] <- 1 # If positive test
cis$positive[cis$result_mk == "Negative"] <- 0 # Else 0

# Tidy ethnicity variable 1
cis$ethnic_small <- NA
cis$ethnic_small[cis$ethnicity == "White-British"] <- "White-British"
cis$ethnic_small[cis$ethnicity == "White-Irish" | cis$ethnicity == "White-Irish" | cis$ethnicity == "White-Gypsy or Irish Traveller" | cis$ethnicity == "Any other white background"] <- "White-Other"
cis$ethnic_small[cis$ethnicity == "Mixed-White & Black Caribbean" | cis$ethnicity == "Mixed-White & Black African" | cis$ethnicity == "Mixed-White & Asian" | cis$ethnicity == "Any other Mixed background"] <- "Mixed"
cis$ethnic_small[cis$ethnicity == "Asian or Asian British-Indian" | cis$ethnicity == "Asian or Asian British-Pakistani" | cis$ethnicity == "Asian or Asian British-Bangladeshi" | cis$ethnicity == "Asian or Asian British-Chinese" | cis$ethnicity == "Any other Asian background"] <- "Asian"
cis$ethnic_small[cis$ethnicity == "Black,Caribbean,African-African" | cis$ethnicity == "Black,Caribbean,Afro-Caribbean" | cis$ethnicity == "Black,Caribbean,Afro-Caribbean"] <- "Black"
cis$ethnic_small[cis$ethnicity == "Other ethnic group-Arab" | cis$ethnicity == "Any other ethnic group"] <- "Other"

# Tidy ethnicity variable 2
cis$ethnicity2 <- as.character(cis$ethnicity)
cis$ethnicity2[cis$ethnicity == "White-Gypsy or Irish Traveller"] <- "Any other white background"
cis$ethnicity2[cis$ethnicity == "Mixed-White & Black Caribbean"] <- "Mixed-White & Black"
cis$ethnicity2[cis$ethnicity == "Mixed-White & Black African"] <- "Mixed-White & Black"
cis$ethnicity2[cis$ethnicity == "Asian or Asian British-Bangladeshi"] <- "Any other ethnic group"
cis$ethnicity2[cis$ethnicity == "Other ethnic group-Arab"] <- "Any other ethnic group"
cis$ethnicity2[cis$ethnicity == "Any other Asian background"] <- "Any other ethnic group"
cis$ethnicity2[cis$ethnicity == "Any other Black background"] <- "Any other ethnic group"
cis$ethnicity2[cis$ethnicity == "Any other Mixed background"] <- "Any other ethnic group"

# Tidy work status
cis$work_status_v1 <- as.character(cis$work_status_v1)
cis$work_status_v1[cis$work_status_v1 == "Child under 5y not attending child care"] <- "Child under 5y"
cis$work_status_v1[cis$work_status_v1 == "Child under 5y attending child care"] <- "Child under 5y"
cis$work_status_v1[cis$work_status_v1 == "Retired"] <- NA

# Tidy work sector
cis$work_sector[cis$work_sector == "NA(Not currently working)"] <- NA

# Define reference groups in regression analyses
cis$ethnic_small <- as.factor(cis$ethnic_small) # Set as factor
cis$ethnic_small <- relevel(cis$ethnic_small, ref = "White-British") # Set reference category (largest category)
cis$work_status_v1 <- as.factor(cis$work_status_v1) # Set as factor
cis$work_status_v1 <- relevel(cis$work_status_v1, ref = "Employed and currently working") # Set reference category (largest category)
cis$work_sector <- as.factor(cis$work_sector) # Set as factor
cis$work_sector <- relevel(cis$work_sector, ref = "Information technology and communication") # Set reference category
cis$ethnicity2 <- as.factor(cis$ethnicity2) # Set as factor
cis$ethnicity2 <- relevel(cis$ethnicity2, ref = "White-British") # Set reference category (largest category)

# Identify time periods
cis$month <- month(cis$visit_date) # Identify month from date (numeric)
#cis$week <- format(cis$visit_date, "%Y-%V") # Week and year
#cis$week[cis$week == "2021-53"] <- "2020-53" # As final week spans year, need to convert year else will appear as last data point at end of 2021
cis$week <- lubridate::floor_date(cis$visit_date, "week") # Week but defined as monday of the week

# Recode data as missing data
cis$geography_code[cis$geography_code == ""] <- NA # Define as missing



# 2. Descriptive statistics # 


# Table 1
mean(cis$age_at_visit)
sd(cis$age_at_visit)
table(cis$sex, exclude = NULL)
table(cis$ethnicity2, exclude = NULL)
table(cis$work_sector, exclude = NULL)
table(cis$month, exclude = NULL)
table(cis$work_status, exclude = NULL)
table(cis$travel_abroad, exclude = NULL)
mean(cis$hhsize)
sd(cis$hhsize)
table(cis$result_mk, exclude = NULL)


# 3. Summary plots #


# How does COVID=19 prevalence vary over time #

# Aggregate counts of tests by sex, week and outcome
trends_data <- cis[, list(count = .N), by = c("week", "result_mk", "sex")]

# Calculate percentage of tests as positive
trends_data_wide <- spread(trends_data, result_mk, count)
trends_data_wide$Positive[is.na(trends_data_wide$Positive)] <- 0 # Fill in 0s
trends_data_wide$Negative[is.na(trends_data_wide$Negative)] <- 0 # Fill in 0s
trends_data_wide$total <- trends_data_wide$Negative + trends_data_wide$Positive # Create total
trends_data_wide$pc_positive <- (trends_data_wide$Positive / trends_data_wide$total) * 100 # Create percentage
trends_data_wide$pc_positive[is.nan(trends_data_wide$pc_positive)] <- 0 # Fill in missing data
trends_data_wide <- trends_data_wide[!is.na(trends_data_wide$sex),] # Drop missing sex
trends_data_wide <- trends_data_wide[trends_data_wide$Positive >= 10] # Supress low counts

# Plot percentage of tests that were positive
ggplot(trends_data_wide, aes(x = week, y = pc_positive, group=sex, color=sex)) +
  geom_point() +
  geom_path() +
  xlab("Date") +
  ylab("Percentage of tests positive") +
  scale_x_date(date_breaks = "months", date_labels = "%b")  +
  labs(color = "Sex")

# Save data for plot
trends_data_wide$Void <- NULL # Drop columns do not need
trends_data_wide$`<NA>` <- NULL
write.csv(trends_data_wide, "./Outputs/CIS Occupational LA Reports/Eng report/plot1.csv")

# Is there are variation in risk of COVID-19 by age and sex #

# Aggregate counts of tests by sex, age and outcome
age_count <- cis[, list(count = .N), by = c("result_mk", "age_at_visit", "sex")]

# Calculate percentage of tests as positive
age_count_wide <- spread(age_count, result_mk, count)
age_count_wide$total <- age_count_wide$Negative + age_count_wide$Positive
age_count_wide$pc_positive <- (age_count_wide$Positive / age_count_wide$total) * 100
age_count_wide$pc_positive[is.nan(age_count_wide$pc_positive)] <- 0 # Fill in missing data
age_count_wide <- age_count_wide[age_count_wide$Positive >= 10] # Supress low counts

# Plot
ggplot(age_count_wide, aes(x = age_at_visit, y = pc_positive, color = sex)) +
  geom_point() +
  geom_path() +
  ylim(0, max(age_count_wide$pc_positive)) +
  xlab("Age (years)") +
  ylab("Percentage of tests positive")

# Save data for plot
age_count_wide$Void <- NULL # Drop columns do not need
age_count_wide$`<NA>` <- NULL
write.csv(age_count_wide, "./Outputs/CIS Occupational LA Reports/Eng report/plot2.csv")


# Ethnicity descriptives #

# Aggregate counts of tests by sex, age and outcome
eth_count <- cis[, list(count = .N), by = c("result_mk", "ethnicity2")]

# Calculate percentage of tests as positive
eth_count_wide <- spread(eth_count, result_mk, count)
eth_count_wide$total <- eth_count_wide$Negative + eth_count_wide$Positive
eth_count_wide$pc_positive <- (eth_count_wide$Positive / eth_count_wide$total) * 100
eth_count_wide$pc_positive[is.nan(eth_count_wide$pc_positive)] <- 0 # Fill in missing data
eth_count_wide <- eth_count_wide[eth_count_wide$Positive >= 10] # Supress low counts

# Plot
ggplot(eth_count_wide, aes(x = ethnicity2, y = pc_positive)) +
  geom_bar(stat = "identity") +
  ylim(0, max(eth_count_wide$pc_positive)) +
  xlab("Ethnic group") +
  ylab("Percentage of tests positive") +
  coord_flip()

# Save data for plot
eth_count_wide$Void <- NULL # Drop columns do not need
eth_count_wide$`<NA>` <- NULL
write.csv(eth_count_wide, "./Outputs/CIS Occupational LA Reports/Eng report/plot3.csv")


# Which work status categories at higher or lower risk of COVID-19 #

# Aggregate counts of tests by sex, age and outcome
wst_count <- cis[, list(count = .N), by = c("result_mk", "work_status")]

# Calculate percentage of tests as positive
wst_count_wide <- spread(wst_count, result_mk, count)
wst_count_wide$total <- wst_count_wide$Negative + wst_count_wide$Positive
wst_count_wide$pc_positive <- (wst_count_wide$Positive / wst_count_wide$total) * 100
wst_count_wide$pc_positive[is.nan(wst_count_wide$pc_positive)] <- 0 # Fill in missing data
wst_count_wide <- wst_count_wide[wst_count_wide$Positive >= 10] # Supress low counts

# Plot
ggplot(wst_count_wide, aes(x = work_status, y = pc_positive)) +
  geom_bar(stat = "identity") +
  ylim(0, max(wst_count_wide$pc_positive)) +
  xlab("Work status") +
  ylab("Percentage of tests positive") +
  coord_flip()

# Save data for plot
wst_count_wide$Void <- NULL # Drop columns do not need
wst_count_wide$`<NA>` <- NULL
write.csv(wst_count_wide, "./Outputs/CIS Occupational LA Reports/Eng report/plot4.csv")


# Which work sectors at higher or lower risk of COVID-19 #

# Aggregate counts of tests by sex, age and outcome
ws_count <- cis[, list(count = .N), by = c("result_mk", "work_sector")]

# Calculate percentage of tests as positive
ws_count_wide <- spread(ws_count, result_mk, count)
ws_count_wide$total <- ws_count_wide$Negative + ws_count_wide$Positive
ws_count_wide$pc_positive <- (ws_count_wide$Positive / ws_count_wide$total) * 100
ws_count_wide$pc_positive[is.nan(ws_count_wide$pc_positive)] <- 0 # Fill in missing data
ws_count_wide <- ws_count_wide[ws_count_wide$Positive >= 10] # Supress low counts

# Plot
ggplot(ws_count_wide, aes(x = work_sector, y = pc_positive)) +
  geom_bar(stat = "identity") +
  ylim(0, max(ws_count_wide$pc_positive)) +
  xlab("Work sector") +
  ylab("Percentage of tests positive") +
  coord_flip()

# Save data for plot
ws_count_wide$Void <- NULL # Drop columns do not need
ws_count_wide$`<NA>` <- NULL
write.csv(ws_count_wide, "./Outputs/CIS Occupational LA Reports/Eng report/plot5.csv")


# 4. Analytical modelling #

# Set month as reference group
cis$month[cis$month == 1] <- 13 # To place in temporal order
cis$month <- as.factor(cis$month) # Set as factor
cis$month <- relevel(cis$month, ref = "8") # Set reference category (August)

# Select sequence of observations
cis <- cis[, sequence := sequence(.N), by = c("participant_id", "month")]

# Create age squared
cis$age2 <- cis$age_at_visit * cis$age_at_visit

# Scale numeric variables
cis$age <- scale(cis$age_at_visit, center = T, scale = T)
cis$age2 <- scale(cis$age2, center = T, scale = T)
cis$hhsize <- scale(cis$hhsize, center = T, scale = T)

# a. Work status as covariate #

# i. Females #

# Fixed
ws1_all <- glmer(positive ~ sex + age + age2 + ethnicity2 + work_status + factor(month) + travel_abroad + hhsize + (1|participant_id) + (1|geography_code), family = binomial(link="logit"), data = cis, nAGQ = 0)

# Interactions
ws1_all2 <- glmer(positive ~ age + age2 + ethnicity2 + sex*work_status*factor(month) + travel_abroad + hhsize + (1|participant_id) + (1|geography_code), family = binomial(link="logit"), data = cis, nAGQ = 0)


# b. work sector #

# Subset only working adults
for_analysis <- cis[cis$work_status_v1 == "Employed and currently working" | cis$work_status_v1 == "Self-employed and currently working"] # n=1121684
for_analysis$work_location[for_analysis$work_location == ">=75y"] <- NA # n = 4936 (plus 34 missing)
for_analysis$work_location[for_analysis$work_location == "Not applicable, not currently working"] <- NA # n = 10799

# Set month as reference group
for_analysis$work_location <- as.factor(for_analysis$work_location) # Set as factor
for_analysis$work_location <- relevel(for_analysis$work_location, ref = "Working from home") # Set reference category 

# Overall
ws2_all <- glmer(positive ~ sex + age + age2 + ethnicity2 + hhsize + work_sector + factor(month) + work_location + travel_abroad + hhsize + (1|participant_id) + (1|geography_code), family = binomial(link="logit"), data = for_analysis, nAGQ = 0)

# Interactions
ws2_all2 <- glmer(positive ~ age + age2 + ethnicity2 + hhsize + sex*work_sector*factor(month) + work_location + travel_abroad + hhsize + (1|participant_id) + (1|geography_code), family = binomial(link="logit"), data = for_analysis, nAGQ = 0)


# 5. Subgroup analysis - Age #

# a. work status #

# Females
ws1_young <- glmer(positive ~ age + age2 + ethnicity2 + sex*work_status*factor(month) + travel_abroad + hhsize + (1|participant_id) + (1|geography_code), family = binomial(link="logit"), data = cis[age_at_visit < 40], nAGQ = 0)

ws1_older <- glmer(positive ~ age + age2 + ethnicity2 + sex*work_status*factor(month) + travel_abroad + hhsize + (1|participant_id) + (1|geography_code), family = binomial(link="logit"), data = cis[age_at_visit >= 40], nAGQ = 0)

# b. work sector #

# Females
ws2_young <- glmer(positive ~ age + age2 + ethnicity2 + sex*work_sector*factor(month) + travel_abroad + hhsize + (1|participant_id) + (1|geography_code), family = binomial(link="logit"), data = for_analysis[for_analysis$age_at_visit < 40], nAGQ = 0)

ws2_older <- glmer(positive ~ age + age2 + ethnicity2 + sex*work_sector*factor(month) + travel_abroad + hhsize + (1|participant_id) + (1|geography_code), family = binomial(link="logit"), data = for_analysis[for_analysis$age_at_visit >= 40], nAGQ = 0)

# Save model summaries
source("./Scripts/clean_reg_models_eng_report.R")


# 6. Predicted probabilities - quick version for visualising results #

# Work status
predframe <- cis[,c("positive", "sex", "age", "age2", "ethnicity2", "work_status", "travel_abroad", "hhsize", "month", "participant_id")] # Subset dataset for making predictions for individuals
preds <- as.data.frame(predict(ws1_all2, newdata = predframe, type = "response", re.form=NA))
predframe <- cbind(predframe, preds) # Combine with individual level data
dt1 <- data.table(predframe) # Store as data.table
dt1 <- dt1[, list(count = .N, positive = sum(positive, na.rm=T), fit = mean(`predict(ws1_all2, newdata = predframe, type = \"response\", re.form = NA)`, na.rm=T), sd = sd(`predict(ws1_all2, newdata = predframe, type = \"response\", re.form = NA)`, na.rm=T)), by = c("work_status", "month", "sex")] # Aggregate for plotting
dt1 <- dt1[complete.cases(dt1[,1:3])] # Drop NAs on covariates
write.csv(dt1, "./Outputs/CIS Occupational LA Reports/Eng report/table5.csv") # Save

# Work sector
predframe <- for_analysis[,c("positive", "sex", "age", "age2", "ethnicity2", "work_sector", "work_location", "travel_abroad", "hhsize", "month", "participant_id")] # Subset dataset for making predictions for individuals
preds <- as.data.frame(predict(ws2_all2, newdata = predframe, type = "response", re.form=NA))
predframe <- cbind(predframe, preds) # Combine with individual level data
dt2 <- data.table(predframe) # Store as data.table
dt2 <- dt2[, list(count = .N, positive = sum(positive, na.rm=T), fit = mean(`predict(ws2_all2, newdata = predframe, type = \"response\", re.form = NA)`, na.rm=T), sd = sd(`predict(ws2_all2, newdata = predframe, type = \"response\", re.form = NA)`, na.rm=T)), by = c("work_sector", "month", "sex")] # Aggregate for plotting
dt2 <- dt2[complete.cases(dt2[,1:3])] # Drop NAs on covariates
write.csv(dt2, "./Outputs/CIS Occupational LA Reports/Eng report/table6.csv") # Save

# Work status - younger
hold <- cis[cis$age_at_visit < 40] # Subset only adults < 40
predframe <- hold[,c("positive", "sex", "age", "age2", "ethnicity2", "work_status", "travel_abroad", "hhsize", "month", "participant_id")] # Subset dataset for making predictions for individuals
preds <- as.data.frame(predict(ws1_young, newdata = predframe, type = "response", re.form=NA))
predframe <- cbind(predframe, preds) # Combine with individual level data
dt3 <- data.table(predframe) # Store as data.table
dt3 <- dt3[, list(count = .N, positive = sum(positive, na.rm=T), fit = mean(`predict(ws1_young, newdata = predframe, type = \"response\", re.form = NA)`, na.rm=T), sd = sd(`predict(ws1_young, newdata = predframe, type = \"response\", re.form = NA)`, na.rm=T)), by = c("work_status", "month", "sex")] # Aggregate for plotting
dt3 <- dt3[complete.cases(dt3[,1:3])] # Drop NAs on covariates
write.csv(dt3, "./Outputs/CIS Occupational LA Reports/Eng report/table7.csv") # Save

# Work status - older
hold <- cis[cis$age_at_visit >= 40] # Subset only adults < 40
predframe <- hold[,c("positive", "sex", "age", "age2", "ethnicity2", "work_status", "travel_abroad", "hhsize", "month", "participant_id")] # Subset dataset for making predictions for individuals
preds <- as.data.frame(predict(ws1_older, newdata = predframe, type = "response", re.form=NA))
predframe <- cbind(predframe, preds) # Combine with individual level data
dt4 <- data.table(predframe) # Store as data.table
dt4 <- dt4[, list(count = .N, positive = sum(positive, na.rm=T), fit = mean(`predict(ws1_older, newdata = predframe, type = \"response\", re.form = NA)`, na.rm=T), sd = sd(`predict(ws1_older, newdata = predframe, type = \"response\", re.form = NA)`, na.rm=T)), by = c("work_status", "month", "sex")] # Aggregate for plotting
dt4 <- dt4[complete.cases(dt4[,1:3])] # Drop NAs on covariates
write.csv(dt4, "./Outputs/CIS Occupational LA Reports/Eng report/table8.csv") # Save


# Work sector - younger
hold <- for_analysis[for_analysis$age_at_visit < 40] # Subset only adults < 40
predframe <- hold[,c("positive", "sex", "age", "age2", "ethnicity2", "work_sector", "work_location", "travel_abroad", "hhsize", "month", "participant_id")] # Subset dataset for making predictions for individuals
preds <- as.data.frame(predict(ws2_young, newdata = predframe, type = "response", re.form=NA))
predframe <- cbind(predframe, preds) # Combine with individual level data
dt5 <- data.table(predframe) # Store as data.table
dt5 <- dt5[, list(count = .N, positive = sum(positive, na.rm=T), fit = mean(`predict(ws2_young, newdata = predframe, type = \"response\", re.form = NA)`, na.rm=T), sd = sd(`predict(ws2_young, newdata = predframe, type = \"response\", re.form = NA)`, na.rm=T)), by = c("work_sector", "month", "sex")] # Aggregate for plotting
dt5 <- dt5[complete.cases(dt5[,1:3])] # Drop NAs on covariates
write.csv(dt5, "./Outputs/CIS Occupational LA Reports/Eng report/table9.csv") # Save

# Work sector - older
hold <- for_analysis[for_analysis$age_at_visit >= 40] # Subset only adults < 40
predframe <- hold[,c("positive", "sex", "age", "age2", "ethnicity2", "work_sector", "work_location", "travel_abroad", "hhsize", "month", "participant_id")] # Subset dataset for making predictions for individuals
preds <- as.data.frame(predict(ws2_older, newdata = predframe, type = "response", re.form=NA))
predframe <- cbind(predframe, preds) # Combine with individual level data
dt6 <- data.table(predframe) # Store as data.table
dt6 <- dt6[, list(count = .N, positive = sum(positive, na.rm=T), fit = mean(`predict(ws2_older, newdata = predframe, type = \"response\", re.form = NA)`, na.rm=T), sd = sd(`predict(ws2_older, newdata = predframe, type = \"response\", re.form = NA)`, na.rm=T)), by = c("work_sector", "month", "sex")] # Aggregate for plotting
dt6 <- dt6[complete.cases(dt6[,1:3])] # Drop NAs on covariates
write.csv(dt6, "./Outputs/CIS Occupational LA Reports/Eng report/table10.csv") # Save

# Plot
ggplot(dt1, aes(x = month, y = fit, group = sex, color = sex)) +
  geom_point() +
  facet_wrap(~work_status) +
  ylab("Predicted probability") +
  xlab("Month")


# 7. Predicted probabilities - Bootstrap method (slow) #

predframe <- cis[,c("sex", "age", "age2", "ethnicity2", "work_status", "travel_abroad", "hhsize", "month", "participant_id")] # Subset dataset for making predictions for individuals

# Define functions
# Make predictions
mySumm <- function(.){
  predict(., newdata = predframe, type = "response", re.form=NA) 
}
# Extract key measures from bootstrap
sumBoot <- function(merBoot) {
  return(
    data.frame(fit = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=0.5, na.rm=T))),
               lwr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=0.025, na.rm=T))),
               upr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=0.975, na.rm=T)))
    )
  )
}
# Estimate predictions for each model
boot1 <- lme4::bootMer(ws1_all, mySumm, nsim = 100, use.u = F, type = "parametric") # Make predictions (~30mins per nsim)
preds <- sumBoot(boot1) # Extract key measures (~15mins)
predframe <- cbind(predframe, preds) # Combine with individual level data
dt1 <- data.table(predframe) # Store as data.table
dt1 <- dt1[, list(count = .N, pred = mean(fit, na.rm=T), lwr = mean(lwr, na.rm=T), upr = mean(upr, na.rm=T)), by = c("work_status", "month")] # Aggregate for plotting

# Plot
ggplot(dt1, aes(x = month, y = pred)) +
  geom_point() +
  geom_pointrange(aes(ymin = lwr, ymax = upr), size = 0.1) + # or geom_errorbar (width = 0.2)
  facet_wrap(~work_status) +
  ylab("Predicted probability") +
  xlab("Month")
