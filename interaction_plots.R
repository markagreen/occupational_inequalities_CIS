############################################
### Creating interaction plots for paper ###
############################################


## Purpose: Create plots for interaction models.

# Libraries
library(patchwork)
library(ggplot2)
library(viridis)

## Model 1 - all ##

# Load data and tidy
model1_all <- read.csv("./20210304 MG Pub 2001021/model1_all.csv") # Load

# Tidy up work status variable
model1_all$work_status <- as.character(model1_all$work_status)
model1_all <- model1_all[!is.na(model1_all$work_status),] # Drop missing data summary
model1_all$work_status[model1_all$work_status == "Furloughed (temporarily not working)"] <- "Furloughed"
model1_all$work_status[model1_all$work_status == "Not working (unemployed, retired, long-term sick etc.)"] <- "Not working"

# Convert to percentages
model1_all$fit <- model1_all$fit * 100
model1_all$lwr <- model1_all$lwr * 100
model1_all$upr <- model1_all$upr * 100

# Plot
ggplot(model1_all, aes(x = month, y = fit, group=sex, color=sex)) +
  geom_point(position=position_dodge(width = 0.5)) +
  #geom_path() +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.5)) +
  facet_wrap(~work_status) +
  xlab("Month") +
  ylab("Predicted percent of positive tests (%)") +
  #scale_x_date(date_breaks = "months", date_labels = "%b")  +
  labs(color = "Sex") +
  scale_x_continuous(breaks = c(8,9,10,11,12,13), labels = c("Aug", "Sept", "Oct", "Nov", "Dec", "Jan")) + # Revise month from number to label
theme(text = element_text(size=16), axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) # Set text size and revolve x-axis to improve spacing

# Save
ggsave("./Plots/model1_all.jpeg", dpi = 300)
rm(model1_all)


## Model 2- all ##

# Load data and tidy
model2_all <- read.csv("./20210304 MG Pub 2001021/model2_all.csv") # Load

# Tidy up work sector variable
model2_all$work_sector <- as.character(model2_all$work_sector)
model2_all <- model2_all[!is.na(model2_all$work_sector),] # Drop missing data summary
model2_all$work_sector[model2_all$work_sector == "Arts,Entertainment or Recreation"] <- "Entertainment"
model2_all$work_sector[model2_all$work_sector == "Civil service or Local Government"] <- "Civil service"
model2_all$work_sector[model2_all$work_sector == "Financial services incl. insurance"] <- "Finance"
model2_all$work_sector[model2_all$work_sector == "Food production, agriculture, farming"] <- "Food production"
model2_all$work_sector[model2_all$work_sector == "Hospitality (e.g. hotel, restaurant)"] <- "Hospitality"
model2_all$work_sector[model2_all$work_sector == "Information technology and communication"] <- "ICT"
model2_all$work_sector[model2_all$work_sector == "Manufacturing or construction"] <- "Manufacturing"
model2_all$work_sector[model2_all$work_sector == "Other occupation sector"] <- "Other"
model2_all$work_sector[model2_all$work_sector == "Personal services (e.g. hairdressers)"] <- "Personal services"
model2_all$work_sector[model2_all$work_sector == "Retail sector (incl. wholesale)"] <- "Retail"
model2_all$work_sector[model2_all$work_sector == "Teaching and education"] <- "Education"
model2_all$work_sector[model2_all$work_sector == "Transport (incl. storage, logistic)"] <- "Transport"

# Convert to percentages
model2_all$fit <- model2_all$fit * 100
model2_all$lwr <- model2_all$lwr * 100
model2_all$upr <- model2_all$upr * 100
model2_all$lwr[model2_all$lwr < 0] <- 0 # Can't be less than 0

# Plot
ggplot(model2_all, aes(x = month, y = fit, group=sex, color=sex)) +
  geom_point(position=position_dodge(width = 0.5)) +
  #geom_path() +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.5)) +
  facet_wrap(~work_sector) +
  xlab("Month") +
  ylab("Predicted percent of positive tests (%)") +
  #scale_x_date(date_breaks = "months", date_labels = "%b")  +
  labs(color = "Sex") +
  scale_x_continuous(breaks = c(8,9,10,11,12,13), labels = c("Aug", "Sept", "Oct", "Nov", "Dec", "Jan")) + # Revise month from number to label
  theme(text = element_text(size=14), axis.text.x = element_text(angle = 45, hjust=1))

# Save
ggsave("./Plots/model2_all.jpeg", dpi = 300)
rm(model2_all)


## Model by age group <40 ##

# Load data and tidy
model1_age1839 <- read.csv("./20210304 MG Pub 2001021/model1_age1839.csv") # Load

# Tidy up work status variable
model1_age1839$work_status <- as.character(model1_age1839$work_status)
model1_age1839 <- model1_age1839[!is.na(model1_age1839$work_status),] # Drop missing data summary
model1_age1839$work_status[model1_age1839$work_status == "Furloughed (temporarily not working)"] <- "Furloughed"
model1_age1839$work_status[model1_age1839$work_status == "Not working (unemployed, retired, long-term sick etc.)"] <- "Not working"

# Convert to percentages
model1_age1839$fit <- model1_age1839$fit * 100
model1_age1839$lwr <- model1_age1839$lwr * 100
model1_age1839$upr <- model1_age1839$upr * 100
model1_age1839$lwr[model1_age1839$lwr < 0] <- 0 # Can't be less than 0

# Plot
ggplot(model1_age1839, aes(x = month, y = fit, group=sex, color=sex)) +
  geom_point(position=position_dodge(width = 0.5)) +
  #geom_path() +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.5)) +
  facet_wrap(~work_status) +
  xlab("Month") +
  ylab("Predicted percent of positive tests (%)") +
  #scale_x_date(date_breaks = "months", date_labels = "%b")  +
  labs(color = "Sex") +
  scale_x_continuous(breaks = c(8,9,10,11,12,13), labels = c("Aug", "Sept", "Oct", "Nov", "Dec", "Jan")) + # Revise month from number to label
  theme(text = element_text(size=16), axis.text.x = element_text(angle = 45, hjust=1)) +
  coord_cartesian(ylim = c(0, 4.25))

# Save
ggsave("./Plots/model1_age1839.jpeg", dpi = 300)


## Model 1 by age group >=40 ##

# Load data and tidy
model1_age40p<- read.csv("./20210304 MG Pub 2001021/model1_age40p.csv") # Load

# Tidy up work status variable
model1_age40p$work_status <- as.character(model1_age40p$work_status)
model1_age40p<- model1_age40p[!is.na(model1_age40p$work_status),] # Drop missing data summary
model1_age40p$work_status[model1_age40p$work_status == "Furloughed (temporarily not working)"] <- "Furloughed"
model1_age40p$work_status[model1_age40p$work_status == "Not working (unemployed, retired, long-term sick etc.)"] <- "Not working"

# Convert to percentages
model1_age40p$fit <- model1_age40p$fit * 100
model1_age40p$lwr <- model1_age40p$lwr * 100
model1_age40p$upr <- model1_age40p$upr * 100
model1_age40p$lwr[model1_age40p$lwr < 0] <- 0 # Can't be less than 0

# Plot
ggplot(model1_age40p, aes(x = month, y = fit, group=sex, color=sex)) +
  geom_point(position=position_dodge(width = 0.5)) +
  #geom_path() +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.5)) +
  facet_wrap(~work_status) +
  xlab("Month") +
  ylab("Predicted percent of positive tests (%)") +
  #scale_x_date(date_breaks = "months", date_labels = "%b")  +
  labs(color = "Sex") +
  scale_x_continuous(breaks = c(8,9,10,11,12,13), labels = c("Aug", "Sept", "Oct", "Nov", "Dec", "Jan")) + # Revise month from number to label
  theme(text = element_text(size=16), axis.text.x = element_text(angle = 45, hjust=1))

# Save
ggsave("./Plots/model1_age40p.jpeg", dpi = 300)


## Model 1 - single plot by age ##

# Tidy data and join into single file
model1_age1839$age <- "Less than 40" # Create age variable
model1_age40p$age <- "40 and over"
data <- rbind(model1_age1839, model1_age40p) # Join together
data <- tidyr::unite(data, "age_sex", age, sex, remove = F) # Make variable of age and sex combination

# Plot
ggplot(data, aes(x = month, y = fit, group=age_sex, color=age_sex)) +
  geom_point(position=position_dodge(width = 0.7), size = 0.2) +
  #geom_path() +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.7), size = 0.2) +
  facet_wrap(~work_status) +
  xlab("Month") +
  ylab("Predicted percent of positive tests (%)") +
  #scale_x_date(date_breaks = "months", date_labels = "%b")  +
  labs(color = "Sex") +
  scale_x_continuous(breaks = c(8,9,10,11,12,13), labels = c("Aug", "Sept", "Oct", "Nov", "Dec", "Jan")) + # Revise month from number to label
  scale_color_discrete(name = "Age Group by Sex", labels = c("Females 40+", "Males 40+", "Females <40", "Males <40")) +
  theme(text = element_text(size=14), axis.text.x = element_text(angle = 45, hjust=1)) +
  coord_cartesian(ylim = c(0, 4.5)) # Fix axis

# Save
ggsave("./Plots/model1_age_both.jpeg", dpi = 300)
rm(data, model1_age40p, model1_age1839)



## Model 2 - age < 40 ##

# Load data and tidy
model2_age1839 <- read.csv("./20210304 MG Pub 2001021/model2_age1839.csv") # Load

# Tidy up work sector variable
model2_age1839$work_sector <- as.character(model2_age1839$work_sector)
model2_age1839 <- model2_age1839[!is.na(model2_age1839$work_sector),] # Drop missing data summary
model2_age1839$work_sector[model2_age1839$work_sector == "Arts,Entertainment or Recreation"] <- "Entertainment"
model2_age1839$work_sector[model2_age1839$work_sector == "Civil service or Local Government"] <- "Civil service"
model2_age1839$work_sector[model2_age1839$work_sector == "Financial services incl. insurance"] <- "Finance"
model2_age1839$work_sector[model2_age1839$work_sector == "Food production, agriculture, farming"] <- "Food production"
model2_age1839$work_sector[model2_age1839$work_sector == "Hospitality (e.g. hotel, restaurant)"] <- "Hospitality"
model2_age1839$work_sector[model2_age1839$work_sector == "Information technology and communication"] <- "ICT"
model2_age1839$work_sector[model2_age1839$work_sector == "Manufacturing or construction"] <- "Manufacturing"
model2_age1839$work_sector[model2_age1839$work_sector == "Other occupation sector"] <- "Other"
model2_age1839$work_sector[model2_age1839$work_sector == "Personal services (e.g. hairdressers)"] <- "Personal services"
model2_age1839$work_sector[model2_age1839$work_sector == "Retail sector (incl. wholesale)"] <- "Retail"
model2_age1839$work_sector[model2_age1839$work_sector == "Teaching and education"] <- "Education"
model2_age1839$work_sector[model2_age1839$work_sector == "Transport (incl. storage, logistic)"] <- "Transport"

# Convert to percentages
model2_age1839$fit <- model2_age1839$fit * 100
model2_age1839$lwr <- model2_age1839$lwr * 100
model2_age1839$upr <- model2_age1839$upr * 100
model2_age1839$lwr[model2_age1839$lwr < 0] <- 0 # Can't be less than 0

# Plot
ggplot(model2_age1839, aes(x = month, y = fit, group=sex, color=sex)) +
  geom_point(position=position_dodge(width = 0.5)) +
  #geom_path() +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.5)) +
  facet_wrap(~work_sector) +
  xlab("Month") +
  ylab("Predicted percent of positive tests (%)") +
  #scale_x_date(date_breaks = "months", date_labels = "%b")  +
  labs(color = "Sex") +
  scale_x_continuous(breaks = c(8,9,10,11,12,13), labels = c("Aug", "Sept", "Oct", "Nov", "Dec", "Jan")) + # Revise month from number to label
  theme(text = element_text(size=14), axis.text.x = element_text(angle = 45, hjust=1)) +
  coord_cartesian(ylim = c(0, 4.5)) # Fix axis

# Save
ggsave("./Plots/model2_age1839.jpeg", dpi = 300)



## Model 2 - age > 40 ##

# Load data and tidy
model2_age40p <- read.csv("./20210304 MG Pub 2001021/model2_age40p.csv") # Load

# Tidy up work sector variable
model2_age40p$work_sector <- as.character(model2_age40p$work_sector)
model2_age40p <- model2_age40p[!is.na(model2_age40p$work_sector),] # Drop missing data summary
model2_age40p$work_sector[model2_age40p$work_sector == "Arts,Entertainment or Recreation"] <- "Entertainment"
model2_age40p$work_sector[model2_age40p$work_sector == "Civil service or Local Government"] <- "Civil service"
model2_age40p$work_sector[model2_age40p$work_sector == "Financial services incl. insurance"] <- "Finance"
model2_age40p$work_sector[model2_age40p$work_sector == "Food production, agriculture, farming"] <- "Food production"
model2_age40p$work_sector[model2_age40p$work_sector == "Hospitality (e.g. hotel, restaurant)"] <- "Hospitality"
model2_age40p$work_sector[model2_age40p$work_sector == "Information technology and communication"] <- "ICT"
model2_age40p$work_sector[model2_age40p$work_sector == "Manufacturing or construction"] <- "Manufacturing"
model2_age40p$work_sector[model2_age40p$work_sector == "Other occupation sector"] <- "Other"
model2_age40p$work_sector[model2_age40p$work_sector == "Personal services (e.g. hairdressers)"] <- "Personal services"
model2_age40p$work_sector[model2_age40p$work_sector == "Retail sector (incl. wholesale)"] <- "Retail"
model2_age40p$work_sector[model2_age40p$work_sector == "Teaching and education"] <- "Education"
model2_age40p$work_sector[model2_age40p$work_sector == "Transport (incl. storage, logistic)"] <- "Transport"

# Convert to percentages
model2_age40p$fit <- model2_age40p$fit * 100
model2_age40p$lwr <- model2_age40p$lwr * 100
model2_age40p$upr <- model2_age40p$upr * 100
model2_age40p$lwr[model2_age40p$lwr < 0] <- 0 # Can't be less than 0

# Plot
ggplot(model2_age40p, aes(x = month, y = fit, group=sex, color=sex)) +
  geom_point(position=position_dodge(width = 0.5)) +
  #geom_path() +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.5)) +
  facet_wrap(~work_sector) +
  xlab("Month") +
  ylab("Predicted percent of positive tests (%)") +
  #scale_x_date(date_breaks = "months", date_labels = "%b")  +
  labs(color = "Sex") +
  scale_x_continuous(breaks = c(8,9,10,11,12,13), labels = c("Aug", "Sept", "Oct", "Nov", "Dec", "Jan")) + # Revise month from number to label
  theme(text = element_text(size=14), axis.text.x = element_text(angle = 45, hjust=1)) +
  coord_cartesian(ylim = c(0, 4.5)) # Fix axis

# Save
ggsave("./Plots/model2_age40p.jpeg", dpi = 300)


## Model 2 - single plot by age ##

# Tidy data and join into single file
model2_age1839$age <- "Less than 40" # Create age variable
model2_age40p$age <- "40 and over"
data <- rbind(model2_age1839, model2_age40p) # Join together
data <- tidyr::unite(data, "age_sex", age, sex, remove = F) # Make variable of age and sex combination

# Plot
ggplot(data, aes(x = month, y = fit, group=age_sex, color=age_sex)) +
  geom_point(position=position_dodge(width = 0.7), size = 0.2) +
  #geom_path() +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.7), size = 0.2) +
  facet_wrap(~work_sector) +
  xlab("Month") +
  ylab("Predicted percent of positive tests (%)") +
  #scale_x_date(date_breaks = "months", date_labels = "%b")  +
  labs(color = "Sex") +
  scale_x_continuous(breaks = c(8,9,10,11,12,13), labels = c("Aug", "Sept", "Oct", "Nov", "Dec", "Jan")) + # Revise month from number to label
  scale_color_discrete(name = "Age Group by Sex", labels = c("Females 40+", "Males 40+", "Females <40", "Males <40")) +
  theme(text = element_text(size=14), axis.text.x = element_text(angle = 45, hjust=1)) +
  coord_cartesian(ylim = c(0, 4.5)) # Fix axis

# Save
ggsave("./Plots/model2_age_both.jpeg", dpi = 300)
rm(data, model2_age40p, model2_age1839)
