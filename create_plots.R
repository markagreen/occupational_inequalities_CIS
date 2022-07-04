############################################
### Creating descriptive plots for paper ###
############################################

## Purpose: Create a suite of plots for descriptive statistics.

# Libraries
library(patchwork)
library(ggplot2)
library(viridis)


## Plot 1 ##

# Load data and tidy
plot1 <- read.csv("./20210304 MG Pub 2001021/plot1.csv") # Load
plot1$week <- as.Date(plot1$week, format = "%Y-%m-%d") # Define as date

# Create 95% confidence intervals as measure of uncertainty
plot1$lwr <- plot1$pc_positive - (1.96 * sqrt((plot1$pc_positive * (100 - plot1$pc_positive)) / plot1$total))
plot1$upr <- plot1$pc_positive + (1.96 * sqrt((plot1$pc_positive * (100 - plot1$pc_positive)) / plot1$total))

# Plot percentage of tests that were positive
ggplot(plot1, aes(x = week, y = pc_positive, group=sex, color=sex)) +
  geom_point(position=position_dodge(width = 2)) +
  #geom_path() +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 2)) +
  xlab("Week") +
  ylab("Percentage of tests positive (%)") +
  scale_x_date(date_breaks = "months", date_labels = "%b")  +
  labs(color = "Sex") +
  theme(text = element_text(size=16))

# Save
ggsave("./Plots/plot1.jpeg", dpi = 300)
rm(plot1)

## Plot 2 ##

# Load data and tidy
plot2 <- read.csv("./20210304 MG Pub 2001021/plot2.csv") # Load

# Create 95% confidence intervals as measure of uncertainty
plot2$lwr <- plot2$pc_positive - (1.96 * sqrt((plot2$pc_positive * (100 - plot2$pc_positive)) / plot2$total))
plot2$upr <- plot2$pc_positive + (1.96 * sqrt((plot2$pc_positive * (100 - plot2$pc_positive)) / plot2$total))

# Plot percentage of tests that were positive
ggplot(plot2, aes(x = age_at_visit, y = pc_positive, color = sex)) +
  geom_point(position=position_dodge(width = 0.5), size = 0.3) +
  #geom_path() +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.5), size = 0.3) +
  ylim(0, max(plot2$upr)) +
  xlab("Age (years)") +
  ylab("Percentage of tests positive (%)") +
  labs(color = "Sex") +
  theme(text = element_text(size=16))

# Save
ggsave("./Plots/plot2.jpeg", dpi = 300)
rm(plot2)


## Plot 3 ##

# Load data and tidy
plot3 <- read.csv("./20210304 MG Pub 2001021/plot3.csv") # Load

# Create 95% confidence intervals as measure of uncertainty
plot3$lwr <- plot3$pc_positive - (1.96 * sqrt((plot3$pc_positive * (100 - plot3$pc_positive)) / plot3$total))
plot3$upr <- plot3$pc_positive + (1.96 * sqrt((plot3$pc_positive * (100 - plot3$pc_positive)) / plot3$total))

# Rename groups
plot3$ethnicity2 <- as.character(plot3$ethnicity2)
plot3$ethnicity2[plot3$ethnicity2 == "White-British"] <- "White British"
plot3$ethnicity2[plot3$ethnicity2 == "White-Irish"] <- "White Irish"
plot3$ethnicity2[plot3$ethnicity2 == "Mixed-White & Asian"] <- "Mixed White & Asian"
plot3$ethnicity2[plot3$ethnicity2 == "Mixed-White & Black"] <- "Mixed White & Black"
plot3$ethnicity2[plot3$ethnicity2 == "Asian or Asian British-Pakistani"] <- "Pakistani"
plot3$ethnicity2[plot3$ethnicity2 == "Asian or Asian British-Indian"] <- "Indian"
plot3$ethnicity2[plot3$ethnicity2 == "Asian or Asian British-Chinese"] <- "Chinese"
plot3$ethnicity2[plot3$ethnicity2 == "Black,Caribbean,African-African"] <- "Black African"
plot3$ethnicity2[plot3$ethnicity2 == "Black,Caribbean,Afro-Caribbean"] <- "Black Afro-Caribbean"
plot3$ethnicity2[plot3$ethnicity2 == "Any other white background"] <- "White Other"
plot3$ethnicity2[plot3$ethnicity2 == "Any other ethnic group"] <- "Other"

# Plot percentage of tests that were positive
ggplot(plot3, aes(x = ethnicity2, y = pc_positive)) +
  geom_point() +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.5), size = 0.3) +
  ylim(0, max(plot3$upr)) +
  xlab("Ethnic group") +
  ylab("Percentage of tests positive (%)") +
  coord_flip() +
  theme(text = element_text(size=16))

# Save
ggsave("./Plots/plot3.jpeg", dpi = 300)
rm(plot3)


## Plot 3 by sex ##

# Load data and tidy
plot3_sex <- read.csv("./20210304 MG Pub 2001021/plot3_sex.csv") # Load

# Create 95% confidence intervals as measure of uncertainty
plot3_sex$lwr <- plot3_sex$pc_positive - (1.96 * sqrt((plot3_sex$pc_positive * (100 - plot3_sex$pc_positive)) / plot3_sex$total))
plot3_sex$upr <- plot3_sex$pc_positive + (1.96 * sqrt((plot3_sex$pc_positive * (100 - plot3_sex$pc_positive)) / plot3_sex$total))

# Rename groups
plot3_sex$ethnicity2 <- as.character(plot3_sex$ethnicity2)
plot3_sex$ethnicity2[plot3_sex$ethnicity2 == "White-British"] <- "White British"
plot3_sex$ethnicity2[plot3_sex$ethnicity2 == "White-Irish"] <- "White Irish"
plot3_sex$ethnicity2[plot3_sex$ethnicity2 == "Mixed-White & Asian"] <- "Mixed White & Asian"
plot3_sex$ethnicity2[plot3_sex$ethnicity2 == "Mixed-White & Black"] <- "Mixed White & Black"
plot3_sex$ethnicity2[plot3_sex$ethnicity2 == "Asian or Asian British-Pakistani"] <- "Pakistani"
plot3_sex$ethnicity2[plot3_sex$ethnicity2 == "Asian or Asian British-Indian"] <- "Indian"
plot3_sex$ethnicity2[plot3_sex$ethnicity2 == "Asian or Asian British-Chinese"] <- "Chinese"
plot3_sex$ethnicity2[plot3_sex$ethnicity2 == "Black,Caribbean,African-African"] <- "Black African"
plot3_sex$ethnicity2[plot3_sex$ethnicity2 == "Black,Caribbean,Afro-Caribbean"] <- "Black Afro-Caribbean"
plot3_sex$ethnicity2[plot3_sex$ethnicity2 == "Any other white background"] <- "White Other"
plot3_sex$ethnicity2[plot3_sex$ethnicity2 == "Any other ethnic group"] <- "Other"

# Plot percentage of tests that were positive
ggplot(plot3_sex, aes(x = ethnicity2, y = pc_positive, color = sex)) +
  geom_point(position=position_dodge(width = 0.5)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.5)) +
  ylim(0, max(plot3_sex$upr)) +
  xlab("Ethnic group") +
  ylab("Percentage of tests positive (%)") +
  coord_flip() +
  labs(color = "Sex") +
  theme(text = element_text(size=16))

# Save
ggsave("./Plots/plot3_sex.jpeg", dpi = 300)
rm(plot3_sex)


## Plot 4 ##

# Load data and tidy
plot4 <- read.csv("./20210304 MG Pub 2001021/plot4.csv") # Load

# Create 95% confidence intervals as measure of uncertainty
plot4$lwr <- plot4$pc_positive - (1.96 * sqrt((plot4$pc_positive * (100 - plot4$pc_positive)) / plot4$total))
plot4$upr <- plot4$pc_positive + (1.96 * sqrt((plot4$pc_positive * (100 - plot4$pc_positive)) / plot4$total))

# Tidy up work status variable
plot4$work_status <- as.character(plot4$work_status)
plot4 <- plot4[!is.na(plot4$work_status),] # Drop missing data summary
plot4$work_status[plot4$work_status == "Furloughed (temporarily not working)"] <- "Furloughed"
plot4$work_status[plot4$work_status == "Not working (unemployed, retired, long-term sick etc.)"] <- "Not working"

# Plot
ggplot(plot4, aes(x = work_status, y = pc_positive)) +
  geom_point(position=position_dodge(width = 0.5)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.5)) +
  ylim(0, max(plot4$upr)) +
  xlab("Work status") +
  ylab("Percentage of tests positive (%)") +
  coord_flip() +
  theme(text = element_text(size=16))

# Save
ggsave("./Plots/plot4.jpeg", dpi = 300)
rm(plot4)


## Plot 4 by sex ##

# Load data and tidy
plot4_sex <- read.csv("./20210304 MG Pub 2001021/plot4_sex.csv") # Load

# Create 95% confidence intervals as measure of uncertainty
plot4_sex$lwr <- plot4_sex$pc_positive - (1.96 * sqrt((plot4_sex$pc_positive * (100 - plot4_sex$pc_positive)) / plot4_sex$total))
plot4_sex$upr <- plot4_sex$pc_positive + (1.96 * sqrt((plot4_sex$pc_positive * (100 - plot4_sex$pc_positive)) / plot4_sex$total))

# Tidy up work status variable
plot4_sex$work_status <- as.character(plot4_sex$work_status)
plot4_sex <- plot4_sex[!is.na(plot4_sex$work_status),] # Drop missing data summary
plot4_sex$work_status[plot4_sex$work_status == "Furloughed (temporarily not working)"] <- "Furloughed"
plot4_sex$work_status[plot4_sex$work_status == "Not working (unemployed, retired, long-term sick etc.)"] <- "Not working"

# Plot
ggplot(plot4_sex, aes(x = work_status, y = pc_positive, color = sex)) +
  geom_point(position=position_dodge(width = 0.5)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.5)) +
  ylim(0, max(plot4_sex$upr)) +
  xlab("Work status") +
  ylab("Percentage of tests positive (%)") +
  coord_flip() +
  labs(color = "Sex") +
  theme(text = element_text(size=16))

# Save
ggsave("./Plots/plot4_sex.jpeg", dpi = 300)
rm(plot4_sex)


## Plot 4 by age and sex

# Load data and tidy
plot4_age_sex <- read.csv("./20210304 MG Pub 2001021/plot4_age_sex.csv") # Load

# Create 95% confidence intervals as measure of uncertainty
plot4_age_sex$lwr <- plot4_age_sex$pc_positive - (1.96 * sqrt((plot4_age_sex$pc_positive * (100 - plot4_age_sex$pc_positive)) / plot4_age_sex$total))
plot4_age_sex$upr <- plot4_age_sex$pc_positive + (1.96 * sqrt((plot4_age_sex$pc_positive * (100 - plot4_age_sex$pc_positive)) / plot4_age_sex$total))

# Tidy up work status variable
plot4_age_sex$work_status <- as.character(plot4_age_sex$work_status)
plot4_age_sex <- plot4_age_sex[!is.na(plot4_age_sex$work_status),] # Drop missing data summary
plot4_age_sex$work_status[plot4_age_sex$work_status == "Furloughed (temporarily not working)"] <- "Furloughed"
plot4_age_sex$work_status[plot4_age_sex$work_status == "Not working (unemployed, retired, long-term sick etc.)"] <- "Not working"

# Combine age and sex variables into a single variable
plot4_age_sex <- tidyr::unite(plot4_age_sex, "age_sex", age_1840, sex, remove = F)

# Plot
ggplot(plot4_age_sex, aes(x = work_status, y = pc_positive, color = age_sex)) +
  geom_point(position=position_dodge(width = 0.5)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.5)) +
  ylim(0, max(plot4_age_sex$upr)) +
  xlab("Work status") +
  ylab("Percentage of tests positive (%)") +
  coord_flip() +
  scale_color_discrete(name = "Age Group by Sex", labels = c("Females 40+", "Males 40+", "Females <40", "Males <40")) +
  theme(text = element_text(size=16))

# Save
ggsave("./Plots/plot4_age_sex.jpeg", dpi = 300)
rm(plot4_age_sex)


## Plot 5 ##

# Load data and tidy
plot5 <- read.csv("./20210304 MG Pub 2001021/plot5.csv") # Load

# Create 95% confidence intervals as measure of uncertainty
plot5$lwr <- plot5$pc_positive - (1.96 * sqrt((plot5$pc_positive * (100 - plot5$pc_positive)) / plot5$total))
plot5$upr <- plot5$pc_positive + (1.96 * sqrt((plot5$pc_positive * (100 - plot5$pc_positive)) / plot5$total))

# Tidy up work sector variable
plot5$work_sector <- as.character(plot5$work_sector)
plot5 <- plot5[!is.na(plot5$work_sector),] # Drop missing data summary
plot5$work_sector[plot5$work_sector == "Arts,Entertainment or Recreation"] <- "Entertainment"
plot5$work_sector[plot5$work_sector == "Civil service or Local Government"] <- "Civil service"
plot5$work_sector[plot5$work_sector == "Financial services incl. insurance"] <- "Finance"
plot5$work_sector[plot5$work_sector == "Food production, agriculture, farming"] <- "Food production"
plot5$work_sector[plot5$work_sector == "Hospitality (e.g. hotel, restaurant)"] <- "Hospitality"
plot5$work_sector[plot5$work_sector == "Information technology and communication"] <- "ICT"
plot5$work_sector[plot5$work_sector == "Manufacturing or construction"] <- "Manufacturing"
plot5$work_sector[plot5$work_sector == "Other occupation sector"] <- "Other"
plot5$work_sector[plot5$work_sector == "Personal services (e.g. hairdressers)"] <- "Personal services"
plot5$work_sector[plot5$work_sector == "Retail sector (incl. wholesale)"] <- "Retail"
plot5$work_sector[plot5$work_sector == "Teaching and education"] <- "Education"
plot5$work_sector[plot5$work_sector == "Transport (incl. storage, logistic)"] <- "Transport"

# Plot
ggplot(plot5, aes(x = work_sector, y = pc_positive)) +
  geom_point(position=position_dodge(width = 0.5)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.5)) +
  ylim(0, max(plot5$upr)) +
  xlab("Work sector") +
  ylab("Percentage of tests positive (%)") +
  coord_flip() +
  theme(text = element_text(size=16))

# Save
ggsave("./Plots/plot5.jpeg", dpi = 300)
rm(plot5)


## Plot 5 by sex ##

# Load data and tidy
plot5_sex <- read.csv("./20210304 MG Pub 2001021/plot5_sex.csv") # Load

# Create 95% confidence intervals as measure of uncertainty
plot5_sex$lwr <- plot5_sex$pc_positive - (1.96 * sqrt((plot5_sex$pc_positive * (100 - plot5_sex$pc_positive)) / plot5_sex$total))
plot5_sex$upr <- plot5_sex$pc_positive + (1.96 * sqrt((plot5_sex$pc_positive * (100 - plot5_sex$pc_positive)) / plot5_sex$total))

# Tidy up work sector variable
plot5_sex$work_sector <- as.character(plot5_sex$work_sector)
plot5_sex <- plot5_sex[!is.na(plot5_sex$work_sector),] # Drop missing data summary
plot5_sex$work_sector[plot5_sex$work_sector == "Arts,Entertainment or Recreation"] <- "Entertainment"
plot5_sex$work_sector[plot5_sex$work_sector == "Civil service or Local Government"] <- "Civil service"
plot5_sex$work_sector[plot5_sex$work_sector == "Financial services incl. insurance"] <- "Finance"
plot5_sex$work_sector[plot5_sex$work_sector == "Food production, agriculture, farming"] <- "Food production"
plot5_sex$work_sector[plot5_sex$work_sector == "Hospitality (e.g. hotel, restaurant)"] <- "Hospitality"
plot5_sex$work_sector[plot5_sex$work_sector == "Information technology and communication"] <- "ICT"
plot5_sex$work_sector[plot5_sex$work_sector == "Manufacturing or construction"] <- "Manufacturing"
plot5_sex$work_sector[plot5_sex$work_sector == "Other occupation sector"] <- "Other"
plot5_sex$work_sector[plot5_sex$work_sector == "Personal services (e.g. hairdressers)"] <- "Personal services"
plot5_sex$work_sector[plot5_sex$work_sector == "Retail sector (incl. wholesale)"] <- "Retail"
plot5_sex$work_sector[plot5_sex$work_sector == "Teaching and education"] <- "Education"
plot5_sex$work_sector[plot5_sex$work_sector == "Transport (incl. storage, logistic)"] <- "Transport"

# Plot
ggplot(plot5_sex, aes(x = work_sector, y = pc_positive, color = sex)) +
  geom_point(position=position_dodge(width = 0.5)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.5)) +
  ylim(0, max(plot5_sex$upr)) +
  xlab("Work sector") +
  ylab("Percentage of tests positive (%)") +
  coord_flip() +
  labs(color = "Sex") +
  theme(text = element_text(size=16))

# Save
ggsave("./Plots/plot5_sex.jpeg", dpi = 300)
rm(plot5_sex)


## Plot 5 by age and sex

# Load data and tidy
plot5_age_sex <- read.csv("./20210304 MG Pub 2001021/plot5_age_sex.csv") # Load

# Create 95% confidence intervals as measure of uncertainty
plot5_age_sex$lwr <- plot5_age_sex$pc_positive - (1.96 * sqrt((plot5_age_sex$pc_positive * (100 - plot5_age_sex$pc_positive)) / plot5_age_sex$total))
plot5_age_sex$upr <- plot5_age_sex$pc_positive + (1.96 * sqrt((plot5_age_sex$pc_positive * (100 - plot5_age_sex$pc_positive)) / plot5_age_sex$total))

# Tidy up work sector variable
plot5_age_sex$work_sector <- as.character(plot5_age_sex$work_sector)
plot5_age_sex <- plot5_age_sex[!is.na(plot5_age_sex$work_sector),] # Drop missing data summary
plot5_age_sex$work_sector[plot5_age_sex$work_sector == "Arts,Entertainment or Recreation"] <- "Entertainment"
plot5_age_sex$work_sector[plot5_age_sex$work_sector == "Civil service or Local Government"] <- "Civil service"
plot5_age_sex$work_sector[plot5_age_sex$work_sector == "Financial services incl. insurance"] <- "Finance"
plot5_age_sex$work_sector[plot5_age_sex$work_sector == "Food production, agriculture, farming"] <- "Food production"
plot5_age_sex$work_sector[plot5_age_sex$work_sector == "Hospitality (e.g. hotel, restaurant)"] <- "Hospitality"
plot5_age_sex$work_sector[plot5_age_sex$work_sector == "Information technology and communication"] <- "ICT"
plot5_age_sex$work_sector[plot5_age_sex$work_sector == "Manufacturing or construction"] <- "Manufacturing"
plot5_age_sex$work_sector[plot5_age_sex$work_sector == "Other occupation sector"] <- "Other"
plot5_age_sex$work_sector[plot5_age_sex$work_sector == "Personal services (e.g. hairdressers)"] <- "Personal services"
plot5_age_sex$work_sector[plot5_age_sex$work_sector == "Retail sector (incl. wholesale)"] <- "Retail"
plot5_age_sex$work_sector[plot5_age_sex$work_sector == "Teaching and education"] <- "Education"
plot5_age_sex$work_sector[plot5_age_sex$work_sector == "Transport (incl. storage, logistic)"] <- "Transport"

# Combine age and sex variables into a single variable
plot5_age_sex <- tidyr::unite(plot5_age_sex, "age_sex", age_1840, sex, remove = F)

# Plot
ggplot(plot5_age_sex, aes(x = work_sector, y = pc_positive, color = age_sex)) +
  geom_point(position=position_dodge(width = 0.5)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position=position_dodge(width = 0.5)) +
  ylim(0, max(plot5_age_sex$upr)) +
  xlab("Work sector") +
  ylab("Percentage of tests positive (%)") +
  coord_flip() +
  scale_color_discrete(name = "Age Group by Sex", labels = c("Females 40+", "Males 40+", "Females <40", "Males <40")) +
  theme(text = element_text(size=16))

# Save
ggsave("./Plots/plot5_age_sex.jpeg", dpi = 300)
rm(plot5_age_sex)
