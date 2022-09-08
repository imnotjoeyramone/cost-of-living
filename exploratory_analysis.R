### Scoping for CoL project 
# Housing exploratory analysis here
# Key findings will go into final report

# Load libraries needed
library(tidyverse)
library(readxl)

#### Gas price analysis #####
# ONS SAP Sep 22 https://www.ons.gov.uk/economy/economicoutputandproductivity/output/datasets/systemaveragepricesapofgas
sap <- 
  read_xlsx("systemaveragepriceofgasdataset010922.xlsx", sheet = 2, skip = 1)[1:1701,1:3]
colnames(sap) <- c("date_from", "price", "avg_7_day")

sap <- sap %>%
  mutate(month = factor(format(date_from, "%B"), 
                        levels = c("January", "February", "March", "April", "May", "June", "July","August", "September", "October", "November", "December")),
         year = format(date_from, "%Y"))

# Price cap data from OFGEM 
# https://www.ofgem.gov.uk/publications/default-tariff-cap-level-1-october-2022-31-december-2022
price_cap <- 
  read_csv("prepayment-price-cap-and.csv")[,c(1,4)]
colnames(price_cap) <- c("date_from", "price_cap")

price_cap$date_from <- as.POSIXct(price_cap$date_from, format = "%d/%m/%Y")

ggplot(data = price_cap %>% filter(date_from > '2020-01-01'), aes(x=date_from, y = price_cap))+
  geom_line()+
  ggtitle("OFGEM Annual Energy Price Cap since 2020")+
  xlab("Date")+
  ylab("Price Cap (£)")


# 2020-2022 plot of SAP
ggplot(data = sap %>% filter(date_from > '2020-01-01') , aes(x= date_from, y = avg_7_day))+
  geom_line()+
  ggtitle("System Average Price (SAP) of Gas since 2020")+
  xlab("Date")+
  ylab("SAP of Gas")

# Summarised version
sap_summary <- sap %>%
  group_by(month, year) %>%
  summarize(mean = mean(price),
            sd = sd(price))

# Plot average by month and year to highlight seasonal trends and standard deviation
ggplot(data = sap_summary, aes(x = month, y = mean, colour = year, group = year))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Monthly mean SAP 2018-2022")+
  xlab("")+
  ylab("Mean SAP (p/kWh)")+
  geom_linerange(aes(ymin=mean-sd, ymax=mean+sd))

#### Family expenditure analysis #####
# coicop_A6_cleaned.xlsx contains expenditure data for years 2019-2021 
# I complied this from datasets @ https://www.ons.gov.uk/peoplepopulationandcommunity/personalandhouseholdfinances/expenditure/datasets/familyspendingworkbook1detailedexpenditureandtrends
# 2021
#Expenditure %
expenditure_pct <- read_xlsx("coicop_A6_cleaned.xlsx", sheet = "expenditure_pct")

# Specifically energy
energy_pct <- expenditure_pct %>% 
  filter(substr(COPI, 1,3) == "4.4")%>%
  select(-COPI) %>%
  pivot_longer(cols = 2:12, names_to = "decile", values_to = "percent")%>%
  mutate(percent = as.numeric(percent),
         decile = factor(decile, levels = c("All_household", "Lowest_10", "Second_10", "Third_10", "Fourth_10", "Fifth_10", "Sixth_10", "Seventh_10", "Eighth_10", "Ninth_10", "Highest_10")),
         Commodity = factor(Commodity, levels = c("Electricity, gas and other fuels", "Electricity", "Gas", "Other fuels")))

# Plot spend by income deciles, facet by energy type
ggplot(energy_pct, aes(x = decile, y = percent))+
  geom_col()+
  facet_grid(~Commodity)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Rent expenditure data
netrent_pct <- expenditure_pct %>% 
  filter(COPI == "4.1.3")%>%
  select(-COPI) %>%
  pivot_longer(cols = 2:12, names_to = "decile", values_to = "percent")%>%
  mutate(percent = as.numeric(percent),
         decile = factor(decile, levels = c("All_household", "Lowest_10", "Second_10", "Third_10", "Fourth_10", "Fifth_10", "Sixth_10", "Seventh_10", "Eighth_10", "Ninth_10", "Highest_10")))

# Join rent and energy data for income deciles together
rent_energy <- 
  inner_join(filter(energy_pct, Commodity == "Electricity, gas and other fuels"),netrent_pct, by = "decile")

# simple linear model 
lm <- lm(rent_energy$percent.y ~ rent_energy$percent.x)
summary(lm)

# plot relationship, include adjusted R squared
ggplot(rent_energy, aes(x = percent.x, y = percent.y))+
  geom_point(aes(colour = decile))+
  xlab("% of income spent on energy")+
  ylab("% of income spent on rent")+
  geom_smooth(method='lm', formula= y~x)+
  geom_text(x = 4, y = 14, label = paste0("Adjusted R-squared = ",round(summary(lm)$adj.r.squared, 3)))

# 2020 version of dataset
expenditure_pct_2020 <- read_xlsx("coicop_A6_cleaned.xlsx", sheet = "expenditure_pct_2020")

# Clean as with 2021
energy_pct_2020 <- expenditure_pct_2020 %>% 
  filter(substr(COPI, 1,3) == "4.4")%>%
  select(-COPI) %>%
  pivot_longer(cols = 2:12, names_to = "decile", values_to = "percent")%>%
  mutate(percent = as.numeric(percent),
         decile = factor(decile, levels = c("All_household", "Lowest_10", "Second_10", "Third_10", "Fourth_10", "Fifth_10", "Sixth_10", "Seventh_10", "Eighth_10", "Ninth_10", "Highest_10")),
         Commodity = factor(Commodity, levels = c("Electricity, gas and other fuels", "Electricity", "Gas", "Other fuels")))

# 2019 version
expenditure_pct_2019 <- read_xlsx("coicop_A6_cleaned.xlsx", sheet = "expenditure_pct_2019")

# Clean 
energy_pct_2019 <- expenditure_pct_2019 %>% 
  filter(substr(COPI, 1,3) == "4.4")%>%
  select(-COPI) %>%
  pivot_longer(cols = 2:12, names_to = "decile", values_to = "percent")%>%
  mutate(percent = as.numeric(percent),
         decile = factor(decile, levels = c("All_household", "Lowest_10", "Second_10", "Third_10", "Fourth_10", "Fifth_10", "Sixth_10", "Seventh_10", "Eighth_10", "Ninth_10", "Highest_10")),
         Commodity = factor(Commodity, levels = c("Electricity, gas and other fuels", "Electricity", "Gas", "Other fuels")))

# Add year columns for easier plotting
energy_pct$year <- "2021"
energy_pct_2020$year <- "2020"
energy_pct_2019$year <- "2019"

# bind together
energy_3_yr <- rbind(energy_pct, energy_pct_2020, energy_pct_2019) %>%
  filter(Commodity == "Electricity, gas and other fuels" & decile != "All_household")

# Plot 3 year deciles
ggplot(data = energy_3_yr, aes(x=decile, y = percent, colour = year, group = year))+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("")+
  ylab("% of household income spent each month")+
  labs(caption = "Source: ONS Family Spending Workbook 1 - detailed expenditure and trends (table 3.2E)")

# Find difference between 2019 and 2021 expenditure
energy_3_yr_wide <- energy_3_yr %>%
  pivot_wider(names_from = year, names_prefix = "pct_", values_from = percent) %>%
  mutate(pct_point_diff = pct_2021 - pct_2019)

# Plot differences
ggplot(data = energy_3_yr_wide, aes(x = decile, y = pct_point_diff))+
  geom_col(aes(fill = pct_point_diff >0))+
  theme(legend.position="none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("% point change of proportion of household income spent on energy from 2019 to 2021 by income decile")+
  xlab("Income decile")+
  ylab("% point difference")+
  labs(caption = "Source: ONS Family Spending Workbook 1 - detailed expenditure and trends (table 3.2E)")

#### Opinions and Lifestyles July 2022 analysis #####
# downloaded from https://www.ons.gov.uk/peoplepopulationandcommunity/wellbeing/bulletins/publicopinionsandsocialtrendsgreatbritain/20to31july2022
# Cost of living survey responses - difficulity in paying bills
col_survey <- read_xlsx("costofliving050822.xlsx", sheet = "3.7", skip = 13)

# specific question - clean
bill_difficulty <- col_survey[8:13, c(1, 3:9)] %>%
  pivot_longer(cols = 2:8, names_to = "income_band", values_to = "percentage") %>%
  mutate(percentage = as.numeric(percentage),
         income_band = factor(income_band, 
                              levels = c("Up to £10,000 \r\n%", 
                                         "£10,000 up to £15,000 \r\n%",
                                         "£15,000 up to £20,000 \r\n%",
                                         "£20,000 up to £30,000\r\n %",
                                         "£30,000 up to £40,000\r\n %",
                                         "£40,000 up to £50,000\r\n %",
                                         "£50,000 or more\r\n%")),
         response = factor(`Survey question and response options`,
                           levels = c( "Prefer not to say", "Don't know","Very easy", "Somewhat easy", "Somewhat difficult", "Very difficult"))
         )

# Plot stacked bar chart for responses across income bands
ggplot(data = bill_difficulty, aes(x = income_band, y = percentage, fill = response))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Response to question: How easy or difficult is it to afford your energy bills?
")+
  labs(caption = "Source: ONS July 2022 Opinions and Lifestyle Survey: Impact of increased cost of living on adults across Great Britain")+
  xlab("Annual income")+
  ylab("% of respondents")

# Repeat for question on increased energy bills
col_survey_inc <- read_xlsx("costofliving050822.xlsx", sheet = "1.7", skip = 12)

increased_col <- col_survey_inc[7, c(1, 3:9)] %>%
  pivot_longer(cols = 2:8, names_to = "income_band", values_to = "percentage") %>%
  mutate(percentage = as.numeric(percentage),
         income_band = factor(income_band, 
                              levels = c("Up to £10,000 \r\n%", 
                                         "£10,000 up to £15,000 \r\n%",
                                         "£15,000 up to £20,000 \r\n%",
                                         "£20,000 up to £30,000 \r\n%",
                                         "£30,000 up to £40,000 \r\n%",
                                         "£40,000 up to £50,000\r\n %",
                                         "£50,000  or more\r\n%")),
  )

# Simple bar plot 
ggplot(data = increased_col, aes(x = income_band, y = percentage))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Response to question: Over the last month, for what reasons has your cost of living increased?")+
  labs(caption = "Source: ONS July 2022 Opinions and Lifestyle Survey: Impact of increased cost of living on adults across Great Britain")+
  xlab("Annual income")+
  ylab("% of respondents")
