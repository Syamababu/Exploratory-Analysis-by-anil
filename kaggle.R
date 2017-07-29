# White House employee salary: Obama vs Trump
# R setup
options(width = 100)
knitr::opts_chunk$set(out.width = "1000px", dpi = 200, message = FALSE, warning = FALSE)

# load/install packages and csv files
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(gridExtra))
suppressMessages(library(RColorBrewer))
suppressMessages(library(ggthemes))
suppressMessages(library(gender))

# genderdata package needs to be installed using git repo
install.packages("genderdata", repos = "http://packages.ropensci.org")
suppressMessages(library(genderdata))

# Data Preparation
# * set the same names for the features in both files
# * regexp to convert salary into numeric (remove "$" and ",")
# * create new column with the President's name
# * rbind data

# obama admin
setwd("C:/Users/Ju Young/Desktop/University of Cape Town/RStudio scripts/kaggle")
df1 <- read.csv("./obama_staff_salaries.csv", sep = ",", stringsAsFactors = F)
df1$president <- rep("Obama", nrow(df1))
df1$salary <- sapply(df1$salary, function(x) as.numeric(gsub("\\$", "", x)))
df1$status <- trimws(df1$status, "r") # remove white space

# trump admin 2017
df2 <- read.csv("./white_house_2017_salaries.csv", sep = ",", stringsAsFactors = F)
colnames(df2) <- c("name", "status", "salary", "pay_basis", "title")
df2$status <- trimws(df2$status, "r")
df2$year <- rep.int(2017, nrow(df2))
df2$president <- rep("Trump", nrow(df2))
dim(df1); dim(df2) # check dimensions

# gsub is in two steps because df2$salary is character variables
# first need to remove the $ sign and then remove "," and convert them to numeric values
df2$salary <- sapply(df2$salary, function(x) gsub("\\$", "", x))
df2$salary <- sapply(df2$salary, function(x) as.numeric(gsub("\\,", "", x)))
res <- data.frame(rbind(df1,df2))
keepDF <- res
exf <- res

# uptil here was data cleaning process
# --------------------------------------------------------------------------

# Salary's Overview
## Per range
exf %>%
  mutate(cuts = cut(salary, seq(0,250000,25000))) %>%
  group_by(cuts, president) %>%
  summarise(n = n()) %>% # counts the number
  ggplot(aes(x = cuts, y = n, fill = president)) +
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + # flips the x and y coordinates 
  theme_fivethirtyeight() +
  scale_fill_manual(name = "President",
                    values = c("#46ACC8", "#F21A00")) +
  ggtitle("Employee\'s population per salary\'s range($)")

# Distribution per 'President'
exf %>%
  group_by(salary,president) %>%
  summarize(tot = n()) %>% # counts the number
  ggplot(aes(x = salary, fill = president)) +
  geom_histogram(aes(y = ..count..),alpha = .75, binwidth = 2500) +
  scale_fill_manual(name = "President", values = c("#46ACC8", "#F21A00")) +
  theme_fivethirtyeight() +
  ggtitle('WH employee\'s salary distribution per Presidency')

# Distribution, breakdown per "Year"
exf %>%
  ggplot(aes(x = salary)) +
  geom_histogram(aes(fill = president), bins = 50, alpha = .75) +
  theme_fivethirtyeight() %>%
  scale_fill_manual(name = "President", values = c("#46ACC8", "#F21A00")) +
  facet_wrap(~factor(year), ncol = 2) +
  scale_y_log10() + 
  ggtitle('WH employee\'s salary($)')

# Some statistics
# Overview table
res_table <- data.frame(res %>%
                          group_by(year,president) %>%
                          summarize(mean_salary = round(mean(salary), 0),
                                    median_salary = round(median(salary), 0),
                                    count = n()))

# creates an HTML table widget using the DataTables library
DT::datatable(res_table)

# Aggregation
g1 <- res %>%
  group_by(year, president) %>%
  summarize(mean_salary = mean(salary), count = n()) %>%
  ggplot(aes(x = factor(year), y = mean_salary)) + 
  geom_point(aes(color = president, size = count)) + 
  scale_color_manual(name = "President", values = c("#46ACC8", "#F21A00")) +
  theme_fivethirtyeight() +
  ggtitle("Mean salary($), number of employee's vs. Year") +
  theme(legend.position = "none")

g2 <- res %>%
  group_by(year, president) %>%
  summarize(median_salary = median(salary), count = n()) %>%
  ggplot(aes(x = factor(year), y = median_salary)) + 
  geom_point(aes(color = president, size = count)) +
  scale_color_manual(name = "President", values = c("#46ACC8", "#F21A00")) +
  theme_fivethirtyeight() +
  ggtitle("Median salary($), number of employee's vs. Year") +
  theme(legend.position = "none")

# setup to put multiple grobs in one page
grid.arrange(g1,g2)

## comments
# The table shows a slight increase of the mean salary during Obama's mandats
# During the first year of Trump's presidency, we already see an increase in the mean salary (by ~12%)
# The first year of Trump's presidency also sees the lowest (although the year is not yet finished so positions may increase) in employee.

# Boxplots
res %>%
  group_by(year) %>%
  ggplot(aes(x = factor(year), y = salary, color = president)) +
  geom_boxplot(colour = "black", size = .4, alpha = .5) +
  ggtitle("WH employee's salary($)") +
  theme_fivethirtyeight() +
  geom_jitter(shape = 16, width = .2, size = 2, alpha = .75) +
  scale_color_manual(name = "President", values = c("#46ACC8", "#F21A00"))

# Status histogram
gg1 <- res %>%
  group_by(year, president, status) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = status, y = count, fill = president)) +
  geom_bar(stat = "identity") +
  facet_wrap(~factor(year)) +
  scale_color_manual(name = "President", values = c("#46ACC8", "#F21A00")) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = .5), legend.position = "top") +
  scale_y_log10()

gg2 <- res %>%
  group_by(year, president, status) %>%
  summarize(tot = n()) %>%
  mutate(frac = tot / sum(tot)) %>%
  ggplot(aes(x = factor(year), y = frac, fill = status)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Accent") + 
  theme_fivethirtyeight()

# set up a gtable layout to place multiple grobs on a page
grid.arrange(gg1, gg2, ncol = 2)

## comments
# up to now Trump has kept the same proportion of 'employee' / 'detaillee' as Obama did during his years
# the proportion is ~95% / 5%
# Obama is the only president with Employee status however the definition of Detailee is actually:
# A detaille is a civil servant from one agency who is assigned temporarily to another agnecy

# salary by 'Status'
s1 <- res %>%
  select(year,status,salary) %>%
  group_by(year,status) %>%
  summarize(count = n(), median_salary = median(salary)) %>%
  ggplot(aes(x = factor(year), y = median_salary)) + 
  geom_point(aes(color = status, size = count)) +
  scale_color_manual(name = "Status", values = c("#7FC97F", "#BEAED4", "#FDC086")) +
  theme_fivethirtyeight() +
  ggtitle("Median salary($) per status vs. year")

## comments
# as seen before, the median salary increases for all status vs. Year
# Detailee seem to get higher salary than Employee
# The trend is continuing under Trump's presidency.

# First year mandate comparsion
# To be fair with Trump, we need to compare the data from the first year of each mandate
obama_term_1 <- data.frame(df1 %>% filter(year == 2009))
obama_term_2 <- data.frame(df1 %>% filter(year == 2013))
obama_term_1$president <- rep("Obama 1st year term 1", nrow(obama_term_1))
obama_term_2$president <- rep("Obama 1st year term 2", nrow(obama_term_2))
obama_term_1$salary <- sapply(obama_term_1$salary, function(x) as.numeric(gsub("\\$", "", x)))
obama_term_2$salary <- sapply(obama_term_2$salary, function(x) as.numeric(gsub("\\$", "", x)))
res <- data.frame(rbind(obama_term_1, obama_term_2))

# Salary Distribution
res %>%
  filter(status != "Employee (part-time)") %>%
  group_by(president,status) %>%
  ggplot(aes(x = salary)) +
  geom_histogram(aes(fill = president), alpha = .5, binwidth = 10000) +
  facet_wrap(~status) +
  scale_fill_manual(name = "", values = c("#A6CEE3", "#1F78B4", "#F21A00")) +
  theme_fivethirtyeight() +
  ggtitle("Median Salary($)") + 
  theme(legend.position = "top")

# number of employee vs. mean salary
res %>%
  filter(status != "Employee (part-time)") %>%
  group_by(president,status) %>%
  summarize(median_salary = median(salary), count = n()) %>%
  ggplot(aes(x = median_salary, y = count)) +
  geom_point(aes(color = president, shape = status), size = 4) +
  scale_color_manual(name = "", values = c("#A6CEE3", "#1F78B4", "#F21A00")) +
  theme_fivethirtyeight() + 
  ggtitle("Number of employee vs. (median) Salary($)") +
  theme(legend.position = "top")

########################
#### small practice #### 
# how to remove background + grid etc
a <- seq(1, 20)
b <- a^0.25
df <- as.data.frame(cbind(a,b))
df$president <- president_names <- c(rep("Obama", 14), rep("Trump", 6))
myplot <- df %>%
  ggplot(aes(x = a, y = b)) +
  geom_point(aes(color = president), size = 3, alpha = .5) + 
  scale_color_manual(name = "President", values = c("#A6CEE3", "#1F78B4")) +
  theme_bw() + # get rid of the background 
  #theme(panel.grid.major = element_blank(), # removes the grid line
  #      panel.grid.minor = element_blank()) + 
  theme(panel.border = element_blank()) + # removes border line
  theme(panel.background = element_blank()) + # removes background but not grid line
  ggtitle("Exponential Plot") +
  theme(legend.position = "top")

########################
########################

## comments
# for both president, "Detailee's" salary is higher than "Employee"'s salary
# for each group, the salary is lower for Obama's first year
# The number of "Detailee" is relatively the same for both presidents (~20-30)
# The number of "Employee" is higher for Obama (both mandates, ~450) compared to Trump (~350)

# Employee's Gender
# Motivation: although the gender is not specified in the dataset, we can _try_ to guess the gender by the first name.
# R has package ("gender") that enocdes gender based on names and dates of birth
# example:
g1 <- gender("madison", method = "demo", years = c(1900, 1985))

# to get the first name of each employee, a bit of regexp is needed
decodeFirstName <- function(x) {
  # split by a comma and get the length
  # if length = 2, re-split by tab " "
  # if length = 3 --> there is a Jr after the first name
  ll <- strsplit(x, ",")[[1]] # as a list
  if (length(ll) == 2) {
    first_name <- strsplit(strsplit(x, ",")[[1]][2], " ")[[1]][2]
  } else if (length(ll) == 3) {
    first_name <- strsplit(strsplit(x, ",")[[1]][2], " ")[[1]][2]
  }
  return(first_name)
}

getProbabilityFemale <- function(x) {
  return((gender(x, method = "ssa", countries = c("United States"), years = c(1940, 1990)))$proportion_female)
}

res <- keepDF
res$name <- tolower(res$name)
res$firstName <- sapply(res$name, decodeFirstName)
res$probFemale <- sapply(res$firstName, getProbabilityFemale) # takes bit long to execute!
# if probab >.5 set it to factor
res$Gender <- ifelse(res$probFemale > .5, "F", "M") # if probability more than .5 then female
# during probability calculation some NA values were created as well
head(res)

# visualize 
ggplot(data = res, aes(x = factor(year))) +
  geom_bar(aes(fill = Gender), position = "fill") +
  scale_fill_manual(name = "Gender", values = c(F = "#DD8D29", M = "#46ACC8"), na.value = "#33A02C") + 
  theme_fivethirtyeight() +
  theme(legend.position = "top") +
  geom_hline(yintercept = .5, size = .5, lty = 2) +
  ggtitle("WH Employee's Gender estimation (based on first name)")

# please take the result with a gain of salt since the "Gender" is a guess based on the first name
# it seems that during obama's year te balance woman/man is pretty equal
# we start to even see a number of women > men in the last years
# and sadly the opposite seems to be seen under Trump's presidency (so far)

f1 <- res %>%
  select(year, Gender, salary) %>%
  group_by(year, Gender) %>%
  summarize(count = n(),
            median_salary = median(salary)) %>%
  ggplot(aes(x = factor(year), y = median_salary)) +
  geom_point(aes(color = Gender, size = count)) +
  scale_color_manual(name = "Gender", values = c(F = "#DD8D29", M = "#46ACC8"), na.value = "#33A02C") +
  theme_fivethirtyeight() + 
  ggtitle("WH employee median salary($) per Gender vs. Year")

# the code will go to Git repository!