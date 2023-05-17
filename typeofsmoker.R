library(tidyverse)

setwd("/Users/mariamniazi/Downloads/cchs-82M0013-E-2017-2018-Annual-component 2")

# Read the data
f1 <- read.csv("cchs-82M0013-E-2017-2018-Annual-component_F1.csv")

# Select the required variables
myvars <- c("SDCDGRES", "SMK_005")
mydata <- f1[myvars]

# Filter the data
mydata <- subset(mydata, SDCDGRES < 3)
mydata <- subset(mydata, SMK_005  < 4)

# Aggregate the data
agg_data <- mydata %>% group_by(SDCDGRES) %>% 
  summarize(mean_smoke = mean(SMK_005),
            sd_smoke = sd(SMK_005))

# Calculate proportion data
prop_data <- mydata %>% group_by(SDCDGRES, SMK_005) %>% 
  summarize(count = n()) %>%
  mutate(prop = count / sum(count))

# Create horizontal bar graph
ggplot(prop_data, aes(x = prop, y = factor(SDCDGRES), fill = factor(SMK_005))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("dodgerblue", "darkolivegreen1", "lightpink"),
                    labels = c("Daily smoker", "Occasional smoker", "Non-smoker")) +
  labs(title = "Type of Smoker by Length of Time in Canada",
       y = "Length of time in Canada (years)", 
       x = "Proportion of respondents",
       fill = "Type of smoker") +
  scale_y_discrete(breaks = c("1", "2"), labels = c("0-9 years", "9+ years")) +
  theme_minimal()
