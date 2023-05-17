library(tidyverse)
setwd("/Users/mariamniazi/Downloads/cchs-82M0013-E-2017-2018-Annual-component 2")
f1 <- read.csv("cchs-82M0013-E-2017-2018-Annual-component_F1.csv")
f1
myvars <- c("SDCDGRES", "ALCDVTTM")
mydata <- f1[myvars]

mydata <- subset(mydata, SDCDGRES <3)
mydata <- subset(mydata, ALCDVTTM  <4)


# Calculate the mean distress level for each combination of race and years in Canada
agg_data <- mydata %>% group_by(SDCDGRES)   %>% 
  summarize(mean_alc = mean(ALCDVTTM),
            sd_alc=sd(ALCDVTTM))
summary(mydata$ALCDVTTM)
agg_data

# Create bar graph
table(mydata$SDCDGRES)

prop_data <- mydata %>% group_by(SDCDGRES, ALCDVTTM) %>% 
  summarize(count = n()) %>%
  mutate(prop = count / sum(count))

ggplot(prop_data, aes(x = factor(SDCDGRES), y = prop, fill = factor(ALCDVTTM))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#7fc97f", "#beaed4", "#fdc086"),
  labels = c("Regular Drinker", "Occasional drinker", "Non-drinker")) +
  labs(title = "Type of Drinker by Length of Time in Canada",
       x = "Length of time in Canada (years)", 
       y = "Proportion of respondents",
       fill = "Type of drinker") +
  scale_x_discrete(breaks=c("1","2"), labels=c("0-9 years", "9+ years")) +
  theme_minimal()