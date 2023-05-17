library(tidyverse)
setwd("/Users/mariamniazi/Downloads/cchs-82M0013-E-2017-2018-Annual-component 2")
f1 <- read.csv("cchs-82M0013-E-2017-2018-Annual-component_F1.csv")
f1
myvars <- c("SDCDVIMM", "SDCDGRES", "SDCDGCGT", "GEN_010")
mydata <- f1[myvars]
mydata <- subset(mydata, SDCDGCGT < 6)
mydata <- subset(mydata, SDCDVIMM < 9)
mydata <- subset(mydata, SDCDGRES <3)
mydata <- subset(mydata, GEN_010 <96)


# Create a new variable for race based on the SDCDGCGT variable (1 = white, 2 = visible minority)
mydata <- mydata %>% mutate(race = ifelse(SDCDGCGT == 1, "White", "Visible Minority"))

# Calculate the mean distress level for each combination of race and years in Canada
agg_data <- mydata %>% group_by(SDCDGRES, race)   %>% 
  summarize(mean_satisfaction = mean(GEN_010),
                sd_satisfaction=sd(GEN_010))
summary(mydata$GEN_010)
agg_data

# Create bar graph
table(mydata$SDCDGRES)

ggplot(agg_data, aes(x = factor(SDCDGRES), y = mean_satisfaction, fill = race)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_satisfaction - sd_satisfaction, ymax = mean_satisfaction + sd_satisfaction),
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Overall Life Satisfaction by Race and Length of Time in Canada",
       x = "Length of Time in Canada",
       y = "Mean Life Satisfaction Level",
       fill = "Race") +
  scale_x_discrete(breaks=c("1","2"),
                      labels=c("0-9 years", "9+ years")) +
  theme(plot.title = element_text(size = 12))