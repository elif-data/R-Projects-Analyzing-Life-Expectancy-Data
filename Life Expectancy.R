install.packages("tidyverse")
install.packages("ggrepel")
library(tidyverse)
library(ggrepel)


df <- read_csv("UNdata_Export_20220619_094324772.csv")
head(df)

#First Part: Life expectancy of men vs. women by country between 2000-2005

#Convert Subgroup into two other columns called Female and Male, 
# reshaping data set from long to wide.

df %>% 
  filter(`Year` == "2000-2005") %>%
  summarise(`Country or Area`, Subgroup, Value) %>% 
  pivot_wider(names_from = Subgroup,
              values_from = Value) %>% 
#Alternatively, instead of pivot_wider, use:  spread(Subgroup, Value) 
  ggplot(aes(x = Male, y = Female, label = `Country or Area`)) + 
  geom_point(colour="white", fill="chartreuse3", shape=21, alpha=.55, size=5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_x_continuous(limits = c(35, 85)) +
  scale_y_continuous(limits = c(35, 85)) +
  labs(title="Life Expectancy at Birth by Country",
       subtitle="Years. Period: 2000-2005. Average.",
       caption="Source: United Nations Statistics Division",
       x="Males",
       y="Females")+
# We want to draw attention to some special countries where the gap in 
# life expectancy between men and women is significantly high.
  geom_text(data = top_male,
            size = 4, color = "blue") +
  geom_text(data = top_female,
            size = 4, color = "magenta") +
  theme_bw()

# Sub-setting data to obtain countries of interest
# where the gap in life expectancy between men and women is significantly high
subdata <- df %>% 
  filter(`Year` == "2000-2005") %>%
  summarise(`Country or Area`, Subgroup, Value) %>% 
  pivot_wider(names_from = Subgroup,
              values_from = Value)

top_male <- subdata %>% arrange(Male-Female) %>% head(3) 
top_female <- subdata %>% arrange(Female-Male) %>% head(3)

# Second Part: How has life expectancy by gender evolved?
# Let's find average life expectancy differences between "1985-1990" and "2000-2005" for men and women.
df %>% 
  filter(Year %in% c("1985-1990", "2000-2005")) %>% 
  mutate(Sub_Year=paste(Subgroup, Year, sep="_")) %>% 
  mutate(Sub_Year=gsub("-", "_", Sub_Year)) %>%
  select(-Subgroup, -Year, -Source) %>% 
  spread(Sub_Year, Value) %>% 
  mutate(diff_Female = Female_2000_2005 - Female_1985_1990) %>% 
  mutate(diff_Male = Male_2000_2005 - Male_1985_1990) %>% 
  ggplot(aes(x = diff_Male, y = diff_Female, label = `Country or Area`)) + 
  geom_point(colour="white", fill="chartreuse3", shape=21, alpha=.55, size=5)+
  geom_abline(intercept = 0, slope = 1, linetype=2)+
  scale_x_continuous(limits = c(-25, 25)) +
  scale_y_continuous(limits = c(-25, 25)) +
  labs(title="Life Expectancy at Birth by Country",
       subtitle="Years. Difference between 1985-1990 and 2000-2005. Average.",
       caption="Source: United Nations Statistics Division",
       x="diff_Male",
       y="diff_Female")+
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_text(data = top,
            size = 4) +
  geom_text(data = bottom,
            size = 4) +
  theme_bw()



subdata2 <- df %>% 
  filter(Year %in% c("1985-1990", "2000-2005")) %>% 
  mutate(Sub_Year=paste(Subgroup, Year, sep="_")) %>% 
  mutate(Sub_Year=gsub("-", "_", Sub_Year)) %>%
  select(-Subgroup, -Year, -Source) %>% 
  spread(Sub_Year, Value) %>% 
  mutate(diff_Female = Female_2000_2005 - Female_1985_1990) %>% 
  mutate(diff_Male = Male_2000_2005 - Male_1985_1990)
  
top <- subdata2 %>% arrange(diff_Male+diff_Female) %>% head(3)
bottom <- subdata2 %>% arrange(desc(diff_Male+diff_Female)) %>% head(3)
