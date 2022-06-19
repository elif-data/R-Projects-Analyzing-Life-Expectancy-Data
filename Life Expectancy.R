install.packages("tidyverse")
install.packages("ggrepel")
library(tidyverse)
library(ggrepel)


df <- read_csv("UNdata_Export_20220619_094324772.csv")
head(df)

subdata <- df %>% 
  filter(`Year` == "2000-2005") %>%
  summarise(`Country or Area`, Subgroup, Value) %>% 
  pivot_wider(names_from = Subgroup,
              values_from = Value)

#Convert Subgroup into two other columns called Female and Male, 
# reshaping dataset from long to wide.

df %>% 
  filter(`Year` == "2000-2005") %>%
  summarise(`Country or Area`, Subgroup, Value) %>% 
  pivot_wider(names_from = Subgroup,
              values_from = Value) %>% 
#To pivot it this way, also use:  spread(Subgroup, Value) 
  ggplot(aes(x = Male, y = Female)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_x_continuous(limits = c(35, 85)) +
  scale_y_continuous(limits = c(35, 85)) +
  labs(title="Life Expectancy at Birth by Country",
       subtitle="Years. Period: 2000-2005. Average.",
       caption="Source: United Nations Statistics Division",
       x="Males",
       y="Females")

## Subseting data to obtain countries of interest
# Where the gap in life expectancy between men and women is significantly high
top_male <- subdata %>% arrange(Male-Female) %>% head(3) 
top_female <- subdata %>% arrange(Female-Male) %>% head(3)




