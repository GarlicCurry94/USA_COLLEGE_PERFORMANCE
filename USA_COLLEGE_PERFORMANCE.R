install.packages("learningr")
install.packages("gapminder")
install.packages("rmarkdown")
install.packages("tinytex")
install.packages("ISLR2")

library(readxl)
library(tidyverse)
library(janitor)
library(palmerpenguins)
library(learningr)
library(gapminder)
library(ggplot2)
library(rmarkdown)
library(tinytex)
library(dplyr)
library(ISLR2)
library("data.table")

View(College)

# exploratory graphics

ggplot(College, aes(x = Grad.Rate)) +
  geom_histogram() # one outlier over 100%

suspicious <- filter(College, Grad.Rate >= 100)
View(suspicious)

ggplot(College, aes(x = F.Undergrad,
                    y = Grad.Rate)) +
  geom_point()

# log transform F.undergrad

ggplot(College, aes(x =log10 (F.Undergrad),
                    y = Grad.Rate)) +
  geom_point()

#adding log10 colum to dataframe

college_log10 <- College %>%
  mutate(log_full = log10(F.Undergrad)) %>%
  select(Grad.Rate,
         log_full,
         Private,
         Top25perc)
View(college_log10)

#modeling

ggplot(College, aes(x =log10 (F.Undergrad),
                    y = Grad.Rate)) +
  geom_point()+
  geom_smooth(method = "lm")

model_undergrad <- lm(Grad.Rate ~ log_full,
                      data = college_log10)
summary(model_undergrad)
plot(model_undergrad)

#private?

ggplot(College, aes(x =log10 (F.Undergrad),
                    y = Grad.Rate,
                    color = Private)) +
  geom_point()+
  geom_smooth(method = "lm",
              se = FALSE)+
  scale_color_brewer(palette = "Dark2")

model_private <- lm(Grad.Rate ~ Private + log_full,
                    data = college_log10)
summary(model_private)

#interaction: private & F.Undergrad

model_private_int <- lm(Grad.Rate ~ Private * log_full,
                        data = college_log10)
summary(model_private_int)

anova(model_private_int)

#top 25%

model_top <- lm(Grad.Rate ~ Private +
                  log_full +
                  Top25perc,
                data = college_log10)
summary(model_top)