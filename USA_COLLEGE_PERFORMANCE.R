setwd("~/GitHub/USA_COLLEGE_PERFORMANCE")

library(tidyverse)
library(ggplot2)
library(ISLR2)


View(College)

# exploratory graphics

ggplot(College, aes(x = Grad.Rate)) +
  geom_histogram() # one outlier over 100%
ggsave("Grad_rate.png")

suspicious <- filter(College, Grad.Rate >= 100)
View(suspicious)

ggplot(College, aes(x = F.Undergrad,
                    y = Grad.Rate)) +
  geom_point()
ggsave("Undergrad.png")

# log transform F.undergrad

ggplot(College, aes(x =log10 (F.Undergrad),
                    y = Grad.Rate)) +
  geom_point()
ggsave("Undergrad_log_transform.png")

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
ggsave("Modeling.png")

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
ggsave("Private.png")


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

#Favourite Colleges 

head(college_log10)
class(college_log10)

#College Name is the index colum and is not recognized by the data frame
#the code below fixes it

college_log10_2 <- cbind(Name = rownames(college_log10), college_log10)
rownames(college_log10) <- 1:nrow(college_log10)

college_log10_2 |>
  filter(Name == "Bowling Green State University" |
           Name == "East Carolina University" |
           Name == "Johns Hopkins University") |>
  ggplot(aes(x = Name,
             y = Grad.Rate, colour= Name))+
  geom_point(size=5)+
  scale_color_manual(values = c("Bowling Green State University" = "brown",
                                "East Carolina University"="purple",
                                "Johns Hopkins University"="lightblue")) +
  coord_flip()+
  labs(title = "Favourite Colleges Grad Rate")
ggsave("Favourite_colleges_grad_rate.png", scale = 2)


College <- cbind(Name = rownames(College), College)
rownames(College) <- 1:nrow(College)
View(College)

College <- College[,-1]
View(College)

College |>
filter(Name == "Bowling Green State University" |
         Name == "East Carolina University" |
         Name == "Johns Hopkins University") |>
  ggplot(aes(Accept, Enroll, colour = Name))+
  geom_col()+
  scale_color_manual(values = c("Bowling Green State University" = "brown",
                                "East Carolina University"="purple",
                                "Johns Hopkins University"="lightblue")) +
  facet_wrap(~Name) 
ggsave("Favourite_colleges_accept_v_enroll.png", scale = 2)


