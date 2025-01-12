# average wage by degree field
# ACS 2021


library(ggplot2)
library(tidyverse)
library(haven)

load("acs2021_recoded.RData")



compare_fields <- (   (acs2021$DEGFIELDD == "Civil Engineering") |
                  (acs2021$DEGFIELDD == "Electrical Engineering") |
                  (acs2021$DEGFIELDD == "Mechanical Engineering") |
                  (acs2021$DEGFIELDD == "Computer Science") | 
                  (acs2021$DEGFIELDD == "Psychology") |
                  (acs2021$DEGFIELDD == "Political Science and Government") |
                  (acs2021$DEGFIELDD == "Economics") |
                  (acs2021$EDUC == "Grade 12")  )

acs_compare_f <- acs2021 %>% filter((compare_fields & (AGE >= 25) & (AGE <= 65)) )

acs_compare_f$DEGFIELDD <- fct_drop(acs_compare_f$DEGFIELDD)


acs_compare_f$degree_recode <- recode_factor(acs_compare_f$DEGFIELDD, 
                                            "Economics" = "Econ",
                                            "Civil Engineering" = "Engr",
                                            "Electrical Engineering" = "Engr",
                                            "Mechanical Engineering" = "Engr",
                                            "Computer Science" = "CS",
                                            "Political Science and Government" = "PoliSci",
                                            "Psychology" = "Psych",
                                            .default = "HS")


p_age_income <- ggplot(data = acs_compare_f,
                       mapping = aes(x = AGE,
                                     y = INCWAGE,
                                     color = degree_recode,
                                     fill = degree_recode))

p_age_income + geom_smooth(aes(color=degree_recode, fill=degree_recode)) + 
  scale_color_viridis_d(option = "inferno", end = 0.85) + 
  scale_fill_viridis_d(option = "inferno", end = 0.65) + 
  labs(x = "Age", y = "Income",color = "field") + guides(fill = "none")


ggsave("compare_HS_Econ_Engr_CS_PoliSci_Psych.png")

acs_compare_f$Age_band <- cut(acs_compare_f$AGE, breaks = c(25,30,35,40,45,50,55))

s1 <- acs_compare_f %>% group_by(DEGFIELDD, Age_band) %>% summarize(avg_wage = mean(INCWAGE))

write.csv(s1,"avg_wage_by_major_age_w_HS.csv", row.names = FALSE)


