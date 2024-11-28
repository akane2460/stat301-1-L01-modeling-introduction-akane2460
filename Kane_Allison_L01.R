# L01 modeling introduction ----
# Stat 301-2

## load packages----
# see exercise 1

## Exercises----

### Ex 1----
library(tidyverse)
library(tidymodels)
library(ISLR)
library(ISLR2)
library(corrplot)

# manage conflicts with tidymodels_conflicts() in this lab


### Ex 2----
auto_data <- read_csv("data/Auto.csv")
college_data <- read_csv("data/College.csv")

# codebooks
?ISLR2::Auto
?ISLR2::College

### Ex 3----

# auto_data cleaning
auto_data |>  skimr::skim_without_charts()
  # major issues:
    # horsepower is character-- not good, also missingness encoded as ?
    # cylinders shouldn't be numerical-- should be factor/categorical
    # origin should be a factor

auto_data |> 
  janitor::clean_names() |> 
  mutate(horsepower = as.numeric(horsepower),
         cylinders = factor(cylinders),
         origin = factor(origin, levels = 1:3, labels = c("American", "European", "Japanese"))) |> 
  write_rds("data/processed/auto_cleaned.rds")

auto_cleaned <- read_rds("data/processed/auto_cleaned.rds")

auto_cleaned |> skimr::skim_without_charts()

# college_data cleaning
college_data |>  skimr::skim_without_charts()
  # major issues:
    # x1 isn't a good name
    # private should be a factor

college_data |> 
  janitor::clean_names() |> 
  rename(college_name = x1) |> 
  mutate(private = factor(private)) |> 
  write_rds("data/processed/college_cleaned.rds")

college_cleaned <- read_rds("data/processed/college_cleaned.rds")

college_cleaned |> skimr::skim_without_charts()

### Ex 4----

# part a
  # see qmd

# part b
  # see qmd

# part c

# summary

auto_cleaned |> 
  filter(is.na(horsepower) == FALSE) |> 
  group_by(origin) |> 
  summarise(
    median_displacement = median(displacement),
    median_mpg = median(mpg),
    median_weight = median(weight),
    median_acceleration = median(acceleration),
    median_horsepower = median(horsepower)
  )
# notes from summary
  # some differences in displacement 
    # american much greater than european and japanese, but not much diff btw the two, prob not the best predictor
  # japanese cars med mpg > euro > US
  # japanese med weight < euro < us
  # median accel not a HUGE difference, US < European < Japanese, probably not the best predictor

# vizualizing

  # displacement? 
auto_cleaned |> 
  ggplot(aes(x = displacement)) +
  facet_wrap(~ origin) +
  geom_histogram()
    # not a ton of differences by origin, besides american cars having a greater variation and wider range

  # mpg?
auto_cleaned |> 
  ggplot(aes(x = mpg)) +
  facet_wrap(~ origin) +
  geom_histogram()
  # definitely differences in their center, japan > euro > us

  # weight?
auto_cleaned |> 
  ggplot(aes(x = weight)) +
  facet_wrap(~ origin) +
  geom_histogram()
  # MUCH greater variation and range in american cars
  # japan and euro a bit more alike 

  # acceleration?
auto_cleaned |> 
  ggplot(aes(x = acceleration)) +
  facet_wrap(~ origin) +
  geom_histogram()
  # MUCH greater range and variation in american cars AGAIN
  # european 

# horsepower?
auto_cleaned |> 
  ggplot(aes(x = horsepower)) +
  facet_wrap(~ origin) +
  geom_histogram()
  # greater range and variation for US than euro and japanese
  # euro and japan about the same

# mpg seems like its worth examining


# how does it relate to other variables?
auto_cleaned |> 
  filter(is.na(horsepower) == FALSE) |> 
  select(mpg, displacement, weight, acceleration, horsepower) |> 
  cor() |> 
  corrplot()
  # mpg strong and negatively associated with displacement, horsepower and weight
  # mpg is weak and positively associated with acceleration, probably not the best to use

# what should we examine going forward? mpg and weight could be worth exploring
  # horsepower doesn't seem to be a great predictor when differentiating between japan and us
  # acceleration doesn't seem to have much of a difference between the three
  # displacement could maybe be worth exploring but it is more difficult to discern between japan and us
  # weight and mpg seem to have a pretty clear difference across all origins
    # these are the ideal variables to explore


### Ex 5----

# part a
# see qmd

# part b
# see qmd

# part c

# summary

# some relevant variables could be: f_undergrad, top10perc, outstate, books, personal, ph_d termina, s_f_ratio, expend

college_cleaned |> 
  summarise(
    median_f_undergrad = median(f_undergrad),
    median_top10perc = median(top10perc),
    median_outstate = median(outstate),
    median_books = median(books),
    median_personal = median(personal),
    median_ph_d = median(ph_d),
    median_terminal = median(terminal),
    median_expend = median(expend)
  )

# univariate

# f_undergrad
college_cleaned |> 
  ggplot(aes(x = f_undergrad)) +
  geom_histogram()

# top10perc
college_cleaned |> 
  ggplot(aes(x = top10perc)) +
  geom_histogram()
  # a bit of variation here, but generally on the lower end

# top25perc
college_cleaned |> 
  ggplot(aes(x = top25perc)) +
  geom_histogram()
# a bit of variation here, but generally on the lower end

# outstate
college_cleaned |> 
  ggplot(aes(x = outstate)) +
  geom_histogram()
  # lots of variation here, median abt 10,000

# books
college_cleaned |> 
  ggplot(aes(x = books)) +
  geom_histogram()
  # books around 500 USD, pretty standard across the board, not much variation
  # maybe not worth exploring as much if there isn't much of a difference across all schools

# personal
college_cleaned |> 
  ggplot(aes(x = personal)) +
  geom_histogram()
  # a little bit of variatin, but not too much
  # doesn't seem like it would be a reliable predictor, for the same reasons as books

# ph_d
college_cleaned |> 
  ggplot(aes(x = ph_d)) +
  geom_histogram()
  # a decent amount of variation, this could be a good predictor
  # since it could be an indication of quality of instruction 
  # because being taught by more experienced/accredited professors

# terminal
college_cleaned |> 
  ggplot(aes(x = terminal)) +
  geom_histogram()
  # similar to ph_d, a bit less variation

# expend
college_cleaned |> 
  ggplot(aes(x = expend)) +
  geom_histogram()
  # not much variation in expenditures per student
  # still should maybe examine

# bivariate
college_cleaned |> 
  select(grad_rate, f_undergrad, top10perc, top25perc, outstate, ph_d, terminal, expend, books, personal) |> 
  cor() |> 
  corrplot()
  # f undergrad has almost no measurable effect
  # top10perc, top25perc, outstate are decently positive associated
  # ph_d, terminal and expend are weakly positive associated
  # personal is weakly negative associated

# overall:
  # the best predictors of grad_rate are outstate, top10perc and top25 perc. 
  # personal and expend could be included if necessary but they don't seem to be as reliable

### Ex 6----

# see qmd

### Ex 7----

# see qmd
