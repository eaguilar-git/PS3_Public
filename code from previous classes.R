#+  --------------------  +
#+  Viz with ggplot2
#+  SIS-750
#+  Feb 19
#+  --------------------  +


# Setup ----------------
library(tidyverse); library(haven)
library(scales)

# Data
df <-
  read_sav('viz1.sav') %>%
  mutate(across(where(is.labelled), as_factor)) %>%
  filter(year > 1990)

# freq table
tabs <-
  df %>%
  count(continent)


# OVERVIEW -------------

# Create plot: gdp per capita over life expectancy
myplot <-
  df %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point(shape = 1, color = 'gray40', alpha = 0.5) +
  geom_smooth(se = F, color = 'red') + 
  scale_x_log10(labels = dollar) + 
  scale_y_continuous(limits = c(0, 90)) +
  labs(y = 'Life expectancy',
       x = 'GDP per capita') + 
  theme_classic() 

# save as image file
ggsave(
  'plot1.png',
  plot = myplot,
  width = 4, height = 3,
  dpi = 6
)
# for screen: pdf or svg
# for hard copy: png or TIFF


# Layers ----------------------------------------

# Order matters
ggplot(data = df, aes(y = lifeExp, x = gdpPercap)) +
  geom_point(shape = 1, alpha = 0.5) + # try as last layer
  stat_smooth(method = 'loess', se = F) 

ggplot(tabs, aes(x = continent, y = n)) +
  geom_col(fill = 'cornflowerblue') + # try as last layer
  geom_text(aes(label = n)) 

# For full range of examples on geoms & stats, see: 
# Claus Wilke, Winston Chang, or ggplot cheat sheet


# Scales ---------------
# base plot
p <-
  df %>%
  ggplot(aes(x = lifeExp, y = gdpPercap)) + 
  theme_classic() +
  geom_point() +
  stat_smooth(se = F) 

## x/y mapping ---- 
# axis breaks, limits, labels
p + 
  scale_x_continuous(
    limits = c(0,90),
    breaks = c(45, 65, 71),
    labels = c('red','orange','banana')
  )

# scale transformations
p + 
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  )

p + scale_y_reverse(labels = label_dollar())

# order discrete mapping     
tabs %>%
  ggplot(aes(x = fct_reorder(continent, -n), y = n)) +
  geom_col() +
  scale_x_discrete(
    labels = c('AFR', 'ASIA', 'EUR', 
               'NRTH AM\nSTH AM','OCEANIA')
  )

## aesthetic scales ----
# color, size, etc (continuous scaling)
df %>%
  ggplot(aes(x = lifeExp, y = gdpPercap, 
             size = pop, fill = pop)) + 
  geom_point(shape = 21, alpha = .5) +
  scale_fill_viridis_c(option = 'plasma') +
  scale_size(range = c(1,9))

# shape and more (discrete scaling)
df %>%
  ggplot(aes(x = lifeExp, y = gdpPercap, 
             shape = continent=='Europe',
             color = continent=='Europe')) +
  geom_point(size = 2, alpha = .7) +
  geom_smooth(se = F) +
  scale_shape_manual(values = c(1,2)) +
  labs(
    color = 'Europe', # must match to merge
    shape = 'Europe'
  )

# note redundant coding
# consider print/screen and color vision deficiency

# text geoms for annotation/labels
df %>%
  ggplot(aes(y = lifeExp, x = gdpPercap)) +
  geom_point(shape = 21) +
  geom_text(
    data = df %>% filter(lifeExp < 38), # match data
    aes(label = country),
    hjust = -0.1
  )


# Coordinates --------------------
# flip axes
tabs %>%
  ggplot(aes(x = continent, y =  n)) +
  geom_col() +
  coord_flip() +
  theme_minimal()

# zoom in (akin to cropping a photo)
tabs %>%
  ggplot(aes(x = continent, y = n)) +
  geom_col() +
  scale_y_continuous(
    expand = expansion(mult = c(0,0.05)) # kill the ghost!
  ) +
  coord_cartesian(ylim = c(0,125))

# compare to limits  
tabs %>%
  ggplot(aes(x = continent, y = n)) +
  geom_col() +
  scale_y_continuous(
    limits = c(0,125),
    expand = expansion(mult = c(0, .05))
  )


# Themes ----------------------------------------
# baseline plot
p <- 
  df %>%
  ggplot(aes(y = lifeExp, x = continent, fill = continent)) +
  geom_boxplot(outlier.shape = 1) +
  labs(y = 'Years', title = 'Life expectancy', x = NULL)

# basic visual themes
p + theme_bw()

# customizing everything
mytheme = theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_text(face = 'bold', color = 'green'),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(face = 'italic'),
    panel.grid.major.y = element_line(color = 'red'),
    plot.title = element_text(hjust = 0.5),
    legend.position = 'none'
  )

p + mytheme

#+ Merging and appending data frames
#+ Feb 5, 2025

# load packages
library(tidyverse)





# APPEND ------------------------------
#+ Add new obs/cases to a frame
#+ command: bind_rows()
#+ Requires:
#+ - same var names for matched vars
#+ - same var types for matched vars

# two data frames to combine
cd1 <- readxl::read_excel('cds Alab NewY.xlsx')
cd2 <- readxl::read_excel('cds NorthC Wyo.xlsx')

## Add to original (requires same col names)  
cds <- bind_rows(cd1, cd2)
cdsB <- bind_rows(
  list(a = cd1, b = cd2), 
  .id = 'source' # adds id variable for source
)

#+ if it doesn't work, check var names and fix (DistNum?)


# MERGE/JOIN --------------------------
#+ A mutating join to add variables
#+ Matches cases by unique ids

## Data to incorporate
states <- read_csv('states.csv')
inc <- read_csv('incumb.csv')
demog <- read_csv('cddem.csv')

## Merge two frames by unique identifier
df1 <-
  left_join(
    cds,    # df 1
    demog,  # df 2
    by = join_by(District) # unique id var
  )
#+ note what happened to 'party' variable in that join.
#+ drop duplicates up front where possible (select(-party))

#+ right_join() ... keeps only records in second frame
#+ inner_join() ... keeps only records in BOTH frames
#+ full_join() ... keeps all records from each frame

## Where id vars don't match: merge sate data
df2 <-
  left_join(
    df1, 
    states, 
    by = join_by(state == stcode)
  ) 
# can you find the new additions?


## where multiple columns jointly identify cases: join with inc    
df3 <-
  left_join(
    df2, 
    inc,
    by = join_by(inc.first, inc.last)
  )

#+ we didn't do great with names, and we have duplicates
#+ we'd need to clean up now or go back and clean as we code


# PIVOT/RESHAPE -----------------------
## data
wdi <- readxl::read_excel('WDI pull.xlsx', na = '..')

## lengthening pivots -----------
## small frame as example
mx <- 
  wdi %>%
  filter(
    cName == 'Mexico',
    vCode == 'EN.ATM.CO2E.PP.GD'
  )

## "tidy" by lengthening
mx_long <-
  mx %>%
  pivot_longer(
    cols = 5:15, # try different specs
    names_to = 'Year', # new var recording col names
    names_transform = list(Year = as.integer),
    values_to = 'CO2emit'  # new var recording the data
  )

## widening pivot --------------
## start with small frame for example
y2005 <-
  wdi %>%
  select(cName:`2005`)  # use `...` when var name is a number

## widen the data
wdi_wide <-
  y2005 %>%
  pivot_wider(
    names_from = c(vCode, vName), # try only one and see what happens
    values_from = `2005`,
    names_glue = '{vCode}' # try w/o this line
  )


# FIX THE WDI -------------------------
# lengthen first
wdi_long <-
  wdi %>%
  pivot_longer(
    cols = -c(1:4),
    names_to = 'year',
    names_transform = list(year = as.integer),
    values_to = 'scores'
  )

# widen from there
wdi2 <-
  longwdi %>%
  select(-vName) %>%
  pivot_wider(
    names_from = vCode,
    values_from = scores
  )


#+ Why bother with pivots?
#+ widen for making tables
#+ lengthen for graphs and basic tidy

#+ TRY ON YOUR OWN:  
#+ Compare Brazil and Canada in C02 emissions
#+ Select the countries and vars from wdi2
#+ Use group_by() and summarize() to get your stats
#+ Pivot to make a nicer table




#+ Wrangling: tidy variables
#+ Jan 29, 2025

# Setup ----------------
# Packages
library(tidyverse)
library(knitr)

# Custom function
fctr = function(...) {
  args = rlang::list2(...)
  rhs = map(args, rlang::f_rhs)
  cases = case_when( !!!args )
  exec(fct_relevel, cases, !!!rhs)
}  

# Data
df <- read_csv('water.csv')


# Get the data you want ----------
# Select variables
df2 <-
  df %>%
  select(
    hh_id, age,   # items I want
    hv201:hv215,  # range of vars I want
    -hv213.       # don't want this
  )

# Select cases (to keep)
df3 <-
  df %>%
  filter(
    Q100 == 1,  # males only
    Q1   <= 30  # check: is this a good idea?
  ) 

rm(df2, df3)

# mutate basics ------------------  
# Create columns
df2 <-
  df %>%
  mutate(
    rural = if_else(hv025 == 'rural', 1, 0),
    age_months = age * 12,
    age_dev = age - median(age, na.rm = T)
  )

# Place the columns
df2 <- 
  df %>%
  mutate(
    r_id = row_number(),
    int_year = hv007,
    .before = 1 # use column number or name; also .after
  )

# Decide what to keep
df2 <-
  df %>%
  mutate(
    r_id = row_number(),
    age_yrs = age * 12,
    .keep = 'none' # alts: 'used', 'unused'
  )


# NAs and Strings ----------------
# unwanted strings
df2 <-
  df %>%
  mutate(
    watEx1 = as.numeric(water_mins), # changes strings to NA
    waterFetchMins = case_when(
      water_mins == 'on premises' ~ 0,
      water_mins == '999' ~ NA, 
      TRUE ~ as.numeric(water_mins)
    ),
    .keep = 'used'
  )  # review the Warning messages; check: summary(df2$waterFetchMins)

# Combine or split strings
df2 <-
  df %>%
  mutate(
    r_id = row_number(),
    region_x_rid = paste(region_id, r_id, sep = '_'),
    region2 = str_extract(region_x_rid, "[^_]+"),
    rid2 = str_extract(region_x_rid, "[^_]*$"),
    .keep = 'used'
  )

# Change values to/from missing
df2 <-
  df %>%
  mutate(
    attend1 = na_if(hv121, '9'),
    attend2 = if_else(hv121 == '9', NA, hv121),
    attend3 = if_else(is.na(attend2), 'dk/nr', attend2),
    .keep = 'used'
  )

# Factors and labels -------------  
# Relevel
df2 <-
  df %>%
  mutate(
    inc_quint = fct_relevel(
      hh_income, # source variable
      'poorest','poorer', 'middle','richer','richest'
    )
  )

# create ordered categories
df2 <-
  df %>%
  mutate(
    ageFact = fctr(
      age <= 9 ~ '6-9 years',
      age %in% 10:12 ~ '10-12',
      age >= 13 ~ '13-15' # vs TRUE
    ),
    .keep = 'used'
  )

# Repeat across columns ----------  
# Mutate across
df2 <-
  df %>%
  mutate(
    across(where(is.character), ~na_if(.x, '9'))
  )

# Grouping -----------------------
# Aggregate: group and summarize
df2 <-
  df %>%
  group_by(hh_income) %>%
  summarize(
    mean_edu = mean(eduyrs, na.rm = T)
  )

# Group level stats: group and mutate
df2 <-
  df %>%
  group_by(hh_income) %>%
  mutate(
    mean_edu = mean(eduyrs, na.rm = T)
  ) %>%
  ungroup()






#+ Statistical programming in R
#+ Austin Hart
#+ Spring 2024


# Packages ---------------------------------
# attaching a library (for installed package)
library(tidyverse)

# access a function on the fly
df <- readr::read_csv('trash_wheels.csv')


# Data I/O ---------------------------------
# Import our trash wheel data
df <- read_csv('trash_wheels.csv')

# compare two alternatives:
read_csv(file = 'trash_wheels.csv')
df2 <- read_csv(file = 'trash_wheels.csv')

# Output
save(df1, 'newdata.RData')
write_csv(df1, 'newdata.csv')  

#+ WARNINGS: 
#+    Please avoid read.csv() 
#+    Do not use point + click for Data I/O


# Explore ----------------------------------
# Structure of a data frame
str(df)
summary(df)  

## character/factor vars -------
## frequency table
tab1 <-
  df %>%
  count(wheel, name = 'N') %>%
  mutate(Per = N / sum(N) * 100)

## Beautify the table for printing
tab1 %>%
  knitr::kable(digits = 1L) # optional last step

## Graphing
plot1 <-
  tab1 %>%
  ggplot(aes(x = wheelName, y = percent)) +
  geom_col()  


## numeric vars ----------------
## Summary stats
summary(df['cigarettes'])

df %>% # get summary stats
  summarize(
    AvgCigs = mean(cigarettes, na.rm = T),
    AvgBottles = mean(plastic_bottles, na.rm = T),
    Diff = mean(cigarettes, na.rm = T) - mean(plastic_bottles, na.rm = T)
  )

## Plot the distribution    
df %>%
  ggplot(aes(x = plastic_bags)) +
  geom_histogram(color = 'white')    


# Objects --------------
# Vectors/values
a <- letters[1:10] # check type: is(a)
b <- 100:109

# Data frames/tibbles
df1 <- data.frame(a, b)
df2 <- tibble(b, a)
l1 <- list(d1 = df1, d2 = df2)

# Extract an item from list or frame
ex1 <- df1$b
ex2 <- df1[['b']]
ex3 <- 
  df1 %>% 
  pull(b)

# Subset an element (maintains structure)
sub1 <- df1[2]
sub2 <- df1['b'] # use quotes to call by name
sub3 <- df1[1:4, 'b'] # select rows 1-4, variable 'b'
sub4 <- 
  df1 %>%
  select(b) %>%
  filter(b <= 103)  


#+ MAIN FUNCTIONS ENCOUNTERED
#+    read_***() to input data
#+    mutate() to create new coulmns
#+    select() to select a dataframe  
#+    filter() to select subset of cases from df
#+    = or <- to assign new objects
#+    $ and [[]] to extract element of object
#+    [] to subset object



---
title: "Monty Hall Exercise"
author: "Austin Hart"
date: "Jan 15, 2025"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE,
  eval = TRUE,
  fig.align = 'center',
  message = FALSE,
  warning = FALSE
)

# load packages
  library(tidyverse)
  library(knitr)
```


1. See my simulation below. I chose 10,000 trials.

```{r}
# Simulation -----------  
## set the stage
  doors = c('A', 'B', 'C')

## keep score 10,000
  results = tibble(
    trial = 1:10000,
    winner = NA
  )
  
## simulate the game  
  for (i in 1:10000){
    car = sample(doors, 1)  # place car at random
    selection = sample(doors, 1)
    reveal = sample(doors[doors != car & doors != selection], 1)  
    switch = sample(doors[doors != reveal & doors != selection], 1)
    results[i, 'winner'] = if_else(car == switch, 'Marilyn', 'Paul')
  }
```

2. See the table below

```{r}
## tabulate wins, calculate win %
  outcome =
    results %>%
    count(winner) %>%
    mutate(win_per = n / sum(n) * 100)

  kable(outcome, caption = 'Simulation winners') 

```

3. See graph.

```{r fig1, fig.width=3, fig.height=2, fig.cap = 'Monty Hall Simulations'}
## Graph
  ggplot(data = outcome, aes(x = winner, y = win_per)) +
    geom_col(fill = 'cornflowerblue', color = 'gray27') +
    labs(
      x = 'Winner is...',
      y = 'Percent'
    ) +
    scale_y_continuous(limits = c(0,100)) +
    coord_flip() +
    theme_minimal()

```

4. In `r nrow(results)` simulations, the contestant who switched won a total of `r nrow(filter(results, winner == "Marilyn"))` times. This is consistent with Marilyn's prediction: it's better to switch.