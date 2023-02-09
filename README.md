Lab 05 - Data Wrangling
================

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/jsc370-2023/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages if you
    plan to work with those).

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

2.  Load the met data from
    <https://github.com/JSC370/jsc370-2023/blob/main/labs/lab03/met_all.gz>
    or (Use
    <https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab03/met_all.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
fn <- "https://raw.githubusercontent.com/JSC370/jsc370-2023/main/labs/lab03/met_all.gz"
if (!file.exists("met_all.gz"))
  download.file(fn, destfile = "met_all.gz")
met <- data.table::fread("met_all.gz")
```

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

3.  Merge the data as we did during the lecture.

``` r
met_merged <- merge(
  # Data
  x = met,
  y = stations,
  # List of variables to match 
  by.x = "USAFID",
  by.y = "USAF",
  # Which obs to keep?
  all.x = TRUE,
  all.y = FALSE
)
```

## Question 1: Representative station for the US

Across all weather stations, what is the median station in terms of
temperature, wind speed, and atmospheric pressure? Look for the three
weather stations that best represent continental US using the
`quantile()` function. Do these three coincide?

``` r
met_avg_lz <- met_merged %>% 
  group_by(USAFID) %>% 
  summarise(
    temp = mean(temp, na.rm = TRUE),
    wind.sp = mean(wind.sp, na.rm=TRUE),
    atm.press = mean(atm.press, na.rm=TRUE)
  ) %>% collect()
```

``` r
met_med_lz <- met_avg_lz %>% 
  summarise(across(
    c(temp, wind.sp, atm.press), 
    function(x) quantile(x, prob = 0.5, na.rm = TRUE) 
    )
  ) %>% collect()
```

``` r
met_avg_lz %>% 
  filter(
    temp == met_med_lz %>%  pull(temp) | 
    wind.sp == met_med_lz %>%  pull(wind.sp) |
    atm.press == met_med_lz %>%  pull(atm.press)
  ) %>% collect()
```

    ## # A tibble: 1 × 4
    ##   USAFID  temp wind.sp atm.press
    ##    <int> <dbl>   <dbl>     <dbl>
    ## 1 720929  17.4    2.46       NaN

``` r
met_avg_lz %>% 
  filter(
    between(temp, met_med_lz %>%  pull(temp) - 0.003, met_med_lz %>%  pull(temp) + 0.002) | 
    wind.sp == met_med_lz %>%  pull(wind.sp) |
    between(atm.press, met_med_lz %>%  pull(atm.press) - 0.0005, met_med_lz %>%  pull(atm.press) + 0.001) 
  ) %>% collect()
```

    ## # A tibble: 3 × 4
    ##   USAFID  temp wind.sp atm.press
    ##    <int> <dbl>   <dbl>     <dbl>
    ## 1 720458  23.7    1.21      NaN 
    ## 2 720929  17.4    2.46      NaN 
    ## 3 723200  25.8    1.54     1015.

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.

``` r
udist <- function(a, b, c, d, e, f) {
    sqrt((a - d)^2 + (b - e)^2 + (c - f)^2)
}
```

``` r
met_avg_state <- met_merged %>% 
  group_by(USAFID) %>% 
  summarise(
    temp = mean(temp, na.rm = TRUE),
    wind.sp = mean(wind.sp, na.rm=TRUE),
    atm.press = mean(atm.press, na.rm=TRUE),
    state = STATE
  )  %>% distinct() %>%  collect()

# find median of each state
state_med <-  met_avg_state %>% group_by(state) %>% summarise(
    state_temp = median(temp, na.rm=TRUE),
    state_wind.sp = median(wind.sp, na.rm=TRUE),
    state_atm.press = median(atm.press, na.rm=TRUE),
    state = state
  ) %>% distinct() %>% collect()
```

    ## `summarise()` has grouped output by 'state'. You can override using the
    ## `.groups` argument.

``` r
state_med
```

    ## # A tibble: 48 × 4
    ## # Groups:   state [48]
    ##    state state_temp state_wind.sp state_atm.press
    ##    <chr>      <dbl>         <dbl>           <dbl>
    ##  1 AL          26.3          1.66           1015.
    ##  2 AR          26.2          1.94           1015.
    ##  3 AZ          30.3          3.07           1010.
    ##  4 CA          22.7          2.57           1013.
    ##  5 CO          21.5          3.10           1013.
    ##  6 CT          22.4          2.10           1015.
    ##  7 DE          24.6          2.75           1015.
    ##  8 FL          27.6          2.71           1015.
    ##  9 GA          26.7          1.50           1015.
    ## 10 IA          21.3          2.68           1015.
    ## # … with 38 more rows

``` r
met_avg_state_andmed <- merge(
  x = met_avg_state,
  y = state_med,
  by.x = "state",
  by.y = "state",
  all.x = TRUE,
  all.y = FALSE
)
```

``` r
met_avg_state_andmed[is.na(met_avg_state_andmed)] <- 0
diff_station <- met_avg_state_andmed %>% 
  mutate(distance = udist(temp, wind.sp, atm.press, state_temp, state_wind.sp, state_atm.press))
```

``` r
min_diff <- diff_station %>% group_by(state) %>% summarise(min_diff = min(distance))
merge(
  x = min_diff,
  y = diff_station,
  by.x = c("min_diff", "state"),
  by.y = c("distance", "state"),
  all.x = FALSE,
  all.y = FALSE) %>% select(USAFID, state)
```

    ##    USAFID state
    ## 1  724180    DE
    ## 2  720254    WA
    ## 3  722106    FL
    ## 4  722286    AL
    ## 5  725064    MA
    ## 6  722416    TX
    ## 7  723545    OK
    ## 8  720911    ND
    ## 9  725480    IA
    ## 10 722358    MS
    ## 11 724176    WV
    ## 12 722745    AZ
    ## 13 724016    VA
    ## 14 724580    KS
    ## 15 725560    NE
    ## 16 723174    NC
    ## 17 725327    IN
    ## 18 724090    NJ
    ## 19 725079    RI
    ## 20 725867    ID
    ## 21 722486    LA
    ## 22 722970    CA
    ## 23 723346    TN
    ## 24 723160    GA
    ## 25 726077    ME
    ## 26 724298    OH
    ## 27 725395    MI
    ## 28 725087    CT
    ## 29 725755    UT
    ## 30 725130    PA
    ## 31 726115    VT
    ## 32 724057    MD
    ## 33 726050    NH
    ## 34 726590    SD
    ## 35 723495    MO
    ## 36 726798    MT
    ## 37 723407    AR
    ## 38 725194    NY
    ## 39 726650    WY
    ## 40 725440    IL
    ## 41 724240    KY
    ## 42 726452    WI
    ## 43 723190    SC
    ## 44 726550    MN
    ## 45 725805    NV
    ## 46 724767    CO
    ## 47 725895    OR
    ## 48 722686    NM

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all \~100 points
in the same figure, applying different colors for those identified in
this question.

Knit the doc and save it on GitHub.

## Question 4: Means of means

Using the `quantile()` function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

- low: temp \< 20
- Mid: temp \>= 20 and temp \< 25
- High: temp \>= 25

Once you are done with that, you can compute the following:

- Number of entries (records),
- Number of NA entries,
- Number of stations,
- Number of states included, and
- Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, examine the
  association between median temperature (y) and median wind speed (x).
  Create a scatterplot of the two variables using ggplot2. Add both a
  linear regression line and a smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.
