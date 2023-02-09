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

``` r
library(leaflet)
```

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
    lat = mean(lat, na.rm=TRUE),
    lon = mean(lon, na.rm=TRUE),
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
med_close <- merge(
  x = min_diff,
  y = diff_station,
  by.x = c("min_diff", "state"),
  by.y = c("distance", "state"),
  all.x = FALSE,
  all.y = FALSE)
med_close
```

    ##      min_diff state USAFID     temp  wind.sp atm.press      lat        lon
    ## 1  0.00000000    DE 724180 24.56026 2.752929  1015.046 39.67400  -75.60600
    ## 2  0.00000000    WA 720254 19.24684 1.268571     0.000 46.68276 -122.98300
    ## 3  0.04772342    FL 722106 27.52774 2.711121  1015.322 26.58501  -81.86101
    ## 4  0.05608376    AL 722286 26.35793 1.675828  1014.909 33.21200  -87.61600
    ## 5  0.13084377    MA 725064 21.40933 2.786213  1014.721 41.91000  -70.72900
    ## 6  0.18029339    TX 722416 29.75394 3.539980  1012.331 29.70899  -98.04599
    ## 7  0.18052457    OK 723545 27.03555 3.852697  1012.711 36.16199  -97.08896
    ## 8  0.18672684    ND 720911 18.34248 3.940128     0.000 46.94196  -98.01800
    ## 9  0.19926933    IA 725480 21.43686 2.764312  1014.814 42.55358  -92.40088
    ## 10 0.21966149    MS 722358 26.54093 1.747426  1014.722 31.18298  -90.47100
    ## 11 0.22082482    WV 724176 21.94072 1.649151  1015.982 39.64300  -79.91600
    ## 12 0.23342190    AZ 722745 30.31538 3.307632  1010.144 32.16695 -110.88300
    ## 13 0.23665335    VA 724016 24.29327 1.588105  1014.946 38.13701  -78.45499
    ## 14 0.24751336    KS 724580 24.01181 3.548029  1013.449 39.55090  -97.65090
    ## 15 0.25159903    NE 725560 21.80411 3.428358  1014.386 41.98568  -97.43479
    ## 16 0.26213187    NC 723174 24.95288 1.744838  1015.350 36.04700  -79.47700
    ## 17 0.26577311    IN 725327 22.40044 2.547951  1015.145 41.45300  -87.00600
    ## 18 0.26971491    NJ 724090 23.47238 2.148606  1015.095 40.03300  -74.35016
    ## 19 0.28039594    RI 725079 22.27697 2.583469  1014.620 41.53300  -71.28300
    ## 20 0.28377685    ID 725867 20.81272 2.702517  1012.802 42.54201 -113.76605
    ## 21 0.29399685    LA 722486 28.16413 1.592840  1014.544 32.51596  -92.04098
    ## 22 0.30049511    CA 722970 22.76040 2.325982  1012.710 33.81264 -118.14652
    ## 23 0.30391254    TN 723346 24.59407 1.493531  1015.144 35.59302  -88.91700
    ## 24 0.31157584    GA 723160 26.59746 1.684538  1014.985 31.53600  -82.50700
    ## 25 0.31653296    ME 726077 18.49969 2.337241  1014.475 44.45000  -68.36677
    ## 26 0.32969606    OH 724298 21.79537 2.771958  1015.248 40.70800  -84.02700
    ## 27 0.33875622    MI 725395 20.44096 2.357275  1015.245 42.26697  -84.46697
    ## 28 0.34635143    CT 725087 22.57539 2.126514  1014.534 41.73601  -72.65098
    ## 29 0.34923813    UT 725755 24.31031 3.361211  1012.243 41.11737 -111.96637
    ## 30 0.36234584    PA 725130 21.69177 1.970192  1015.125 41.33380  -75.72500
    ## 31 0.36261055    VT 726115 18.60548 1.101301  1014.985 43.34400  -72.51800
    ## 32 0.37630511    MD 724057 25.00877 2.033233  1014.497 39.47174  -76.16996
    ## 33 0.40778497    NH 726050 19.86188 1.732752  1014.487 43.20409  -71.50245
    ## 34 0.42910869    SD 726590 19.95928 3.550722  1014.284 45.44377  -98.41344
    ## 35 0.44048404    MO 723495 24.31621 2.550940  1014.296 37.15200  -94.49501
    ## 36 0.44582815    MT 726798 19.47014 4.445783  1014.072 45.69800 -110.44004
    ## 37 0.46112989    AR 723407 25.86949 2.208652  1014.575 35.83100  -90.64600
    ## 38 0.46256996    NY 725194 20.37207 2.444051  1015.327 42.64299  -77.05599
    ## 39 0.52649035    WY 726650 19.75554 4.243727  1013.527 44.33905 -105.54099
    ## 40 0.53059335    IL 725440 22.84806 2.566829  1014.760 41.46325  -90.52032
    ## 41 0.57786362    KY 724240 23.79463 2.450704  1015.375 37.90032  -85.96723
    ## 42 0.58447881    WI 726452 19.21728 2.411747  1015.180 44.35900  -89.83701
    ## 43 0.58529642    SC 723190 25.73726 2.253408  1015.116 34.49800  -82.71000
    ## 44 0.62096399    MN 726550 19.11831 2.832794  1015.319 45.54301  -94.05102
    ## 45 0.70623784    NV 725805 25.21743 3.101560  1012.461 40.06799 -118.56898
    ## 46 0.94422827    CO 724767 21.97732 2.780364  1014.082 37.30699 -108.62600
    ## 47 1.02527449    OR 725895 18.79793 2.307326  1014.726 42.14705 -121.72405
    ## 48 1.30437627    NM 722686 26.00522 4.503611  1012.742 34.38358 -103.31565
    ##    state_temp state_wind.sp state_atm.press
    ## 1    24.56026      2.752929        1015.046
    ## 2    19.24684      1.268571           0.000
    ## 3    27.57325      2.705069        1015.335
    ## 4    26.33664      1.662132        1014.959
    ## 5    21.30662      2.710944        1014.751
    ## 6    29.75188      3.413737        1012.460
    ## 7    27.14427      3.852697        1012.567
    ## 8    18.52849      3.956459           0.000
    ## 9    21.33461      2.680875        1014.964
    ## 10   26.69258      1.636392        1014.836
    ## 11   21.94446      1.633487        1015.762
    ## 12   30.32372      3.074359        1010.144
    ## 13   24.37799      1.653032        1015.158
    ## 14   24.21220      3.680613        1013.389
    ## 15   21.87354      3.192539        1014.332
    ## 16   24.72953      1.627306        1015.420
    ## 17   22.25059      2.344333        1015.063
    ## 18   23.47238      2.148606        1014.825
    ## 19   22.53551      2.583469        1014.728
    ## 20   20.56798      2.568944        1012.855
    ## 21   27.87430      1.592840        1014.593
    ## 22   22.66268      2.565445        1012.557
    ## 23   24.88657      1.576035        1015.144
    ## 24   26.70404      1.495596        1015.208
    ## 25   18.79016      2.237210        1014.399
    ## 26   22.02062      2.554397        1015.351
    ## 27   20.51970      2.273423        1014.927
    ## 28   22.36880      2.101801        1014.810
    ## 29   24.35182      3.145427        1011.972
    ## 30   21.69177      1.784167        1015.435
    ## 31   18.61379      1.408247        1014.792
    ## 32   24.89883      1.883499        1014.824
    ## 33   19.55054      1.563826        1014.689
    ## 34   20.35662      3.665638        1014.398
    ## 35   23.95109      2.453547        1014.522
    ## 36   19.15492      4.151737        1014.185
    ## 37   26.24296      1.938625        1014.591
    ## 38   20.40674      2.304075        1014.887
    ## 39   19.80699      3.873392        1013.157
    ## 40   22.43194      2.237622        1014.760
    ## 41   23.88844      1.895486        1015.245
    ## 42   18.85524      2.053283        1014.893
    ## 43   25.80545      1.696119        1015.281
    ## 44   19.63017      2.617071        1015.042
    ## 45   24.56293      3.035050        1012.204
    ## 46   21.49638      3.098777        1013.334
    ## 47   17.98061      2.011436        1015.269
    ## 48   24.94447      3.776083        1012.525

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use `leaflet()` to visualize all \~100 points
in the same figure, applying different colors for those identified in
this question.

``` r
mid_point_state <- met_merged %>% group_by(STATE)%>% summarise(
  mean_lat = mean(lat, na.rm=TRUE),
  mean_long = mean(lon, na.rm=TRUE)
) %>% collect()
```

``` r
met_avg_state_loc <- met_merged %>% 
  group_by(USAFID) %>% 
  summarise(
    temp = mean(temp, na.rm = TRUE),
    wind.sp = mean(wind.sp, na.rm=TRUE),
    atm.press = mean(atm.press, na.rm=TRUE),
    lat = mean(lat, na.rm=TRUE),
    lon = mean(lon, na.rm=TRUE),
    state = STATE
  )  %>% distinct() %>%  collect()
```

``` r
met_avg_state_andloc <- merge(
   x = met_avg_state_loc,
   y = mid_point_state,
   by.x = "state",
   by.y = "STATE",
   all.x = TRUE,
   all.y = FALSE
 )
```

``` r
dist <- function(a, b, c, d) {
    sqrt((a - c)^2 + (b - d)^2)
}
```

``` r
diff_station_loc <- met_avg_state_andloc %>% 
  mutate(distance_geo = dist(lat, lon, mean_lat, mean_long))
```

``` r
min_diff_loc <- diff_station_loc %>% group_by(state) %>% summarise(min_diff_loc = min(distance_geo))
med_close_loc <- merge(
  x = min_diff_loc,
  y = diff_station_loc,
  by.x = c("min_diff_loc", "state"),
  by.y = c("distance_geo", "state"),
  all.x = FALSE,
  all.y = FALSE)
# stations that are closest to midpoint for each state
med_close_loc
```

    ##    min_diff_loc state USAFID     temp   wind.sp atm.press      lat        lon
    ## 1    0.02845150    DE 724088 24.72840 3.0324747  1014.860 39.13291  -75.46697
    ## 2    0.04422564    SC 723105 26.29473 1.8259936  1015.636 33.96470  -80.80005
    ## 3    0.08136566    NC 722201 24.18637 0.9103853       NaN 35.58208  -79.10100
    ## 4    0.08816932    RI 725074 24.45822 4.7099462       NaN 41.59700  -71.41200
    ## 5    0.09696293    MI 725405 19.91292 1.9441737       NaN 43.32200  -84.68800
    ## 6    0.11435555    CT 725027 21.87299 1.6481552  1014.760 41.51000  -72.82800
    ## 7    0.11604334    ME 726073 18.82098 1.4142598  1015.944 44.53300  -69.66723
    ## 8    0.12249025    NH 726155 19.96899 1.9610188  1014.689 43.56721  -71.43251
    ## 9    0.12439979    MD 722244 25.36363 1.1028311       NaN 38.98100  -76.92200
    ## 10   0.14086998    WI 726452 19.21728 2.4117467  1015.180 44.35900  -89.83701
    ## 11   0.14923985    FL 722014 26.78411 2.2798568  1015.526 28.47400  -82.45400
    ## 12   0.15544135    VT 726145 17.25013 1.4379588  1016.065 44.20400  -72.56200
    ## 13   0.16333791    MA 725068 20.83820 1.3823436  1014.377 41.87600  -71.02100
    ## 14   0.17965421    IA 725466 21.79148 2.7225459       NaN 41.69100  -93.56600
    ## 15   0.18507428    TX 720647 29.36251 2.2187708       NaN 31.10600  -98.19600
    ## 16   0.19492582    IN 720961 20.94252 2.0536596       NaN 40.71100  -86.37500
    ## 17   0.20170939    CO 726396 12.93812 2.5374887       NaN 39.05000 -105.51004
    ## 18   0.21204857    MN 726583 20.84136 2.0966314       NaN 45.14115  -94.50700
    ## 19   0.21480860    OH 720928 21.87803 2.0763441       NaN 40.28000  -83.11500
    ## 20   0.26508302    OK 723540 26.70174 4.1168113  1013.747 35.41690  -97.38319
    ## 21   0.27453676    MO 724459 23.45250 1.9242899  1014.980 38.09600  -92.55288
    ## 22   0.27606261    GA 722175 26.53220 1.8966816  1015.247 32.63325  -83.59972
    ## 23   0.27669778    KS 724506 24.63259 3.6742527  1012.323 38.06500  -97.86100
    ## 24   0.28841727    IL 724397 22.03413 3.4911565  1015.253 40.48271  -88.94836
    ## 25   0.29209064    VA 724017 24.20988 1.3314016       NaN 37.35800  -78.43800
    ## 26   0.30202578    TN 723273 25.01262 1.7828603  1004.715 36.00900  -86.52000
    ## 27   0.30604500    NJ 724090 23.47238 2.1486061  1015.095 40.03300  -74.35016
    ## 28   0.31660468    LA 720967 27.42055 1.1627569       NaN 30.71800  -91.47900
    ## 29   0.31872040    PA 725128 19.61798 1.2563596       NaN 40.84995  -77.84995
    ## 30   0.35720727    KY 720448 23.52994 1.6049055       NaN 37.57800  -84.77000
    ## 31   0.39610992    AZ 723745 25.47964 1.9323676       NaN 34.25700 -111.33900
    ## 32   0.41168326    NE 725520 21.85539 3.6775510  1015.050 40.96155  -98.31427
    ## 33   0.41231499    AR 723429 26.41317 1.2792990  1013.768 35.25800  -93.09499
    ## 34   0.41567668    WV 720328 21.94820 1.6178231       NaN 39.00000  -80.27400
    ## 35   0.41584497    CA 723890 29.15926 3.3677047  1011.154 36.77819 -119.71872
    ## 36   0.41867432    WA 720388 19.35326 0.5583820       NaN 47.10383 -122.28683
    ## 37   0.43842488    WY 726720 21.70287 3.8003344  1012.771 43.06440 -108.45695
    ## 38   0.44166402    AL 722300 26.61913 1.4962283  1014.860 33.17800  -86.78200
    ## 39   0.44274073    MT 726770 22.99419 4.1517371  1013.286 45.80547 -108.54002
    ## 40   0.45002838    SD 726560 20.55785 4.5733734  1013.479 44.38101 -100.28500
    ## 41   0.51351247    NY 725150 18.89641 2.7945743  1015.606 42.20630  -75.98030
    ## 42   0.63304562    ND 720867 18.20367 4.2037684       NaN 48.39000 -100.02400
    ## 43   0.64788225    NV 724855 24.34157 3.9398406  1011.842 38.05101 -117.09000
    ## 44   0.73919591    NM 722677 21.33037 4.9883663  1015.128 35.00300 -105.66201
    ## 45   0.79421755    MS 722350 27.03108 2.0681041  1014.575 32.32020  -90.07897
    ## 46   0.91217904    OR 725975 16.97502 2.0807921  1014.410 42.60000 -123.36400
    ## 47   0.93881671    ID 725864 18.78831 1.9405114  1014.374 44.88900 -116.10100
    ## 48   1.03208345    UT 725724 24.39332 2.7791506  1012.675 40.21900 -111.72300
    ##    mean_lat  mean_long
    ## 1  39.15950  -75.47708
    ## 2  33.92152  -80.79047
    ## 3  35.55578  -79.17800
    ## 4  41.62342  -71.49612
    ## 5  43.41830  -84.69928
    ## 6  41.48119  -72.71733
    ## 7  44.60213  -69.57403
    ## 8  43.54523  -71.55302
    ## 9  39.06089  -76.82664
    ## 10 44.46353  -89.93145
    ## 11 28.33518  -82.39921
    ## 12 44.35726  -72.58793
    ## 13 42.03785  -70.99897
    ## 14 41.85552  -93.49382
    ## 15 31.12371  -98.01178
    ## 16 40.51988  -86.33665
    ## 17 39.12441 -105.69753
    ## 18 45.23699  -94.31785
    ## 19 40.40494  -82.94026
    ## 20 35.55121  -97.15465
    ## 21 38.28066  -92.75603
    ## 22 32.56718  -83.33168
    ## 23 38.32236  -97.96264
    ## 24 40.22512  -88.81862
    ## 25 37.54765  -78.21585
    ## 26 35.70926  -86.55706
    ## 27 40.31345  -74.47269
    ## 28 30.51183  -91.71927
    ## 29 40.62764  -77.62156
    ## 30 37.50187  -85.11900
    ## 31 33.91659 -111.54154
    ## 32 41.26627  -98.59109
    ## 33 35.18904  -92.68849
    ## 34 38.72488  -80.58560
    ## 35 36.50457 -120.03186
    ## 36 47.41976 -122.56157
    ## 37 42.72014 -108.18546
    ## 38 32.75554  -86.65318
    ## 39 45.81591 -108.98264
    ## 40 44.22241  -99.86385
    ## 41 42.40904  -75.50851
    ## 42 47.75992 -100.08523
    ## 43 38.68753 -117.21082
    ## 44 34.30261 -105.89839
    ## 45 33.05395  -89.77502
    ## 46 43.31317 -122.79527
    ## 47 45.02638 -115.17229
    ## 48 39.38740 -112.33426

``` r
# answer to q2 with lat and long
q2_loc <- merge(
  x=med_close,
  y=met_avg_state,
  by.x = c("USAFID", "state"),
  by.y = c("USAFID", "state"),
  all.x = TRUE,
  all.y = FALSE
)
```

``` r
leaflet() %>% 
  addProviderTiles("OpenStreetMap") %>% 
  addCircles(lat=~lat.x, lng=~lon.x, color = "RED",data = q2_loc) %>% 
  addMarkers(lat=~lat, lng=~lon, data = med_close_loc) 
```

<div class="leaflet html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-39945fd4f1210e57819a" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-39945fd4f1210e57819a">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["OpenStreetMap",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircles","args":[[46.6827613941019,46.9419577656676,26.5850051546392,33.212,31.1829822852081,29.7089922705314,32.5159599542334,34.3835781584582,32.1669504405286,33.8126445959104,31.536,36.047,34.4979967776584,35.5930237288136,35.831000967118,37.152,36.1619871031746,38.137011684518,39.4717428571429,40.033,39.643,39.6740047984645,37.9003206568712,40.708,39.5509035769829,37.3069902080783,41.9099972527472,41.5329991281604,41.7360101010101,41.3338032388664,42.642987628866,41.4530010638298,42.2669724409449,41.4632502351834,42.5535839285714,41.9856836734694,41.1173656050955,40.0679923664122,42.5420088495575,42.1470528169014,43.2040901033973,44.45,43.344,44.3590049261084,45.5430087241003,45.443765323993,44.3390462962963,45.6980046136102],[-122.983,-98.018,-81.8610051546392,-87.616,-90.4710035429584,-98.0459922705314,-92.04097597254,-103.315653104925,-110.883,-118.146523855891,-82.507,-79.477,-82.7099989258861,-88.9169966101695,-90.646,-94.4950114942529,-97.0889613095238,-78.454988315482,-76.1699571428571,-74.3501562130177,-79.916,-75.6060009596929,-85.9672290406223,-84.027,-97.6509035769829,-108.626004895961,-70.729,-71.2829991281604,-72.6509797979798,-75.7249967611336,-77.055993814433,-87.0060010638298,-84.466968503937,-90.5203170272813,-92.4008803571429,-97.4347891156463,-111.966365605096,-118.568984732824,-113.766053097345,-121.724052816901,-71.5024542097489,-68.3667746192893,-72.5179990974729,-89.8370098522168,-94.0510196292257,-98.413442206655,-105.540990740741,-110.440038062284],10,null,null,{"interactive":true,"className":"","stroke":true,"color":"RED","weight":5,"opacity":0.5,"fill":true,"fillColor":"RED","fillOpacity":0.2},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null,null]},{"method":"addMarkers","args":[[39.1329054054054,33.9646951983298,35.5820807424594,41.597,43.322,41.5099990825688,44.533,43.5672086956522,38.981,44.3590049261084,28.474,44.204,41.876,41.691,31.106,40.711,39.05,45.1411456215152,40.28,35.4169039487727,38.096,32.6332458296752,38.0650046816479,40.4827108433735,37.358,36.009,40.033,30.718,40.849950395399,37.578,34.257,40.9615540935673,35.2580031347962,39,36.7781931187569,47.1038340767172,43.0643970117396,33.1779980392157,45.8054722474977,44.3810077071291,42.206297648013,48.39,38.0510112923463,35.0029964747356,32.3202025316456,42.6,44.8890012437811,40.219],[-75.4669684684685,-80.8000501043841,-79.101,-71.412,-84.688,-72.8280009174312,-69.6672303618711,-71.4325130434783,-76.922,-89.8370098522168,-82.4540026064292,-72.562,-71.021,-93.566,-98.196,-86.375,-105.510043030031,-94.507,-83.115,-97.3831921024546,-92.5528783269962,-83.5997190517998,-97.861,-88.9483614457831,-78.438,-86.52,-74.3501562130177,-91.479,-77.849950395399,-84.77,-111.339,-98.3142660818713,-93.0949937304075,-80.274,-119.718720310766,-122.286834076717,-108.456947705443,-86.7820019607843,-108.540024567789,-100.285004816956,-75.980301703163,-100.024,-117.089996235885,-105.662009400705,-90.0789738924051,-123.364,-116.101,-111.723],null,null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},null,null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[26.5850051546392,48.39],"lng":[-123.364,-68.3667746192893]}},"evals":[],"jsHooks":[]}</script>

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

``` r
avg_by_state <-  met_merged %>% 
  group_by(STATE) %>% 
  summarise(
    mean_temp = mean(temp, na.rm = TRUE),
    mean_wind.sp = mean(wind.sp, na.rm=TRUE),
    mean_atm.press = mean(atm.press, na.rm=TRUE)
  )
avg_by_state <- avg_by_state %>% mutate(temp_level = case_when(
  mean_temp < 20 ~ "low",
  mean_temp >= 20 &  mean_temp < 25~ "Mid",
  TRUE ~ "High"
)) %>% collect()
```

Once you are done with that, you can compute the following:

- Number of entries (records),
- Number of NA entries,
- Number of stations,
- Number of states included, and
- Mean temperature, wind-speed, and atmospheric pressure.

All by the levels described before.

``` r
avg_by_state %>% group_by(temp_level) %>% summarise(entries = n(),
                           NAs = sum(is.na(mean_temp) + is.na(mean_wind.sp) + is.na(mean_atm.press)),
                           n_state = n(),
                          mean_temp = mean(mean_temp, na.rm = TRUE),
                          mean_wind.sp = mean(mean_wind.sp, na.rm=TRUE),
                          mean_atm.press = mean(mean_atm.press, na.rm=TRUE))
```

    ## # A tibble: 3 × 7
    ##   temp_level entries   NAs n_state mean_temp mean_wind.sp mean_atm.press
    ##   <chr>        <int> <int>   <int>     <dbl>        <dbl>          <dbl>
    ## 1 High            12     0      12      27.0         2.43          1014.
    ## 2 low             11     2      11      18.7         2.55          1014.
    ## 3 Mid             25     0      25      22.6         2.39          1015.

``` r
avg_by_state
```

    ## # A tibble: 48 × 5
    ##    STATE mean_temp mean_wind.sp mean_atm.press temp_level
    ##    <chr>     <dbl>        <dbl>          <dbl> <chr>     
    ##  1 AL         26.2         1.57          1016. High      
    ##  2 AR         26.2         1.84          1015. High      
    ##  3 AZ         28.8         2.98          1011. High      
    ##  4 CA         22.4         2.61          1013. Mid       
    ##  5 CO         19.5         3.08          1014. low       
    ##  6 CT         22.3         2.19          1015. Mid       
    ##  7 DE         24.6         2.76          1015. Mid       
    ##  8 FL         27.5         2.50          1015. High      
    ##  9 GA         26.5         1.51          1015. High      
    ## 10 IA         21.3         2.57          1015. Mid       
    ## # … with 38 more rows

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
