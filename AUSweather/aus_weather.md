AUS Weather
================
Tucker Morgan - tlm2152
3/17/2022

Quickly looking at the Australian weather dataset. The goal here would
be to predict `RainTomorrow`, with each day being its own observation.

``` r
library(tidyverse)

weather_aus <- read_csv("./AUSweather/weatherAUS.csv")
skimr::skim(weather_aus)
```

|                                                  |             |
|:-------------------------------------------------|:------------|
| Name                                             | weather_aus |
| Number of rows                                   | 145460      |
| Number of columns                                | 23          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |             |
| Column type frequency:                           |             |
| character                                        | 6           |
| Date                                             | 1           |
| numeric                                          | 16          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |             |
| Group variables                                  | None        |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| Location      |         0 |          1.00 |   4 |  16 |     0 |       49 |          0 |
| WindGustDir   |     10326 |          0.93 |   1 |   3 |     0 |       16 |          0 |
| WindDir9am    |     10566 |          0.93 |   1 |   3 |     0 |       16 |          0 |
| WindDir3pm    |      4228 |          0.97 |   1 |   3 |     0 |       16 |          0 |
| RainToday     |      3261 |          0.98 |   2 |   3 |     0 |        2 |          0 |
| RainTomorrow  |      3267 |          0.98 |   2 |   3 |     0 |        2 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| Date          |         0 |             1 | 2007-11-01 | 2017-06-25 | 2013-06-02 |     3436 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |    sd |    p0 |    p25 |    p50 |    p75 |   p100 | hist  |
|:--------------|----------:|--------------:|--------:|------:|------:|-------:|-------:|-------:|-------:|:------|
| MinTemp       |      1485 |          0.99 |   12.19 |  6.40 |  -8.5 |    7.6 |   12.0 |   16.9 |   33.9 | ▁▅▇▅▁ |
| MaxTemp       |      1261 |          0.99 |   23.22 |  7.12 |  -4.8 |   17.9 |   22.6 |   28.2 |   48.1 | ▁▂▇▅▁ |
| Rainfall      |      3261 |          0.98 |    2.36 |  8.48 |   0.0 |    0.0 |    0.0 |    0.8 |  371.0 | ▇▁▁▁▁ |
| Evaporation   |     62790 |          0.57 |    5.47 |  4.19 |   0.0 |    2.6 |    4.8 |    7.4 |  145.0 | ▇▁▁▁▁ |
| Sunshine      |     69835 |          0.52 |    7.61 |  3.79 |   0.0 |    4.8 |    8.4 |   10.6 |   14.5 | ▃▃▅▇▃ |
| WindGustSpeed |     10263 |          0.93 |   40.04 | 13.61 |   6.0 |   31.0 |   39.0 |   48.0 |  135.0 | ▅▇▁▁▁ |
| WindSpeed9am  |      1767 |          0.99 |   14.04 |  8.92 |   0.0 |    7.0 |   13.0 |   19.0 |  130.0 | ▇▁▁▁▁ |
| WindSpeed3pm  |      3062 |          0.98 |   18.66 |  8.81 |   0.0 |   13.0 |   19.0 |   24.0 |   87.0 | ▇▇▁▁▁ |
| Humidity9am   |      2654 |          0.98 |   68.88 | 19.03 |   0.0 |   57.0 |   70.0 |   83.0 |  100.0 | ▁▁▅▇▆ |
| Humidity3pm   |      4507 |          0.97 |   51.54 | 20.80 |   0.0 |   37.0 |   52.0 |   66.0 |  100.0 | ▂▅▇▆▂ |
| Pressure9am   |     15065 |          0.90 | 1017.65 |  7.11 | 980.5 | 1012.9 | 1017.6 | 1022.4 | 1041.0 | ▁▁▇▇▁ |
| Pressure3pm   |     15028 |          0.90 | 1015.26 |  7.04 | 977.1 | 1010.4 | 1015.2 | 1020.0 | 1039.6 | ▁▁▇▇▁ |
| Cloud9am      |     55888 |          0.62 |    4.45 |  2.89 |   0.0 |    1.0 |    5.0 |    7.0 |    9.0 | ▇▃▃▇▅ |
| Cloud3pm      |     59358 |          0.59 |    4.51 |  2.72 |   0.0 |    2.0 |    5.0 |    7.0 |    9.0 | ▆▅▃▇▃ |
| Temp9am       |      1767 |          0.99 |   16.99 |  6.49 |  -7.2 |   12.3 |   16.7 |   21.6 |   40.2 | ▁▃▇▃▁ |
| Temp3pm       |      3609 |          0.98 |   21.68 |  6.94 |  -5.4 |   16.6 |   21.1 |   26.4 |   46.7 | ▁▃▇▃▁ |

``` r
comp_data <- 
  weather_aus %>% 
  na.omit()

nrow(comp_data)
```

    ## [1] 56420

``` r
nrow(comp_data) / nrow(weather_aus)
```

    ## [1] 0.387873

Unfortunately, looks like the majority of days have some sort of missing
data. Using this dataset would require extensive imputation. I imagine
it wouldn’t be too difficult to make a more complete dataset for this.
Maybe using NOAA data and creating a variable that indicates rain the
next day… a task for another occasion.

``` r
skimr::skim(comp_data)
```

|                                                  |           |
|:-------------------------------------------------|:----------|
| Name                                             | comp_data |
| Number of rows                                   | 56420     |
| Number of columns                                | 23        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |           |
| Column type frequency:                           |           |
| character                                        | 6         |
| Date                                             | 1         |
| numeric                                          | 16        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |           |
| Group variables                                  | None      |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| Location      |         0 |             1 |   4 |  16 |     0 |       26 |          0 |
| WindGustDir   |         0 |             1 |   1 |   3 |     0 |       16 |          0 |
| WindDir9am    |         0 |             1 |   1 |   3 |     0 |       16 |          0 |
| WindDir3pm    |         0 |             1 |   1 |   3 |     0 |       16 |          0 |
| RainToday     |         0 |             1 |   2 |   3 |     0 |        2 |          0 |
| RainTomorrow  |         0 |             1 |   2 |   3 |     0 |        2 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| Date          |         0 |             1 | 2007-11-01 | 2017-06-25 | 2012-07-28 |     3416 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |    sd |    p0 |    p25 |    p50 |    p75 |   p100 | hist  |
|:--------------|----------:|--------------:|--------:|------:|------:|-------:|-------:|-------:|-------:|:------|
| MinTemp       |         0 |             1 |   13.46 |  6.42 |  -6.7 |    8.6 |   13.2 |   18.4 |   31.4 | ▁▅▇▆▁ |
| MaxTemp       |         0 |             1 |   24.22 |  6.97 |   4.1 |   18.7 |   23.9 |   29.7 |   48.1 | ▁▇▇▅▁ |
| Rainfall      |         0 |             1 |    2.13 |  7.01 |   0.0 |    0.0 |    0.0 |    0.6 |  206.2 | ▇▁▁▁▁ |
| Evaporation   |         0 |             1 |    5.50 |  3.70 |   0.0 |    2.8 |    5.0 |    7.4 |   81.2 | ▇▁▁▁▁ |
| Sunshine      |         0 |             1 |    7.74 |  3.76 |   0.0 |    5.0 |    8.6 |   10.7 |   14.5 | ▃▃▅▇▃ |
| WindGustSpeed |         0 |             1 |   40.88 | 13.34 |   9.0 |   31.0 |   39.0 |   48.0 |  124.0 | ▃▇▂▁▁ |
| WindSpeed9am  |         0 |             1 |   15.67 |  8.32 |   2.0 |    9.0 |   15.0 |   20.0 |   67.0 | ▇▅▁▁▁ |
| WindSpeed3pm  |         0 |             1 |   19.79 |  8.51 |   2.0 |   13.0 |   19.0 |   26.0 |   76.0 | ▅▇▁▁▁ |
| Humidity9am   |         0 |             1 |   65.87 | 18.51 |   0.0 |   55.0 |   67.0 |   79.0 |  100.0 | ▁▂▅▇▅ |
| Humidity3pm   |         0 |             1 |   49.60 | 20.20 |   0.0 |   35.0 |   50.0 |   63.0 |  100.0 | ▂▅▇▅▂ |
| Pressure9am   |         0 |             1 | 1017.24 |  6.91 | 980.5 | 1012.7 | 1017.2 | 1021.8 | 1040.4 | ▁▁▇▇▁ |
| Pressure3pm   |         0 |             1 | 1014.80 |  6.87 | 977.1 | 1010.1 | 1014.7 | 1019.4 | 1038.9 | ▁▁▇▇▁ |
| Cloud9am      |         0 |             1 |    4.24 |  2.80 |   0.0 |    1.0 |    5.0 |    7.0 |    8.0 | ▆▃▁▃▇ |
| Cloud3pm      |         0 |             1 |    4.33 |  2.65 |   0.0 |    2.0 |    5.0 |    7.0 |    9.0 | ▆▃▃▇▂ |
| Temp9am       |         0 |             1 |   18.20 |  6.57 |  -0.7 |   13.1 |   17.8 |   23.3 |   39.4 | ▁▇▇▅▁ |
| Temp3pm       |         0 |             1 |   22.71 |  6.84 |   3.7 |   17.4 |   22.4 |   27.9 |   46.1 | ▁▇▇▃▁ |

But we do retain 3,416 unique dates, which is very close to the same
number of unique dates in the full dataset. We lose almost half of our
locations, indicating some problematic geographies. We would want to
remove `Location`, and we would need to code `WindGustDir` and its kin
somehow. Not sure if factorization would be the best route or not.

What’s the breakout of “Yes” vs “No” for our outcome?

``` r
comp_data %>% 
  filter(RainTomorrow == "Yes") %>% 
  nrow() 
```

    ## [1] 12427

``` r
comp_data %>% 
  filter(RainTomorrow == "Yes") %>% 
  nrow()/ nrow(comp_data)
```

    ## [1] 0.2202588

Hmm… 22% positivity is pretty good! Might be usable. I’m really not sure
if it matters that we exclude most days. We seem to have a large number
of days with full data and a seemingly reasonable frequency.

``` r
weather_aus %>% 
  filter(RainTomorrow == "Yes") %>% 
  nrow()/ nrow(weather_aus)
```

    ## [1] 0.2191462

Very similar percentages!! So missing data may not be related to whether
it rains or not…
