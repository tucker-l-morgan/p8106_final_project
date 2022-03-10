P8106 Final Project - Data Cleaning
================
Tucker Morgan - tlm2152
3/10/2022

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.1.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

First, let’s pull in the Framingham Heart Disease dataset and take a
look at the data structure and check for missing values.

``` r
heart_df <- read_csv(file = "framingham_heart_disease.csv")
```

    ## Rows: 4238 Columns: 16

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## dbl (16): male, age, education, currentSmoker, cigsPerDay, BPMeds, prevalent...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
skimr::skim(heart_df)
```

|                                                  |          |
|:-------------------------------------------------|:---------|
| Name                                             | heart_df |
| Number of rows                                   | 4238     |
| Number of columns                                | 16       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| numeric                                          | 16       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

Data summary

**Variable type: numeric**

| skim_variable   | n_missing | complete_rate |   mean |    sd |     p0 |    p25 |   p50 |    p75 |  p100 | hist  |
|:----------------|----------:|--------------:|-------:|------:|-------:|-------:|------:|-------:|------:|:------|
| male            |         0 |          1.00 |   0.43 |  0.50 |   0.00 |   0.00 |   0.0 |   1.00 |   1.0 | ▇▁▁▁▆ |
| age             |         0 |          1.00 |  49.58 |  8.57 |  32.00 |  42.00 |  49.0 |  56.00 |  70.0 | ▃▇▆▆▂ |
| education       |       105 |          0.98 |   1.98 |  1.02 |   1.00 |   1.00 |   2.0 |   3.00 |   4.0 | ▇▆▁▃▂ |
| currentSmoker   |         0 |          1.00 |   0.49 |  0.50 |   0.00 |   0.00 |   0.0 |   1.00 |   1.0 | ▇▁▁▁▇ |
| cigsPerDay      |        29 |          0.99 |   9.00 | 11.92 |   0.00 |   0.00 |   0.0 |  20.00 |  70.0 | ▇▃▁▁▁ |
| BPMeds          |        53 |          0.99 |   0.03 |  0.17 |   0.00 |   0.00 |   0.0 |   0.00 |   1.0 | ▇▁▁▁▁ |
| prevalentStroke |         0 |          1.00 |   0.01 |  0.08 |   0.00 |   0.00 |   0.0 |   0.00 |   1.0 | ▇▁▁▁▁ |
| prevalentHyp    |         0 |          1.00 |   0.31 |  0.46 |   0.00 |   0.00 |   0.0 |   1.00 |   1.0 | ▇▁▁▁▃ |
| diabetes        |         0 |          1.00 |   0.03 |  0.16 |   0.00 |   0.00 |   0.0 |   0.00 |   1.0 | ▇▁▁▁▁ |
| totChol         |        50 |          0.99 | 236.72 | 44.59 | 107.00 | 206.00 | 234.0 | 263.00 | 696.0 | ▆▇▁▁▁ |
| sysBP           |         0 |          1.00 | 132.35 | 22.04 |  83.50 | 117.00 | 128.0 | 144.00 | 295.0 | ▇▇▁▁▁ |
| diaBP           |         0 |          1.00 |  82.89 | 11.91 |  48.00 |  75.00 |  82.0 |  89.88 | 142.5 | ▁▇▅▁▁ |
| BMI             |        19 |          1.00 |  25.80 |  4.08 |  15.54 |  23.07 |  25.4 |  28.04 |  56.8 | ▅▇▁▁▁ |
| heartRate       |         1 |          1.00 |  75.88 | 12.03 |  44.00 |  68.00 |  75.0 |  83.00 | 143.0 | ▂▇▃▁▁ |
| glucose         |       388 |          0.91 |  81.97 | 23.96 |  40.00 |  71.00 |  78.0 |  87.00 | 394.0 | ▇▁▁▁▁ |
| TenYearCHD      |         0 |          1.00 |   0.15 |  0.36 |   0.00 |   0.00 |   0.0 |   0.00 |   1.0 | ▇▁▁▁▂ |

Looks like we will want to recode some variables, and there are missing
values to contend with. Since our response variable, `TenYearCHD`, is
measured for each observation, we might consider a simple imputation
technique for missing values to limit lost information.

``` r
heart_df_cl <- heart_df %>% 
  # converting categorical variables to factors
  mutate(male = factor(male),
         education = factor(education, levels = c("1", "2", "3", "4"), ordered = TRUE),
         currentSmoker = factor(currentSmoker),
         BPMeds = factor(BPMeds),
         prevalentStroke = factor(prevalentStroke),
         prevalentHyp = factor(prevalentHyp),
         diabetes = factor(diabetes),
         TenYearCHD = factor(TenYearCHD))
  # we could pipe to missing value imputation here

skimr::skim(heart_df_cl)
```

|                                                  |             |
|:-------------------------------------------------|:------------|
| Name                                             | heart_df_cl |
| Number of rows                                   | 4238        |
| Number of columns                                | 16          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |             |
| Column type frequency:                           |             |
| factor                                           | 8           |
| numeric                                          | 8           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |             |
| Group variables                                  | None        |

Data summary

**Variable type: factor**

| skim_variable   | n_missing | complete_rate | ordered | n_unique | top_counts                       |
|:----------------|----------:|--------------:|:--------|---------:|:---------------------------------|
| male            |         0 |          1.00 | FALSE   |        2 | 0: 2419, 1: 1819                 |
| education       |       105 |          0.98 | TRUE    |        4 | 1: 1720, 2: 1253, 3: 687, 4: 473 |
| currentSmoker   |         0 |          1.00 | FALSE   |        2 | 0: 2144, 1: 2094                 |
| BPMeds          |        53 |          0.99 | FALSE   |        2 | 0: 4061, 1: 124                  |
| prevalentStroke |         0 |          1.00 | FALSE   |        2 | 0: 4213, 1: 25                   |
| prevalentHyp    |         0 |          1.00 | FALSE   |        2 | 0: 2922, 1: 1316                 |
| diabetes        |         0 |          1.00 | FALSE   |        2 | 0: 4129, 1: 109                  |
| TenYearCHD      |         0 |          1.00 | FALSE   |        2 | 0: 3594, 1: 644                  |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |   mean |    sd |     p0 |    p25 |   p50 |    p75 |  p100 | hist  |
|:--------------|----------:|--------------:|-------:|------:|-------:|-------:|------:|-------:|------:|:------|
| age           |         0 |          1.00 |  49.58 |  8.57 |  32.00 |  42.00 |  49.0 |  56.00 |  70.0 | ▃▇▆▆▂ |
| cigsPerDay    |        29 |          0.99 |   9.00 | 11.92 |   0.00 |   0.00 |   0.0 |  20.00 |  70.0 | ▇▃▁▁▁ |
| totChol       |        50 |          0.99 | 236.72 | 44.59 | 107.00 | 206.00 | 234.0 | 263.00 | 696.0 | ▆▇▁▁▁ |
| sysBP         |         0 |          1.00 | 132.35 | 22.04 |  83.50 | 117.00 | 128.0 | 144.00 | 295.0 | ▇▇▁▁▁ |
| diaBP         |         0 |          1.00 |  82.89 | 11.91 |  48.00 |  75.00 |  82.0 |  89.88 | 142.5 | ▁▇▅▁▁ |
| BMI           |        19 |          1.00 |  25.80 |  4.08 |  15.54 |  23.07 |  25.4 |  28.04 |  56.8 | ▅▇▁▁▁ |
| heartRate     |         1 |          1.00 |  75.88 | 12.03 |  44.00 |  68.00 |  75.0 |  83.00 | 143.0 | ▂▇▃▁▁ |
| glucose       |       388 |          0.91 |  81.97 | 23.96 |  40.00 |  71.00 |  78.0 |  87.00 | 394.0 | ▇▁▁▁▁ |
