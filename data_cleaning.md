P8106 Final Project: Data Cleaning
================
Hun Lee, Tucker Morgan, Zachary Katz
3/15/2022

## Data dictionary

Note that for binary variable, 1: yes & 0: No

sex : the gender of the observations. The variable is a binary named
“male” in the dataset.

age : Age at the time of medical examination in years.

education : A categorical variable of the participants education, with
the levels: Some high school (1), high school/GED (2), some
college/vocational school (3), college (4)

currentSmoker: Current cigarette smoking at the time of examinations

cigsPerDay: Number of cigarettes smoked each day

BPmeds: Use of Anti-hypertensive medication at exam

prevalentStroke: Prevalent Stroke (0 = free of disease)

prevalentHyp: Prevalent Hypertensive. Subject was defined as
hypertensive if treated

diabetes: Diabetic according to criteria of first exam treated

totChol: Total cholesterol (mg/dL)

sysBP: Systolic Blood Pressure (mmHg)

diaBP: Diastolic blood pressure (mmHg)

BMI: Body Mass Index, weight (kg)/height (m)^2

heartRate: Heart rate (beats/minute)

glucose: Blood glucose level (mg/dL)

And finally the response variable : + TenYearCHD : The 10 year risk of
coronary heart disease(CHD).

## Importing and partitioning data

``` r
x <- getURL("https://raw.githubusercontent.com/TarekDib03/Analytics/master/Week3%20-%20Logistic%20Regression/Data/framingham.csv")

data <- read.csv(text = x) %>% janitor::clean_names()

data <- distinct(data) # in case there are duplicated rows

heart_data = data %>% 
  na.omit() %>% 
  mutate(male = factor(male),
         current_smoker = factor(current_smoker),
         bp_meds = factor(bp_meds),
         prevalent_stroke = factor(prevalent_stroke),
         prevalent_hyp = factor(prevalent_hyp),
         diabetes = factor(diabetes))  %>%
  mutate(ten_year_chd = ifelse(ten_year_chd == "1", "CHD_present","CHD_absent") %>%
           fct_relevel("CHD_present", "CHD_absent")) %>%
  rename(sex = male) %>%
  mutate(sex = ifelse(sex == "1", "male","female") %>%
           fct_relevel("male", "female")) %>% 
  mutate(
    education = case_when(
      education == "1" ~ "some_HS",
      education == "2" ~ "HS_grad",
      education == "3" ~ "some_college",
      education == "4" ~ "college_grad"
    ),
    current_smoker = recode(
      current_smoker,
      "1" = "yes",
      "0" = "no"
    ),
    bp_meds = recode(
      bp_meds,
      "1" = "yes",
      "0" = "no"
    ),
    prevalent_stroke = recode(
      prevalent_stroke,
      "1" = "yes",
      "0" = "no"
    ),
    prevalent_hyp = recode(
      prevalent_hyp,
      "1" = "yes",
      "0" = "no"
    ),
    diabetes = recode(
      diabetes,
      "1" = "yes",
      "0" = "no"
    ),
    education = factor(education, levels = c("some_HS", "HS_grad", "some_college", "college_grad"))
  )
```

## Data summary

``` r
str(heart_data)
```

    ## 'data.frame':    3658 obs. of  16 variables:
    ##  $ sex             : Factor w/ 2 levels "male","female": 1 2 1 2 2 2 2 2 1 1 ...
    ##  $ age             : int  39 46 48 61 46 43 63 45 52 43 ...
    ##  $ education       : Factor w/ 4 levels "some_HS","HS_grad",..: 4 2 1 3 3 2 1 2 1 1 ...
    ##  $ current_smoker  : Factor w/ 2 levels "no","yes": 1 1 2 2 2 1 1 2 1 2 ...
    ##  $ cigs_per_day    : int  0 0 20 30 23 0 0 20 0 30 ...
    ##  $ bp_meds         : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ prevalent_stroke: Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ prevalent_hyp   : Factor w/ 2 levels "no","yes": 1 1 1 2 1 2 1 1 2 2 ...
    ##  $ diabetes        : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ tot_chol        : int  195 250 245 225 285 228 205 313 260 225 ...
    ##  $ sys_bp          : num  106 121 128 150 130 ...
    ##  $ dia_bp          : num  70 81 80 95 84 110 71 71 89 107 ...
    ##  $ bmi             : num  27 28.7 25.3 28.6 23.1 ...
    ##  $ heart_rate      : int  80 95 75 65 85 77 60 79 76 93 ...
    ##  $ glucose         : int  77 76 70 103 85 99 85 78 79 88 ...
    ##  $ ten_year_chd    : Factor w/ 2 levels "CHD_present",..: 2 2 2 1 2 2 1 2 2 2 ...
    ##  - attr(*, "na.action")= 'omit' Named int [1:582] 15 22 27 34 37 43 50 55 71 73 ...
    ##   ..- attr(*, "names")= chr [1:582] "15" "22" "27" "34" ...

``` r
skim(heart_data) 
```

<table style="width: auto;" class="table table-condensed">
<caption>
Data summary
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Name
</td>
<td style="text-align:left;">
heart\_data
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
3658
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Column type frequency:
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
factor
</td>
<td style="text-align:left;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Group variables
</td>
<td style="text-align:left;">
None
</td>
</tr>
</tbody>
</table>

**Variable type: factor**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim\_variable
</th>
<th style="text-align:right;">
n\_missing
</th>
<th style="text-align:right;">
complete\_rate
</th>
<th style="text-align:left;">
ordered
</th>
<th style="text-align:right;">
n\_unique
</th>
<th style="text-align:left;">
top\_counts
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
sex
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
fem: 2035, mal: 1623
</td>
</tr>
<tr>
<td style="text-align:left;">
education
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
som: 1526, HS\_: 1101, som: 608, col: 423
</td>
</tr>
<tr>
<td style="text-align:left;">
current\_smoker
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
no: 1869, yes: 1789
</td>
</tr>
<tr>
<td style="text-align:left;">
bp\_meds
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
no: 3547, yes: 111
</td>
</tr>
<tr>
<td style="text-align:left;">
prevalent\_stroke
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
no: 3637, yes: 21
</td>
</tr>
<tr>
<td style="text-align:left;">
prevalent\_hyp
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
no: 2518, yes: 1140
</td>
</tr>
<tr>
<td style="text-align:left;">
diabetes
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
no: 3559, yes: 99
</td>
</tr>
<tr>
<td style="text-align:left;">
ten\_year\_chd
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
CHD: 3101, CHD: 557
</td>
</tr>
</tbody>
</table>

**Variable type: numeric**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim\_variable
</th>
<th style="text-align:right;">
n\_missing
</th>
<th style="text-align:right;">
complete\_rate
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:right;">
sd
</th>
<th style="text-align:right;">
p0
</th>
<th style="text-align:right;">
p25
</th>
<th style="text-align:right;">
p50
</th>
<th style="text-align:right;">
p75
</th>
<th style="text-align:right;">
p100
</th>
<th style="text-align:left;">
hist
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
age
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
49.55
</td>
<td style="text-align:right;">
8.56
</td>
<td style="text-align:right;">
32.00
</td>
<td style="text-align:right;">
42.00
</td>
<td style="text-align:right;">
49.00
</td>
<td style="text-align:right;">
56.00
</td>
<td style="text-align:right;">
70.0
</td>
<td style="text-align:left;">
▃▇▆▆▂
</td>
</tr>
<tr>
<td style="text-align:left;">
cigs\_per\_day
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
9.03
</td>
<td style="text-align:right;">
11.92
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
20.00
</td>
<td style="text-align:right;">
70.0
</td>
<td style="text-align:left;">
▇▃▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
tot\_chol
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
236.85
</td>
<td style="text-align:right;">
44.10
</td>
<td style="text-align:right;">
113.00
</td>
<td style="text-align:right;">
206.00
</td>
<td style="text-align:right;">
234.00
</td>
<td style="text-align:right;">
263.00
</td>
<td style="text-align:right;">
600.0
</td>
<td style="text-align:left;">
▃▇▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
sys\_bp
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
132.37
</td>
<td style="text-align:right;">
22.09
</td>
<td style="text-align:right;">
83.50
</td>
<td style="text-align:right;">
117.00
</td>
<td style="text-align:right;">
128.00
</td>
<td style="text-align:right;">
143.88
</td>
<td style="text-align:right;">
295.0
</td>
<td style="text-align:left;">
▇▇▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
dia\_bp
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
82.92
</td>
<td style="text-align:right;">
11.97
</td>
<td style="text-align:right;">
48.00
</td>
<td style="text-align:right;">
75.00
</td>
<td style="text-align:right;">
82.00
</td>
<td style="text-align:right;">
90.00
</td>
<td style="text-align:right;">
142.5
</td>
<td style="text-align:left;">
▁▇▅▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
bmi
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
25.78
</td>
<td style="text-align:right;">
4.07
</td>
<td style="text-align:right;">
15.54
</td>
<td style="text-align:right;">
23.08
</td>
<td style="text-align:right;">
25.38
</td>
<td style="text-align:right;">
28.04
</td>
<td style="text-align:right;">
56.8
</td>
<td style="text-align:left;">
▅▇▁▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
heart\_rate
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
75.73
</td>
<td style="text-align:right;">
11.98
</td>
<td style="text-align:right;">
44.00
</td>
<td style="text-align:right;">
68.00
</td>
<td style="text-align:right;">
75.00
</td>
<td style="text-align:right;">
82.00
</td>
<td style="text-align:right;">
143.0
</td>
<td style="text-align:left;">
▂▇▃▁▁
</td>
</tr>
<tr>
<td style="text-align:left;">
glucose
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
81.85
</td>
<td style="text-align:right;">
23.90
</td>
<td style="text-align:right;">
40.00
</td>
<td style="text-align:right;">
71.00
</td>
<td style="text-align:right;">
78.00
</td>
<td style="text-align:right;">
87.00
</td>
<td style="text-align:right;">
394.0
</td>
<td style="text-align:left;">
▇▁▁▁▁
</td>
</tr>
</tbody>
</table>
