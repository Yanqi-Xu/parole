Parole
================

## Load libraries

``` r
options(scipen = 2)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.6     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.0.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(here)
```

    ## here() starts at /Users/yanqixu/Documents/ffp_data

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(googlesheets4)
```

## Roster

### Read

Inmate roster contains everyone who’s ever involved in the Nebraska
prison system.

``` r
inmateDB_path <- here("parole","inmateDB","inmateDB1.csv")
inmateDB <- read_csv(inmateDB_path)
```

    ## New names:
    ## * `FIRST NAME` -> `FIRST NAME...3`
    ## * `MIDDLE NAME` -> `MIDDLE NAME...4`
    ## * `NAME EXTENSION` -> `NAME EXTENSION...5`
    ## * `FIRST NAME` -> `FIRST NAME...7`
    ## * `MIDDLE NAME` -> `MIDDLE NAME...8`
    ## * ...

    ## Warning: One or more parsing issues, see `problems()` for details

    ## Rows: 75103 Columns: 37

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (27): COMMITTED LAST NAME, FIRST NAME...3, MIDDLE NAME...4, NAME EXTENSI...
    ## dbl  (9): ID NUMBER, MIN MONTH, MIN DAY, MAX MONTH, MAX DAY, MAN MIN TERM/YE...
    ## lgl  (1): NAME EXTENSION...9

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
inmateDB2_path <- here("parole","inmateDB","inmateDB2.csv")                      
inmateDB2 <- read_csv(inmateDB2_path)
```

    ## Rows: 134825 Columns: 18

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (10): OFFENSE MINIMUM YEAR OR TERM, OFFENSE MAXIMUM YEAR OR TERM, OFFENS...
    ## dbl  (8): ID NUMBER, MINIMUM MONTH, MINIMUM DAY, MAXIMUM MONTH, MAXIMUM DAY,...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
inmateDB <- inmateDB %>% clean_names()
inmateDB2 <- inmateDB2 %>% clean_names()
```

### Clean columns

``` r
inmateDB1 <- inmateDB %>% 
  mutate_at(.vars = vars(ends_with("date")), .funs = as.Date, format = "%m/%d/%Y")
#inmateDB1$id_number = as.character(inmateDB1$id_number)
remove(inmateDB)
```

### Layout

``` r
inmateDB1 <- inmateDB1 %>% 
  select(30,16,17,27,1:3,10:12,everything())
```

## Parole by votes

### Read

clean columns

``` r
pr_votes_path <- here("parole","parole_by_votes.csv")
pr_votes <- read_csv(pr_votes_path)
```

    ## Rows: 32605 Columns: 9

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (8): Inmate Name, Hearing Date, Paroled, Deferred, Denied, Defer to MD, ...
    ## dbl (1): ID Number

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pr_votes <- pr_votes %>% clean_names()
pr_votes1 <- pr_votes %>%
  mutate(hearing_date = as.Date(hearing_date, format = "%m/%d/%Y"))
```

### Fill down rows

Writing a function here to fill down the motion for all five members.

``` r
pr_votes1 <- pr_votes1 %>% 
  fill(1:3,.direction = "down")
# pr_votes1 %>% group_by(id_number, hearing_date) %>% summarise(n=n()) %>% .$n %>% table()
fill_results <- function(df) {
  for (i in 0: (nrow(df)/5-1)) {
    df_cols = which(!is.na(df[5*i + 1,4:7]) %>% as.vector())
    df_cols = df_cols + 3
    for (df_col in df_cols) {
      if (!is.na(df[5*i+1,df_col])) {
            df[i*5+2,df_col] = df[5*i+1,df_col]
            df[i*5+3,df_col] = df[5*i+1,df_col]
            df[i*5+4,df_col] = df[5*i+1,df_col]
            df[i*5+5,df_col] = df[5*i+1,df_col]
      }
    }
  }
  return(df)
}

pr_by_votes <- fill_results(pr_votes1)
#decisions_number <- rowSums(is.na(pr_by_votes))
#pr_val <- pr_by_votes %>% add_column(decisions_number)
```

### Summarize

#### individual by board member

``` r
pr_ind <- pr_by_votes %>% 
  #group_by(id_number, inmate_name, hearing_date,vote) %>% summarize(count  = n())
  pivot_wider(names_from = board_member_last_name, values_from = vote) 
# new table by motion, member, vote
pr_motion <- pr_by_votes %>% 
  pivot_longer(cols = c(paroled,denied,deferred),names_to = "motion") 
pr_motion <- pr_motion %>% 
  filter(!is.na(value)) %>% select(-value)
```

#### individual vote pattern

``` r
# new table by motion and type of vote counts
pr_ind_pattern <- pr_motion %>% group_by(id_number, hearing_date,vote,motion) %>% summarize(number = n())
```

    ## `summarise()` has grouped output by 'id_number', 'hearing_date', 'vote'. You can override using the `.groups` argument.

``` r
pr_ind_pattern <- pr_ind_pattern %>% pivot_wider(names_from = vote, values_from = number) %>% clean_names()
```

### Compare missing

Of these 6443 hearings, 0

``` r
# add parole indicator column 
pr_ind_pattern <- pr_ind_pattern %>% 
  mutate(is_paroled = if_else(condition = motion == "paroled" & yes >=3, true = "PAROLED", false = "NOT PAROLED"),
         missing_member = if_else(is.na(not_available), true = "NO MISSING", false = "MISSING MEMBER"))

with(pr_ind_pattern, table(missing_member, is_paroled))
```

    ##                 is_paroled
    ## missing_member   NOT PAROLED PAROLED
    ##   MISSING MEMBER        1765    2268
    ##   NO MISSING             901    1509

``` r
# when at least a board member was missing, the parole board granted parole 56.2% of the time, versus 62.6% when all five members voted. 
# when there were missing members
pr_ind_pattern %>% filter(not_available >= 1) %>% .$motion %>% tabyl()
```

    ##         .    n   percent
    ##  deferred 1322 0.3277957
    ##    denied  443 0.1098438
    ##   paroled 2268 0.5623605

``` r
pr_ind_pattern %>% filter(not_available >= 1) %>% .$is_paroled %>% tabyl()
```

    ##            .    n   percent
    ##  NOT PAROLED 1765 0.4376395
    ##      PAROLED 2268 0.5623605

``` r
# without missing members
pr_ind_pattern %>% filter(is.na(not_available)) %>% .$motion %>% tabyl()
```

    ##         .    n    percent
    ##  deferred  685 0.28423237
    ##    denied  216 0.08962656
    ##   paroled 1509 0.62614108

``` r
pr_ind_pattern %>% filter(is.na(not_available)) %>% .$is_paroled %>% tabyl()
```

    ##            .    n   percent
    ##  NOT PAROLED  901 0.3738589
    ##      PAROLED 1509 0.6261411

``` r
# There were 2,491 hearings with one missing member. There were two hearings with 2-2 votes but those were deferment anyway.
split_votes <- pr_ind_pattern %>% filter(not_available == 1) %>% arrange(desc(no )) 
split_votes$is_paroled %>% tabyl()
```

    ##            .    n  percent
    ##  NOT PAROLED 1085 0.435568
    ##      PAROLED 1406 0.564432

#### Race

``` r
#add race description
pr_ind_pattern1 <- pr_ind_pattern %>% 
  left_join(inmateDB1 %>% select(id_number, race_desc), by = "id_number")
#missing status by race by motion
pr_ind_pattern2 <- pr_ind_pattern1 %>% group_by(race_desc, motion, missing_member) %>% summarize(n = n()) %>% left_join(pr_ind_pattern1 %>% group_by(missing_member,race_desc) %>% summarize(total_hearings =n()))
```

    ## `summarise()` has grouped output by 'race_desc', 'motion'. You can override using the `.groups` argument.

    ## `summarise()` has grouped output by 'missing_member'. You can override using the `.groups` argument.

    ## Joining, by = c("race_desc", "missing_member")

``` r
pr_ind_pattern2 <- pr_ind_pattern2 %>% mutate(motion_rate = n/total_hearings)

pr_ind_pattern2 %>% ggplot(aes(y = motion_rate, x = race_desc, fill = missing_member)) +
  geom_bar(position = "dodge", stat = "identity") + facet_wrap(~motion,nrow = 3)
```

![](parole_files/figure-gfm/missing%20racial%20pattern-1.png)<!-- -->

### Board members’ voting patterns

``` r
pr_bm <- pr_motion %>% group_by(board_member_last_name, motion,vote) %>% summarize( count = n())
```

    ## `summarise()` has grouped output by 'board_member_last_name', 'motion'. You can override using the `.groups` argument.

``` r
pr_bm <- pr_bm %>% add_tally(count)  
pr_bm <- pr_bm %>% mutate(type_rate = count/n)

library(wesanderson)
pr_bm %>% ggplot(aes(x=motion,y=count, fill = vote)) + geom_col() +
  facet_wrap(~board_member_last_name) +
  scale_fill_manual(values=wes_palette(n=5, name="Darjeeling1"))+
  theme_minimal()
```

![](parole_files/figure-gfm/votes%20data%20by%20member-1.png)<!-- -->
