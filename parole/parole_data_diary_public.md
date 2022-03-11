Parole hearing data diary
================

## Load libraries

``` r
options(scipen = 2)
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(campfin)
library(calendar)
library(googlesheets4)
```

## Parole by votes

The data was obtained via a public records request filled by the
Nebraska Board of Parole. We ask three data questions: 1. How many
individual cases/hearings had at least one absence and how many had the
full board in attendance? 2. How many full days had at least one board
member missing the full day, what’s the percentage of absence days out
of all hearing days? 3. What’s the parole grant rate when the parole was
full, short 1,2,and 3 members respectively? ### Read clean columns

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

In the original data, each individual hearing takes up five lines, with
the first line showing the motions (there are seveal columns
`deferred`,`paroled`,`denied` and `defer to md`, which means deferred to
mandatory discharge and is not a motion of its own but often accompanies
a denial).

First, I filled down the person’s id number, name and hearing days. Then
I wrote a function `fill_results()` here to fill down the motion for all
five members.

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
```

### Summarize

#### Data manipulation

pr_ind: transpose the data from (board member last name and their votes)
into (cotton, gissler, etc.) still 6521 individual hearings pr_motion:
collapse the three columns `paroled`, `denied` and `deferred` into one
single column motion Then since we’ll end up with lots of empty rows
resulted from votes on decisions on motions that weren’t actually made.
It has 97815 rows. Then we need to get rid of empty rows. By doing this
we also exclude hearings with no motions indicated in the data. Now we
have 6,643 actual hearings.

``` r
pr_ind <- pr_by_votes %>% 
  pivot_wider(names_from = board_member_last_name, values_from = vote) 
# new table by motion, member, vote
pr_motion <- pr_by_votes %>% 
  pivot_longer(cols = c(paroled,denied,deferred),names_to = "motion") 
pr_motion <- pr_motion %>% 
  filter(!is.na(value)) %>% select(-value)
```

#### individual vote pattern

``` r
# new table by motion and type of vote counts, 36150 records 
pr_ind_pattern <- pr_motion %>% group_by(id_number,hearing_date,vote,motion) %>% summarize(number = n())
```

    ## `summarise()` has grouped output by 'id_number', 'hearing_date', 'vote'. You can override using the `.groups` argument.

``` r
# pivot from vote-yes,not available, no to how many yes votes, how many no votes and how many NAs. 
pr_ind_pattern <- pr_ind_pattern %>% pivot_wider(names_from = vote, values_from = number) %>% clean_names()
```

### Count the number of hearings with at least one missing member

``` r
# add parole indicator column & absence indicator column
pr_ind_pattern <- pr_ind_pattern %>% 
  mutate(is_paroled = if_else(condition = motion == "paroled" & yes >=3, true = "PAROLED", false = "NOT PAROLED"),
         missing_member = if_else(is.na(not_available), true = "NO MISSING", false = "MISSING MEMBER"))
```

Of these 6443 hearings, 4033 had at least one absence. ### Number of
hearings attended by partial v. full board This section seeks to answer
the first question: How many individual hearings had the full board in
attendance and how many had at least one member missing?

``` r
pr_ind_pattern$missing_member %>% tabyl()
```

    ##               .    n   percent
    ##  MISSING MEMBER 4033 0.6259506
    ##      NO MISSING 2410 0.3740494

We can break it down by year and the missing rate again.

``` r
pr_ind_pattern %>% mutate(year = year(hearing_date)) %>% tabyl(year, missing_member) %>% clean_names() %>% mutate(rate = missing_member/(missing_member+no_missing))
```

    ##  year missing_member no_missing      rate
    ##  2018            606        601 0.5020713
    ##  2019           1175        727 0.6177708
    ##  2020           1246        624 0.6663102
    ##  2021           1006        458 0.6871585

### Number of hearing days with at least one absence

This answers the second question: 2. How many full days had at least one
board member missing the full day, what’s the percentage of absence days
out of all hearing days?

``` r
#all the types of votes a board member ever cast that day. how many were yes, no, not available
pr_miss <- pr_motion %>% group_by(board_member_last_name, hearing_date,vote) %>% summarize( count = n())
```

    ## `summarise()` has grouped output by 'board_member_last_name', 'hearing_date'. You can override using the `.groups` argument.

``` r
#the total number of votes that day (should be equal to the total of yes, no and not available votes)
pr_miss_tj <- pr_miss %>% group_by(board_member_last_name, hearing_date) %>% summarize( total = sum(count))
```

    ## `summarise()` has grouped output by 'board_member_last_name'. You can override using the `.groups` argument.

``` r
# join them together, so we see what portion of the vote that day was yes, no or not_available
pr_miss <-  pr_miss %>% left_join(pr_miss_tj)
```

    ## Joining, by = c("board_member_last_name", "hearing_date")

``` r
# calculate the rate 
pr_miss <- pr_miss %>% mutate(missing_rate = count/total)

# meetings where board members missed the full day of meetings, but if one day two board members were absent that date would show up twice -- we have 253 records
pr_miss_true <- pr_miss %>% filter(missing_rate ==1 & vote == "Not Available")

# how many board members missed that full day by date. The actual number of days is 184.
# 119 days with one board member absent, 69 days when two board members were missing
pr_board_miss <- pr_miss_true %>% group_by(hearing_date) %>% summarize(member_absent = n())

#break it down by year
x <- pr_board_miss %>% mutate(year=year(hearing_date)) %>% tabyl(year) %>% select(-percent) %>% rename(missed = n)

y <- pr_ind_pattern$hearing_date %>% unique() %>% year() %>% tabyl() %>% select(-percent) %>% rename(year=1, total = n)
z <- x %>% left_join(y) %>% mutate(missing_rate = missed/total)
```

    ## Joining, by = "year"

``` r
z
```

    ##  year missed total missing_rate
    ##  2018     21    52    0.4038462
    ##  2019     44    83    0.5301205
    ##  2020     62    96    0.6458333
    ##  2021     57    91    0.6263736

``` r
# how many days did the board have hearings, in total, count the distinct days -- 322
pr_ind_pattern$hearing_date %>% n_distinct()
```

    ## [1] 322

``` r
# it's the same thing if we apply it to the original data
pr_votes1$hearing_date %>% n_distinct()
```

    ## [1] 322

184/322 = 57%

#### Parole grant rate in cases of a 5-member board, 4-member board and 3-member board.

``` r
# look at how many assessments were approved and total hearings heard by 5,4,3,and2 board members
#here I also substituted all the NAs (not_available was NA means no one was unavailable)
cross_tabs <- xtabs(~not_available + is_paroled, data = pr_ind_pattern %>% mutate(not_available = replace_na(not_available, replace = 0)))

cross_tabs
```

    ##              is_paroled
    ## not_available NOT PAROLED PAROLED
    ##             0         901    1509
    ##             1        1085    1406
    ##             2         677     862
    ##             3           3       0

``` r
# cross tabulation in percentage
100* prop.table(cross_tabs, margin = 1) -> cross_tabs_perc
cross_tabs_perc
```

    ##              is_paroled
    ## not_available NOT PAROLED   PAROLED
    ##             0    37.38589  62.61411
    ##             1    43.55680  56.44320
    ##             2    43.98960  56.01040
    ##             3   100.00000   0.00000
