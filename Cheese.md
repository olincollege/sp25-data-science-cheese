Cheese Data Analysis
================
Dakota Chang, Katherine Danielson
2025-04-17

- [Cheese Data Intro](#cheese-data-intro)
- [Importing Data and Libraries](#importing-data-and-libraries)
- [EDA and Basic Checks](#eda-and-basic-checks)
- [Exploration of Country](#exploration-of-country)
- [Exploration of Milk Types](#exploration-of-milk-types)
- [Cheese Types](#cheese-types)
- [Data Analysis](#data-analysis)
- [Conclusion and Remaining
  Questions](#conclusion-and-remaining-questions)

## Cheese Data Intro

#### **Background Information**

Source:
<https://github.com/rfordatascience/tidytuesday/blob/main/data/2024/2024-06-04/readme.md>

*Sourcing and Credibility*

This project uses the dataset from the [TidyTuesday: Cheese Week (June
4,
2024)](https://github.com/rfordatascience/tidytuesday/blob/main/data/2024/2024-06-04/readme.md)
initiative. The data is from Cheese.com, a long-standing online
encyclopedia for cheeses, and was scraped by data scientist Thomas Mock
and Jon Harmon for the purposes of public exploration and education.

This is a popular reference sites and is one of the largest cheese
databases in the world, hosting extensive listings and descriptions of
cheese worldwide. However, while it hosts a large amount of data, it is
not academic nor industry-certified; thus, it introduces uncertainty
about standardization and objectivity surrounding entries.
Unfortunately, the original site’s data-entry methodology is not
transparent.

*Potential Errors, Uncertainties and Ambiguities*

With a lack of transparent data entry, this introduces potential errors
in data classification. If the website was created through other
available data sources or different customer entries, there could be
potential errors in classification (e.g., inconsistent naming of
regions, vague flavor descriptors) and missing values due to non-uniform
entries across cheeses. Additionally, with a lack of transparency, there
could be a bias in certain aspects of data entry. For example, it
appears in this dataset that the United States of America produces the
most kinds of cheese, with France and Italy at different points behind.
However, France and Italy are renowned for being some of the top
producers of cheeses worldwide with the United States always behind
them.

This dataset is a non-random sample, containing ~1,200 cheeses
documented on Cheese.com as of 2024. This does not represent the full
global population of cheeses but is instead a convenience sample—limited
to what has been posted/entered online. Therefore, selection bias is
present. Well-known cheeses or those with wider international
distribution are likely overrepresented. Regional or artisanal cheeses
with less online presence may be underrepresented.

#### **Question and Exploration**

- What question did you set out to answer?

- What data did you find to help answer that question?

#### **Population**

- We will be looking at the non-random sample (1187 cheese) from the
  cheese.com TidyTuesday dataset.

#### **Quantity of interest**

- Our main quantity of interest is the country. This recognizes where a
  cheese is being produced.

#### **Question / Hypothesis**

- How are country and cheese qualities, namely milk type, flavor and
  cheese type, related?

  - Null hypothesis: There is no statistical difference between
    different countries based on certain cheese qualities.

    - There is no statistical difference between different countries
      based on type of cheese.

    - There is no statistical difference between different countries
      based on the milk used in cheese.

    - There is no statistical difference between different countries
      based on the flavor of cheese.

## Importing Data and Libraries

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggplot2)
library(dplyr)
library(tidyr)
```

``` r
df_cheeses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-06-04/cheeses.csv')
```

    ## Rows: 1187 Columns: 19
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (17): cheese, url, milk, country, region, family, type, fat_content, cal...
    ## lgl  (2): vegetarian, vegan
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df_cheeses
```

    ## # A tibble: 1,187 × 19
    ##    cheese    url   milk  country region family type  fat_content calcium_content
    ##    <chr>     <chr> <chr> <chr>   <chr>  <chr>  <chr> <chr>       <chr>          
    ##  1 Aarewass… http… cow   Switze… <NA>   <NA>   semi… <NA>        <NA>           
    ##  2 Abbaye d… http… sheep France  Pays … <NA>   semi… <NA>        <NA>           
    ##  3 Abbaye d… http… cow   France  <NA>   <NA>   semi… 40-46%      <NA>           
    ##  4 Abbaye d… http… cow   France  Burgu… <NA>   semi… <NA>        <NA>           
    ##  5 Abbaye d… http… cow   France  Savoie <NA>   soft… <NA>        <NA>           
    ##  6 Abbaye d… http… cow   France  provi… <NA>   semi… <NA>        <NA>           
    ##  7 Abbaye d… http… cow   France  Nord-… <NA>   semi… 50%         <NA>           
    ##  8 Abbot’s … http… cow   Englan… North… Chedd… semi… <NA>        <NA>           
    ##  9 Abertam   http… sheep Czech … Karlo… <NA>   hard… 45%         <NA>           
    ## 10 Abondance http… cow   France  <NA>   <NA>   semi… <NA>        <NA>           
    ## # ℹ 1,177 more rows
    ## # ℹ 10 more variables: texture <chr>, rind <chr>, color <chr>, flavor <chr>,
    ## #   aroma <chr>, vegetarian <lgl>, vegan <lgl>, synonyms <chr>,
    ## #   alt_spellings <chr>, producers <chr>

## EDA and Basic Checks

``` r
ncol(df_cheeses)
```

    ## [1] 19

``` r
nrow(df_cheeses)
```

    ## [1] 1187

``` r
summary(df_cheeses)
```

    ##     cheese              url                milk             country         
    ##  Length:1187        Length:1187        Length:1187        Length:1187       
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##     region             family              type           fat_content       
    ##  Length:1187        Length:1187        Length:1187        Length:1187       
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##  calcium_content      texture              rind              color          
    ##  Length:1187        Length:1187        Length:1187        Length:1187       
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##     flavor             aroma           vegetarian        vegan        
    ##  Length:1187        Length:1187        Mode :logical   Mode :logical  
    ##  Class :character   Class :character   FALSE:386       FALSE:742      
    ##  Mode  :character   Mode  :character   TRUE :362       TRUE :6        
    ##                                        NA's :439       NA's :439      
    ##    synonyms         alt_spellings       producers        
    ##  Length:1187        Length:1187        Length:1187       
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ## 

``` r
df_cheeses %>% 
  distinct(cheese)
```

    ## # A tibble: 1,187 × 1
    ##    cheese                 
    ##    <chr>                  
    ##  1 Aarewasser             
    ##  2 Abbaye de Belloc       
    ##  3 Abbaye de Belval       
    ##  4 Abbaye de Citeaux      
    ##  5 Abbaye de Tamié        
    ##  6 Abbaye de Timadeuc     
    ##  7 Abbaye du Mont des Cats
    ##  8 Abbot’s Gold           
    ##  9 Abertam                
    ## 10 Abondance              
    ## # ℹ 1,177 more rows

``` r
sapply(df_cheeses, function(x) sum(is.na(x)))  # Count missing values per column
```

    ##          cheese             url            milk         country          region 
    ##               0               0              36              11             332 
    ##          family            type     fat_content calcium_content         texture 
    ##             698              13             939            1162              58 
    ##            rind           color          flavor           aroma      vegetarian 
    ##             242             142              98             258             439 
    ##           vegan        synonyms   alt_spellings       producers 
    ##             439             893            1078             400

**Observations:**

- This dataset has 19 columns and 1187 rows.

  - As each cheese only has one set of information associated with it,
    that means there are 1187 distinct cheeses in the dataset. Thus,
    each cheese is only found in one county/region.

- The majority of data in the dataset is in character form.

  - Things like `type`, `texture` and `flavor` are all characters
    separated by commas. Thus, data analysis can both be performed with
    the data as it is and data where the lists are wrangled apart.

- There are a lot of missing datapoints in a variety of columns.

  - The only two columns that have no missing information are `cheese`
    and its associated `url`.

  - Comparatively, `alt_spellings` has the highest number at 1078. This
    indicates that 1078 of the cheeses do no have any other spellings,
    which should not impact data analysis.

  - However, there are some more valuable variables that are missing a
    high quantity of their data such as `fat_content` and `region` and
    if a cheese is `vegetarian`.

- The majority of cheese recorded are not `vegan` while roughly 50% of
  the cheeses that report if they are `vegetarian` show that they are.

The important variables in this dataset are:

| Variable | Class | Description |
|----|----|----|
| cheese | character | Name of the cheese. |
| url | character | Location of the cheese’s description at cheese.com |
| milk | character | The type of milk used for the cheese, when known. |
| country | character | The country or countries of origin of the cheese. |
| region | character | The region in which the cheese is produced, either within the country of origin, or as a wider description of multiple countries. |
| family | character | The family to which the cheese belongs, if any. |
| type | character | The broad type or types to describe the cheese. |
| fat_content | character | The fat content of the cheese, as a percent or range of percents. |
| calcium_content | character | The calcium content of the cheese, when known. Values include units. |
| texture | character | The texture of the cheese. |
| rind | character | The type of rind used in producing the cheese. |
| color | character | The color of the cheese. |
| flavor | character | Characteristic(s) of the taste of the cheese. |
| aroma | character | Characteristic(s) of the smell of the cheese. |
| vegetarian | logical | Whether cheese.com considers the cheese to be vegetarian. |
| vegan | logical | Whether cheese.com considers the cheese to be vegan. |
| synonyms | character | Alternative names of the cheese. |
| alt_spellings | character | Alternative spellings of the name of the cheese (likely overlaps with synonyms). |
| producers | character | Known producers of the cheese. |

## Exploration of Country

``` r
df_cheeses %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(country, -count), y = count)) +
  geom_bar(stat = "identity", fill = '#eecc11') +
  coord_flip() +
  labs(title = "Top 10 Countries by Cheese Count", x = "Country", y = "Count")+
  theme_minimal()
```

![](Cheese_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

**Observations:**

- In this database, the United States has a significantly higher number
  of cheese types represented compared to other countries. The U.S.
  lists over 300 types, while the second-highest, France, has just over
  150—meaning the U.S. has nearly twice as many. This discrepancy could
  be due to a few factors. The most likely explanations are: (1) the
  U.S. genuinely produces a wider variety of cheeses, or (2) the source
  of the data—a cheese e-commerce platform—may be based in the U.S. or
  have more partnerships with U.S.-based producers.

``` r
# stacked bar chart of milk type in top 5 country

df_cheeses %>%
  mutate(
    milk_list = str_split(milk, ", ")
  ) %>%
  unnest(milk_list) %>%
  filter(country %in% c("United States", "France", "Italy", "Canada", "Australia")) %>%
  ggplot(aes(x = country, fill = milk_list)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  labs(
    title = "Top 5 Countries by Cheese Milk Type Composition",
    x = "Country", y = "Percentage", fill = "Milk Type"
  ) +
  theme_minimal()
```

![](Cheese_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

**Observations:**

- This stacked bar chart shows that certain countries bias developing
  cheeses with certain types of milk. For example, Italy has a lot more
  cheeses made with sheep milk compared to all other countries. (TODO:
  Is this true? our sample might be wrong? do i need to amend this?)

## Exploration of Milk Types

``` r
df_cheeses %>%
  mutate(milk = str_split(milk, ", ")) %>%
  unnest(milk) %>%
  group_by(milk) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(milk, -count), y = count)) +
  geom_bar(stat = "identity", fill = '#eecc11') +
  coord_flip() +
  labs(title = "Milk Types by Cheese Count", x = "Milk Type", y = "Count") +
  geom_text(aes(x = milk, y = count,label=count),hjust=-0.1) +
  theme_minimal()
```

![](Cheese_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

**Observations:**

- It seems that the overwhelming majority of cheese is made from cow’s
  milk, with goat’s milk being the second most common. This is not
  surprising, as cow’s milk is the most widely produced milk in the
  world. Sheep’s milk is the third most common, but it is still
  significantly less common than cow’s and goat’s milk. This is likely
  due to the fact that sheep’s milk is less widely available and more
  expensive than cow’s and goat’s milk. We also see a few interesting
  milks, for example, there is one cheese made of moose milk, one of
  donkey milk, one of camel milk, and four of yak milks. We also see
  that plant-based milks are listed, but not with any specifications of
  which kind. It is important to note that this graph removes the detail
  of cheese made with a few different milks.

``` r
df_cheeses %>%
  filter(str_detect(milk, ", ")) %>%  # Only rows with multiple milk types
  group_by(milk) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(milk, -count), y = count)) +
  geom_bar(stat = "identity", fill = '#eecc11') +
  coord_flip() +
  labs(title = "Cheeses Made with Mixed Milk Types", x = "Milk Combination", y = "Count") +
  geom_text(aes(label = count), hjust = -0.1) +
  theme_minimal()
```

![](Cheese_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

**Observations:**

- The most common combination of milk types is cow and goat, which is
  not surprising given that these are the two most common types of milk
  used in cheese production. We also see that there are two cheeses that
  are made from four types of milk (cow, goat, sheep, water buffalo).

## Cheese Types

``` r
df_cheeses %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(type, -count), y = count)) +
  geom_bar(stat = "identity", fill = '#eecc11') +
  coord_flip() +
  labs(title = "Top 10 Types (non-separated) by Cheese Count", x = "Cheese Type", y = "Count")+
  theme_minimal()
```

![](Cheese_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

**Observations:**

- The most common cheese type is semi-hard and artisan. The top four all
  include artisan as a quality, which is not a regulated term, and might
  be used as a marketing tactic when it comes to types.

``` r
df_cheeses %>%
  mutate(type = str_split(type, ", ")) %>%
  unnest(type) %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(type, -count), y = count)) +
  geom_bar(stat = "identity", fill = '#eecc11') +
  coord_flip() +
  labs(title = "Types by Cheese Count", x = "Country", y = "Count") +
  geom_text(aes(x = type, y = count,label=count),hjust=-0.1) +
  theme_minimal()
```

![](Cheese_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

**Observations:**

- We see that artisan is the most popular typing. We aso see that soft
  and semi-soft lead the charts, suggesting that they are the most
  popular types of cheese on cheese.com.

## Data Analysis

## Conclusion and Remaining Questions
