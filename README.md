TidyTuesday \| Coffee Ratings
================
fth

## Coffee Rating Dataset

Exploring an older TidyTuesday dataset with new tools.

setup chunk:

``` r
library(dplyr)
```

    Warning: package 'dplyr' was built under R version 4.2.1


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(tidyr)
```

    Warning: package 'tidyr' was built under R version 4.2.1

``` r
library(corrr) 
```

    Warning: package 'corrr' was built under R version 4.2.2

``` r
library(ggplot2)
```

    Warning: package 'ggplot2' was built under R version 4.2.1

``` r
library(gt)            
```

    Warning: package 'gt' was built under R version 4.2.2

``` r
library(patchwork)
```

    Warning: package 'patchwork' was built under R version 4.2.2

``` r
library(monochromeR)
```

    Warning: package 'monochromeR' was built under R version 4.2.2

``` r
library(PrettyCols)
```

    Warning: package 'PrettyCols' was built under R version 4.2.2

``` r
library(emo)
# emo::ji_find("hot_beverage")
emo::ji("hot_beverage") -> cup 
gt_grob <- function(gt_object, ...){
# https://github.com/rstudio/gt/issues/961  
  out_name <- file.path(
    tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".png")
  )
  
  gtsave(gt_object, out_name, ...)
  
  in_png <- png::readPNG(out_name)
  
  on.exit(file.remove(out_name), add=TRUE)
  
  grid::rasterGrob(in_png)
  
}
```

get the dataset:

``` r
# tidytuesday coffee ---------------------------
coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')
```

    Rows: 1339 Columns: 43
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (24): species, owner, country_of_origin, farm_name, lot_number, mill, ic...
    dbl (19): total_cup_points, number_of_bags, aroma, flavor, aftertaste, acidi...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

run some calculations:

``` r
# average rating by country  --
coffee_ratings %>% 
  group_by(country_of_origin) %>%
  summarize(
    mean_total_cup_pts = mean(total_cup_points)
  ) %>% 
  arrange(desc(mean_total_cup_pts)) -> average_rating_by_country


# average rating subgroups
coffee_ratings %>% 
  group_by(species, variety, processing_method) %>%
  summarize(
    mean_total_cup_pts = mean(total_cup_points, na.rm = TRUE),
    mean_alt = mean(altitude_mean_meters, na.rm = TRUE)
  ) %>% 
  arrange(desc(mean_total_cup_pts)) -> average_rating_groups
```

    `summarise()` has grouped output by 'species', 'variety'. You can override
    using the `.groups` argument.

``` r
# unique coffees -- 
coffee_ratings %>% 
  group_by(country_of_origin) %>% 
  count %>% 
  arrange(desc(n)) -> unique_coffees

# top rated with many coffees -- 
unique_coffees %>% 
  left_join(average_rating_by_country, 
            by = 'country_of_origin') %>% 
  ungroup() %>% 
  head(15) %>% # want top ranked origins with large variety
  arrange(desc(mean_total_cup_pts)) %>% 
  head(10) -> unique_coffees 
```

removing some unnecessary cols, & filtering on various row splices

``` r
# wrangling // filtering ----
coffee_filtered <- coffee_ratings %>% 
                   select(total_cup_points, species, country_of_origin, region, harvest_year,
                          variety:category_two_defects,
                          altitude_mean_meters) 

coffee_filtered %>% 
  filter(
    country_of_origin %in% unique_coffees$country_of_origin
    ) -> coffee_filtered
```

creating some colors and themes

``` r
brownie <- generate_palette("brown", modification = "go_both_ways", 
                                   n_colours = 20, 
                                   view_palette = TRUE, 
                                   view_labels = FALSE)
```

![](README_files/figure-commonmark/theming-1.png)

``` r
brown_theme <- theme(plot.background = element_rect(fill = brownie[8], 
                                                    colour = brownie[8]),
                     panel.grid = element_line(colour = brownie[5]),
                     panel.background = element_rect(fill = brownie[8], 
                                                     colour = brownie[8]),
                     text = element_text(colour = brownie[15]),
                     axis.text = element_text(colour = brownie[17]),
                     plot.title = element_text(colour = brownie[20], hjust = 0),
                     plot.subtitle = element_text(colour = brownie[18], hjust = 0)) + 
                theme(
                  axis.text =   element_text(size = 12),
                  plot.title =  element_text(size = 20),
                  plot.subtitle = element_text(size = 12),
                  axis.title =  element_text(size = 12),
                  legend.text = element_text(size = 12)
                )
```

# gt tables

``` r
# gt -----------------

# most unique and average rating
gt_countries <- unique_coffees %>% 
                # left_join(average_rating_by_country, 
                #           by = 'country_of_origin') %>% 
                # ungroup() %>% 
                # head(15) %>% # want countries with high variety (i.e. not 1 coffee:country)
                # arrange(desc(mean_total_cup_pts)) %>%
                # head(10) %>% # top ranks per high distinct counts
                gt() %>% 
                data_color(
                  columns = mean_total_cup_pts, 
                  colors = brownie
                ) %>% 
                fmt_number(
                  columns = mean_total_cup_pts
                ) %>% 
                tab_header(
                  title = paste0("Origin Samples & Mean Total ", cup, " Points ")
                ) %>% 
                cols_label(
                  country_of_origin  =   "Origin",
                  n =                    "Distinct Samples",
                  mean_total_cup_pts =   "Avg Rating"
                ) %>% 
               tab_options(table.background.color = brownie[5],
                           table_body.hlines.color = brownie[3],
                           column_labels.border.bottom.color = brownie[10],
                           heading.border.bottom.color = brownie[10])

gt_grob(gt_countries)
```

    rastergrob[GRID.rastergrob.21] 

``` r
# top corrs to cup points
gt_corrs <-
  coffee_filtered %>% 
  select(total_cup_points, aroma:cupper_points) %>% 
  correlate() %>%
  focus(total_cup_points) %>% 
  arrange(desc(total_cup_points)) %>% 
  gt() %>% 
  data_color(
    columns = total_cup_points, 
    colors = brownie
  ) %>% 
  fmt_number(
    columns = total_cup_points
  ) %>%
  tab_header(
    title = paste0(cup, " Points & Rating Correlations")
  ) %>% 
  cols_label(
    total_cup_points  =   "Total Cup Correlation",
    term =                "Other Ratings",
  ) %>% 
  tab_options(table.background.color = brownie[5],
              table_body.hlines.color = brownie[3],
              column_labels.border.bottom.color = brownie[10],
              heading.border.bottom.color = brownie[10])
```

    Correlation computed with
    • Method: 'pearson'
    • Missing treated using: 'pairwise.complete.obs'

``` r
gt_grob(gt_corrs)
```

    rastergrob[GRID.rastergrob.22] 

# gg section

``` r
# gg -------------------
top_countries <- c("Ethiopia", "Kenya", "Uganda", "Costa Rica", "Colombia", "El Salvador")

gg_elev <- 
coffee_filtered %>%
  filter(country_of_origin %in% top_countries) %>% 
  filter(altitude_mean_meters < 2500) %>% 
  filter(altitude_mean_meters > 600) %>% 
  filter(total_cup_points > 50) %>% 
  mutate(
    country_of_origin = forcats::fct_reorder(country_of_origin, total_cup_points, .fun='mean')
  ) %>% 
  ggplot(.) + 
  geom_point(aes(x = altitude_mean_meters, y = total_cup_points, 
                 color = country_of_origin), size = 3) + 
  geom_smooth(aes(x = altitude_mean_meters, y = total_cup_points), 
              color = brownie[15], 
              se = FALSE, linetype = "longdash", size = 1.2) + 
  labs(
    title = "Cup Points & Elevation",
    x = "Farm Mean Elevation (m)",
    y = paste0("Total ", cup, " Points")
  ) + 
  scale_color_pretty_d("Autumn") + 
  theme_minimal() + 
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) + brown_theme

gg_jitter <- 
coffee_filtered %>%
  filter(country_of_origin %in% top_countries) %>% 
  mutate(
    country_of_origin = forcats::fct_reorder(country_of_origin, total_cup_points, .fun='mean')
  ) %>% 
  ggplot(.) + 
  geom_jitter(aes(y = country_of_origin, x = total_cup_points, 
                  color = country_of_origin),
              size = 3, alpha = .9) +
  labs(
    title = "Top Rated Origins",
    x = paste0("Total ", cup, " Points"),
    y = "Origin"
  ) + 
  scale_color_pretty_d("Autumn") + 
  theme_minimal() + 
  theme(
    legend.title = element_blank(),
    legend.position = "none"
  ) + brown_theme


# create ggplot of gt info, gt does not jive well with patchwork
gg_countries <- 
unique_coffees %>% 
  # left_join(average_rating_by_country, 
  #           by = 'country_of_origin') %>% 
  # ungroup() %>% 
  # head(15) %>% # want countries with high variety (i.e. not 1 coffee:country)
  # arrange(desc(mean_total_cup_pts)) %>%
  # head(10) %>% 
  ggplot() + 
  geom_text(aes(x = n, y = mean_total_cup_pts, 
                label = country_of_origin, color = country_of_origin), size = 5) + 
  labs(
    title = "Average Origin Rating & Total Coffees Rated",
    x = "Unique Coffees Rated",
    y = paste0("Mean Total ", cup, " Points")
  ) +
  scale_color_pretty_d("Autumn") + 
  theme_minimal() + 
  theme(
    legend.title = element_blank(),
    legend.position = "none"
  ) + brown_theme

gg_corrs <- 
  coffee_filtered %>% 
  select(total_cup_points, aroma:cupper_points) %>% 
  correlate() %>%
  focus(total_cup_points) %>% 
  arrange(desc(total_cup_points)) %>% 
  ggplot(.) + 
  geom_col(aes(x = forcats::fct_rev(forcats::fct_reorder(term, total_cup_points)), y = total_cup_points), 
           fill = brownie[15]) +
    labs(
      title = paste0("Correlations to Total ", cup ," Points"),
      x = "",
      y = ""
    ) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    brown_theme 
```

    Correlation computed with
    • Method: 'pearson'
    • Missing treated using: 'pairwise.complete.obs'

# patchwork

``` r
# patchwork -----------------------------------------------------------------------------
layout <- "
AC
BD
"
gg_jitter + gg_elev + gg_corrs + gg_countries +
  plot_layout(design = layout) + 
  plot_annotation(
    title = 'Coffee Rating & Origin Exploration',
    subtitle = 'A Story of Total Cup Ratings & Other Features',
    caption = 'TidyTuesday Coffee Rating Dataset  
               Using some new friends: patchwork, monochromeR, emo & PrettyCols +   
               some old faithfuls: dplyr, tidyr, corrr, & ggplot2',
    theme = brown_theme + theme(plot.title = element_text(size = 26))
  ) 
```

    `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](README_files/figure-commonmark/patchy-1.png)
