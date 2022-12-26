TidyTuesday \| Coffee Ratings
================
fth

## Coffee Rating Dataset

Exploring an older TidyTuesday dataset with new tools.

setup chunk:

``` r
library(dplyr)
library(tidyr)
library(corrr) 

library(ggplot2)
library(gt)            
library(patchwork)

library(monochromeR)
library(PrettyCols)
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

as_raw_html(gt_countries) # https://github.com/rstudio/gt/issues/104
```

<div id="yxnygavqhj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table" style="font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif; display: table; border-collapse: collapse; margin-left: auto; margin-right: auto; color: #FFFFFF; font-size: 16px; font-weight: normal; font-style: normal; background-color: #D09090; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;" bgcolor="#D09090">
  <thead class="gt_header">
    <tr>
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style="background-color: #D09090; text-align: center; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; color: #FFFFFF; font-size: 125%; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #AC3B3B; font-weight: normal;" bgcolor="#D09090" align="center">Origin Samples &amp; Mean Total ☕ Points </td>
    </tr>
    
  </thead>
  <thead class="gt_col_headings" style="border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #AC3B3B; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Origin" style="color: #FFFFFF; background-color: #D09090; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;" bgcolor="#D09090" valign="bottom" align="left">Origin</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Distinct Samples" style="color: #FFFFFF; background-color: #D09090; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#D09090" valign="bottom" align="right">Distinct Samples</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Avg Rating" style="color: #FFFFFF; background-color: #D09090; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#D09090" valign="bottom" align="right">Avg Rating</th>
    </tr>
  </thead>
  <tbody class="gt_table_body" style="border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;">
    <tr><td headers="country_of_origin" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">Ethiopia</td>
<td headers="n" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">44</td>
<td headers="mean_total_cup_pts" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #2E0B0B; color: #FFFFFF;" bgcolor="#2E0B0B" valign="middle" align="right">85.48</td></tr>
    <tr><td headers="country_of_origin" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">Kenya</td>
<td headers="n" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">25</td>
<td headers="mean_total_cup_pts" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #822121; color: #FFFFFF;" bgcolor="#822121" valign="middle" align="right">84.31</td></tr>
    <tr><td headers="country_of_origin" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">Uganda</td>
<td headers="n" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">36</td>
<td headers="mean_total_cup_pts" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #B44D4D; color: #FFFFFF;" bgcolor="#B44D4D" valign="middle" align="right">83.45</td></tr>
    <tr><td headers="country_of_origin" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">Colombia</td>
<td headers="n" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">183</td>
<td headers="mean_total_cup_pts" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #C16E6D; color: #FFFFFF;" bgcolor="#C16E6D" valign="middle" align="right">83.11</td></tr>
    <tr><td headers="country_of_origin" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">El Salvador</td>
<td headers="n" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">21</td>
<td headers="mean_total_cup_pts" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #C37272; color: #FFFFFF;" bgcolor="#C37272" valign="middle" align="right">83.05</td></tr>
    <tr><td headers="country_of_origin" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">Costa Rica</td>
<td headers="n" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">51</td>
<td headers="mean_total_cup_pts" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #CE8B8B; color: #FFFFFF;" bgcolor="#CE8B8B" valign="middle" align="right">82.79</td></tr>
    <tr><td headers="country_of_origin" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">Thailand</td>
<td headers="n" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">32</td>
<td headers="mean_total_cup_pts" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #D69F9F; color: #000000;" bgcolor="#D69F9F" valign="middle" align="right">82.57</td></tr>
    <tr><td headers="country_of_origin" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">Brazil</td>
<td headers="n" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">132</td>
<td headers="mean_total_cup_pts" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #DDAEAE; color: #000000;" bgcolor="#DDAEAE" valign="middle" align="right">82.41</td></tr>
    <tr><td headers="country_of_origin" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">Tanzania, United Republic Of</td>
<td headers="n" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">40</td>
<td headers="mean_total_cup_pts" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #DEB2B2; color: #000000;" bgcolor="#DEB2B2" valign="middle" align="right">82.37</td></tr>
    <tr><td headers="country_of_origin" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">Taiwan</td>
<td headers="n" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" valign="middle" align="right">75</td>
<td headers="mean_total_cup_pts" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #EDD4D4; color: #000000;" bgcolor="#EDD4D4" valign="middle" align="right">82.00</td></tr>
  </tbody>
  
  
</table>
</div>

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
as_raw_html(gt_corrs)
```

<div id="nzjcbsvqqk" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
  
  <table class="gt_table" style="font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif; display: table; border-collapse: collapse; margin-left: auto; margin-right: auto; color: #FFFFFF; font-size: 16px; font-weight: normal; font-style: normal; background-color: #D09090; width: auto; border-top-style: solid; border-top-width: 2px; border-top-color: #A8A8A8; border-right-style: none; border-right-width: 2px; border-right-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #A8A8A8; border-left-style: none; border-left-width: 2px; border-left-color: #D3D3D3;" bgcolor="#D09090">
  <thead class="gt_header">
    <tr>
      <td colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style="background-color: #D09090; text-align: center; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; color: #FFFFFF; font-size: 125%; padding-top: 4px; padding-bottom: 4px; padding-left: 5px; padding-right: 5px; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #AC3B3B; font-weight: normal;" bgcolor="#D09090" align="center">☕ Points &amp; Rating Correlations</td>
    </tr>
    
  </thead>
  <thead class="gt_col_headings" style="border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #AC3B3B; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3;">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Other Ratings" style="color: #FFFFFF; background-color: #D09090; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: left;" bgcolor="#D09090" valign="bottom" align="left">Other Ratings</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Total Cup Correlation" style="color: #FFFFFF; background-color: #D09090; font-size: 100%; font-weight: normal; text-transform: inherit; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: bottom; padding-top: 5px; padding-bottom: 6px; padding-left: 5px; padding-right: 5px; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums;" bgcolor="#D09090" valign="bottom" align="right">Total Cup Correlation</th>
    </tr>
  </thead>
  <tbody class="gt_table_body" style="border-top-style: solid; border-top-width: 2px; border-top-color: #D3D3D3; border-bottom-style: solid; border-bottom-width: 2px; border-bottom-color: #D3D3D3;">
    <tr><td headers="term" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">flavor</td>
<td headers="total_cup_points" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #2E0B0B; color: #FFFFFF;" bgcolor="#2E0B0B" valign="middle" align="right">0.83</td></tr>
    <tr><td headers="term" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">aftertaste</td>
<td headers="total_cup_points" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #3C0F0F; color: #FFFFFF;" bgcolor="#3C0F0F" valign="middle" align="right">0.79</td></tr>
    <tr><td headers="term" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">acidity</td>
<td headers="total_cup_points" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #561515; color: #FFFFFF;" bgcolor="#561515" valign="middle" align="right">0.72</td></tr>
    <tr><td headers="term" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">balance</td>
<td headers="total_cup_points" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #5A1616; color: #FFFFFF;" bgcolor="#5A1616" valign="middle" align="right">0.71</td></tr>
    <tr><td headers="term" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">cupper_points</td>
<td headers="total_cup_points" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #651919; color: #FFFFFF;" bgcolor="#651919" valign="middle" align="right">0.68</td></tr>
    <tr><td headers="term" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">aroma</td>
<td headers="total_cup_points" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #671A1A; color: #FFFFFF;" bgcolor="#671A1A" valign="middle" align="right">0.68</td></tr>
    <tr><td headers="term" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">body</td>
<td headers="total_cup_points" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #791E1E; color: #FFFFFF;" bgcolor="#791E1E" valign="middle" align="right">0.63</td></tr>
    <tr><td headers="term" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">clean_cup</td>
<td headers="total_cup_points" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #B24A4A; color: #FFFFFF;" bgcolor="#B24A4A" valign="middle" align="right">0.44</td></tr>
    <tr><td headers="term" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">uniformity</td>
<td headers="total_cup_points" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #BA5E5E; color: #FFFFFF;" bgcolor="#BA5E5E" valign="middle" align="right">0.40</td></tr>
    <tr><td headers="term" class="gt_row gt_left" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: left;" valign="middle" align="left">sweetness</td>
<td headers="total_cup_points" class="gt_row gt_right" style="padding-top: 8px; padding-bottom: 8px; padding-left: 5px; padding-right: 5px; margin: 10px; border-top-style: solid; border-top-width: 1px; border-top-color: #DEB2B2; border-left-style: none; border-left-width: 1px; border-left-color: #D3D3D3; border-right-style: none; border-right-width: 1px; border-right-color: #D3D3D3; vertical-align: middle; overflow-x: hidden; text-align: right; font-variant-numeric: tabular-nums; background-color: #EDD4D4; color: #000000;" bgcolor="#EDD4D4" valign="middle" align="right">0.16</td></tr>
  </tbody>
  
  
</table>
</div>

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
  ) + 
  brown_theme #+ 
    #theme(aspect.ratio = 1)

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
  ) + 
  brown_theme #+ 
    #theme(aspect.ratio = 1)


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
  ) + 
  brown_theme #+ 
    #theme(aspect.ratio = 1)

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
    brown_theme #+
```

    Correlation computed with
    • Method: 'pearson'
    • Missing treated using: 'pairwise.complete.obs'

``` r
  #theme(aspect.ratio = 1)
```

## EDA

of the top rated origins, below shows the distributions of cup ratings
for each origin using jitter plots.

``` r
gg_jitter
```

![](README_files/figure-commonmark/unnamed-chunk-1-1.png)

exploring the slight relationship seen when viewing cup rating as a
function of farm elevation.

``` r
gg_elev
```

    `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](README_files/figure-commonmark/unnamed-chunk-2-1.png)

correlations between total cup rating & various contributing
factors/ratings

``` r
gg_corrs
```

![](README_files/figure-commonmark/unnamed-chunk-3-1.png)

country ratings among the most diverse origins

``` r
gg_countries
```

![](README_files/figure-commonmark/unnamed-chunk-4-1.png)

# patchwork

``` r
# patchwork -----------------------------------------------------------------------------
# layout <- "
# AC
# BD
# "
# gg_jitter + gg_elev + gg_corrs + gg_countries +
#   plot_layout(design = layout) + 
#   plot_annotation(
#     title = 'Coffee Rating & Origin Exploration',
#     subtitle = 'A Story of Total Cup Ratings & Other Features',
#     caption = 'TidyTuesday Coffee Rating Dataset  
#                Using some new friends: patchwork, monochromeR, emo & PrettyCols +   
#                some old faithfuls: dplyr, tidyr, corrr, & ggplot2',
#     theme = brown_theme + theme(plot.title = element_text(size = 26))
#   ) 

# wrap_plots(gg_jitter, gg_corrs, gg_elev, gg_countries, 
#            ncol = 2) + 
#   plot_annotation(
#     title = 'Coffee Rating & Origin Exploration',
#     subtitle = 'A Story of Total Cup Ratings & Other Features',
#     caption = 'TidyTuesday Coffee Rating Dataset
#                Using some new friends: patchwork, monochromeR, emo & PrettyCols +
#                some old faithfuls: dplyr, tidyr, corrr, & ggplot2',
#     theme = brown_theme + theme(plot.title = element_text(size = 26))
#   )
```
