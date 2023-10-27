#===============================================================================
# 2023-10-12 -- dk migr
# Visualize foreign-born population in Denmark and Sweden
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

# prepare session ---------------------------------------------------------

library(tidyverse)
library(magrittr)
library(prismatic)
library(janitor)
library(patchwork)
library(paletteer)
library(hrbrthemes)
library(cowplot)
# devtools::install_github("liamgilbey/ggwaffle")
library(ggwaffle)
library(ggflags)

library(showtext)
sysfonts::font_add_google("Roboto Condensed", "rc")
sysfonts::font_add_google("Atkinson Hyperlegible", "ah")
showtext_auto()

# set ggplot2 theme
devtools::source_gist("653e1040a07364ae82b1bb312501a184")
theme_set(theme_ik())


# read data ---------------------------------------------------------------

raw_dk <- read_csv("dat/raw-dk.csv", skip = 1) %>%
  clean_names() %>% 
  pivot_longer(cols = -1, names_to = "year") %>% 
  transmute(cntr = x1 %>% str_to_lower() %>% as_factor, year = year %>% str_remove("x") %>% as.numeric, value)

# data from Sven
raw_se <- rio::import("dat/raw-se.ods") %>%
  clean_names()

se_90 <- raw_se[1:48, 5:6] %>%
  transmute(cntr = countries1990 %>% str_to_lower %>% as_factor, year = 1990, value = freq1990)
se_00 <- raw_se[1:48, 9:10] %>%
  transmute(cntr = countries2000 %>% str_to_lower %>% as_factor, year = 2000, value = freq2000)
se_10 <- raw_se[1:48, 13:14] %>%
  transmute(cntr = countries2010 %>% str_to_lower %>% as_factor, year = 2010, value = freq2010)
se_19 <- raw_se[1:48, 17:18] %>%
  transmute(cntr = countries2019 %>% str_to_lower %>% as_factor, year = 2019, value = freq2019)


# only the needed years and top-5 per year 
df_dk <- raw_dk %>% filter(year %in% c(1990, 2000, 2010, 2019))
df_se <- bind_rows(se_90, se_00, se_10, se_19)


# raw_se <- read_csv("dat/raw-se.csv", skip = 1) %>%
#   clean_names() %>% 
#   pivot_longer(cols = -1, names_to = "year") %>% 
#   transmute(cntr = country_of_birth %>% str_to_lower() %>% as_factor, year = year %>% str_remove("x") %>% as.numeric, value)

# # only the needed years and top-5 per year 
# df_dk <- raw_dk %>% filter(year %in% c(2000, 2010, 2020))
# df_se <- raw_se %>% filter(year %in% c(2000, 2010, 2020))

# calculate the proportions of foreign-born
foreign_dk <- df_dk %>% 
  mutate(orig = case_when(cntr == "denmark" ~ "native", TRUE ~ "foreign")) %>% 
  group_by(orig, year) %>% 
  summarise(value = value %>% sum) %>% 
  group_by(year) %>% 
  summarise(host = "dk", prop = value[1]/(value[1]+value[2]))

foreign_se <- df_se %>% 
  mutate(orig = case_when(cntr == "sweden" ~ "native", TRUE ~ "foreign")) %>% 
  group_by(orig, year) %>% 
  summarise(value = value %>% sum) %>% 
  group_by(year) %>% 
  summarise(host = "se", prop = value[1]/(value[1]+value[2]))

# join together
df_prop <- bind_rows(foreign_dk, foreign_se)

# only the needed years and top-5 per year 
dk5 <- df_dk %>% 
  filter(!cntr == "denmark") %>% 
  group_by(cntr, year) %>% 
  arrange(year, value %>% desc) %>% 
  group_by(year) %>%
  # https://stackoverflow.com/a/68740477/4638884
  mutate(cntr = ifelse(row_number() < 6, cntr %>%  paste, "other")) %>% 
  group_by(cntr, year) %>% 
  summarise(value = value %>% sum) %>% 
  ungroup() 

se5 <- df_se %>% 
  filter(!cntr == "sweden") %>% 
  group_by(cntr, year) %>% 
  arrange(year, value %>% desc) %>% 
  group_by(year) %>%
  mutate(cntr = ifelse(row_number() < 6, cntr %>%  paste, "other")) %>% 
  group_by(cntr, year) %>% 
  summarise(value = value %>% sum) %>% 
  ungroup() %>% 
  mutate(
    host = "se",
    cntr = cntr %>% 
      str_replace("bosnia-hercegovina", "bosnia and herzegovina") %>% 
      str_replace("former yugoslavia \\(rest of\\)", "rest of former yugoslavia")
  )

# join together
df_counts <- bind_rows(
  dk5 %>% mutate(host = "dk"), 
  se5 %>% mutate(host = "se")
) %>% 
  arrange(host, year, value %>% desc)

# data transformations needed for the waffle plot
# multiply the number of rows based on the percentages
waffle_data <- df_counts %>%
    group_by(year, host) %>%
    mutate(
      prop = prop.table(value),
      prom = prop %>% multiply_by(1e3) %>% round()
    ) %>% 
    ungroup() %>%
    mutate(cntr = cntr  %>% as_factor() %>% fct_relevel("other", after = Inf)) %>% 
    arrange(host, year, cntr) %>% 
    uncount(prom) %>%
    group_by(year, host) %>%
    group_modify(~ {
        .x %>%
            arrange(cntr) %>%
            waffle_iron(rows = 25, aes_d(group = cntr))
    }) %>%
    group_by(group) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    mutate(
        group = group  %>% as_factor() %>% fct_reorder(n) %>% fct_rev() %>% fct_relevel("other", after = Inf)
    ) %>% 
    arrange(host, year, group)

# HOTFIX waffle data to get rid of the rounding errors
waffle_data_hotfix <- waffle_data %>% 
  filter(! x == 41) %>% 
  bind_rows(
    tibble(
      year = 2000, host = "dk", y = 25, x = 40, group = "other", n = 4986
    )
  )

# colors
colors_match <- waffle_data_hotfix %>%
    group_by(group) %>%
    summarise(tot = n()) %>%
    mutate(group = group %>% as_factor() %>% fct_reorder(tot) %>% fct_rev() %>% fct_relevel("other", after = Inf)) %>%
    arrange(group) %>%
    # https://materialui.co/colors/
    mutate(
        col = c(
            "#80DEEA", # Finland = cyan 200
            "#6D4C41", # Turkey = brown 600
            "#FFC107", # Iraq = amber 500
            "#6A1B9A", # Germany = purple 800
            "#9E9D24", # Rest of former Yugoslavia = lime 800
            "#7986CB", # Poland = indigo 300
            "#455A64", # Syria = blue grey 700
            "#C2185B", # Norway = pink 700
            "#CDDC39", # Bosnia and Herzegovina = lime 500
            "#388E3C", # Pakistan = green 700
            "#F57C00", # Iran = orange 700
            "#81C784", # Lebanon = green 300
            "#2196F3", # Sweden = blue 500
            "#9C27B0", # German-speaking countries = purple 500
            "#F44336", # Denmark = red 500
            "#B0BEC5" # Other countries = blue grey 200
        )
    )


# plotting function
one_waffle_please <- function(df){

    pal_cntr <- df %>%
        distinct(group) %>%
        left_join(colors_match) %>%
        arrange(group) %>%
        pull(col)

    df %>%
        droplevels() %>%
        arrange(group) %>%
        ggplot(aes(x, y, fill = group))+
        geom_waffle(size = .05)+
        coord_equal() +
        scale_fill_manual(values = pal_cntr) +
        theme_void()+
        theme(legend.position = "none")
}

# produce a list of waffles
list_waffles <- waffle_data_hotfix  %>%
    group_by(year, host) %>%
    group_map(~ one_waffle_please(.x))

# # check the plots
# list_waffles %>% map(print)

# the order of grouping
waffle_data_hotfix  %>%
    distinct(year, host)

# scaling factors
df_prop %>%
  mutate(sq_prop = prop %>%  sqrt %>% divide_by(2))

# check scaling factors for small multiples
label_perc <- df_prop %>%
    mutate(
        size = prop %>% multiply_by(100) %>% divide_by(19.56) %>% sqrt %>% multiply_by(22),
        size_x2 = size * 2,
        x2_dk = .1 + size_x2/100,
        x1_se = .98 - size_x2/100,
        x = case_when(host == "dk"~ x2_dk - .02, TRUE ~ x1_se + .02),
        y = (((2021 - year) * 2.4 / 100) + size/100),
        y = case_when(year == 2019 ~ y - 0.024, TRUE ~ y) # hotfix
    )

# calculate the % of "other"
label_other <- df_counts %>%
  group_by(host, year) %>% 
  summarise(prop_other = value[1]/(sum(value))*100 %>% round(1)) %>% 
  ungroup() %>% 
  left_join(label_perc) %>% 
  mutate(
    x = case_when(host == "dk"~ x - size_x2/500, TRUE ~ x + size_x2/200),
    y = y - size/200
  )



# legend
(
    legend <- colors_match %>%
        ggplot(aes(label = group, color = col))+
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)+
        theme_void()+
        theme(legend.position = "none")+
        geom_text(
            x = .1, y = seq(.9, .1, length.out = 16), size = 10,
            family = "ah", fontface = 2, hjust = 0
        )+
        geom_point(
            x = .05, y = seq(.9, .1, length.out = 16), size = 3,
            shape = 15
        )+
        scale_color_identity()
)

# assemble
(
    out <- ggdraw() +
        # Denmark
        draw_plot(list_waffles[[1]], x = .1, .73, width = .203, height = .102)+
        draw_plot(list_waffles[[3]], x = .1, .49, width = .265, height = .132)+
        draw_plot(list_waffles[[5]], x = .1, .25, width = .312, height = .156)+
        draw_plot(list_waffles[[7]], x = .1, .01, width = .368, height = .184)+
        # Sweden
        draw_plot(list_waffles[[2]], x = .655, .73, width = .302, height = .151)+
        draw_plot(list_waffles[[4]], x = .626, .49, width = .334, height = .167)+
        draw_plot(list_waffles[[6]], x = .587, .25, width = .382, height = .191)+
        draw_plot(list_waffles[[8]], x = .54, .01, width = .440, height = .220)+
        geom_flag(
            data = tibble(country = c("dk", "se")),
            aes(country = country),
            x = c(.05, .95), y = .95, size = 15
        )+
        draw_plot(legend, x = .35, y = .67, width = .3, height = .28)+
        annotate(
            "text", label = c(1990, 2000, 2010, 2019),
            x = .08, y = c(.74, .5, .26, .02), size = 14, color = "#264444",
            family = "ah", fontface = 2,
            hjust = 0, vjust = 1, angle = 90
        )+
        annotate(
            "text", label = c("Denmark", "Sweden"),
            x = c(.1, .9), y = .95, size = 24,
            color = c("#F44336","#2196F3"),
            family = "ah", fontface = 2,
            hjust = c(0, 1), vjust = .5
        )+
        annotate(
            "text", label = "Rectangles are scaled to represent the proportion of immigrant population in the country" %>% str_wrap(20),
            x = .5, y = .5, size = 12, color = "#264444",
            family = "ah", fontface = 2,
            hjust = .5, vjust = .5, lineheight = .25
        )+
        geom_text(
            data = label_perc, aes(label = prop %>% multiply_by(100) %>% 
                                     round(1), x = x, y = y - .02),
            color = c("#F44336","#2196F3") %>% rep(each = 4), size = 14,
            family = "ah", fontface = 2,
            hjust = c(1, 0) %>% rep(each = 4), vjust = 0
        )+
      geom_text(
        data = label_other, aes(label = prop_other %>% round(), x = x, y = y - .02),
        color = c("#eeeeee"), size = 14,
        family = "ah", fontface = 2,
        hjust = .5, vjust = 0
      )
)


ggsave("fig-waffle.png", out, width = 7.5, height = 10, bg = "#ccffff")
 