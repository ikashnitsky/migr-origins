#===============================================================================
# 2023-03-22 -- dk migr
# Visualize Table 1
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

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

raw <- read_csv("230321-julia-cosmo-paper/raw-table.csv") %>%
    clean_names() %>%
    fill(year) %>%
    pivot_longer(cols = contains("den"), names_to = "host", values_to = "origin") %>%
    mutate(
        fix = case_when(host == "denmark" ~ x3, TRUE ~ x5) %>% as.numeric(),
        year = year %>% str_remove("\\*") %>% as.numeric()
    ) %>%
    select(-x3, -x5) %>%
    drop_na(year)

perc_foreign <- raw %>%
    filter(origin == "% of foreign in population")

foreign_countries <- raw %>%
    filter(!origin == "% of foreign in population")


# data transformations needed for the waffle plot
# multiply the number of rows based on the percentages
waffle_data <- foreign_countries %>%
    group_by(year, host) %>%
    group_modify(~ adorn_totals(.x, "row", waffle_data, name = "Other countries")) %>%
    mutate(
        percent = case_when(
            origin == "Other countries" ~ 100-fix,
            TRUE ~ fix
        ),
        prop_x10 = percent %>% multiply_by(10) %>% round()
    ) %>%
    ungroup() %>%
    mutate(origin = origin  %>% as_factor() %>% fct_reorder(prop_x10) %>% fct_rev() %>% fct_relevel("Other countries", after = Inf)) %>%
    uncount(prop_x10) %>%
    group_by(year, host) %>%
    group_modify(~ {
        .x %>%
            arrange(origin) %>%
            waffle_iron(rows = 25, aes_d(group = origin))
    }) %>%
    group_by(group) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    mutate(
        group = group  %>% as_factor() %>% fct_reorder(n) %>% fct_rev() %>% fct_relevel("Other countries", after = Inf)
    )

# colors
colors_match <- waffle_data %>%
    group_by(group) %>%
    summarise(tot = n()) %>%
    mutate(group = group %>% as_factor() %>% fct_reorder(tot) %>% fct_rev() %>% fct_relevel("Other countries", after = Inf)) %>%
    arrange(group) %>%
    # https://materialui.co/colors/
    mutate(
        col = c(
            "#80DEEA", # Finland = cyan 200
            "#6A1B9A", # Germany = purple 800
            "#C2185B", # Norway = pink 700
            "#9C27B0", # German-speaking countries = purple 500
            "#8D6E63", # Turkey = brown 400
            "#9E9D24", # Rest of former Yugoslavia = lime 800
            "#2196F3", # Sweden, blue 500
            "#F44336", # Denmark = red 500
            "#CDDC39", # Bosnia and Herzegovina = lime 500
            "#FFC107", # Iraq = amber 500
            "#F57C00", # Iran = orange 700
            "#FFEB3B", # Poland = yellow 500
            "#7986CB", # United Kingdom = indigo 300
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

list_waffles <- waffle_data  %>%
    group_by(year, host) %>%
    group_map(~ one_waffle_please(.x))

# # check the plots
# list_waffles %>% map(print)

# the order of groupping
waffle_data  %>%
    distinct(year, host)

# check scaling factors for small multiples
label_perc <- perc_foreign %>%
    mutate(
        size = fix %>% divide_by(15.9) %>% sqrt %>% multiply_by(22),
        size_x2 = size * 2,
        x2_dk = .1 + size_x2/100,
        x1_se = .98 - size_x2/100,
        x = case_when(host == "denmark"~ x2_dk - .02, TRUE ~ x1_se + .02),
        y = (((2019 - year) * 2.4 / 100) + size/100)
    )

# calculate the % of "Other"
label_other <- waffle_data %>%
    distinct(year, host, group, n) %>%
    mutate(other = group %>% fct_lump_n(1, other_level = "REST")) %>%
    group_by(year, host, other) %>%
    summarise(n = n %>% sum) %>%
    group_by(year, host) %>%
    summarise(prop_other = n[1]/(n[1]+n[2])*100 %>% round(1))


# legend
(
    legend <- colors_match %>%
        ggplot(aes(label = group, color = col))+
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE)+
        theme_void()+
        theme(legend.position = "none")+
        geom_text(
            x = .1, y = seq(.89, .11, -.06), size = 10,
            family = "ah", fontface = 2, hjust = 0
        )+
        geom_point(
            x = .05, y = seq(.89, .11, -.06), size = 3,
            shape = 15
        )+
        scale_color_identity()
)

# assemble
(
    out <- ggdraw() +
        # Denmark
        draw_plot(list_waffles[[1]], x = .1, .73, width = .215, height = .108)+
        draw_plot(list_waffles[[3]], x = .1, .49, width = .261, height = .131)+
        draw_plot(list_waffles[[5]], x = .1, .25, width = .290, height = .145)+
        draw_plot(list_waffles[[7]], x = .1, .01, width = .312, height = .156)+
        # Sweden
        draw_plot(list_waffles[[2]], x = .655, .73, width = .325, height = .163)+
        draw_plot(list_waffles[[4]], x = .626, .49, width = .354, height = .177)+
        draw_plot(list_waffles[[6]], x = .587, .25, width = .393, height = .197)+
        draw_plot(list_waffles[[8]], x = .54, .01, width = .440, height = .220)+
        geom_flag(
            data = tibble(country = c("dk", "se")),
            aes(country = country),
            x = c(.05, .95), y = .95, size = 15
        )+
        draw_plot(legend, x = .4, y = .73, width = .3, height = .22)+
        annotate(
            "text", label = c(1988, 1998, 2008, 2018),
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
            data = label_perc, aes(label = fix %>% round(1), x = x, y = y - .02),
            color = c("#F44336","#2196F3") %>% rep(4), size = 14,
            family = "ah", fontface = 2,
            hjust = c(1, 0) %>% rep(4), vjust = 0
        )
)


ggsave("230321-julia-cosmo-paper/fig-tab-1.png", out, width = 7.5, height = 10, bg = "#ccffff")
