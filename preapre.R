#===============================================================================
# 2023-08-14 -- migr origins
# prepare all data
#===============================================================================

library(tidyverse)
library(magrittr)
library(viridis)
library(scales)
library(cowplot)
library(prismatic)

# custom theme
theme_custom <- 
  function(
    base_size = 12,
    base_family = "sans",
    labs_color = "#074949",
    axis_color = "#002F2F",
    bg_color = "#ffffff",
    grid_color = "#ccffff"
  ){
    theme_minimal(base_size = base_size, base_family = base_family)+
      theme(
        plot.title = element_text(size = base_size*2, face = 2, color = labs_color),
        plot.subtitle = element_text(color = labs_color),
        plot.caption = element_text(color = labs_color),
        axis.title = element_text(color = axis_color),
        axis.text = element_text(color = axis_color),
        plot.background = element_rect(color = NA, fill = bg_color),
        legend.position = "bottom",
        panel.spacing = unit(1, "lines"),
        panel.grid.major = element_line(color = grid_color),
        panel.grid.minor = element_blank(),
        line = element_line(lineend = "round")
      )
  }
theme_set(
  theme_custom()
)


# colors ------------------------------------------------------------------

pal_all <- c(
  # "#c8102e", # wiki flag color Denmark
  # "#006aa7", # wiki flag color Sweden
  "#B5223B", # custom Denmark,
  "#03A9F4", # custom Sweden
  "#BA68C8", # Europe excl.
  "#4A148C" # outside Europe
)


# colors
pal_dk <- pal_all[-2]
pal_se <- pal_all[-1]

vB<-viridis(option="B", 30) #1:tot; 

#load data (results)
tot <- dget(file = "dat/results_total.R")
sex <- dget(file = "dat/results_sex.R")
ses <- dget(file = "dat/results_ecincQ.R")
dk <- dget(file = "dat/results_dk.R") # dk = 2: native
euro <- dget(file = "dat/results_euro.R")
sex_swe <- dget(file = "dat/results_sex_swe.R")
swe <- dget(file = "dat/results_swe.R") 
euro_swe <- dget(file = "dat/results_euro_swe.R")


# data for Fig 1 -- total
df_dk <- 
  bind_rows(
    tot %>% 
      mutate(Sex = "Total",
             euro = 0, #total
             eqincQ = 0), #total
    sex %>% 
      mutate(euro = 0, #total
             eqincQ = 0), #total
    euro %>% 
      mutate(eqincQ = 0), #total
    ses %>% 
      mutate(euro = 1)  #only native
  ) %>% 
  mutate(
    euro = as.factor(
      case_when(
        euro == 0 ~ "Total",
        euro == 1 ~ "Denmark",
        euro == 2 ~ "Europe\n(excl. Denmark)",
        euro == 3 ~ "Outside Europe"
      )
    )
  ) 
