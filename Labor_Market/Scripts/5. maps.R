# ---------------------------------------------------------------------------- #
# RMSP and TRT2 Maps
# 
# Last edited by: Tuffy Licciardi Issa
# Date: 06/04/2026
# ---------------------------------------------------------------------------- #

# Load packages used to download, manipulate, and plot spatial data
library(geobr)
library(sf)
library(tidyverse)
library(ggplot2)
library(cowplot)

# ---------------------------------------------------------------------------- #
# 1. Data ----
# ---------------------------------------------------------------------------- #

# Read municipality-level spatial data for the state of São Paulo in 2015
mun <- read_municipality(code_muni = "SP", year = 2015)

# Read metropolitan area boundaries and municipality assignments for 2015
metro <- read_metro_area(year = 2015)

# Build an auxiliary dataset with the municipalities belonging to the
# metropolitan areas relevant for TRT-2:
# - RM São Paulo
# - RM Baixada Santista
# Then remove municipalities that are in the Baixada Santista metro area
# but are not under TRT-2 jurisdiction: Mongaguá, Itanhaém, and Peruíbe
trt2_aux <- metro %>%
  filter(name_metro %in% c("RM São Paulo", "RM Baixada Santista")) %>%
  filter(!code_muni %in% c(3531100, 3537602, 3522109) ) %>% # Removes Mongaguá, Itanhaém, and Peruíbe
  select(code_muni, name_metro)

# Create the TRT-2 municipality set:
# - all municipalities in trt2_aux
# - plus Ibiúna, which is under TRT-2 jurisdiction but does not belong
#   to either of the selected metropolitan areas
trt2 <- mun %>%
  filter(code_muni %in% trt2_aux$code_muni |
           code_muni == 3519709) # Adds Ibiúna, which is not part of any metro area

# Remove temporary object no longer needed
rm(trt2_aux)

# Create binary indicators in the municipality dataset:
# - trt2 = 1 if municipality belongs to TRT-2 jurisdiction
# - rmsp = 1 if municipality belongs to the Metropolitan Region of São Paulo
mun <- mun %>%
  mutate(trt2 = ifelse(code_muni %in% trt2$code_muni, 1, 0),
         rmsp = ifelse(code_muni %in% metro$code_muni[metro$name_metro == "RM São Paulo"], 1, 0))

# Remove auxiliary datasets used only to construct treatment indicators
rm(metro, trt2, rmsp)

# ---------------------------------------------------------------------------- #
# Quick diagnostic maps ----
# ---------------------------------------------------------------------------- #

# Plot the RMSP municipalities over the São Paulo state map
ggplot() +
  geom_sf(data = mun, fill = "gray90", color = NA) +
  geom_sf(data = filter(mun, rmsp == 1), fill = "red", color = "black") +
  theme_minimal() +
  labs(title = "Região Metropolitana de São Paulo (RMSP)")

# Plot the TRT-2 municipalities over the São Paulo state map
ggplot() +
  geom_sf(data = mun, fill = "gray90", color = NA) +
  geom_sf(data = filter(mun, trt2 == 1), fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Municípios do TRT-2")

# ---------------------------------------------------------------------------- #
# AER-style zoomed map ----
# ---------------------------------------------------------------------------- #

# Subset RMSP municipalities
mun_rmsp <- mun %>% filter(rmsp == 1)

# Subset TRT-2 municipalities
mun_trt2 <- mun %>% filter(trt2 == 1)

# Keep only municipalities that are in TRT-2 but outside RMSP
# These are the "additional" TRT-2 municipalities relative to the RMSP
mun_trt2_only <- mun %>% filter(trt2 == 1 & rmsp == 0)

# Define the full area of analytical interest as the union of:
# - RMSP municipalities
# - TRT-2 municipalities
area_focus <- mun %>% 
  filter(rmsp == 1 | trt2 == 1)

# Compute the bounding box of the area of interest
bb <- st_bbox(area_focus)

# Add small horizontal and vertical margins to avoid a tight crop
x_margin <- (bb["xmax"] - bb["xmin"]) * 0.05
y_margin <- (bb["ymax"] - bb["ymin"]) * 0.05

# Define a clean theme intended to mimic a minimalist journal-style map
theme_map_aer <- function() {
  theme_void(base_size = 11) +
    theme(
      plot.title = element_text(size = 11, hjust = 0.5, face = "plain"),
      plot.caption = element_text(size = 9),
      legend.position = "none",
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# Plot a zoomed map:
# - all SP municipalities in light gray
# - RMSP municipalities filled in darker gray
# - TRT-2 municipalities outside RMSP shown as borders only
ggplot() +
  geom_sf(data = mun, fill = "grey95", color = NA) +
  geom_sf(data = mun_rmsp, fill = "grey60", color = "black", linewidth = 0.2) +
  geom_sf(data = mun_trt2_only, fill = NA, color = "black", linewidth = 0.5) +
  coord_sf(
    xlim = c(bb["xmin"] - x_margin, bb["xmax"] + x_margin),
    ylim = c(bb["ymin"] - y_margin, bb["ymax"] + y_margin),
    expand = FALSE
  ) +
  theme_map_aer()

# ---------------------------------------------------------------------------- #
# 2. Plot with Inset ----
# ---------------------------------------------------------------------------- #

# Recreate RMSP subset for the final map with inset
mun_rmsp <- mun %>% filter(rmsp == 1)

# Recreate full TRT-2 subset for the final map with inset
mun_trt2 <- mun %>% filter(trt2 == 1)

# Recreate the TRT-2 municipalities outside the RMSP
mun_trt2_only <- mun %>% filter(trt2 == 1 & rmsp == 0)

# Recreate the analytical area used to zoom into the main map
area_focus <- mun %>% filter(rmsp == 1 | trt2 == 1)

# Compute the bounding box for the main map
bb <- st_bbox(area_focus)

# Add a small margin around the bounding box
x_margin <- (bb["xmax"] - bb["xmin"]) * 0.05
y_margin <- (bb["ymax"] - bb["ymin"]) * 0.05

# Read the São Paulo state boundary to be used in the inset map
sp_state <- read_state(code_state = "SP", year = 2015)

# Collapse the full analytical area into a single geometry so that the inset
# can highlight the study region within São Paulo state
focus_union <- area_focus %>%
  summarise(geometry = st_union(geom))

# Define again the minimalist theme used in the final figure
theme_map_aer <- function() {
  theme_void(base_size = 11) +
    theme(
      plot.title = element_text(size = 11, hjust = 0.5),
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# Build the main map:
# - background: all municipalities in SP
# - shaded area: RMSP
# - outlined area: municipalities in TRT-2 but outside RMSP
p_main <- ggplot() +
  geom_sf(data = mun, fill = "grey95", color = NA) +
  geom_sf(data = mun_rmsp, fill = "grey60", color = "black", linewidth = 0.2) +
  geom_sf(data = mun_trt2_only, fill = NA, color = "black", linewidth = 0.5) +
  coord_sf(
    xlim = c(bb["xmin"] - x_margin, bb["xmax"] + x_margin),
    ylim = c(bb["ymin"] - y_margin, bb["ymax"] + y_margin),
    expand = FALSE
  ) +
  theme_map_aer()

# Build the inset map:
# - São Paulo state in light gray
# - study area highlighted in darker gray
p_inset <- ggplot() +
  geom_sf(data = sp_state, fill = "grey90", color = "grey40", linewidth = 0.2) +
  geom_sf(data = focus_union, fill = "grey30", color = "black", linewidth = 0.3) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Combine the main map and inset using cowplot
# The inset is positioned in the upper-left portion of the main figure
p_final <- ggdraw() +
  draw_plot(p_main) +
  draw_plot(p_inset, x = 0.01, y = 0.70, width = 0.21, height = 0.2)

# Save the final figure as a PDF for use in the paper
ggsave(
  filename = "C:/Users/tuffy/Documents/IC/Graphs/map_rmsp_trt2.pdf",
  plot = p_final,
  width = 7,
  height = 5,
  device = cairo_pdf
)
