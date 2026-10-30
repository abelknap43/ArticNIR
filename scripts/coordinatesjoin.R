install.packages(c("ggplot2", "tidyr", "dplyr", "readr"))
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
setwd("C:\\Users\\ashley.belknap\\OneDrive - Texas A&M AgriLife\\Barboza Lab Shared\\PCH_CaribouForage\\NIR_CBforage\\R files")

forage_data <- read.csv("forage_data.csv")
coordinates <- read.csv("coordinates.csv")

forage_with_coords <- forage_data %>%
  left_join(coordinates, by = "macroplot")

head(forage_with_coords)
tail(forage_with_coords)
View(forage_with_coords)
str(forage_with_coords)
summary(forage_with_coords)
sum(is.na(forage_with_coords$latitude))
sum(is.na(forage_with_coords$longitude))

unique(forage_data$macroplot)
unique(forage_with_coords$macroplot)

# Check if you lost or gained rows
nrow(forage_data)           # Original rows
nrow(forage_with_coords)    # After join
nrow(coordinates)           # Coordinate file
nrow(forage_data) == nrow(forage_with_coords)
ggplot(forage_with_coords, aes(x = long, y = lat.y, color = macroplot)) +
  geom_point(size = 3) +
  geom_text(aes(label = macroplot), vjust = -1) +
  theme_minimal() +
  labs(title = "Macroplot Coordinates Check")


write_csv(forage_with_coords, "forage_with_coordinates.csv")
###############################################################################

install.packages(c("ggplot2", "tidyr", "dplyr", "readr"))
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
setwd("C:\\Users\\ashley.belknap\\OneDrive - Texas A&M AgriLife\\Barboza Lab Shared\\PCH_CaribouForage\\NIR_CBforage\\R files")

forage_veg <- read.csv("forage_with_coordinates.csv")

# renaming lat and long columns
names(forage_veg)[names(forage_veg) == "lat.x"] <- "latitude"
names(forage_veg)[names(forage_veg) == "H"] <- "longitude"


# Dropping repeat lat and long (use minus sign)
forage_veg <- forage_veg %>%
  select(-lat.y)
forage_veg <- forage_veg %>%
  select(-long)

forage_veg %>%
  filter(transect == "W") %>%
  summarize(
    n_total = n(),
    n_with_N_digest = sum(!is.na(N_digest)),
    n_with_dig_e = sum(!is.na(dig_e)),
    n_with_both = sum(!is.na(N_digest) & !is.na(dig_e))
  )

nutrition_summary <- forage_veg %>%
  group_by(transect, macroplot, plantgroup, latitude, longitude) %>%
  summarize(
    digestible_nitrogen = mean(N_digest, na.rm = TRUE),
    digestible_energy = mean(dig_e, na.rm = TRUE),
    mean_Na = mean(Na_ppm, na.rm = TRUE),
    mean_K = mean(K_ppm, na.rm = TRUE),
    mean_P = mean(P_ppm, na.rm = TRUE),
    mean_Ca = mean(Ca_ppm, na.rm = TRUE),
    n_samples = n(),
    n_nutrition = sum(!is.na(N_digest) & !is.na(dig_e)),
    n_minerals = sum(!is.na(Na_ppm)),
    .groups = "drop"
  )


##NO paired samples for dig_e and dig_N
my_summary <- forage_veg %>%
  group_by(transect) %>%
  summarise(
    # Digestible nutrition
    mean_dig_energy = mean(dig_e, na.rm = TRUE),
    mean_dig_nitrogen = mean(N_digest, na.rm = TRUE),
    
    # Minerals
    mean_Na = mean(Na_ppm, na.rm = TRUE),
    mean_K = mean(K_ppm, na.rm = TRUE),
    mean_P = mean(P_ppm, na.rm = TRUE),
    mean_Ca = mean(Ca_ppm, na.rm = TRUE),
    
    # Sample counts
    count = n(),
    n_nutrition = sum(!is.na(N_digest) & !is.na(dig_e)),
    n_minerals = sum(!is.na(Na_ppm))
  )

# View the results
print(my_summary)

write_csv(nutrition_summary, "nutrition_for_arcgis.csv")
print(nutrition_summary)
###############################################################################
library(sf)
library(dplyr)

# Read your data with vegetation values
forage_with_veg <- read_csv("forage_with_vegetation.csv")

# Convert to spatial shapefile
forage_veg_sf <- st_as_sf(forage_with_veg,
                          coords = c("longitude", "latitude"),
                          crs = 4326) %>%
  st_transform(crs = 3338)  # Alaska Albers

# Export for ArcGIS
st_write(forage_veg_sf, "forage_nutrition_vegetation.shp", delete_dsn = TRUE)
###############################################################################

library(dplyr)
install.packages("sf")
library(sf)

# Summarize by macroplot
nutrition_summary <- forage_veg %>%
  group_by(macroplot, latitude, longitude) %>%
  summarize(
    digestible_nitrogen = mean(N_digest, na.rm = TRUE),
    digestible_energy = mean(dig_e, na.rm = TRUE),
    .groups = "drop"
  )

# Convert to spatial
nutrition_sf <- st_as_sf(nutrition_summary,
                         coords = c("longitude", "latitude"),
                         crs = 4326) %>%
  st_transform(crs = 3338)  # Alaska Albers
getwd()
# Export
st_write(nutrition_sf, "nutrition_for_interpolation.shp", delete_dsn = TRUE)

################################################################################

# Read your data
forage_data <- read_csv("forage_with_coordinates.csv")

# Add ecoregion to each macroplot (from your data [1])
ecoregion_lookup <- data.frame(
  macroplot = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
                "N", "O", "PA", "PB", "Q", "R", "S", "T"),
  ecoregion = c("Brooks Range", "Brooks Range", 
                "Arctic Foothills", "Arctic Foothills", "Arctic Foothills",
                "Arctic Foothills", "Arctic Foothills", 
                "Coastal Plain", "Coastal Plain",
                "Arctic Foothills", "Coastal Plain", "Coastal Plain", 
                "Coastal Plain","Brooks Range", "Arctic Foothills", "Arctic Foothills",
                "Arctic Foothills", "Arctic Foothills", "Arctic Foothills",
                "Arctic Foothills", "Coastal Plain")
)

# Join ecoregion data
forage_with_ecoregion <- forage_data %>%
  left_join(ecoregion_lookup, by = "macroplot")

# Summarize by ecoregion and plant group
nutrition_by_ecoregion <- forage_veg %>%
  filter(!is.na(N_digest) & !is.na(dig_e)) %>%
  group_by(ecoregion, plantgroup) %>%
  summarize(
    mean_dig_nitrogen = mean(N_digest, na.rm = TRUE),
    sd_dig_nitrogen = sd(N_digest, na.rm = TRUE),
    mean_dig_energy = mean(dig_e, na.rm = TRUE),
    sd_dig_energy = sd(dig_e, na.rm = TRUE),
    n_samples = n(),
    n_macroplots = n_distinct(macroplot),
    n_species = n_distinct(species),
    .groups = "drop"
  )

# View results
print(nutrition_by_ecoregion)
write_csv(nutrition_by_ecoregion, "nutrition_by_ecoregion_plantgroup.csv")



# Overall ecoregion comparison (all plant groups combined)
ecoregion_summary <- forage_veg %>%
  filter(!is.na(N_digest) & !is.na(dig_e)) %>%
  group_by(ecoregion) %>%
  summarize(
    mean_dig_nitrogen = mean(N_digest, na.rm = TRUE),
    mean_dig_energy = mean(dig_e, na.rm = TRUE),
    
    # By plant group percentages
    pct_browse = sum(plantgroup == "browse") / n() * 100,
    pct_forb = sum(plantgroup == "forb") / n() * 100,
    pct_graminoid = sum(plantgroup == "graminoid") / n() * 100,
    
    n_total_samples = n(),
    .groups = "drop"
  )

print(ecoregion_summary)


top_quality_species <- forage_veg %>%
  filter(!is.na(N_digest) & !is.na(dig_e)) %>%
  group_by(species, plantgroup, ecoregion) %>%
  summarize(
    mean_dig_nitrogen = mean(N_digest, na.rm = TRUE),
    mean_dig_energy = mean(dig_e, na.rm = TRUE),
    n_samples = n(),
    .groups = "drop"
  ) %>%
  filter(n_samples >= 3) %>%  # Only species with 3+ samples
  arrange(desc(mean_dig_nitrogen))

write_csv(top_quality_species, "top_species_by_nutrition.csv")


