library(sf)
library(tidyverse)
library(gstat)
library(stars)
library(ggspatial)

# โหลด shapefile จังหวัด (เช่นจาก GADM หรือ NESDC)
thai_prov <- st_read("data/provinces.geojson")

# ข้อมูล risk index สมมติ
risk_data <- data.frame(
  province = c("Chiang Mai", "Khon Kaen", "Suphan Buri", "Trang"),
  month = c("Jan", "Jan", "Jan", "Jan"),
  risk = c(2, 3, 1, 4) # 0–4
)

# รวมเข้ากับ shapefile
thai_map <- thai_prov %>%
  left_join(risk_data, by = c("pro_en" = "province"))

# วาดแผนที่
ggplot(thai_map) +
  geom_sf(aes(fill = risk)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal()
