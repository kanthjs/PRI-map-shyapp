# 1. ติดตั้งและเรียกใช้ Package ที่จำเป็น
# หากยังไม่ได้ติดตั้ง ให้รัน install.packages("...") ก่อน
# install.packages(c("sf", "dplyr", "ggplot2", "gstat", "stars", "ggspatial"))

library(sf)
library(dplyr)
library(ggplot2)
library(gstat)
library(stars)
library(ggspatial)

thai_prov <- st_read("data/provinces.geojson")
thai_prov_utm <- st_transform(thai_prov, crs = 32647)

# 3. จำลองข้อมูลจุดตัวอย่าง (Simulate Sample Points)
# ในโลกจริง ข้อมูลนี้จะมาจากการเก็บตัวอย่างภาคสนาม
# เราจะสุ่มจุดขึ้นมา 100 จุดภายในขอบเขตของ North Carolina
set.seed(123) # กำหนด seed เพื่อให้ผลการสุ่มเหมือนเดิมทุกครั้ง
sample_points <- st_sample(thai_prov_utm, 100) %>%
  st_as_sf() %>%
  mutate(cfu_g_soil = runif(100, 0, 200))

# 4. สร้างกริดสำหรับประมาณค่า (Create Interpolation Grid)
# เราต้องสร้างกริด (ตาราง) ว่างๆ ที่ครอบคลุมพื้นที่ทั้งหมด
# เพื่อใช้เป็นพื้นที่ในการประมาณค่าจากจุดตัวอย่างของเรา
grid <- st_as_stars(st_bbox(thai_prov_utm), dx = 10000, dy = 10000)

# 5. ประมาณค่าเชิงพื้นที่ด้วย IDW (Perform Interpolation)
# ใช้ package gstat เพื่อประมาณค่า cfu_g_soil ในทุกๆ ช่องของกริด
# โดยให้น้ำหนักกับจุดตัวอย่างที่อยู่ใกล้มากกว่า
idw_result <- gstat::idw(formula = cfu_g_soil ~ 1,
                         locations = sample_points,
                         newdata = grid)

final_map <- ggplot() +
  # พื้นหลัง: ค่า CFU ที่ประมาณได้
  geom_stars(data = idw_result, aes(fill = var1.pred), alpha = 0.8) +
  
  # สเกลสี: Blue (ต่ำ) -> Yellow (กลาง) -> Red (สูง)
  scale_fill_gradientn(
    colors = c("#2166AC", "#4393C3", "#92C5DE", "#FFFFBF", 
               "#FDAE61", "#F46D43", "#D73027"),
    name = "Risk Index",
    na.value = "transparent",
    limits = c(0, 200)
  ) +
  
  # เส้นขอบเขตจังหวัด
  geom_sf(data = thai_prov_utm, fill = NA, color = "black", linewidth = 0.1) +
  
  # จุดตัวอย่าง
#  geom_sf(data = sample_points, shape = 21, fill = "white", 
#          color = "black", size = 2, stroke = 0.5) +
  
  # มาตราส่วนและทิศเหนือ
  annotation_scale(
    location = "bl",
    width_hint = 0.3,
    text_cex = 0.8
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering(),
    height = unit(1.2, "cm"),
    width = unit(1.2, "cm")
  ) +
  
  # ชื่อและรายละเอียด
  labs(
    title = "Risk Map of Blast in North Thailand",
    subtitle = "Inverse Distance Weighted (IDW) Interpolation from 100 Sample Points",
    x = "Easting (m)",
    y = "Northing (m)"
  ) +
  
  # ธีมแผนที่
  theme_minimal() +
  theme(
    panel.grid = element_line(color = "gray90", linewidth = 0.1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    axis.text = element_text(size = 8),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm")
  )
final_map