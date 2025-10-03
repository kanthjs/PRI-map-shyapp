# ------- loading library ----
# Pest Risk Index with Improved Features
# Shiny app is built for representing the pest risk index based on the survey dat
library(shiny)
library(leaflet)
library(readxl)
library(dplyr)
library(tidyr)
library(sf)
library(curl)
library(ggplot2)
library(plotly)
library(viridis)

# ---- CONFIG ----
excel_path <- "data/Pest_Risk_Template_77_Provinces_ByPest.xlsx"
geojson_path <- "data/provinces.geojson"

# ---- Enhanced Helper Functions ----
ensure_geojson <- function(path = geojson_path) {
  tryCatch(
    {
    if (!file.exists(path)) {
      dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
      url <- "https://raw.githubusercontent.com/chingchai/OpenGISData-Thailand/master/provinces.geojson"
      curl_download(url, path, mode = "wb")
        message("✓ Downloaded GeoJSON successfully.")
    }
    path
    },
    error = function(e) {
    stop("ไม่สามารถดาวน์โหลด GeoJSON: ", e$message)
    }
  )
}

th_month_levels <- c(
  "ม.ค.",
  "ก.พ.",
  "มี.ค.",
  "เม.ย.",
  "พ.ค.",
  "มิ.ย.",
  "ก.ค.",
  "ส.ค.",
  "ก.ย.",
  "ต.ค.",
  "พ.ย.",
  "ธ.ค."
)

norm_month <- function(x) {
  x <- trimws(as.character(x))
  x <- dplyr::recode(
    x,
    "มกราคม" = "ม.ค.",
    "กุมภาพันธ์" = "ก.พ.",
    "มีนาคม" = "มี.ค.",
    "เมษายน" = "เม.ย.",
    "พฤษภาคม" = "พ.ค.",
    "มิถุนายน" = "มิ.ย.",
    "กรกฎาคม" = "ก.ค.",
    "สิงหาคม" = "ส.ค.",
    "กันยายน" = "ก.ย.",
    "ตุลาคม" = "ต.ค.",
    "พฤศจิกายน" = "พ.ย.",
    "ธันวาคม" = "ธ.ค.",
    "Jan" = "ม.ค.",
    "January" = "ม.ค.",
    "Feb" = "ก.พ.",
    "February" = "ก.พ.",
    "Mar" = "มี.ค.",
    "March" = "มี.ค.",
    "Apr" = "เม.ย.",
    "April" = "เม.ย.",
    "May" = "พ.ค.",
    "Jun" = "มิ.ย.",
    "June" = "มิ.ย.",
    "Jul" = "ก.ค.",
    "July" = "ก.ค.",
    "Aug" = "ส.ค.",
    "August" = "ส.ค.",
    "Sep" = "ก.ย.",
    "Sept" = "ก.ย.",
    "September" = "ก.ย.",
    "Oct" = "ต.ค.",
    "October" = "ต.ค.",
    "Nov" = "พ.ย.",
    "November" = "พ.ย.",
    "Dec" = "ธ.ค.",
    "December" = "ธ.ค.",
    .default = x
  )
  factor(
    ifelse(x %in% th_month_levels, x, NA_character_),
    levels = th_month_levels,
    ordered = TRUE
  )
}

norm_prov <- function(x) {
  gsub("\\s+", "", trimws(as.character(x)))
}

# Function to validate risk values
validate_risk <- function(x) {
  x <- as.numeric(x)
  if (is.na(x)) {
    return(NA)
  }
  if (x < 0 | x > 4) {
    warning(paste("Risk value out of range [0-4]:", x))
    return(NA)
  }
  return(x)
}

# ---- Load Data with Error Handling ----
load_data <- function() {
  tryCatch(
    {
    # 1) Load GeoJSON
    ensure_geojson(geojson_path)
    thai_sf <- st_read(geojson_path, quiet = TRUE)
    
      if (!"pro_th" %in% names(thai_sf)) {
      stop("GeoJSON ไม่มีฟิลด์ 'pro_th' สำหรับชื่อจังหวัดภาษาไทย")
    }
    thai_sf <- thai_sf |> mutate(pro_th = norm_prov(pro_th))
    
    # 2) Load Excel
      if (!file.exists(excel_path)) {
        stop(paste(
          "ไม่พบไฟล์ Excel:",
          excel_path,
          "- กรุณาตรวจสอบว่าไฟล์อยู่ในโฟลเดอร์ data"
        ))
    }
    
    sheets <- excel_sheets(excel_path)
      if (length(sheets) == 0) {
      stop("ไฟล์ Excel ไม่มี sheet ข้อมูล")
    }
    
      pest_long <- purrr::map_dfr(sheets, \(sh) {
        tryCatch(
          {
        df <- read_excel(excel_path, sheet = sh)
        
            if (!"province" %in% names(df)) {
              warning(paste("Sheet", sh, "ไม่มีคอลัมน์ 'province' - ข้าม"))
          return(NULL)
        }
        
        df |>
              pivot_longer(
                !c(Region, province),
                names_to = "month",
                values_to = "risk_index"
              ) |>
          mutate(
            pest = sh,
            month = norm_month(month),
            province = norm_prov(province),
            risk_index = sapply(risk_index, validate_risk)
          ) |>
              filter(!is.na(month)) # กรองเดือนที่แปลงไม่ได้ออก
          },
          error = function(e) {
        warning(paste("Error reading sheet", sh, ":", e$message))
        return(NULL)
          }
        )
    })
    
      if (nrow(pest_long) == 0) {
        stop("ไม่สามารถอ่านข้อมูลจาก Excel ได้ หรือข้อมูลในไฟล์ไม่ถูกต้อง")
    }
    
    # Return all necessary data
    list(
      thai_sf = thai_sf,
      pest_long = pest_long,
      pest_choices = sort(unique(pest_long$pest)),
      month_choices = th_month_levels,
      province_list = sort(unique(pest_long$province))
    )
    },
    error = function(e) {
      stop("เกิดข้อผิดพลาดในการโหลดข้อมูล: ", e$message)
    }
  )
}

# Load data
data <- tryCatch(load_data(), error = function(e) {
  shiny::showNotification(e$message, type = "error", duration = NULL)
  NULL
})

# ---- Enhanced UI ----
ui <- fluidPage(
  tags$head(
    # Import Google Font 'Mitr'
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Mitr&display=swap"
    ),
    tags$style(HTML(
      "
      body { font-family: 'Mitr', sans-serif; }
      h2, h4 { font-family: 'Mitr', sans-serif; }
      .well { background-color: #f8f9fa; }
      .stats-box { 
        background: white; 
        padding: 15px; 
        border-radius: 5px; 
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-bottom: 15px;
      }
      .high-risk { color: #d73027; font-weight: bold; }
      .medium-risk { color: #fdae61; font-weight: bold; }
      .low-risk { color: #fee08b; }
    "
    ))
  ),
  
  titlePanel("ระบบแสดงดัชนีความเสี่ยงศัตรูข้าว (Pest Risk Index)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("ตัวเลือกการแสดงผล"),
      
      selectInput(
        "pest",
                  "เลือกศัตรูข้าว:", 
                  choices = data$pest_choices, 
        selected = data$pest_choices[1]
      ),
      
      selectInput(
        "month",
                  "เลือกเดือน:", 
                  choices = data$month_choices, 
        selected = data$month_choices[1]
      ),
      
      hr(),
      
      h4("สถิติภาพรวม"),
      div(class = "stats-box", uiOutput("stats_summary")),
      
      hr(),
      
      h4("ตัวเลือกเพิ่มเติม"),
      
      checkboxInput("show_trend", "แสดงกราฟแนวโน้มรายปี", value = FALSE),
      
      checkboxInput("compare_mode", "เปรียบเทียบข้ามเดือน", value = FALSE),
      
      conditionalPanel(
        condition = "input.compare_mode",
        selectInput(
          "month2",
                    "เดือนที่ 2 (เปรียบเทียบ):", 
                    choices = data$month_choices,
          selected = data$month_choices[2]
        )
      ),
      
      hr(),
      
      downloadButton(
        "download_data",
                     "ดาวน์โหลดข้อมูลปัจจุบัน",
        class = "btn-primary btn-block"
      ),
      
      br(),
      
      helpText(
        tags$ul(
          tags$li("สีแดง (4) = ความเสี่ยงสูงมาก"),
          tags$li("สีส้ม (3) = ความเสี่ยงสูง"),
          tags$li("สีเหลือง (2) = ความเสี่ยงปานกลาง"),
          tags$li("สีครีม (1) = ความเสี่ยงต่ำ"),
          tags$li("สีเทา (0/NA) = ไม่มีความเสี่ยง/ไม่มีข้อมูล")
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "แผนที่",
          leafletOutput("map", height = 600),
          br(),
          conditionalPanel(
            condition = "input.compare_mode",
            h4("แผนที่เปรียบเทียบ"),
            leafletOutput("map2", height = 400)
          )
        ),
        
        tabPanel("ตารางข้อมูล", br(), DT::dataTableOutput("data_table")),
        
        tabPanel(
          "การวิเคราะห์",
          br(),
          plotlyOutput("risk_distribution"),
          br(),
          conditionalPanel(
            condition = "input.show_trend",
            h4("แนวโน้มความเสี่ยงรายเดือน"),
            plotlyOutput("monthly_trend")
          )
        ),
        
        tabPanel(
          "จังหวัดเสี่ยงสูง",
          br(),
          h4("จังหวัดที่มีความเสี่ยงสูง (Risk Index ≥ 3)"),
          DT::dataTableOutput("high_risk_table"),
          br(),
          plotlyOutput("province_ranking")
        )
      )
    )
  )
)

# ---- Enhanced Server ----
server <- function(input, output, session) {
  # Reactive data selection
  # Stop the app if data loading failed
  if (is.null(data)) {
    shiny::showNotification(
      "ไม่สามารถเริ่มต้นแอปพลิเคชันได้เนื่องจากข้อมูลไม่พร้อมใช้งาน",
      type = "error",
      duration = NULL
    )
    return()
  }

  # Reactive data selection for the main map
  data_sel <- reactive({
    req(input$pest, input$month)
    data$pest_long |>
      filter(pest == input$pest, month == input$month)
  })
  
  # Data for comparison (if enabled)
  data_sel2 <- reactive({
    req(input$pest, input$month2, input$compare_mode)
    data$pest_long |>
      filter(pest == input$pest, month == input$month2)
  })
  
  # Statistics summary
  output$stats_summary <- renderUI({
    df <- data_sel()
    
    total_provinces <- length(unique(df$province))
    high_risk <- sum(df$risk_index >= 3, na.rm = TRUE)
    medium_risk <- sum(df$risk_index == 2, na.rm = TRUE)
    low_risk <- sum(df$risk_index == 1, na.rm = TRUE)
    no_risk <- sum(df$risk_index == 0, na.rm = TRUE)
    avg_risk <- mean(df$risk_index, na.rm = TRUE)
    
    HTML(paste0(
      "<b>ศัตรูข้าว:</b> ",
      input$pest,
      "<br>",
      "<b>เดือน:</b> ",
      input$month,
      "<br>",
      "<b>จังหวัดทั้งหมด:</b> ",
      total_provinces,
      "<br>",
      "<span class='high-risk'>เสี่ยงสูง (3-4):</span> ",
      high_risk,
      " จังหวัด<br>",
      "<span class='medium-risk'>เสี่ยงปานกลาง (2):</span> ",
      medium_risk,
      " จังหวัด<br>",
      "<span class='low-risk'>เสี่ยงต่ำ (1):</span> ",
      low_risk,
      " จังหวัด<br>",
      "<b>ไม่มีความเสี่ยง (0):</b> ",
      no_risk,
      " จังหวัด<br>",
      "<b>ค่าเฉลี่ยความเสี่ยง:</b> ",
      round(avg_risk, 2)
    ))
  })
  
  # --- Map Rendering ---
  output$map <- renderLeaflet({
    # กำหนดขอบเขตสูงสุดของแผนที่ให้อยู่ในประเทศไทย
    bbox <- st_bbox(data$thai_sf)

    df <- data_sel()
    map_df <- data$thai_sf |>
      left_join(df, by = c("pro_th" = "province"))
    
    pal <- colorFactor(
      palette = viridis::viridis_pal(option = "C")(5), # ใช้ชุดสี Viridis
      domain = factor(0:4),
      na.color = "#cccccc"
    )
    
    leaflet(map_df) |>
      addProviderTiles(providers$CartoDB.Positron, layerId = "base_tiles") |>
      addPolygons(
        # This will be updated by the proxy
        fillColor = ~ pal(factor(risk_index)),
        fillOpacity = 0.85,
        color = "#ffffff",
        weight = 1,
        label = ~ paste0(
          pro_th,
          " : Risk = ",
          ifelse(is.na(risk_index), "ไม่มีข้อมูล", risk_index)
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        layerId = ~pro_th, # ทำให้แต่ละจังหวัดสามารถคลิกได้
        highlightOptions = highlightOptions(
          weight = 3, 
          color = "#2c3e50", 
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        # This will be updated by the proxy
        "bottomright",
                pal = pal, 
                values = factor(0:4),
                title = paste("<b>Risk Index</b><br>", input$pest, "<br>", input$month),
        opacity = 1
      )
  })
  
  # Use a proxy to update the main map without redrawing it
  observe({
    df <- data_sel()
    map_df <- data$thai_sf |>
      left_join(df, by = c("pro_th" = "province"))
    
    pal <- colorFactor(
      palette = viridis::viridis_pal(option = "C")(5), # ใช้ชุดสี Viridis
      domain = factor(0:4),
      na.color = "#cccccc"
    )

    leafletProxy("map", data = map_df) |>
      clearShapes() |>
      addPolygons(
        fillColor = ~ pal(factor(risk_index)),
        fillOpacity = 0.85,
        color = "#ffffff",
        weight = 1,
        label = ~ paste0(
          pro_th,
          " : Risk = ",
          ifelse(is.na(risk_index), "ไม่มีข้อมูล", risk_index)
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        layerId = ~pro_th, # ทำให้แต่ละจังหวัดสามารถคลิกได้
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#2c3e50",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) |>
      clearControls() |>
      addLegend(
        "bottomright",
        pal = pal,
        values = factor(0:4),
        title = paste("<b>Risk Index</b><br>", input$pest, "<br>", input$month),
        opacity = 1
      )
  })

  # Function to generate a leaflet map (to reduce code duplication)
  create_map <- function(data_to_map, pest_name, month_name) {
    map_df <- data$thai_sf |>
      left_join(data_to_map, by = c("pro_th" = "province"))
    pal <- colorFactor(
      palette = viridis::viridis_pal(option = "C")(5),
      domain = factor(0:4),
      na.color = "#cccccc"
    )
    
    leaflet(map_df) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~ pal(factor(risk_index)),
        fillOpacity = 0.85,
        color = "#ffffff",
        weight = 1,
        label = ~ paste0(
          pro_th,
          " : Risk = ",
          ifelse(is.na(risk_index), "ไม่มีข้อมูล", risk_index)
        ),
        highlightOptions = highlightOptions(
          weight = 3, 
          color = "#2c3e50", 
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        "bottomright",
                pal = pal, 
                values = factor(0:4),
        title = paste("<b>Risk Index</b><br>", pest_name, "<br>", month_name),
        opacity = 1
      )
  }

  # Comparison map (if enabled) - Now uses the helper function
  output$map2 <- renderLeaflet({
    req(input$compare_mode)
    create_map(data_sel2(), input$pest, input$month2)
  })
  
  # Data table
  output$data_table <- DT::renderDataTable({
    df <- data_sel() |>
      arrange(desc(risk_index), province) |>
      select(province, `ดัชนีความเสี่ยง` = risk_index)
    
    DT::datatable(
      df,
                  options = list(
                    pageLength = 20,
                    language = list(
                      search = "ค้นหา:",
                      lengthMenu = "แสดง _MENU_ รายการ",
                      info = "แสดง _START_ ถึง _END_ จาก _TOTAL_ รายการ",
                      paginate = list(
                        first = "หน้าแรก",
                        last = "หน้าสุดท้าย",
                        `next` = "ถัดไป",
                        previous = "ก่อนหน้า"
                      )
                    )
      )
    ) |>
      DT::formatStyle(
        'ดัชนีความเสี่ยง',
                      backgroundColor = DT::styleInterval(
                        c(0.5, 1.5, 2.5, 3.5),
                        c("#f0f0f0", "#fee08b", "#fdae61", "#f46d43", "#d73027")
        )
      )
  })
  
  # Risk distribution plot
  output$risk_distribution <- renderPlotly({
    df <- data_sel()
    
    risk_summary <- df |>
      mutate(
        risk_category = case_when(
        risk_index == 0 ~ "0: ไม่มีความเสี่ยง",
        risk_index == 1 ~ "1: เสี่ยงต่ำ",
        risk_index == 2 ~ "2: เสี่ยงปานกลาง",
        risk_index == 3 ~ "3: เสี่ยงสูง",
        risk_index == 4 ~ "4: เสี่ยงสูงมาก",
        TRUE ~ "ไม่มีข้อมูล"
        )
      ) |>
      count(risk_category) |>
      mutate(percentage = n / sum(n) * 100)
    
    p <- ggplot(
      risk_summary,
      aes(x = risk_category, y = n, fill = risk_category)
    ) +
      geom_col() +
      scale_fill_manual(
        values = c(
        "0: ไม่มีความเสี่ยง" = "#f0f0f0",
        "1: เสี่ยงต่ำ" = "#fee08b",
        "2: เสี่ยงปานกลาง" = "#fdae61",
        "3: เสี่ยงสูง" = "#f46d43",
        "4: เสี่ยงสูงมาก" = "#d73027",
        "ไม่มีข้อมูล" = "#cccccc"
        )
      ) +
      labs(
        title = paste("การกระจายความเสี่ยง:", input$pest, "-", input$month),
           x = "ระดับความเสี่ยง",
        y = "จำนวนจังหวัด"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p) |> 
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Monthly trend
  output$monthly_trend <- renderPlotly({
    req(input$show_trend)
    
    trend_data <- data$pest_long |>
      filter(pest == input$pest) |>
      group_by(month) |>
      summarise(
        avg_risk = mean(risk_index, na.rm = TRUE),
        max_risk = max(risk_index, na.rm = TRUE),
        min_risk = min(risk_index, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(trend_data, aes(x = month)) +
      geom_ribbon(
        aes(ymin = min_risk, ymax = max_risk),
        fill = "gray80",
        alpha = 0.5
      ) +
      geom_line(aes(y = avg_risk, group = 1), color = "#d73027", size = 2) +
      geom_point(aes(y = avg_risk), color = "#d73027", size = 3) +
      labs(
        title = paste("แนวโน้มความเสี่ยงรายเดือน:", input$pest),
           x = "เดือน",
        y = "ดัชนีความเสี่ยง"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) |> 
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # High risk provinces table
  output$high_risk_table <- DT::renderDataTable({
    high_risk <- data$pest_long |>
      filter(pest == input$pest, risk_index >= 3) |>
      group_by(province) |>
      summarise(
        `เดือนที่เสี่ยงสูง` = paste(month[risk_index >= 3], collapse = ", "),
        `ค่าเฉลี่ยความเสี่ยง` = round(mean(risk_index, na.rm = TRUE), 2),
        `ค่าสูงสุด` = max(risk_index, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(`ค่าเฉลี่ยความเสี่ยง`))
    
    DT::datatable(
      high_risk,
                  options = list(
                    pageLength = 10,
                    language = list(search = "ค้นหา:")
      )
    )
  })
  
  # Province ranking
  output$province_ranking <- renderPlotly({
    ranking <- data_sel() |>
      arrange(desc(risk_index)) |>
      head(15) |>
      mutate(province = factor(province, levels = rev(provinces)))
    
    p <- ggplot(
      ranking,
      aes(x = risk_index, y = province, fill = factor(risk_index))
    ) +
      geom_col() +
      scale_fill_manual(
        values = c(
        "0" = "#f0f0f0",
        "1" = "#fee08b",
        "2" = "#fdae61",
        "3" = "#f46d43",
        "4" = "#d73027"
        )
      ) +
      labs(
        title = paste("15 จังหวัดเสี่ยงสูงสุด:", input$pest, "-", input$month),
           x = "ดัชนีความเสี่ยง",
        y = ""
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p) |> 
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(
        "pest_risk_",
        input$pest,
        "_",
        input$month,
        "_",
        Sys.Date(),
        ".csv"
      )
    },
    content = function(file) {
      df <- data_sel() |>
        arrange(desc(risk_index), province) |>
        mutate(
          pest = input$pest,
          month = input$month
        ) |>
        select(pest, month, province, risk_p = risk_index)
      
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

# Run app
shinyApp(ui, server)
