library(shiny)
library(DT)
library(leaflet)
library(ggplot2)
library(dplyr)
library(rsconnect)

data_raw <- read.csv("Annual_14C_Repository_V6.csv", stringsAsFactors = FALSE)
Intcal20Curve <- read.csv("Intcal20Curve2.csv", stringsAsFactors = FALSE)
SHcal20Curve <- read.csv("SHcal20Curve.csv", stringsAsFactors = FALSE)

Intcal20Curve <- Intcal20Curve %>%
  mutate(
    CAL_BP = suppressWarnings(as.numeric(CAL_BP)),
    Year = ifelse(
      !is.na(CAL_BP),
      ifelse(1950 - CAL_BP >= 1, 1950 - CAL_BP, 1949 - CAL_BP), Year
    )
  )

SHcal20Curve <- SHcal20Curve %>%
  mutate(
    CAL_BP = suppressWarnings(as.numeric(CAL_BP)),
    Year = ifelse(
      !is.na(CAL_BP),
      ifelse(1950 - CAL_BP >= 1, 1950 - CAL_BP, 1949 - CAL_BP), Year
    )
  )

data_raw <- data_raw %>%
  mutate(
    Dated_Year = suppressWarnings(as.numeric(gsub("[^0-9\\.-]", "", Dated_Year))),
    Age_Corrected_D14C_Error = suppressWarnings(as.numeric(Age_Corrected_D14C_Error)),
    Age_Corrected_D14C = suppressWarnings(as.numeric(Age_Corrected_D14C)),
    Latitude = suppressWarnings(as.numeric(Latitude)),
    Longitude = suppressWarnings(as.numeric(Longitude))
  )

last_updated <- local({
  mt <- file.mtime("app.R")
  mt <- mt[!is.na(mt)]
  if (length(mt) == 0) "Not available" else format(max(mt), "%B %d, %Y")
})

n_rows <- format(nrow(data_raw), big.mark = ",")

format_year <- function(y) {
  ifelse(is.na(y), NA_character_,
         ifelse(y < 0, paste(abs(y), "BCE"), paste(y, "CE")))
}

ui <- fluidPage(
  titlePanel("INTRA: International Tree-ring Radiocarbon Archive"),
  
  fluidRow(
    column(
      width = 12,
      wellPanel(
        h3("Information"),
        p("This repository contains ", strong(n_rows), " (sub)annual tree ring radiocarbon (", tags$sup("14"), "C) measurements spanning the past ~14000 years. Dated tree ring ", tags$sup("14"), "C measurements reflect atmospheric ", tags$sup("14"), "C, which is a proxy for past solar activity and extreme SEP events, like Miyake Events. In the modern day, Miyake Events could cause damage to telecommunications and the power grid, as well as cause radiation sickness at high altitudes. An inclusive, machine-readable, and stable repository of ", tags$sup("14"), "C measurements is necessary to conduct variability studies and disentangle patterns of ", tags$sup("14"), "C production and sequestration in order to build resilience to these events."),
        p(strong("Publication: "), "MacMillan, J. et al. In prep. An Open-Source Repository for Research on Tree Ring ", tags$sup("14"), "C: International Tree Ring Radiocarbon Archive (INTRA)")
      )
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      wellPanel(
        h3("How to Use"),
        p("1. Apply desired filters to data."),
        p("2. Observe changes to plot, map, and table."),
        p("3. Download resulting subset using the Download Filtered Data (CSV) button."),
        p("4. Submit your own data for review and addition to INTRA using the Upload Data button.")
      )
    )
  ),
  
  fluidRow(
    column(
      width = 4,
      wellPanel(
        checkboxInput("use_year", "Filter by Year Range", FALSE),
        conditionalPanel(
          condition = "input.use_year == true",
          uiOutput("year_ui")
        ),
        
        checkboxInput("use_country", "Filter by Country", FALSE),
        conditionalPanel(
          condition = "input.use_country == true",
          uiOutput("country_ui")
        ),
        
        checkboxInput("use_latlon", "Filter by Latitude/Longitude", FALSE),
        conditionalPanel(
          condition = "input.use_latlon == true",
          uiOutput("lat_ui"),
          uiOutput("lon_ui")
        ),
        
        checkboxInput("use_authors", "Filter by Authors", FALSE),
        conditionalPanel(
          condition = "input.use_authors == true",
          uiOutput("authors_ui")
        ),
        
        checkboxInput("use_ew", "Filter by EW_LW_WW", FALSE),
        conditionalPanel(
          condition = "input.use_ew == true",
          uiOutput("ew_ui")
        ),
        
        checkboxInput("show_errors", "Show Error Bars", TRUE),
        
        checkboxInput("show_nh_curve", "Show IntCal20 Curve", TRUE),
        
        checkboxInput("show_sh_curve", "Show SHCal20 Curve", FALSE),
        
        br(),
        
        actionButton("reset_filters", "Reset Filters", 
                     class = "btn-sm", style = "width: 100%; margin-bottom: 10px;"),
        downloadButton("download_data", "Download Filtered Data (CSV)", 
                       class = "btn-sm", style = "width: 100%;"),
        # REPLACE WITH URL TO GOOGLE FORM HERE
        tags$a(
          href = "https://google.com", 
          target = "_blank",
          rel = "noopener",
          class = "btn btn-default btn-sm",
          style = "width: 100%; margin-top: 10px;",
          icon("upload"), " Upload Data"
        )
      )
    ),
    column(
      h3(HTML("&Delta;<sup>14</sup>C of Individual Tree Ring Samples"), style = "text-align: center;"),
      width = 8,
      plotOutput("year_plot", height = 400)
    )
  ),
  
  fluidRow(
    h3("Tree Ring Sampling Locations", style = "text-align: center;")
  ),
  
  fluidRow(
    column(
      width = 12,
      div(style = "padding: 0px 20px;",
          leafletOutput("map", height = 600))
    )
  ),
  
  fluidRow(
    p(HTML("Each point on the map respresents one tree ('Tree Series') that was measured for <sup>14</sup>C. Multiple measurements may be derived from the same tree, and multiple trees may be present at the same site/coordinates."), style = "text-align: center;")
  ),
  
  fluidRow(
    h3(HTML("Data Table of <sup>14</sup>C Measurements"), style = "text-align: center;")
  ),
  
  fluidRow(
    column(
      width = 12,
      div(style = "padding: 0px 20px;",
          DTOutput("table"))
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      tags$a(
        href = "https://drive.google.com/file/d/1mgqNZ7cV5i1r6iElCMk9hFer98NfSAjX/view?usp=sharing",
        target = "_blank",
        class = "btn btn-primary btn-lg",
        style = "display: block; width: 100%; margin-bottom: 15px;",
        icon("file-alt"), " Attribute Description File"
      )
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      wellPanel(
        h3("Credits"),
        p(strong("Authors: "), "Jack MacMillan", tags$sup("1"), ", Meagan Walker", tags$sup("1"), ", Sophie von Fromm", tags$sup("3"), ", Lankeswar Dey", tags$sup("2"), ", Rachael Filwett", tags$sup("3"), ", Mariah Carbone", tags$sup("4"), ", Arch Robison", tags$sup("3"), ", Miles Case", tags$sup("1"), ", Martina Jarek", tags$sup("1"), ", Gillian Snyder", tags$sup("1"), ", Amy Hessl", tags$sup("1")),
        p(strong("Institutions: ")), 
        p(tags$sup("1"), "Department of Geology and Geography, West Virginia University, Morgantown, WV, 26505, United States"),
        p(tags$sup("2"), "FORTH Institute of Astrophysics, Heraklion, Greece"),
        p(tags$sup("3"), "Department of Physics, Montana State University, Bozeman, MT, 59717, United States"),
        p(tags$sup("4"), "Center for Ecosystem Science and Society, Northern Arizona University, Flagstaff, AZ, 86011, United States"),
        p(strong("Contact: "), "Jack MacMillan, jtm00036@mix.wvu.edu"),
        p(strong("Funding: "), "NSF Solar Terrestrial #2411569, NSF Graduate Research Fellowship to M. Walker #2136524"),
        p(strong("Last updated: "), last_updated)
      )
    )
  ),
)

server <- function(input, output, session) {
  
  observeEvent(input$reset_filters, {
    updateCheckboxInput(session, "use_ew", value = FALSE)
    updateCheckboxInput(session, "use_country", value = FALSE)
    updateCheckboxInput(session, "use_authors", value = FALSE)
    updateCheckboxInput(session, "use_year", value = FALSE)
    updateCheckboxInput(session, "use_latlon", value = FALSE)
  })
  
  output$ew_ui <- renderUI({
    selectizeInput("ew_value", "Select EW_LW_WW",
                   choices = sort(na.omit(unique(data_raw$EW_LW_WW))),
                   multiple = TRUE)
  })
  
  output$country_ui <- renderUI({
    tagList(
      selectizeInput("country_value", "Include Countries",
                     choices = sort(na.omit(unique(data_raw$Country))),
                     multiple = TRUE),
      selectizeInput("country_exclude", "Exclude Countries",
                     choices = sort(na.omit(unique(data_raw$Country))),
                     multiple = TRUE)
    )
  })
  
  output$authors_ui <- renderUI({
    tagList(
      selectizeInput("authors_value", "Include Authors",
                     choices = sort(na.omit(unique(data_raw$Author_Year))),
                     multiple = TRUE),
      selectizeInput("authors_exclude", "Exclude Authors",
                     choices = sort(na.omit(unique(data_raw$Author_Year))),
                     multiple = TRUE)
    )
  })
  
  output$year_ui <- renderUI({
    valid_years <- na.omit(data_raw$Dated_Year)
    min_year <- floor(min(valid_years))
    max_year <- ceiling(max(valid_years))
    
    tagList(
      numericInput("year_min", "Start Year (Minimum: -5419)", value = min_year, min = min_year, max = max_year),
      numericInput("year_max", "End Year (Maximum: 1933)", value = max_year, min = min_year, max = max_year)
    )
  })
  
  output$lat_ui <- renderUI({
    sliderInput("lat_range", "Latitude Range", 
                min = floor(min(data_raw$Latitude, na.rm = TRUE)), 
                max = ceiling(max(data_raw$Latitude, na.rm = TRUE)), 
                value = c(floor(min(data_raw$Latitude, na.rm = TRUE)), ceiling(max(data_raw$Latitude, na.rm = TRUE))))
  })
  
  output$lon_ui <- renderUI({
    sliderInput("lon_range", "Longitude Range", 
                min = floor(min(data_raw$Longitude, na.rm = TRUE)), 
                max = ceiling(max(data_raw$Longitude, na.rm = TRUE)), 
                value = c(floor(min(data_raw$Longitude, na.rm = TRUE)), ceiling(max(data_raw$Longitude, na.rm = TRUE))))
  })
  
  filtered_data <- reactive({
    df <- data_raw
    
    if (input$use_ew && !is.null(input$ew_value)) {
      df <- df %>% filter(EW_LW_WW %in% input$ew_value)
    }
    
    if (input$use_country) {
      if (!is.null(input$country_value)) {
        df <- df %>% filter(Country %in% input$country_value)
      }
      if (!is.null(input$country_exclude)) {
        df <- df %>% filter(!(Country %in% input$country_exclude))
      }
    }
    if (input$use_authors) {
      if (!is.null(input$authors_value)) {
        df <- df %>% filter(Author_Year %in% input$authors_value)
      }
      if (!is.null(input$authors_exclude)) {
        df <- df %>% filter(!(Author_Year %in% input$authors_exclude))
      }
    }
    
    if (input$use_year) {
      req(input$year_min, input$year_max)
      df <- df %>% filter(!is.na(Dated_Year), Dated_Year >= input$year_min, Dated_Year <= input$year_max)
    }
    
    if (input$use_latlon) {
      req(input$lat_range, input$lon_range)
      df <- df %>%
        filter(!is.na(Latitude), !is.na(Longitude),
               Latitude >= input$lat_range[1], Latitude <= input$lat_range[2],
               Longitude >= input$lon_range[1], Longitude <= input$lon_range[2])
    }
    df
  })
  
  output$table <- renderDT({
    df <- filtered_data() %>% select(!Key & !Tech.1 & !Tech.2 & !Year.0.Check & !Cellulose.Type.Check)
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No records match the filter criteria."),
                       options = list(dom = 't')))
    }
    
    src <- as.character(df$Source)
    has_link <- !is.na(src) & trimws(src) != ""
    
    df$Source <- ifelse(
      has_link,
      sprintf('<a href="%s" target="_blank" rel="noopener">%s</a>', trimws(src), htmltools::htmlEscape(src)),
      ""
    )
    
    datatable(
      df,
      escape = setdiff(names(df), "Source"),
      options = list(scrollX = TRUE, autoWidth = TRUE, pageLength = 10),
      class = 'display nowrap'
    )
  })
  
  output$map <- renderLeaflet({
    series <- filtered_data() %>%
      filter(!is.na(Latitude), !is.na(Longitude), !is.na(Tree_Series_ID)) %>%
      group_by(Tree_Series_ID) %>%
      summarise(
        Genus = first(Genus),
        Species = first(Species),
        Latitude = mean(Latitude),
        Longitude = mean(Longitude),
        Year_Min = suppressWarnings(min(Dated_Year, na.rm = TRUE)),
        Year_Max = suppressWarnings(max(Dated_Year, na.rm = TRUE)),
        N_Samples = n(),
        .groups = "drop"
      ) %>%
      mutate(
        Years = case_when(
          !is.finite(Year_Min)  ~ "Not available",
          Year_Min == Year_Max  ~ format_year(Year_Min),
          TRUE                  ~ paste(format_year(Year_Min), "-", format_year(Year_Max))
        )
      )
    
    if (nrow(series) == 0) {
      return(leaflet() %>% addTiles() %>% addPopups(0, 0, "No data to display"))
    }
    
    tree_icon <- makeAwesomeIcon(
      icon = "tree",
      library = "fa",
      markerColor = "darkblue",
      iconColor = "white"
    )
    
    leaflet(series, options = leafletOptions(minZoom = 1.4)) %>%
      addTiles(options = tileOptions(noWrap = TRUE)) %>%
      setMaxBounds(lng1 = -200, lat1 = -80, lng2 = 200, lat2 = 80) %>%
      addAwesomeMarkers(
        ~Longitude, ~Latitude,
        icon = tree_icon,
        clusterOptions = markerClusterOptions(),
        label = ~Tree_Series_ID,
        popup = ~paste0(
          "<strong>Tree Series ID:</strong> ", Tree_Series_ID, "<br/>",
          "<strong>Genus/Species:</strong> ", Genus, " ", Species, "<br/>",
          "<strong>Dated Year Range:</strong> ", Years, "<br/>",
          "<strong># of Samples:</strong> ", N_Samples
        )
      )
  })
  
  output$year_plot <- renderPlot({
    df <- filtered_data() %>%
      filter(!is.na(Dated_Year), !is.na(Age_Corrected_D14C))
    
    if (nrow(df) == 0) {
      plot.new()
      title("No data to display")
      return()
    }
    
    if (nrow(df) > 5000) {
      df <- df[sample(nrow(df), 5000), ]
      showNotification("Displaying a random sample of 5000 records for performance.", type = "message")
    }
    
    set.seed(123)
    df$Jittered_Year <- jitter(df$Dated_Year, amount = 0.3)
    
    min_year <- min(df$Dated_Year, na.rm = TRUE)
    max_year <- max(df$Dated_Year, na.rm = TRUE)
    
    Intcal20Curve_Filtered <- Intcal20Curve %>%
      filter(Year >= min_year, Year <= max_year)
    
    SHcal20Curve_Filtered <- SHcal20Curve %>%
      filter(Year >= min_year, Year <= max_year)
    
    p <- ggplot(df, aes(x = Jittered_Year, y = Age_Corrected_D14C)) +
      geom_point(alpha = 0.6, color = "darkblue", size = 2) +
      theme_minimal(base_size = 20) +
      labs(
        x = "Dated Year (jittered)",
        y = expression("Age Corrected " * Δ^14 * "C (‰)")
      )
    
    if (isTRUE(input$show_errors)) {
      p <- p + geom_errorbar(
        aes(ymin = Age_Corrected_D14C - Age_Corrected_D14C_Error,
            ymax = Age_Corrected_D14C + Age_Corrected_D14C_Error),
        width = 0.1, color = "darkblue", alpha = 0.4
      )
    }
    if (isTRUE(input$show_nh_curve) && nrow(Intcal20Curve_Filtered) > 1) {
      p <- p + geom_line(data = Intcal20Curve_Filtered, aes(x = Year, y = Delta_14C),
                         color = "red", linewidth = 0.5)
    }
    if (isTRUE(input$show_sh_curve) && nrow(SHcal20Curve_Filtered) > 1) {
      p <- p + geom_line(data = SHcal20Curve_Filtered, aes(x = Year, y = Delta_14C),
                         color = "green", linewidth = 0.5)
    }
    p
  }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_14C_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
