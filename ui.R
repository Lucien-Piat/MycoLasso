library(shiny)
library(leaflet)
library(DT)
library(plotly)
library(shinycssloaders)

ui <- fluidPage(
  
  # Custom CSS for dark theme
  tags$head(
    tags$style(HTML("
      body {
        background-color: #1a1a1a;
        color: #e0e0e0;
      }
      .title-panel {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 5px;
        margin-bottom: 20px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.3);
      }
      .title-panel h2 {
        margin: 0;
        font-weight: 600;
      }
      .subtitle {
        font-size: 14px;
        opacity: 0.9;
        margin-top: 5px;
      }
      
      /* Enhanced Sidebar styling - MORE SPECIFIC SELECTORS */
      .col-sm-3 {
        background-color: #1a1a1a !important;
      }
      
      /* Target the actual sidebar panel specifically */
      .sidebar, .sidebar-panel, .well {
        background-color: #252525 !important; /* Soft dark gray, easier on eyes */
        border: 1px solid #3a3a3a !important;
        border-radius: 8px !important;
        box-shadow: 0 2px 8px rgba(0,0,0,0.3) !important;
        padding: 20px !important;
      }
      
      /* Override Bootstrap's well class specifically */
      .well-sm, .well-lg {
        background-color: #252525 !important;
        border: 1px solid #3a3a3a !important;
      }
      
      /* Target Shiny's sidebar panel more specifically */
      div[class*='col-sm-3'] > .well {
        background-color: #252525 !important;
        border: 1px solid #3a3a3a !important;
      }
      
      /* Force background on all form groups in sidebar */
      .sidebar .form-group,
      .well .form-group {
        background-color: transparent !important;
      }
      
      .btn-warning {
        width: 100%;
        margin-top: 10px;
        background-color: #ff9800;
        border-color: #ff9800;
        color: #000;
        transition: all 0.3s ease;
      }
      .btn-warning:hover {
        background-color: #fb8c00;
        border-color: #fb8c00;
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(255, 152, 0, 0.3);
      }
      .upload-section {
        background-color: #303030; /* Slightly lighter than sidebar for contrast */
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 20px;
        border: 2px dashed #667eea;
        transition: all 0.3s ease;
      }
      .upload-section:hover {
        border-color: #764ba2;
        background-color: #353535;
      }
      .nav-tabs {
        border-bottom: 2px solid #404040;
      }
      .nav-tabs > li > a {
        color: #b0b0b0;
        background-color: #2d2d2d;
        border: 1px solid #404040;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: #ffffff;
        background-color: #1a1a1a;
        border: 1px solid #404040;
        border-bottom-color: transparent;
      }
      .nav-tabs > li > a:hover {
        background-color: #404040;
        border-color: #404040;
      }
      h4 {
        color: #e0e0e0;
        font-weight: 500;
        margin-bottom: 15px;
      }
      label {
        color: #b0b0b0 !important;
        font-weight: 400 !important;
      }
      .checkbox label {
        color: #b0b0b0 !important;
      }
      hr {
        border-top: 1px solid #404040;
        margin: 20px 0;
      }
      
      /* Fix for selectize input visibility with improved contrast */
      .selectize-input {
        background-color: #303030 !important; /* Slightly lighter for better contrast */
        border: 1px solid #454545 !important;
        color: #e0e0e0 !important;
        transition: all 0.3s ease;
      }
      .selectize-input > input {
        color: #e0e0e0 !important;
      }
      .selectize-input .item {
        background-color: #667eea !important;
        color: #ffffff !important;
        border: 1px solid #667eea !important;
      }
      .selectize-input.focus {
        background-color: #353535 !important;
        border-color: #667eea !important;
        color: #e0e0e0 !important;
        box-shadow: 0 0 0 2px rgba(102, 126, 234, 0.2);
      }
      .selectize-dropdown {
        background-color: #303030 !important;
        border: 1px solid #454545 !important;
        color: #e0e0e0 !important;
        box-shadow: 0 4px 6px rgba(0,0,0,0.3);
      }
      .selectize-dropdown-content .option {
        background-color: #303030 !important;
        color: #e0e0e0 !important;
      }
      .selectize-dropdown-content .option:hover,
      .selectize-dropdown-content .option.active {
        background-color: #454545 !important;
        color: #ffffff !important;
      }
      .selectize-dropdown-content .option.selected {
        background-color: #667eea !important;
        color: #ffffff !important;
      }
      
      /* Form controls with better contrast */
      .form-control {
        background-color: #303030 !important;
        border: 1px solid #454545 !important;
        color: #e0e0e0 !important;
        transition: all 0.3s ease;
      }
      .form-control:focus {
        background-color: #353535 !important;
        border-color: #667eea !important;
        color: #e0e0e0 !important;
        box-shadow: 0 0 0 2px rgba(102, 126, 234, 0.2);
      }
      
      /* File input specific styling */
      .btn-default {
        background-color: #303030 !important;
        border: 1px solid #454545 !important;
        color: #e0e0e0 !important;
      }
      .btn-default:hover {
        background-color: #404040 !important;
        border-color: #667eea !important;
      }
      
      /* Form group styling */
      .form-group {
        background-color: transparent !important;
      }
      
      /* Control label */
      .control-label {
        color: #b0b0b0 !important;
      }
      
      /* DataTables styling */
      .dataTables_wrapper {
        color: #e0e0e0;
      }
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_processing,
      .dataTables_wrapper .dataTables_paginate {
        color: #e0e0e0;
      }
      table.dataTable {
        background-color: #2d2d2d;
        color: #e0e0e0;
      }
      table.dataTable thead th,
      table.dataTable thead td {
        background-color: #404040;
        color: #e0e0e0;
        border-bottom: 1px solid #555;
      }
      table.dataTable tbody tr {
        background-color: #2d2d2d;
      }
      table.dataTable tbody tr:hover {
        background-color: #404040;
      }
      table.dataTable.stripe tbody tr.odd,
      table.dataTable.display tbody tr.odd {
        background-color: #252525;
      }
      table.dataTable.stripe tbody tr.odd:hover,
      table.dataTable.display tbody tr.odd:hover {
        background-color: #404040;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: #e0e0e0 !important;
        background-color: #2d2d2d;
        border: 1px solid #404040;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        color: #fff !important;
        background-color: #404040;
        border: 1px solid #667eea;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        color: #fff !important;
        background-color: #667eea;
        border: 1px solid #667eea;
      }
      input[type='search'] {
        background-color: #2d2d2d;
        color: #e0e0e0;
        border: 1px solid #404040;
      }
      select {
        background-color: #2d2d2d;
        color: #e0e0e0;
        border: 1px solid #404040;
      }
      .leaflet-container {
        background-color: #1a1a1a;
      }
      
      /* Main panel background to match */
      .col-sm-9 {
        background-color: #1a1a1a !important;
      }
      
      /* Credits footer */
      .credits-footer {
        background-color: #2d2d2d;
        padding: 20px;
        border-radius: 5px;
        margin-top: 20px;
        border-top: 3px solid #667eea;
        text-align: center;
      }
      .credits-footer p {
        margin: 5px 0;
        color: #b0b0b0;
      }
      .credits-footer strong {
        color: #e0e0e0;
      }
    "))
  ),
  
  div(class = "title-panel",
      h2("🍄 MycoLasso"),
      div(class = "subtitle", "Interactive Spatial Data Mapping & Lasso Selection Tool")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # File upload section
      div(class = "upload-section",
          h4("📁 Upload Data"),
          fileInput(
            inputId = "data_file",
            label = NULL,
            accept = c(
              ".csv",
              ".tsv",
              ".txt",
              ".xlsx",
              ".xls",
              "text/csv",
              "text/tab-separated-values",
              "application/vnd.ms-excel",
              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
            ),
            placeholder = "Choose file or use default..."
          ),
          selectInput(
            inputId = "separator",
            label = "Separator (for text files):",
            choices = c(
              "Comma" = ",",
              "Semicolon" = ";",
              "Tab" = "\t",
              "Space" = " ",
              "Pipe" = "|"
            ),
            selected = ","
          ),
          checkboxInput(
            inputId = "header",
            label = "First row is header",
            value = TRUE
          )
      ),
      
      h4("⚙️ Settings"),
      selectInput(
        inputId = "longitude_col",
        label = "Longitude Column:",
        choices = NULL
      ),
      selectInput(
        inputId = "latitude_col",
        label = "Latitude Column:",
        choices = NULL
      ),
      selectInput(
        inputId = "tag_column",
        label = "Select Tag Column:",
        choices = NULL
      ),
      hr(),
      actionButton("reset_selection", "🔄 Reset Selection", class = "btn-warning")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "📊 Data Table",
          br(),
          withSpinner(
            DTOutput("data_table"),
            type = 5,
            color = "#667eea",
            size = 1.2
          )
        ),
        tabPanel(
          "🗺️ Map",
          br(),
          withSpinner(
            leafletOutput("map", height = "calc(100vh - 200px)"),
            type = 6,
            color = "#667eea",
            size = 1.5
          )
        ),
        tabPanel(
          "📈 Plot",
          br(),
          withSpinner(
            plotlyOutput("barplot", height = "calc(100vh - 200px)"),
            type = 4,
            color = "#764ba2",
            size = 1.2
          )
        )
      )
    )
  ),
  
  # Credits Footer
  hr(),
  div(class = "credits-footer",
      p(strong("MycoLasso"), " - Interactive Spatial Data Mapping Tool"),
      p("Created by ", strong("Lucien PIAT"), " | Bioinformatician"),
      p(style = "font-size: 12px; margin-top: 10px;", "Free of use and distribution | MIT License")
  )
)