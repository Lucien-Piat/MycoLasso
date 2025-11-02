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
      
      /* Enhanced Sidebar styling */
      .col-sm-3 {
        background-color: #1a1a1a !important;
      }
      
      .sidebar, .sidebar-panel, .well {
        background-color: #252525 !important;
        border: 1px solid #3a3a3a !important;
        border-radius: 8px !important;
        box-shadow: 0 2px 8px rgba(0,0,0,0.3) !important;
        padding: 20px !important;
      }
      
      .well-sm, .well-lg {
        background-color: #252525 !important;
        border: 1px solid #3a3a3a !important;
      }
      
      div[class*='col-sm-3'] > .well {
        background-color: #252525 !important;
        border: 1px solid #3a3a3a !important;
      }
      
      .sidebar .form-group,
      .well .form-group {
        background-color: transparent !important;
      }
      
      /* Reduce margin for file input */
      .upload-section .form-group {
        margin-bottom: 10px !important;
      }
      
      .upload-section .form-group:first-child {
        margin-bottom: 5px !important;
      }
      
      .annotation-section .form-group {
        margin-bottom: 10px !important;
      }
      
      /* Button row styling */
      .button-row {
        display: flex;
        gap: 10px;
        margin-top: 10px;
      }
      
      .button-row .btn {
        flex: 1;
        width: auto !important;
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
      
      .btn-success {
        width: 100%;
        margin-top: 10px;
        background-color: #2ecc71;
        border-color: #2ecc71;
        color: #fff;
        transition: all 0.3s ease;
      }
      .btn-success:hover {
        background-color: #27ae60;
        border-color: #27ae60;
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(46, 204, 113, 0.3);
      }
      
      .btn-danger {
        width: 100%;
        margin-top: 10px;
        background-color: #e74c3c;
        border-color: #e74c3c;
        color: #fff;
        transition: all 0.3s ease;
      }
      .btn-danger:hover {
        background-color: #c0392b;
        border-color: #c0392b;
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(231, 76, 60, 0.3);
      }
      
      .upload-section {
        background-color: #303030;
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
      
      .annotation-section {
        background-color: #303030;
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 20px;
        border: 2px solid #2ecc71;
        transition: all 0.3s ease;
      }
      .annotation-section:hover {
        border-color: #27ae60;
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
        margin-top: 15px;
      }
      .nav-tabs + .tab-content h4 {
        border-bottom: 2px solid #667eea;
        padding-bottom: 10px;
        margin-bottom: 20px;
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
      
      /* Selectize styling */
      .selectize-input {
        background-color: #303030 !important;
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
      
      /* Form controls */
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
      
      /* File input */
      .btn-default {
        background-color: #303030 !important;
        border: 1px solid #454545 !important;
        color: #e0e0e0 !important;
      }
      .btn-default:hover {
        background-color: #404040 !important;
        border-color: #667eea !important;
      }
      
      .form-group {
        background-color: transparent !important;
      }
      
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
      .credits-footer a {
        color: #667eea;
        text-decoration: none;
        transition: all 0.3s ease;
      }
      .credits-footer a:hover {
        color: #764ba2;
        text-decoration: underline;
      }
      
      /* Modal styling */
      .modal-content {
        background-color: #2d2d2d;
        color: #e0e0e0;
        border: 1px solid #404040;
      }
      .modal-header {
        border-bottom: 1px solid #404040;
      }
      .modal-footer {
        border-top: 1px solid #404040;
      }
      .modal-title {
        color: #e0e0e0;
      }
      .close {
        color: #e0e0e0;
        opacity: 0.8;
      }
      .close:hover {
        color: #ffffff;
        opacity: 1;
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
          fluidRow(
            column(8,
                   selectInput(
                     inputId = "separator",
                     label = "Separator:",
                     choices = c(
                       "Comma" = ",",
                       "Semicolon" = ";",
                       "Tab" = "\t",
                       "Space" = " ",
                       "Pipe" = "|"
                     ),
                     selected = ","
                   )
            ),
            column(4,
                   br(),  # Add spacing to align checkbox vertically
                   checkboxInput(
                     inputId = "header",
                     label = "Header",
                     value = TRUE
                   )
            )
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
      
      # Annotation section
      div(class = "annotation-section",
          checkboxInput(
            inputId = "annotation_mode",
            label = HTML("<b>Annotation Mode</b><br/><small>Draw shapes or add pins to create named areas</small>"),
            value = FALSE
          ),
          fileInput(
            inputId = "load_annotations_file",
            label = "Load Annotations:",
            accept = ".json",
            placeholder = "Choose JSON file..."
          ),
          div(class = "button-row",
              downloadButton("download_annotations", "💾 Save", class = "btn-success"),
              actionButton("undo_annotation", "↩️ Undo", class = "btn-warning")
          )
      )
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
        ),
        tabPanel(
          "📊 Stats",
          br(),
          fluidRow(
            column(
              width = 12,
              h4("Summary Statistics"),
              withSpinner(
                DTOutput("summary_stats_table"),
                type = 5,
                color = "#667eea",
                size = 1.0
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 12,
              h4("Tag Counts"),
              withSpinner(
                DTOutput("tag_counts_table"),
                type = 5,
                color = "#667eea",
                size = 1.0
              )
            )
          )
        )
      )
    )
  ),
  
  # Footer
  hr(),
  div(class = "credits-footer",
      p(strong("MycoLasso v0.0.2"), " - Interactive Spatial Data Mapping Tool"),
      p("Created by ", tags$a(href = "https://lucienpiat.fr/", target = "_blank", strong("Lucien PIAT")), " | Bioinformatician"),
      p(style = "font-size: 12px; margin-top: 10px;", "Free of use and distribution | MIT License")
  )
)