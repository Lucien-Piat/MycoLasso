library(shiny)
library(leaflet)
library(leaflet.extras)
library(DT)
library(dplyr)
library(plotly)
library(sp)
library(shinycssloaders)
library(readxl)

server <- function(input, output, session) {
  
  # Load data based on file upload or default example.csv
  mushroom_data <- reactive({
    
    # Check if user uploaded a file
    if (!is.null(input$data_file)) {
      file_path <- input$data_file$datapath
      file_ext <- tools::file_ext(input$data_file$name)
    } else if (file.exists("example.csv")) {
      # Load default example.csv
      file_path <- "example.csv"
      file_ext <- "csv"
    } else {
      return(NULL)
    }
    
    tryCatch({
      # Read data based on file extension
      data <- switch(
        tolower(file_ext),
        "csv" = read.csv(
          file_path,
          header = input$header,
          sep = input$separator,
          stringsAsFactors = FALSE,
          na.strings = c("", "NA", "N/A", "null", "NULL", "#N/A")
        ),
        "tsv" = read.delim(
          file_path,
          header = input$header,
          sep = "\t",
          stringsAsFactors = FALSE,
          na.strings = c("", "NA", "N/A", "null", "NULL", "#N/A")
        ),
        "txt" = read.delim(
          file_path,
          header = input$header,
          sep = input$separator,
          stringsAsFactors = FALSE,
          na.strings = c("", "NA", "N/A", "null", "NULL", "#N/A")
        ),
        "xlsx" = read_excel(file_path, na = c("", "NA", "N/A", "null", "NULL")),
        "xls" = read_excel(file_path, na = c("", "NA", "N/A", "null", "NULL")),
        {
          showNotification(
            paste("Unsupported file format:", file_ext),
            type = "error",
            duration = NULL
          )
          return(NULL)
        }
      )
      
      # Convert to data frame and handle column names
      data <- as.data.frame(data)
      original_rows <- nrow(data)
      
      # Validate that data is not empty
      if (nrow(data) == 0) {
        showNotification(
          "Error: File contains no data rows",
          type = "error",
          duration = NULL
        )
        return(NULL)
      }
      
      if (ncol(data) == 0) {
        showNotification(
          "Error: File contains no columns",
          type = "error",
          duration = NULL
        )
        return(NULL)
      }
      
      # Clean column names
      names(data) <- make.names(names(data), unique = TRUE)
      
      # Remove rows with ALL NA values
      data <- data[rowSums(is.na(data)) != ncol(data), ]
      
      # Count rows removed
      rows_removed <- original_rows - nrow(data)
      
      if (nrow(data) == 0) {
        showNotification(
          "Error: All rows contain NA values",
          type = "error",
          duration = NULL
        )
        return(NULL)
      }
      
      # Show notification about loaded data
      if (is.null(input$data_file) && file.exists("example.csv")) {
        showNotification(
          paste("✅ Default example.csv loaded:", nrow(data), "rows,", ncol(data), "columns"),
          type = "message",
          duration = 3
        )
      } else {
        msg <- paste("✅ File loaded successfully:", nrow(data), "rows,", ncol(data), "columns")
        if (rows_removed > 0) {
          msg <- paste(msg, "\n⚠️", rows_removed, "empty row(s) removed")
        }
        showNotification(msg, type = "message", duration = 3)
      }
      
      return(data)
    }, error = function(e) {
      showNotification(
        paste("Error loading file:", e$message),
        type = "error",
        duration = NULL
      )
      return(NULL)
    })
  })
  
  # Reactive value to store selected points
  selected_points <- reactiveVal(NULL)
  
  # Update column choices based on data
  observe({
    data <- mushroom_data()
    
    if (is.null(data)) {
      updateSelectInput(session, "longitude_col", choices = NULL)
      updateSelectInput(session, "latitude_col", choices = NULL)
      updateSelectInput(session, "tag_column", choices = NULL)
      return()
    }
    
    col_names <- names(data)
    
    # Detect potential coordinate columns
    lon_candidates <- grep("lon|lng|x|longitude", col_names, ignore.case = TRUE, value = TRUE)
    lat_candidates <- grep("lat|y|latitude", col_names, ignore.case = TRUE, value = TRUE)
    
    # Update longitude column
    updateSelectInput(
      session,
      "longitude_col",
      choices = col_names,
      selected = if (length(lon_candidates) > 0) lon_candidates[1] else if (length(col_names) >= 2) col_names[length(col_names) - 1] else col_names[1]
    )
    
    # Update latitude column
    updateSelectInput(
      session,
      "latitude_col",
      choices = col_names,
      selected = if (length(lat_candidates) > 0) lat_candidates[1] else 
        if (length(col_names) >= 1) col_names[length(col_names)] else col_names[1]
    )
    
    # Update tag column (prefer categorical or text columns)
    tag_default <- NULL
    for (col in col_names) {
      if (is.character(data[[col]]) || is.factor(data[[col]])) {
        tag_default <- col
        break
      }
    }
    if (is.null(tag_default)) tag_default <- col_names[1]
    
    updateSelectInput(
      session,
      "tag_column",
      choices = col_names,
      selected = tag_default
    )
  })
  
  # Get cleaned data with valid coordinates only
  cleaned_data <- reactive({
    data <- mushroom_data()
    lon_col <- input$longitude_col
    lat_col <- input$latitude_col
    
    req(data, lon_col, lat_col)
    
    # Store original values for reporting
    original_lon <- data[[lon_col]]
    original_lat <- data[[lat_col]]
    original_rows <- nrow(data)
    
    # Try to convert to numeric (handles strings, factors, etc.)
    # Use as.character first to handle factors properly
    # Replace commas with periods to handle European decimal format (e.g., "44,8967" -> "44.8967")
    lon_as_char <- as.character(data[[lon_col]])
    lat_as_char <- as.character(data[[lat_col]])
    
    # Replace comma decimal separators with periods
    lon_as_char <- gsub(",", ".", lon_as_char)
    lat_as_char <- gsub(",", ".", lat_as_char)
    
    # Now convert to numeric
    data[[lon_col]] <- suppressWarnings(as.numeric(lon_as_char))
    data[[lat_col]] <- suppressWarnings(as.numeric(lat_as_char))
    
    # Count successful string-to-number conversions
    converted_from_string <- 0
    had_comma_decimals <- FALSE
    
    if (is.character(original_lon) || is.factor(original_lon) || 
        is.character(original_lat) || is.factor(original_lat)) {
      
      # Check if we had comma decimals
      if (any(grepl(",", as.character(original_lon))) || any(grepl(",", as.character(original_lat)))) {
        had_comma_decimals <- TRUE
      }
      
      successfully_converted <- !is.na(data[[lon_col]]) & !is.na(data[[lat_col]])
      converted_from_string <- sum(successfully_converted, na.rm = TRUE)
      
      if (converted_from_string > 0) {
        msg <- paste("✅", converted_from_string, "coordinate pair(s) successfully converted from text to numbers")
        if (had_comma_decimals) {
          msg <- paste(msg, "(comma decimals → period decimals)")
        }
        showNotification(
          msg,
          type = "message",
          duration = 3
        )
      }
    }
    
    # Filter out rows with NA or invalid coordinates
    valid_rows <- !is.na(data[[lon_col]]) & 
      !is.na(data[[lat_col]]) &
      data[[lon_col]] >= -180 & data[[lon_col]] <= 180 &
      data[[lat_col]] >= -90 & data[[lat_col]] <= 90
    
    filtered_data <- data[valid_rows, ]
    
    # Report on removed rows
    rows_removed <- original_rows - nrow(filtered_data)
    if (rows_removed > 0) {
      # Count different types of invalid data
      na_coords <- sum(is.na(data[[lon_col]]) | is.na(data[[lat_col]]))
      out_of_range <- rows_removed - na_coords
      
      msg_parts <- c()
      msg_parts <- c(msg_parts, paste("⚠️", rows_removed, "row(s) removed:"))
      
      if (na_coords > 0) {
        msg_parts <- c(msg_parts, paste("  •", na_coords, "with non-numeric/invalid coordinates"))
      }
      if (out_of_range > 0) {
        msg_parts <- c(msg_parts, paste("  •", out_of_range, "with coordinates out of valid range"))
      }
      
      showNotification(
        paste(msg_parts, collapse = "\n"),
        type = "warning",
        duration = 4
      )
    }
    
    return(filtered_data)
  })
  
  # Validate coordinate columns
  validate_coords <- reactive({
    data <- cleaned_data()
    
    validate(
      need(!is.null(data), "Please upload a data file to begin"),
      need(nrow(data) > 0, "No valid data rows after filtering NA/invalid values"),
      need(input$longitude_col != "", "Please select a longitude column"),
      need(input$latitude_col != "", "Please select a latitude column"),
      need(input$tag_column != "", "Please select a tag column")
    )
    
    lon_col <- input$longitude_col
    lat_col <- input$latitude_col
    
    # Check if columns exist
    validate(
      need(lon_col %in% names(data), paste("Column", lon_col, "not found in data")),
      need(lat_col %in% names(data), paste("Column", lat_col, "not found in data"))
    )
    
    TRUE
  })
  
  # Create color palette based on selected tag
  color_pal <- reactive({
    data <- cleaned_data()
    tag_col <- input$tag_column
    
    req(data, tag_col)
    
    tryCatch({
      # Convert to character to handle any data type, remove NAs
      tag_values <- as.character(data[[tag_col]])
      tag_values <- tag_values[!is.na(tag_values)]
      unique_values <- unique(tag_values)
      
      if (length(unique_values) == 0) {
        return(colorFactor(palette = "Set1", domain = c("default")))
      }
      
      # Limit number of unique values for color palette
      if (length(unique_values) > 50) {
        showNotification(
          paste("Warning: Tag column has", length(unique_values), "unique values. Consider using a different column."),
          type = "warning",
          duration = 5
        )
      }
      
      # Generate color palette
      if (length(unique_values) <= 10) {
        colors <- rainbow(length(unique_values))
      } else if (length(unique_values) <= 20) {
        colors <- colorRampPalette(c("red", "blue", "green", "orange", "purple", "brown"))(length(unique_values))
      } else {
        colors <- colorRampPalette(c("red", "yellow", "green", "cyan", "blue", "magenta"))(length(unique_values))
      }
      
      colorFactor(palette = colors, domain = unique_values)
    }, error = function(e) {
      showNotification(
        paste("Error creating color palette:", e$message),
        type = "error",
        duration = 5
      )
      return(colorFactor(palette = "Set1", domain = c("default")))
    })
  })
  
  # Render the map
  output$map <- renderLeaflet({
    validate_coords()
    
    data <- cleaned_data()
    lon_col <- input$longitude_col
    lat_col <- input$latitude_col
    tag_col <- input$tag_column
    
    req(data, lon_col, lat_col, tag_col)
    
    tryCatch({
      pal <- color_pal()
      
      # Convert coordinates to numeric
      lon_values <- as.numeric(data[[lon_col]])
      lat_values <- as.numeric(data[[lat_col]])
      
      # Get tag values as character
      tag_values <- as.character(data[[tag_col]])
      
      # Create popup text
      popup_text <- sapply(1:nrow(data), function(i) {
        fields <- sapply(names(data), function(col) {
          paste0("<b>", col, ":</b> ", as.character(data[i, col]))
        })
        paste(c(paste0("<b>Row: ", i, "</b>"), fields), collapse = "<br/>")
      })
      
      leaflet(data) %>%
        addTiles() %>%
        setView(
          lng = mean(lon_values, na.rm = TRUE),
          lat = mean(lat_values, na.rm = TRUE),
          zoom = 10
        ) %>%
        addCircleMarkers(
          lng = lon_values,
          lat = lat_values,
          layerId = seq_len(nrow(data)),
          color = ~pal(tag_values),
          fillOpacity = 0.7,
          radius = 8,
          stroke = TRUE,
          weight = 2,
          popup = popup_text
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = tag_values,
          title = paste("Tag:", tag_col),
          opacity = 1
        ) %>%
        addDrawToolbar(
          targetGroup = "draw",
          editOptions = editToolbarOptions(
            selectedPathOptions = selectedPathOptions()
          ),
          polylineOptions = FALSE,
          circleOptions = FALSE,
          rectangleOptions = drawRectangleOptions(
            shapeOptions = drawShapeOptions(fillOpacity = 0.2, weight = 2)
          ),
          polygonOptions = drawPolygonOptions(
            shapeOptions = drawShapeOptions(fillOpacity = 0.2, weight = 2)
          ),
          markerOptions = FALSE,
          circleMarkerOptions = FALSE
        )
    }, error = function(e) {
      showNotification(
        paste("Error rendering map:", e$message),
        type = "error",
        duration = NULL
      )
      leaflet() %>% addTiles() %>% setView(lng = 0, lat = 0, zoom = 2)
    })
  })
  
  # Handle drawn shapes (lasso selection)
  observeEvent(input$map_draw_new_feature, {
    tryCatch({
      feature <- input$map_draw_new_feature
      data <- cleaned_data()
      lon_col <- input$longitude_col
      lat_col <- input$latitude_col
      
      req(data, lon_col, lat_col)
      
      # Extract polygon coordinates
      if (feature$geometry$type == "Polygon") {
        polygon_coords <- feature$geometry$coordinates[[1]]
      } else if (feature$geometry$type == "Rectangle") {
        polygon_coords <- feature$geometry$coordinates[[1]]
      } else {
        showNotification(
          "Please use polygon or rectangle tool for selection",
          type = "warning",
          duration = 3
        )
        return()
      }
      
      # Convert to matrix
      poly_mat <- do.call(rbind, lapply(polygon_coords, function(x) c(x[[1]], x[[2]])))
      
      # Convert coordinates to numeric
      lon_values <- as.numeric(data[[lon_col]])
      lat_values <- as.numeric(data[[lat_col]])
      
      # Check which points are inside the polygon
      points_in_poly <- sp::point.in.polygon(
        point.x = lon_values,
        point.y = lat_values,
        pol.x = poly_mat[, 1],
        pol.y = poly_mat[, 2]
      )
      
      # Get selected points
      selected <- data[points_in_poly > 0, ]
      
      if (nrow(selected) == 0) {
        showNotification(
          "No points selected in this area",
          type = "warning",
          duration = 3
        )
      } else {
        showNotification(
          paste("🎯", nrow(selected), "point(s) selected"),
          type = "message",
          duration = 2
        )
      }
      
      selected_points(selected)
    }, error = function(e) {
      showNotification(
        paste("Error during selection:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Reset selection
  observeEvent(input$reset_selection, {
    selected_points(NULL)
    
    # Clear drawings from map
    leafletProxy("map") %>%
      clearGroup("draw") %>%
      clearShapes()
    
    showNotification(
      "✅ Selection reset",
      type = "message",
      duration = 2
    )
  })
  
  # Render bar plot
  output$barplot <- renderPlotly({
    selected <- selected_points()
    tag_col <- input$tag_column
    
    req(tag_col)
    
    tryCatch({
      if (is.null(selected) || nrow(selected) == 0) {
        # Show empty plot with message
        plot_ly() %>%
          layout(
            title = list(
              text = "No points selected. Use the lasso tool on the map.",
              font = list(size = 16, color = "#e0e0e0")
            ),
            xaxis = list(title = "", color = "#e0e0e0"),
            yaxis = list(title = "Count", color = "#e0e0e0"),
            paper_bgcolor = "#1a1a1a",
            plot_bgcolor = "#1a1a1a",
            font = list(color = "#e0e0e0")
          )
      } else {
        # Convert tag to character and remove NAs
        selected$tag_temp <- as.character(selected[[tag_col]])
        selected <- selected[!is.na(selected$tag_temp), ]
        
        if (nrow(selected) == 0) {
          plot_ly() %>%
            layout(
              title = list(
                text = "All selected points have NA in tag column",
                font = list(size = 16, color = "#e0e0e0")
              ),
              paper_bgcolor = "#1a1a1a",
              plot_bgcolor = "#1a1a1a",
              font = list(color = "#e0e0e0")
            )
        } else {
          # Count by tag
          count_data <- selected %>%
            group_by(tag_temp) %>%
            summarise(count = n(), .groups = "drop") %>%
            rename(tag = tag_temp) %>%
            arrange(desc(count))
          
          # Limit display if too many categories
          if (nrow(count_data) > 30) {
            count_data <- count_data[1:30, ]
            title_text <- paste("Top 30 counts by", tag_col, "—", nrow(selected), "points selected")
          } else {
            title_text <- paste("Count by", tag_col, "—", nrow(selected), "points selected")
          }
          
          # Create bar plot
          plot_ly(
            data = count_data,
            x = ~tag,
            y = ~count,
            type = "bar",
            marker = list(
              color = color_pal()(count_data$tag),
              line = list(color = "#404040", width = 1)
            ),
            hovertemplate = paste(
              "<b>%{x}</b><br>",
              "Count: %{y}<br>",
              "<extra></extra>"
            )
          ) %>%
            layout(
              title = list(
                text = title_text,
                font = list(size = 18, color = "#e0e0e0")
              ),
              xaxis = list(
                title = tag_col,
                tickangle = -45,
                color = "#e0e0e0",
                gridcolor = "#404040"
              ),
              yaxis = list(
                title = "Count",
                color = "#e0e0e0",
                gridcolor = "#404040"
              ),
              showlegend = FALSE,
              plot_bgcolor = "#1a1a1a",
              paper_bgcolor = "#1a1a1a",
              font = list(color = "#e0e0e0")
            )
        }
      }
    }, error = function(e) {
      showNotification(
        paste("Error creating bar plot:", e$message),
        type = "error",
        duration = 5
      )
      plot_ly() %>%
        layout(
          title = "Error creating plot",
          paper_bgcolor = "#1a1a1a",
          plot_bgcolor = "#1a1a1a"
        )
    })
  })
  
  # Render data table
  output$data_table <- renderDT({
    data <- mushroom_data()
    
    validate(
      need(!is.null(data), "Please upload a data file to view table")
    )
    
    tryCatch({
      datatable(
        data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        filter = "top",
        class = 'cell-border stripe'
      )
    }, error = function(e) {
      showNotification(
        paste("Error rendering table:", e$message),
        type = "error",
        duration = 5
      )
      datatable(data.frame(Error = "Could not render table"))
    })
  })
}