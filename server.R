library(shiny)
library(leaflet)
library(leaflet.extras)
library(DT)
library(dplyr)
library(plotly)
library(sp)
library(shinycssloaders)
library(readxl)
library(jsonlite)

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
  
  # Reactive value to store annotations
  annotations <- reactiveVal(list())
  
  # Reactive value to store temporary drawn feature for naming
  temp_annotation <- reactiveVal(NULL)
  
  # Clear drawn features when annotation mode is toggled off
  observeEvent(input$annotation_mode, {
    if (!input$annotation_mode) {
      # Clear any drawn features when exiting annotation mode
      leafletProxy("map") %>%
        clearGroup("draw")
    }
  })
  
  # Helper function to convert hex color to marker color name
  hex_to_marker_color <- function(hex) {
    color_map <- list(
      "#e74c3c" = "red",
      "#3498db" = "blue",
      "#2ecc71" = "green",
      "#f39c12" = "orange",
      "#9b59b6" = "purple",
      "#1abc9c" = "cadetblue",
      "#e67e22" = "orange"
    )
    
    if (hex %in% names(color_map)) {
      return(color_map[[hex]])
    }
    return("blue")  # default color
  }
  
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
        showNotification(msg, type = "message", duration = 3)
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
      
      showNotification(paste(msg_parts, collapse = "\n"), type = "warning", duration = 4)
    }
    
    return(filtered_data)
  })
  
  # Validate coordinate columns
  validate_coords <- reactive({
    data <- cleaned_data()
    
    if (is.null(data)) {
      validate(need(FALSE, "Please upload a data file to begin"))
    }
    
    if (nrow(data) == 0) {
      validate(need(FALSE, "No valid data rows after filtering NA/invalid values"))
    }
    
    if (input$longitude_col == "") {
      validate(need(FALSE, "Please select a longitude column"))
    }
    
    if (input$latitude_col == "") {
      validate(need(FALSE, "Please select a latitude column"))
    }
    
    if (input$tag_column == "") {
      validate(need(FALSE, "Please select a tag column"))
    }
    
    lon_col <- input$longitude_col
    lat_col <- input$latitude_col
    
    if (!(lon_col %in% names(data))) {
      validate(need(FALSE, paste("Column", lon_col, "not found in data")))
    }
    
    if (!(lat_col %in% names(data))) {
      validate(need(FALSE, paste("Column", lat_col, "not found in data")))
    }
    
    TRUE
  })
  
  # Create color palette based on selected tag
  color_pal <- reactive({
    data <- cleaned_data()
    tag_col <- input$tag_column
    
    req(data, tag_col)
    
    tryCatch({
      tag_values <- as.character(data[[tag_col]])
      tag_values <- tag_values[!is.na(tag_values)]
      unique_values <- unique(tag_values)
      
      if (length(unique_values) == 0) {
        return(colorFactor(palette = "Set1", domain = c("default")))
      }
      
      if (length(unique_values) > 50) {
        showNotification(
          paste("Warning: Tag column has", length(unique_values), "unique values. Consider using a different column."),
          type = "warning",
          duration = 5
        )
      }
      
      if (length(unique_values) <= 10) {
        colors <- rainbow(length(unique_values))
      } else if (length(unique_values) <= 20) {
        colors <- colorRampPalette(c("red", "blue", "green", "orange", "purple", "brown"))(length(unique_values))
      } else {
        colors <- colorRampPalette(c("red", "yellow", "green", "cyan", "blue", "magenta"))(length(unique_values))
      }
      
      colorFactor(palette = colors, domain = unique_values)
    }, error = function(e) {
      showNotification(paste("Error creating color palette:", e$message), type = "error", duration = 5)
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
      
      lon_values <- as.numeric(data[[lon_col]])
      lat_values <- as.numeric(data[[lat_col]])
      tag_values <- as.character(data[[tag_col]])
      
      popup_text <- sapply(1:nrow(data), function(i) {
        fields <- sapply(names(data), function(col) {
          paste0("<b>", col, ":</b> ", as.character(data[i, col]))
        })
        paste(c(paste0("<b>Row: ", i, "</b>"), fields), collapse = "<br/>")
      })
      
      map <- leaflet(data) %>%
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
          color = "#000000",  # Black border
          fillColor = ~pal(tag_values),
          fillOpacity = 0.8,
          radius = 5,  # Smaller radius
          stroke = TRUE,
          weight = 1,  # Thin black border
          popup = popup_text,
          group = "data_points"
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
          markerOptions = drawMarkerOptions(),  # Enable markers
          circleMarkerOptions = FALSE
        )
      
      # Add saved annotations to map
      annots <- annotations()
      if (length(annots) > 0) {
        for (i in seq_along(annots)) {
          annot <- annots[[i]]
          
          # Handle different annotation types
          if (annot$type %in% c("Polygon", "Rectangle")) {
            coords_matrix <- do.call(rbind, lapply(annot$coordinates, function(x) c(x[[1]], x[[2]])))
            
            map <- map %>%
              addPolygons(
                lng = coords_matrix[, 1],
                lat = coords_matrix[, 2],
                fillColor = "transparent",
                fillOpacity = 0,
                color = "#000000",  # Black border
                weight = 2,
                group = "annotations"
              ) %>%
              addLabelOnlyMarkers(
                lng = mean(coords_matrix[, 1]),
                lat = max(coords_matrix[, 2]) + 0.001,  # Position label above
                label = annot$name,
                labelOptions = labelOptions(
                  noHide = TRUE,
                  direction = "top",
                  textOnly = FALSE,
                  style = list(
                    "color" = "#ffffff",
                    "font-weight" = "bold",
                    "font-size" = "12px",
                    "text-shadow" = "2px 2px 4px rgba(0,0,0,0.8)",
                    "background" = "rgba(0,0,0,0.8)",
                    "padding" = "3px 8px",
                    "border-radius" = "4px",
                    "border" = "1px solid #ffffff"
                  )
                ),
                group = "annotations"
              )
          } else if (annot$type == "Marker") {
            # Handle marker annotations
            map <- map %>%
              addAwesomeMarkers(
                lng = annot$coordinates[[1]],
                lat = annot$coordinates[[2]],
                icon = awesomeIcons(
                  icon = "map-pin",
                  iconColor = "#ffffff",
                  library = "fa",
                  markerColor = "black"  # Black marker
                ),
                label = annot$name,
                labelOptions = labelOptions(
                  noHide = TRUE,
                  direction = "right",  # Position to the right of marker
                  textOnly = FALSE,
                  style = list(
                    "color" = "#ffffff",
                    "font-weight" = "normal",
                    "font-size" = "10px",  # Smaller text
                    "text-shadow" = "1px 1px 2px rgba(0,0,0,0.8)",
                    "background" = "rgba(0,0,0,0.7)",
                    "padding" = "2px 5px",
                    "border-radius" = "3px",
                    "border" = "1px solid #ffffff"
                  )
                ),
                group = "annotations"
              )
          }
        }
      }
      
      map
    }, error = function(e) {
      showNotification(paste("Error rendering map:", e$message), type = "error", duration = NULL)
      leaflet() %>% addTiles() %>% setView(lng = 0, lat = 0, zoom = 2)
    })
  })
  
  # Handle drawn shapes for lasso selection
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    
    # Check if annotation mode is enabled
    if (input$annotation_mode) {
      # Store the feature temporarily and show naming modal
      temp_annotation(feature)
      showModal(modalDialog(
        title = "Name this annotation",
        textInput("area_name", "Annotation Name:", placeholder = "e.g., Collection Point A, Study Zone 1"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_annotation", "Save", class = "btn-primary")
        ),
        easyClose = FALSE
      ))
    } else {
      # Normal lasso selection mode - only for polygons/rectangles
      if (feature$geometry$type == "Point") {
        showNotification(
          "Markers are only available in Annotation Mode. Enable it in the sidebar.",
          type = "warning",
          duration = 4
        )
        return()
      }
      
      tryCatch({
        data <- cleaned_data()
        lon_col <- input$longitude_col
        lat_col <- input$latitude_col
        
        req(data, lon_col, lat_col)
        
        if (feature$geometry$type == "Polygon") {
          polygon_coords <- feature$geometry$coordinates[[1]]
        } else if (feature$geometry$type == "Rectangle") {
          polygon_coords <- feature$geometry$coordinates[[1]]
        } else {
          showNotification("Please use polygon or rectangle tool for selection", type = "warning", duration = 3)
          return()
        }
        
        poly_mat <- do.call(rbind, lapply(polygon_coords, function(x) c(x[[1]], x[[2]])))
        
        lon_values <- as.numeric(data[[lon_col]])
        lat_values <- as.numeric(data[[lat_col]])
        
        points_in_poly <- sp::point.in.polygon(
          point.x = lon_values,
          point.y = lat_values,
          pol.x = poly_mat[, 1],
          pol.y = poly_mat[, 2]
        )
        
        selected <- data[points_in_poly > 0, ]
        
        if (nrow(selected) == 0) {
          showNotification("No points selected in this area", type = "warning", duration = 3)
        } else {
          showNotification(paste("🎯", nrow(selected), "point(s) selected"), type = "message", duration = 2)
        }
        
        selected_points(selected)
      }, error = function(e) {
        showNotification(paste("Error during selection:", e$message), type = "error", duration = 5)
      })
    }
  })
  
  # Save annotation with name
  observeEvent(input$save_annotation, {
    area_name <- trimws(input$area_name)
    feature <- temp_annotation()
    
    if (area_name == "") {
      showNotification("Please enter a name", type = "warning", duration = 3)
      return()
    }
    
    if (!is.null(feature)) {
      # Extract coordinates based on type
      if (feature$geometry$type == "Polygon") {
        coords <- feature$geometry$coordinates[[1]]
        geom_type <- "Polygon"
      } else if (feature$geometry$type == "Rectangle") {
        coords <- feature$geometry$coordinates[[1]]
        geom_type <- "Rectangle"
      } else if (feature$geometry$type == "Point") {
        coords <- feature$geometry$coordinates
        geom_type <- "Marker"
      } else {
        showNotification("Invalid shape type", type = "error", duration = 3)
        removeModal()
        return()
      }
      
      # Generate a random color for this annotation
      colors <- c("#e74c3c", "#3498db", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c", "#e67e22")
      new_color <- sample(colors, 1)
      
      # Add to annotations list
      current_annots <- annotations()
      new_annot <- list(
        id = length(current_annots) + 1,
        name = area_name,
        coordinates = coords,
        color = new_color,
        type = geom_type
      )
      
      annotations(c(current_annots, list(new_annot)))
      
      # Update map with new annotation
      if (geom_type %in% c("Polygon", "Rectangle")) {
        coords_matrix <- do.call(rbind, lapply(coords, function(x) c(x[[1]], x[[2]])))
        
        leafletProxy("map") %>%
          addPolygons(
            lng = coords_matrix[, 1],
            lat = coords_matrix[, 2],
            fillColor = "transparent",
            fillOpacity = 0,
            color = "#000000",  # Black border
            weight = 2,
            group = "annotations"
          ) %>%
          addLabelOnlyMarkers(
            lng = mean(coords_matrix[, 1]),
            lat = max(coords_matrix[, 2]) + 0.001,  # Position label above
            label = area_name,
            labelOptions = labelOptions(
              noHide = TRUE,
              direction = "top",
              textOnly = FALSE,
              style = list(
                "color" = "#ffffff",
                "font-weight" = "bold",
                "font-size" = "12px",
                "text-shadow" = "2px 2px 4px rgba(0,0,0,0.8)",
                "background" = "rgba(0,0,0,0.8)",
                "padding" = "3px 8px",
                "border-radius" = "4px",
                "border" = "1px solid #ffffff"
              )
            ),
            group = "annotations"
          )
      } else if (geom_type == "Marker") {
        leafletProxy("map") %>%
          addAwesomeMarkers(
            lng = coords[[1]],
            lat = coords[[2]],
            icon = awesomeIcons(
              icon = "map-pin",
              iconColor = "#ffffff",
              library = "fa",
              markerColor = "black"  # Black marker
            ),
            label = area_name,
            labelOptions = labelOptions(
              noHide = TRUE,
              direction = "right",  # Position to the right of marker
              textOnly = FALSE,
              style = list(
                "color" = "#ffffff",
                "font-weight" = "normal",
                "font-size" = "10px",  # Smaller text
                "text-shadow" = "1px 1px 2px rgba(0,0,0,0.8)",
                "background" = "rgba(0,0,0,0.7)",
                "padding" = "2px 5px",
                "border-radius" = "3px",
                "border" = "1px solid #ffffff"
              )
            ),
            group = "annotations"
          )
      }
      
      showNotification(paste("✅", area_name, "saved"), type = "message", duration = 3)
      temp_annotation(NULL)
      removeModal()
    }
  })
  
  # Undo last annotation
  observeEvent(input$undo_annotation, {
    current_annots <- annotations()
    
    if (length(current_annots) == 0) {
      showNotification("No annotations to undo", type = "warning", duration = 2)
      return()
    }
    
    # Remove the last annotation
    removed_annot <- current_annots[[length(current_annots)]]
    new_annots <- current_annots[-length(current_annots)]
    annotations(new_annots)
    
    # Clear and redraw all remaining annotations
    leafletProxy("map") %>%
      clearGroup("annotations")
    
    # Redraw remaining annotations
    if (length(new_annots) > 0) {
      for (annot in new_annots) {
        if (annot$type %in% c("Polygon", "Rectangle")) {
          coords_matrix <- do.call(rbind, lapply(annot$coordinates, function(x) c(x[[1]], x[[2]])))
          
          leafletProxy("map") %>%
            addPolygons(
              lng = coords_matrix[, 1],
              lat = coords_matrix[, 2],
              fillColor = "transparent",
              fillOpacity = 0,
              color = "#000000",  # Black border
              weight = 2,
              group = "annotations"
            ) %>%
            addLabelOnlyMarkers(
              lng = mean(coords_matrix[, 1]),
              lat = max(coords_matrix[, 2]) + 0.001,  # Position label above
              label = annot$name,
              labelOptions = labelOptions(
                noHide = TRUE,
                direction = "top",
                textOnly = FALSE,
                style = list(
                  "color" = "#ffffff",
                  "font-weight" = "bold",
                  "font-size" = "12px",
                  "text-shadow" = "2px 2px 4px rgba(0,0,0,0.8)",
                  "background" = "rgba(0,0,0,0.8)",
                  "padding" = "3px 8px",
                  "border-radius" = "4px",
                  "border" = "1px solid #ffffff"
                )
              ),
              group = "annotations"
            )
        } else if (annot$type == "Marker") {
          leafletProxy("map") %>%
            addAwesomeMarkers(
              lng = annot$coordinates[[1]],
              lat = annot$coordinates[[2]],
              icon = awesomeIcons(
                icon = "map-pin",
                iconColor = "#ffffff",
                library = "fa",
                markerColor = "black"  # Black marker
              ),
              label = annot$name,
              labelOptions = labelOptions(
                noHide = TRUE,
                direction = "right",  # Position to the right of marker
                textOnly = FALSE,
                style = list(
                  "color" = "#ffffff",
                  "font-weight" = "normal",
                  "font-size" = "10px",  # Smaller text
                  "text-shadow" = "1px 1px 2px rgba(0,0,0,0.8)",
                  "background" = "rgba(0,0,0,0.7)",
                  "padding" = "2px 5px",
                  "border-radius" = "3px",
                  "border" = "1px solid #ffffff"
                )
              ),
              group = "annotations"
            )
        }
      }
    }
    
    showNotification(paste("↩️ Removed:", removed_annot$name), type = "message", duration = 2)
  })
  
  # Save annotations to file
  output$download_annotations <- downloadHandler(
    filename = function() {
      paste0("mycolasso_annotations_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    },
    content = function(file) {
      annots <- annotations()
      if (length(annots) == 0) {
        showNotification("No annotations to save", type = "warning", duration = 3)
        return()
      }
      write_json(annots, file, pretty = TRUE, auto_unbox = TRUE)
      showNotification("✅ Annotations saved successfully", type = "message", duration = 3)
    }
  )
  
  # Load annotations from file
  observeEvent(input$load_annotations_file, {
    req(input$load_annotations_file)
    
    tryCatch({
      loaded_annots <- read_json(input$load_annotations_file$datapath, simplifyVector = FALSE)
      
      if (length(loaded_annots) == 0) {
        showNotification("No annotations found in file", type = "warning", duration = 3)
        return()
      }
      
      annotations(loaded_annots)
      
      # Redraw map with loaded annotations
      leafletProxy("map") %>%
        clearGroup("annotations")
      
      for (annot in loaded_annots) {
        if (annot$type %in% c("Polygon", "Rectangle")) {
          coords_matrix <- do.call(rbind, lapply(annot$coordinates, function(x) c(x[[1]], x[[2]])))
          
          leafletProxy("map") %>%
            addPolygons(
              lng = coords_matrix[, 1],
              lat = coords_matrix[, 2],
              fillColor = "transparent",
              fillOpacity = 0,
              color = "#000000",  # Black border
              weight = 2,
              group = "annotations"
            ) %>%
            addLabelOnlyMarkers(
              lng = mean(coords_matrix[, 1]),
              lat = max(coords_matrix[, 2]) + 0.001,  # Position label above
              label = annot$name,
              labelOptions = labelOptions(
                noHide = TRUE,
                direction = "top",
                textOnly = FALSE,
                style = list(
                  "color" = "#ffffff",
                  "font-weight" = "bold",
                  "font-size" = "12px",
                  "text-shadow" = "2px 2px 4px rgba(0,0,0,0.8)",
                  "background" = "rgba(0,0,0,0.8)",
                  "padding" = "3px 8px",
                  "border-radius" = "4px",
                  "border" = "1px solid #ffffff"
                )
              ),
              group = "annotations"
            )
        } else if (annot$type == "Marker") {
          leafletProxy("map") %>%
            addAwesomeMarkers(
              lng = annot$coordinates[[1]],
              lat = annot$coordinates[[2]],
              icon = awesomeIcons(
                icon = "map-pin",
                iconColor = "#ffffff",
                library = "fa",
                markerColor = "black"  # Black marker
              ),
              label = annot$name,
              labelOptions = labelOptions(
                noHide = TRUE,
                direction = "right",  # Position to the right of marker
                textOnly = FALSE,
                style = list(
                  "color" = "#ffffff",
                  "font-weight" = "normal",
                  "font-size" = "10px",  # Smaller text
                  "text-shadow" = "1px 1px 2px rgba(0,0,0,0.8)",
                  "background" = "rgba(0,0,0,0.7)",
                  "padding" = "2px 5px",
                  "border-radius" = "3px",
                  "border" = "1px solid #ffffff"
                )
              ),
              group = "annotations"
            )
        }
      }
      
      showNotification(
        paste("✅", length(loaded_annots), "annotation(s) loaded successfully"),
        type = "message",
        duration = 3
      )
    }, error = function(e) {
      showNotification(
        paste("Error loading annotations:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Load example_annotations.json by default on startup
  observe({
    if (file.exists("example_annotations.json")) {
      tryCatch({
        loaded_annots <- read_json("example_annotations.json", simplifyVector = FALSE)
        
        if (length(loaded_annots) > 0) {
          annotations(loaded_annots)
          showNotification(
            paste("📁", length(loaded_annots), "example annotation(s) loaded"),
            type = "message",
            duration = 3
          )
        }
      }, error = function(e) {
        # Silently fail if example annotations can't be loaded
      })
    }
  })
  
  # Helper functions for statistics
  calc_distance <- function(lon1, lat1, lon2, lat2) {
    R <- 6371000
    lon1_rad <- lon1 * pi / 180
    lat1_rad <- lat1 * pi / 180
    lon2_rad <- lon2 * pi / 180
    lat2_rad <- lat2 * pi / 180
    dlon <- lon2_rad - lon1_rad
    dlat <- lat2_rad - lat1_rad
    a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1-a))
    distance <- R * c
    return(distance)
  }
  
  calc_max_distance <- function(data, lon_col, lat_col) {
    if (nrow(data) < 2) return(0)
    lon_values <- as.numeric(data[[lon_col]])
    lat_values <- as.numeric(data[[lat_col]])
    max_dist <- 0
    n <- nrow(data)
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        dist <- calc_distance(lon_values[i], lat_values[i], lon_values[j], lat_values[j])
        if (dist > max_dist) max_dist <- dist
      }
    }
    return(max_dist)
  }
  
  calc_area <- function(data, lon_col, lat_col) {
    if (nrow(data) < 3) return(0)
    lon_values <- as.numeric(data[[lon_col]])
    lat_values <- as.numeric(data[[lat_col]])
    tryCatch({
      coords <- cbind(lon_values, lat_values)
      hull_indices <- chull(coords)
      hull_coords <- coords[hull_indices, ]
      n <- nrow(hull_coords)
      area_deg <- 0
      for (i in 1:n) {
        j <- ifelse(i == n, 1, i + 1)
        area_deg <- area_deg + (hull_coords[i, 1] * hull_coords[j, 2])
        area_deg <- area_deg - (hull_coords[j, 1] * hull_coords[i, 2])
      }
      area_deg <- abs(area_deg) / 2
      mean_lat <- mean(lat_values, na.rm = TRUE)
      meters_per_degree_lat <- 111320
      meters_per_degree_lon <- 111320 * cos(mean_lat * pi / 180)
      area_m2 <- area_deg * meters_per_degree_lat * meters_per_degree_lon
      return(area_m2)
    }, error = function(e) {
      return(0)
    })
  }
  
  # Render summary stats table
  output$summary_stats_table <- renderDT({
    data <- cleaned_data()
    selected <- selected_points()
    lon_col <- input$longitude_col
    lat_col <- input$latitude_col
    
    if (is.null(data)) {
      return(NULL)
    }
    
    req(lon_col, lat_col)
    
    tryCatch({
      total_count_global <- nrow(data)
      max_dist_global <- calc_max_distance(data, lon_col, lat_col)
      area_global <- calc_area(data, lon_col, lat_col)
      
      if (!is.null(selected) && nrow(selected) > 0) {
        total_count_selected <- nrow(selected)
        max_dist_selected <- calc_max_distance(selected, lon_col, lat_col)
        area_selected <- calc_area(selected, lon_col, lat_col)
      } else {
        total_count_selected <- 0
        max_dist_selected <- 0
        area_selected <- 0
      }
      
      summary_data <- data.frame(
        Metric = c("Total Points", "Max Distance (m)", "Area (km²)"),
        Global_Dataset = c(
          format(total_count_global, big.mark = ","),
          format(round(max_dist_global, 0), big.mark = ","),
          format(round(area_global / 1000000, 6), big.mark = ",")
        ),
        Selected_Area = c(
          format(total_count_selected, big.mark = ","),
          format(round(max_dist_selected, 0), big.mark = ","),
          format(round(area_selected / 1000000, 6), big.mark = ",")
        ),
        stringsAsFactors = FALSE
      )
      
      datatable(
        summary_data,
        options = list(pageLength = 3, scrollX = FALSE, searching = FALSE, ordering = FALSE, paging = FALSE, info = FALSE, dom = 't'),
        rownames = FALSE,
        colnames = c("Metric", "Global Dataset", "Selected Area"),
        class = 'cell-border stripe'
      )
    }, error = function(e) {
      showNotification(paste("Error calculating summary statistics:", e$message), type = "error", duration = 5)
      datatable(data.frame(Error = "Could not calculate summary statistics"))
    })
  })
  
  # Render tag counts table
  output$tag_counts_table <- renderDT({
    data <- cleaned_data()
    selected <- selected_points()
    tag_col <- input$tag_column
    
    if (is.null(data)) {
      return(NULL)
    }
    
    req(tag_col)
    
    tryCatch({
      tag_values_global <- as.character(data[[tag_col]])
      tag_counts_global <- table(tag_values_global, useNA = "no")
      
      if (!is.null(selected) && nrow(selected) > 0) {
        tag_values_selected <- as.character(selected[[tag_col]])
        tag_counts_selected <- table(tag_values_selected, useNA = "no")
      } else {
        tag_counts_selected <- table(character(0))
      }
      
      all_tags <- unique(c(names(tag_counts_global), names(tag_counts_selected)))
      all_tags <- sort(all_tags)
      
      tag_data <- data.frame(Tag = character(), Global_Dataset = character(), Selected_Area = character(), stringsAsFactors = FALSE)
      
      for (tag in all_tags) {
        global_count <- ifelse(tag %in% names(tag_counts_global), tag_counts_global[tag], 0)
        selected_count <- ifelse(tag %in% names(tag_counts_selected), tag_counts_selected[tag], 0)
        tag_data <- rbind(tag_data, data.frame(Tag = tag, Global_Dataset = format(global_count, big.mark = ","), Selected_Area = format(selected_count, big.mark = ",")))
      }
      
      datatable(
        tag_data,
        options = list(pageLength = 25, scrollX = FALSE, searching = TRUE, ordering = TRUE, paging = TRUE, info = TRUE, dom = 'frtip'),
        rownames = FALSE,
        colnames = c(paste("Tag:", tag_col), "Global Dataset", "Selected Area"),
        class = 'cell-border stripe'
      )
    }, error = function(e) {
      showNotification(paste("Error calculating tag counts:", e$message), type = "error", duration = 5)
      datatable(data.frame(Error = "Could not calculate tag counts"))
    })
  })
  
  # Render bar plot
  output$barplot <- renderPlotly({
    selected <- selected_points()
    tag_col <- input$tag_column
    req(tag_col)
    
    tryCatch({
      if (is.null(selected) || nrow(selected) == 0) {
        plot_ly() %>%
          layout(
            title = list(text = "No points selected. Use the lasso tool on the map.", font = list(size = 16, color = "#e0e0e0")),
            xaxis = list(title = "", color = "#e0e0e0"),
            yaxis = list(title = "Count", color = "#e0e0e0"),
            paper_bgcolor = "#1a1a1a",
            plot_bgcolor = "#1a1a1a",
            font = list(color = "#e0e0e0")
          )
      } else {
        selected$tag_temp <- as.character(selected[[tag_col]])
        selected <- selected[!is.na(selected$tag_temp), ]
        
        if (nrow(selected) == 0) {
          plot_ly() %>%
            layout(
              title = list(text = "All selected points have NA in tag column", font = list(size = 16, color = "#e0e0e0")),
              paper_bgcolor = "#1a1a1a",
              plot_bgcolor = "#1a1a1a",
              font = list(color = "#e0e0e0")
            )
        } else {
          count_data <- selected %>%
            group_by(tag_temp) %>%
            summarise(count = n(), .groups = "drop") %>%
            rename(tag = tag_temp) %>%
            arrange(desc(count))
          
          if (nrow(count_data) > 30) {
            count_data <- count_data[1:30, ]
            title_text <- paste("Top 30 counts by", tag_col, "—", nrow(selected), "points selected")
          } else {
            title_text <- paste("Count by", tag_col, "—", nrow(selected), "points selected")
          }
          
          plot_ly(
            data = count_data,
            x = ~tag,
            y = ~count,
            type = "bar",
            marker = list(color = color_pal()(count_data$tag), line = list(color = "#404040", width = 1)),
            hovertemplate = paste("<b>%{x}</b><br>", "Count: %{y}<br>", "<extra></extra>")
          ) %>%
            layout(
              title = list(text = title_text, font = list(size = 18, color = "#e0e0e0")),
              xaxis = list(title = tag_col, tickangle = -45, color = "#e0e0e0", gridcolor = "#404040"),
              yaxis = list(title = "Count", color = "#e0e0e0", gridcolor = "#404040"),
              showlegend = FALSE,
              plot_bgcolor = "#1a1a1a",
              paper_bgcolor = "#1a1a1a",
              font = list(color = "#e0e0e0")
            )
        }
      }
    }, error = function(e) {
      showNotification(paste("Error creating bar plot:", e$message), type = "error", duration = 5)
      plot_ly() %>% layout(title = "Error creating plot", paper_bgcolor = "#1a1a1a", plot_bgcolor = "#1a1a1a")
    })
  })
  
  # Render data table
  output$data_table <- renderDT({
    data <- mushroom_data()
    
    if (is.null(data)) {
      return(NULL)
    }
    
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