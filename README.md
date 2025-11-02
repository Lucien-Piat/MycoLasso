# MycoLasso: Geospatial Data Explorer

`MycoLasso` is a Shiny app to load, visualize, and interactively explore geospatial data points.

Link : https://lucien-piat.shinyapps.io/mycolasso/

## 🚀 Features

  * **Load Data**: Supports `.csv`, `.tsv`, `.txt`, `.xlsx`, and `.xls` files.
  * **Interactive Map**: Displays data points on a Leaflet map, colored by a chosen "tag" column.
  * **Select Data**: Use the lasso or rectangle tool to select points in a specific area.
  * **Dynamic Plot**: Instantly see a Plotly bar chart showing the count of selected points, grouped by their tag.

-----

## 📦 Installation

Install the required R packages:

```r
install.packages(c(
  "shiny", "leaflet", "leaflet.extras", "DT", "dplyr", 
  "plotly", "sp", "shinycssloaders", "readxl", "jsonlite"
))
```

-----

## 💾 Data Format

  * Your file **must** contain a **Longitude** column and a **Latitude** column.
  * Your file **must** contain at least one categorical (text) column to use as a **Tag** for coloring and grouping.
  * The app will automatically load `example.csv` if it exists in the app directory and no other file is uploaded.