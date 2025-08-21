#-------------------------------------------------------------------------------
# IEP_Chartbook: Creates the chartbook
IEP_Chartbook <- function(filepath) {

  require(openxlsx)
  {
    wb <- createWorkbook()
    addWorksheet(wb, "default")
    saveWorkbook(wb, filepath, overwrite = TRUE)
  }
}

#-------------------------------------------------------------------------------
# IEP_Sheet: Creates a new sheet in the chartbook in the appropriate format
IEP_Sheet <- function(workbook, chart_name) {
  
  require(openxlsx)
  sheet_name = chart_name["sheet"]
  
  # Check if the sheet already exists
  existing_sheets <- sheets(workbook)
  if (!(sheet_name %in% existing_sheets)) {
    addWorksheet(workbook, sheet_name)
  }
  
  # Define and apply styles
  font_style <- createStyle(fontSize = 8, fontName = "Arial")
  addStyle(workbook, sheet = sheet_name, style = font_style, rows = 1:1000, cols = 1:100, gridExpand = TRUE)
  
  setColWidths(workbook, sheet = sheet_name, cols = 1, widths = 1)      # Column 1 width
  setColWidths(workbook, sheet = sheet_name, cols = 2, widths = "auto")  # Auto width for Column B
  
  bold_style <- createStyle(fontName = "Arial", fontSize = 8, textDecoration = "bold")
  addStyle(workbook, sheet = sheet_name, style = bold_style, rows = 1:1000, cols = 2, gridExpand = TRUE)
  
  # Add specific cell text
  writeData(workbook, sheet = sheet_name, x = "Title", startCol = 2, startRow = 2)
  writeData(workbook, sheet = sheet_name, x = "sub-title", startCol = 2, startRow = 3)
  writeData(workbook, sheet = sheet_name, x = "x-axis title", startCol = 2, startRow = 4)
  writeData(workbook, sheet = sheet_name, x = "y-axis title", startCol = 2, startRow = 5)
  writeData(workbook, sheet = sheet_name, x = "source", startCol = 2, startRow = 6)
  writeData(workbook, sheet = sheet_name, x = "Notes", startCol = 2, startRow = 7)
  writeData(workbook, sheet = sheet_name, x = chart_name["title"], startCol = 3, startRow = 2)
  writeData(workbook, sheet = sheet_name, x = chart_name["xtext"], startCol = 3, startRow = 4)
  writeData(workbook, sheet = sheet_name, x = chart_name["ytext"], startCol = 3, startRow = 5)
  writeData(workbook, sheet = sheet_name, x = chart_name["source"], startCol = 3, startRow = 6)
  
}

#-------------------------------------------------------------------------------
# IEP_SheetData: Add data to the chart book
IEP_SheetData <- function(df, workbook, chart_info) {
  require(openxlsx)
  writeData(workbook, sheet = chart_info["sheet"], 
            x = df, startCol = 3, startRow = 9)
}

#-------------------------------------------------------------------------------
# IEP_SheetImage: Copies an image of the chart to the worksheet as a reference
IEP_SheetImage <- function(workbook, chart_info) {
  require(openxlsx)
  insertImage(workbook,chart_info[["sheet"]],paste0(IMAGE_FILES,"/",chart_info[["sheet"]],"_small.png"),
              startRow = 9, startCol = 10)
}

## -- IEP_ProjectExport ----------------------------------------------------------
IEP_ProjectExport <- function(section, workbook, spreadsheet, chartlist) {
  
  require(openxlsx)
  require(rio)
  for (chart_info in chartlist) {
    
    # Transform string into variable
    chart_var = get(chart_info, envir = .GlobalEnv)
    
    if (chart_var['position'] == "Normal") {
      if (chart_var['type'] %in% c("Chart", "Diagram", "Map")) {
        figure_count = figure_count + 1
        figure_xx = paste0(section, ".", figure_count)
        chart_var['counter'] = figure_xx
        chart_var['sheet'] = paste0(figure_xx, "_", chart_var['sheet'])
      } else if (chart_var['type'] == "Table") {
        table_count = table_count + 1
        table_xx = paste0(section, ".", table_count)
        chart_var['counter'] = table_xx
        chart_var['sheet'] = paste0(table_xx, "_", chart_var['sheet'])
      } else {
        chart_var['counter'] = section
      }
    }
    
    if (nchar(chart_var['sheet']) > 31) {
      chart_var['sheet'] = substr(chart_var['sheet'], 1, 31)
    }
    
    if (chart_var["type"] == "Chart") {
      chart_name = paste0("p",chart_info)
      chart_plot = get(chart_name, envir = .GlobalEnv)
      chart_data = paste0(chart_info,".df")
      chart_df = get(chart_data, envir = .GlobalEnv)
      IEP_Sheet(workbook,chart_var)  
      IEP_SheetData(chart_df, workbook, chart_var)  
      IEP_SavePlots(chart_var, chart_plot)  
      Sys.sleep(1)
      IEP_SheetImage(workbook = workbook, chart_info = chart_var) 
    }
    
    # If type is table
    if (chart_var["type"] == "Table") {
      chart_data = paste0(chart_info,".df")
      chart_df = get(chart_data, envir = .GlobalEnv)
      IEP_Sheet(workbook,chart_var) 
      IEP_SheetData(chart_df, workbook, chart_var)  
      rio::export(chart_df,paste0(TABLE_FILES,"/",chart_var[["sheet"]],".csv"), row.names = FALSE)
    }
    
    # If type is Map or Diagram
    if (chart_var["type"] == "Map" | chart_var["type"] == "Diagram") {
      chart_name = paste0("p",chart_info)
      chart_plot = get(chart_name, envir = .GlobalEnv)
      IEP_Sheet(workbook,chart_var)  
      IEP_SavePlots(chart_var, chart_plot)  
      Sys.sleep(1)
      IEP_SheetImage(workbook = workbook, chart_info = chart_var) 
    }
  }
  
  # save the workbook
  saveWorkbook(workbook, spreadsheet, overwrite = TRUE)
  
  # Assign current count for this section to a variable in the global environment
  assign(paste0("figure_count"), figure_count, envir = .GlobalEnv)
  assign(paste0("table_count"), table_count, envir = .GlobalEnv)
}