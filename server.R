library(shiny)
library(shinyjs)
library(data.table)
inFile <- NULL
function(input, output,session) {
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  })
  
}
observe({
  if (is.null(inFile)) {
    shinyjs::disable("process")
  } else {
    shinyjs::enable("process")
  }
})
dtReplaceNA <- function (df) {
  dt <- data.table(df)
  for (j in 1:ncol(dt)) {set(dt, which(is.na(dt[[j]])), j, 0)}
  setDF(dt)  # Return back a data.frame object
}
system.time(df3 <- dtReplaceNA(df))
###function to check and return column name if the column exists in the dataframe
TryGetColumn <- function(x, column.name, value.if.not.exists = NA) {
  if (column.name %in% names(x)) {
    return(x[, column.name])
  }
  return(value.if.not.exists)
}