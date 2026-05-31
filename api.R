library(plumber)

#* Health Check
#* @get /health
function() {
  list(
    status = "running",
    message = "API is working"
  )
}

#* Run Stock Analysis
#* @get /run-analysis
function() {
  
  tryCatch({
    
    source("D:/BUZEE/app.R")
    
    list(
      status = "success",
      message = "Stock analysis completed successfully"
    )
    
  }, error = function(e) {
    
    list(
      status = "error",
      message = as.character(e)
    )
    
  })
}