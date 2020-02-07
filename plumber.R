#* @Plumber Example

library(plumber)
library(stockAPI)
library(tictoc)

# serializer_excel <- function(){
#   function(val, req, res, errorHandler){
#     tryCatch({
#       res$setHeader("Content-Type", "application/vnd.ms-excel")
#       res$setHeader("Content-Disposition", 'attachment; filename=name_of_excel_file.xls')
#       res$body <- paste0(val, collapse="\n")
#       return(res$toResponse())
#     }, error=function(e){
#       errorHandler(req, res, e)
#     })
#   }
# }
#
# plumber::addSerializer("excel", serializer_excel)
#
# serializer_csv <- function(){
#   function(val, req, res, errorHandler){
#     tryCatch({
#       res$setHeader("Content-Type", "application/vnd.ms-excel")
#       res$setHeader("Content-Disposition", 'attachment; filename="xxx.csv"')
#       res$body <- paste0(val, collapse="\n")
#       return(res$toResponse())
#     }, error=function(e){
#       errorHandler(req, res, e)
#     })
#   }
# }
#
# plumber::addSerializer("csv", serializer_csv)





#* @filter cors
cors <- function(req, res) {

  res$setHeader("Access-Control-Allow-Origin", "*")

  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }

}


# #* @serializer csv
# #* @get /csv
# function() {
#   df <- data.frame(CHAR = letters, NUM = rnorm(length(letters)), stringsAsFactors = F)
#   csv_file <- tempfile(fileext = ".csv")
#   on.exit(unlink(csv_file), add = TRUE)
#   write.csv(df, file = csv_file)
#   readLines(csv_file)
# }

#  EXCLUDED@jpeg (width = 1000, height = 800)
#* @jpeg (width = 1523, height = 895)
#* @param stocks  Stocks in JSON
#* @param startDate  Stocks in JSON
#* @param endDate  Stocks in JSON
#* @param ma_days  Stocks in JSON
#* @get /get_stocks
function(stocks = '["AAPL"]',
                       startDate = '2019-01-01',
                       endDate = '2020-01-01',
                       DATA = FALSE,
                       ma_days = 50) {

  # Build the response object (list will be serialized as JSON)
  response <- list(statusCode = 200,
                   data = "",
                   message = "Success!",
                   metaData = list(
                     args = list(
                       stocks = stocks,
                       DATA = DATA,
                       startDate = startDate,
                       endDate = endDate,
                       ma_days = ma_days
                     ),
                     runtime = 0
                   )
  )


  response <- tryCatch(
    {

      # Run the algorithm
      tic()
      response$data <- stockAPI::get_stocks(stocks = stocks,
                                            DATA = DATA,
                                            startDate = startDate,
                                            endDate = endDate,
                                            ma_days = ma_days)
      timer <- toc(quiet = T)
      response$metaData$runtime <- as.numeric(timer$toc - timer$tic)

      return(response)
    },
    error = function(err) {
      response$statusCode <- 400
      response$message <- paste(err)

      return(response)
    }
  )

  return(response)


}


#* @param stocks  Stocks in JSON
#* @param startDate  Stocks in JSON
#* @param endDate  Stocks in JSON
#* @get /get_stocks_data
#* @serializer unboxedJSON
function(stocks = '["AAPL"]',
         startDate = '2019-01-01',
         endDate = '2020-01-01',
         DATA = TRUE) {

  print(stocks)
  # Build the response object (list will be serialized as JSON)
  response <- list(statusCode = 200,
                   data = "",
                   message = "Success!",
                   metaData = list(
                     args = list(
                       stocks = stocks,
                       DATA = DATA,
                       startDate = startDate,
                       endDate = endDate
                     ),
                     runtime = 0
                   )
  )


  response <- tryCatch(
    {

      # Run the algorithm
      tic()
      response$data <- stockAPI::get_stocks(stocks = stocks,
                                            DATA = DATA,
                                            startDate = startDate,
                                            endDate = endDate)
      timer <- toc(quiet = T)
      response$metaData$runtime <- as.numeric(timer$toc - timer$tic)

      return(response)
    },
    error = function(err) {
      response$statusCode <- 400
      response$message <- paste(err)

      return(response)
    }
  )

  return(response)


}

#* @serializer contentType list(type="application/vnd.ms-excel")
#* @param file_name
#* @get /stocks_excel
function(req, res, file_name = 'data.xlsx') {
  # This header is a convention that instructs browsers to present the response
  # as a download named filename rather than trying to render it inline.
  # attachmentString = paste0("attachment; filename=", filename)
  #
  # res$setHeader("Content-Disposition", attachmentString)

  # Read in the raw contents of the binary file


  #Return the binary contents
  # bin
  res$setHeader("Content-Disposition", glue('attachment; filename={file_name}.xlsx'))

  # Create workbook (i.e. file) to put data in
  fileName <- "iris-mtcars.xlsx"
  excel <- createWorkbook(fileName)
  # Create sheet names for each wanted
  firstSheet <- "iris"
  secondSheet <- "mtcars"
  # Add worksheets to workbook
  addWorksheet(excel, firstSheet)
  addWorksheet(excel, secondSheet)
  # Add data to workbook
  writeData(excel, sheet = 1, iris)
  writeData(excel, sheet = 2, mtcars, rowNames = TRUE)
  # Finally write out to file
  saveWorkbook(excel, file = fileName, overwrite = TRUE)


  bin <- readBin(fileName, "raw", n=file.info(fileName)$size)
  bin
  # read.xlsx(filename, 'sheet_1')
  # write.csv(iris, filename, row.names = FALSE)
  # bin
}







