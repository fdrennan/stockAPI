#* @Plumber Example

library(plumber)
library(stockAPI)
library(tictoc)

message(glue('Within Plumber API {Sys.time()}'))
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
  message(glue('Within filter {Sys.time()}'))

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
  message(glue('Within get_stocks {Sys.time()}'))

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
  message(glue('Within get_stocks_data {Sys.time()}'))

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
  message(glue('Within stocks_excel {Sys.time()}'))
  res$setHeader("Content-Disposition", glue('attachment; filename={file_name}.xlsx'))

  make_xlsx(file_name)

  bin <- readBin(file_name, "raw", n=file.info(file_name)$size)
  file.remove(file_name)

  bin
  # read.xlsx(filename, 'sheet_1')
  # write.csv(iris, filename, row.names = FALSE)
  # bin
}







