#* @Plumber Example

library(plumber)
library(stockAPI)
library(tictoc)
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

#  EXCLUDED@jpeg (width = 1000, height = 800)
#* @jpeg (width = 1523, height = 895)
#* @param stocks  Stocks in JSON
#* @param startDate  Stocks in JSON
#* @param endDate  Stocks in JSON
#* @param ma_days  Stocks in JSON
#* @get /get_stocks
get_stocks <- function(stocks = '["AAPL"]',
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
get_stocks_data <- function(stocks = '["AAPL"]',
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












