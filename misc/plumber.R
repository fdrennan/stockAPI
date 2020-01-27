#* @Plumber Example

library(plumber)
library(stockAPI)

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

#* @png (width = 1000, height = 1000)
#* @serializer contentType list(type="image/jpeg")
#* @param stocks  Stocks in JSON
#* @get /get_stocks
get_stocks <- function(stocks = '["AAPL"]') {
  stockAPI::get_stocks(stocks = stocks, DATA = FALSE)
}

#* @param stocks  Stocks in JSON
#* @get /get_stocks_data
get_stocks_data <- function(stocks = '["AAPL"]') {
  stockAPI::get_stocks(stocks = stocks, DATA = TRUE)
}
