#' @param stocks Tickers in JSON format
#' @export get_stocks
get_stocks <- function(stocks = '["AAPL","DIA"] ') {
  stocks <- fromJSON(stocks)
  mult_stocks <- tq_get(stocks,
                        get  = "stock.prices",
                        from = "2016-01-01",
                        to   = "2017-01-01")

  if(length(stocks) == 1) {
    mult_stocks$symbol = stocks
  }
  gg <-
    ggplot(mult_stocks) +
    aes(x = date, y = close, colour = symbol) +
    geom_line()

  print(gg)
}



