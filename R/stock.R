#' @param stocks Tickers in JSON format
#' @param DATA bool
#' @export get_stocks
get_stocks <- function(stocks = '["AAPL","DIA"]',
                       DATA = FALSE,
                       startDate = "2016-01-01",
                       endDate = "2017-01-01") {

  if(str_length(stocks) <= 4) {
    stop(glue('Must submit valid stock ID, you submitted {stocks}'))
  }

  if (startDate >= endDate) {
    stop('startDate must be less than endDate')
  }
  stocks <- fromJSON(stocks)

  mult_stocks <- tq_get(stocks,
                        get  = "stock.prices",
                        from = startDate,
                        to   = endDate)

  if(length(stocks) == 1) {
    mult_stocks$symbol = stocks
  }

  if (DATA) {
    return(toJSON(mult_stocks))
  }

  gg <-
    ggplot(mult_stocks) +
    aes(x = date, y = close, colour = symbol) +
    geom_line() +
    theme(plot.background = element_rect(fill = "#2C4252"),
          panel.background = element_rect(fill = "#2C4252"),
          legend.background = element_rect(fill = "#2C4252"),
          axis.line.x = element_line(color = "white", linetype = "dotted"),
          axis.line.y =  element_line(color = "white", linetype = "dotted"),
          legend.key = element_blank()) +
    xlab('Date') +
    ylab('Price') +
    scale_x_date(date_labels = "%b %d")

  print(gg)
}



