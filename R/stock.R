#' @param stocks Tickers in JSON format
#' @param DATA bool
#' @export get_stocks
get_stocks <- function(stocks = '["AAPL","DIA"]',
                       DATA = FALSE,
                       startDate = "2016-01-01",
                       endDate = "2017-01-01",
                       return_image = FALSE,
                       ma_days = 50) {
  ma_days = as.numeric(ma_days)

  n_days <- as.Date(endDate)-as.Date(startDate)
  max_days <- 365*2
  if(n_days > max_days) {
    stop(glue('You are querying for {n_days} days. Max days allowed: {max_days}'))
  }

  if(str_length(stocks) <= 4) {
    stop(glue('Must submit valid stock ID, you submitted {stocks}'))
  }

  if (startDate >= endDate) {
    stop('startDate must be less than endDate')
  }
  stocks <- fromJSON(stocks)

  if(length(stocks) > 5) {
    stop('Please only search up to 4 stocks at a time')
  }

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
    aes(x = date, y = adjusted, colour = symbol) +
    geom_ma(ma_fun = SMA, n = ma_days) +
    geom_line() +
    theme(plot.background = element_rect(fill = "#2C4252"),
          panel.background = element_rect(fill = "#2C4252"),
          legend.background = element_rect(fill = "#2C4252"),
          axis.title.x = element_text(colour = "white"),
          axis.title.y = element_text(colour = "white"),
          axis.text = element_text(colour = 'white'),
          axis.line.x = element_line(color = "white", linetype = "dotted"),
          axis.line.y =  element_line(color = "white", linetype = "dotted"),
          legend.text = element_text(colour = 'white'),
          legend.title = element_text(colour = 'white'),
          legend.key = element_blank()) +
    xlab('Date') +
    ylab('Price') +
    scale_x_date(date_labels = "%b %d")

  if(return_image) {
    return(gg)
  }
  print(gg)
}



