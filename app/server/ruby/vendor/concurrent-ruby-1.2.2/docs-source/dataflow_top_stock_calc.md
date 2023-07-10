This program determines which stock had the highest price in a given year.
It as an example from chapter 1 "Introduction", section 1.2 "What's Scala?" of the book
[Programming Scala: Tackle Multi-Core Complexity on the Java Virtual Machine](http://pragprog.com/book/vsscala/programming-scala).

## What It Does

This program takes a list of one or more stock symbols and a year. It then concurrently
obtains the relevant stock data from Alpha Vantage service for each symbol. Once all
the data has been retrieved the program determines which stock had the highest year-end
closing price.

To use this example you need to obtain a free api key in [AlphaVantage](https://www.alphavantage.co/support/#api-key).


#### The Ruby Code

```ruby
require 'concurrent'
require 'csv'
require 'open-uri'

def get_year_end_closing(symbol, year, api_key)
  uri = "https://www.alphavantage.co/query?function=TIME_SERIES_MONTHLY&symbol=#{symbol}&apikey=#{api_key}&datatype=csv"
  data = []
  csv = URI.parse(uri).read
  if csv.include?('call frequency')
    return :rate_limit_exceeded
  end
  CSV.parse(csv, headers: true) do |row|
    data << row['close'].to_f if row['timestamp'].include?(year.to_s)
  end
  price = data.max
  [symbol, price]
end

def get_top_stock(symbols, year, timeout = 10)
  api_key = ENV['ALPHAVANTAGE_KEY']
  abort(error_message) unless api_key

  stock_prices = symbols.collect{|symbol| Concurrent::dataflow{ get_year_end_closing(symbol, year, api_key) }}
  Concurrent::dataflow(*stock_prices) { |*prices|
    next :rate_limit_exceeded if prices.include?(:rate_limit_exceeded)
    prices.reduce(['', 0.0]){|highest, price| price.last > highest.last ? price : highest}
  }.value(timeout)
end

def error_message
  <<~EOF
    PLEASE provide a Alpha Vantage api key for the example to work
    usage:
      ALPHAVANTAGE_KEY=YOUR_API_KEY bundle exec ruby top-stock-scala/top-stock.rb
  EOF
end

symbols = ['AAPL', 'GOOG', 'IBM', 'ORCL', 'MSFT']
year = 2018

result = get_top_stock(symbols, year)

if result == :rate_limit_exceeded
  puts "API rate limit exceeded"
else
  top_stock, highest_price = result
  puts "Top stock of #{year} is #{top_stock} closing at price $#{highest_price}"
end
```

#### The Scala Code

```scala
//START:PART1
import scala.actors._
import Actor._

val symbols = List( "AAPL", "GOOG", "IBM", "JAVA", "MSFT")
val receiver = self
val year = 2008

symbols.foreach { symbol =>
  actor { receiver ! getYearEndClosing(symbol, year) }
}

val (topStock, highestPrice) = getTopStock(symbols.length)

printf("Top stock of %d is %s closing at price %f\n", year, topStock, highestPrice)
//END:PART1

//START:PART2
def getYearEndClosing(symbol : String, year : Int) = {
  val url = "http://ichart.finance.yahoo.com/table.csv?s=" +
    symbol + "&a=11&b=01&c=" + year + "&d=11&e=31&f=" + year + "&g=m"

  val data = io.Source.fromURL(url).mkString
  val price = data.split("\n")(1).split(",")(4).toDouble
  (symbol, price)
}
//END:PART2

//START:PART3
def getTopStock(count : Int) : (String, Double) = {
  (1 to count).foldLeft("", 0.0) { (previousHigh, index) =>
    receiveWithin(10000) {
      case (symbol : String, price : Double) =>
        if (price > previousHigh._2) (symbol, price) else previousHigh
    }
  }
}
//START:PART3
```
