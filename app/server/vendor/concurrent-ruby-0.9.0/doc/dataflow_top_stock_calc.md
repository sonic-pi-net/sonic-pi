This program determines which stock had the highest price in a given year.
It as an example from chapter 1 "Introduction", section 1.2 "What's Scala?" of the book
[Programming Scala: Tackle Multi-Core Complexity on the Java Virtual Machine](http://pragprog.com/book/vsscala/programming-scala).

## What It Does

This program takes a list of one or more stock symbols and a year. It then concurrently
obtains the relevant stock data from Yahoo's iChart service for each symbol. Once all
the data has been retrieved the program determines which stock had the highest year-end
closing price.

* A sample of the data can be downloaded [here](http://ichart.finance.yahoo.com/table.csv?s=AAPL&a=11&b=01&c=2008&d=11&e=31&f=2008&g=m).

#### The Ruby Code

```ruby
require 'concurrent'
require 'open-uri'

def get_year_end_closing(symbol, year)
  uri = "http://ichart.finance.yahoo.com/table.csv?s=#{symbol}&a=11&b=01&c=#{year}&d=11&e=31&f=#{year}&g=m"
  data = open(uri) {|f| f.collect{|line| line.strip } }
  price = data[1].split(',')[4]
  price.to_f
  [symbol, price.to_f]
end

def get_top_stock(symbols, year, timeout = 5)
  stock_prices = symbols.collect{|symbol| Concurrent::dataflow{ get_year_end_closing(symbol, year) }}
  Concurrent::dataflow(*stock_prices) { |*prices|
    prices.reduce(['', 0.0]){|highest, price| price.last > highest.last ? price : highest}
  }.value(timeout)
end

symbols = ['AAPL', 'GOOG', 'IBM', 'ORCL', 'MSFT']
year = 2008

top_stock, highest_price = get_top_stock(symbols, year)

puts "Top stock of #{year} is #{top_stock} closing at price $#{highest_price}"
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
