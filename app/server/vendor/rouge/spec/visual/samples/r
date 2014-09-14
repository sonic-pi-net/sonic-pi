#!/usr/bin/env Rscript
### Example R script for syntax highlighting

# This is also a comment

## Valid names
abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUV0123456789._a <- NULL
.foo_ <- NULL
._foo <- NULL

## Invalid names
0abc <- NULL
.0abc <- NULL
abc+cde <- NULL

## Reserved Words
NA
NA_integer_
NA_real_
NA_character_
NA_complex_
NULL
NaN
Inf
## Not reserved
NULLa <- NULL
NULL1 <- NULL
NULL. <- NULL
NA_foo_ <- NULL

## Numbers
12345678901
123456.78901
123e3
123E3
1.23e-3
1.23e3
1.23e-3
## integer constants
123L
1.23L
## imaginary numbers
123i
-123i
123e4i
123e-4i
## Hex numbers
0xabcdefABCDEF01234
0xabcp123
0xabcP123
## Not hex
0xg

## Special operators %xyz%
## %xyz%
1 %% 2
diag(2) %*% diag(2)
1 %/% 2
1 %in% 1:10
diag(2) %o% diag(2)
diag(2) %x% diag(2)
`%foo bar%` <- function(x, y) x + y
1 %foo bar% 2

## Control Structures (3.2) and Function
## if, else
if (TRUE) print("foo") else print("bar")
## For, in
for(i in 1:5) {
    print(i)
}
## While, break
i <- 1
while (TRUE) {
    i <- i + 1
    if (i > 3) break
}
## Repeat
repeat {1+1}
## Switch
x <- 3
switch(x, 2+2, mean(1:10), rnorm(5))
## Function, dot-dot-dot, return
foo <- function(...) {
    return(sum(...))
}
# Not keywords
functiona <- 2 + 2
function. <- 2 + 2
function1 <- 2 + 2


## Grouping Tokens 10.3.7
## Parentheses
1 + (2 + 3)
## brackets
foo <- function(a) {
    a + 1
}

## Indexing 10.3.8
## []
bar <- 1:10
bar[3]
## [[]]
foo <- list(a=1, b=2, c=3)
foo[["a"]]
## $
foo$a
foo$"a"

## Operators
2 - 2
2 + 2
2 ~ 2
! TRUE
?"help"
1:2
2 * 2
2 / 2
2^2
2 < 2
2 > 2
2 == 2
2 >= 2
2 <= 2
2 != 2
TRUE & FALSE
TRUE && FALSE
TRUE | FALSE
TRUE || FALSE
foo <- 2 + 2
foo = 2 + 2
2 + 2 -> foo
foo <<- 2 + 2
2 + 2 ->> foo
base:::sum
base::sum

## Strings
foo <- "hello, world!"
foo <- 'hello, world!'
foo <- "Hello, 'world!"
foo <- 'Hello, "world!'
foo <- 'Hello, \'world!\''
foo <- "Hello, \"world!\""
foo <- "Hello,
world!"
foo <- 'Hello,
world!'

## Backtick strings
`foo123 +!"bar'baz` <- 2 + 2
