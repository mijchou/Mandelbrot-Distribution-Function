# Set up

library(rstudioapi)
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("dpqr-mbrot.R")

## dmbrot(x, shape, start = 1, log = FALSE)

# Testing: x
# Restrictions: x > 1, integer number

x1 <- 1:10
x2 <- c(0, -1:-10)
x3 <- 1.5
x4 <- c(Inf, -Inf)
x5 <- c(NaN, NA)
x6 <- c(1e5, 1e10, 1e30, 1e100)

dmbrot(x = x1, shape = 1.5)
dmbrot(x = x2, shape = 1.5)
dmbrot(x = x3, shape = 1.5)
dmbrot(x = x4, shape = 1.5)
dmbrot(x = x5, shape = 1.5)
dmbrot(x = x6, shape = 1.5) # doesn't handle large numbers?

# Testing: shape
# Restrictions: 1 < b < 2

b1 <- seq(1, 2, by = .1)
b2 <- 1.00000000001
b3 <- c(Inf, 0, -Inf)
b4 <- c(NaN, NA)

dmbrot(x = 10, shape = b1)
dmbrot(x = 10, shape = b2)
dmbrot(x = 10, shape = b3)
dmbrot(x = 10, shape = b4)

# Testing: start
# Restrictions: integer number

a1 <- 1
a2 <- -1
a3 <- 1:5
a4 <- c(Inf, 0, -Inf)
a5 <- c(NaN, NA)

dmbrot(x = 10, shape = 1.5, start = a1)
dmbrot(x = 10, shape = 1.5, start = a2)
dmbrot(x = 10, shape = 1.5, start = a3)
dmbrot(x = 10, shape = 1.5, start = a4)
dmbrot(x = 10, shape = 1.5, start = a5)

# Testing: log
# Restrictions: boolean, 1 input

dmbrot(x = 10, shape = 1.5, log = TRUE)
dmbrot(x = Inf, shape = 1.5, log = TRUE)
dmbrot(x = -Inf, shape = 1.5, log = TRUE)

dmbrot(x = 10, shape = 1.5, log = FALSE)
dmbrot(x = Inf, shape = 1.5, log = FALSE)
dmbrot(x = -Inf, shape = 1.5, log = FALSE)

dmbrot(x = 10, shape = 1.5, log = 3)

## Area under should sum to 1

integrand <- function(x) dmbrot(x, shape = 1.5)
sum(integrand, lower = 1, upper = Inf)


#----------------------------------------------------------#
#----------------------------------------------------------#


## pmbrot(q, shape, start = 1, lower.tail = TRUE)

# Testing: q
# Restrictions: q  > 1, integer number

q1 <- 1:10
q2 <- -1:-10
q3 <- 1.5
q4 <- c(Inf, 0, -Inf)
q5 <- c(NaN, NA)
q6 <- c(1e5, 1e10, 1e30, 1e100) # too large? Should I control this?

pmbrot(q = q1, shape = 1.5) # wrong: ans[1] always = 0
pmbrot(q = q2, shape = 1.5)
pmbrot(q = q3, shape = 1.5)
pmbrot(q = q4, shape = 1.5)
pmbrot(q = q5, shape = 1.5)
pmbrot(q = q6, shape = 1.5)

# Testing: shape
# Restrictions: 1 < b < 2

b1 <- seq(1, 2, by = .1)
b2 <- 1.00000000001
b3 <- c(Inf, 0, -Inf)
b4 <- c(NaN, NA)

pmbrot(q = 10, shape = b1)
pmbrot(q = 10, shape = b2)
pmbrot(q = 10, shape = b3)
pmbrot(q = 10, shape = b4)

# Testing: start
# Restrictions: integer number

a1 <- 1
a2 <- -1
a3 <- 1:5
a4 <- c(Inf, 0, -Inf)
a5 <- c(NaN, NA)

pmbrot(q = 10, shape = 1.5, start = a1)
pmbrot(q = 10, shape = 1.5, start = a2)
pmbrot(q = 10, shape = 1.5, start = a3)
pmbrot(q = 10, shape = 1.5, start = a4)
pmbrot(q = 10, shape = 1.5, start = a5)

# Testing: lower.tail
# Restrictions: boolean, 1 input

pmbrot(q = 10, shape = 1.5, lower.tail = TRUE)
pmbrot(q = Inf, shape = 1.5, lower.tail = TRUE)
pmbrot(q = -Inf, shape = 1.5, lower.tail = TRUE)

pmbrot(q = 10, shape = 1.5, lower.tail = FALSE)
pmbrot(q = Inf, shape = 1.5, lower.tail = FALSE)
pmbrot(q = -Inf, shape = 1.5, lower.tail = FALSE)

pmbrot(q = 10, shape = 1.5, lower.tail = 3)


## Checking with dmbrot()

cumsum(dmbrot(x = 1:10, shape = 1.5, start = 1))
pmbrot(q = 1:10, shape = 1.5, start = 1)


#----------------------------------------------------------#
#----------------------------------------------------------#


## qmbrot(p, shape, start = 1, log = FALSE)

# Testing: p
# Restrictions: 0 <= p <= 1 (probability)
