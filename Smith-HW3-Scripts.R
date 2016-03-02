## HW 3 Probability & Stats
##
#
# sample <- rgeom(300, prob = .471)
# summary(sample)
# hist(sample, breaks=seq(-0.5,12,1), col='light grey', border='grey', xlab = '')
#
#
# normalPlot(mean = 0, sd = 1, bounds = c(-4, .5))
#
#            normalPlot(
#
#
#
#


negpossible <- TRUE
zcalc <-
  function(x, mu, sigma, positivealignment = TRUE, negpossible = TRUE, ... )
  {
    if( positivealignment == TRUE)
    {
      Lbound <- ifelse( negpossible == TRUE, (-4 * sigma), max( 0, (-4 * sigma) ))
      Rbound <- ((x - mu) + mu)
      normalPlot(mean = mu, sd = sigma, bounds = c( Lbound, Rbound ) )
      zans <- ( x - mu ) / sigma
    }
    else
    {
      mu <- -mu
      x <- -x
      Lbound <- (-4 * sigma) + mu
      Rbound <- ((x - mu) + mu)
      normalPlot(mean = mu, sd = sigma, bounds = c( Lbound, Rbound ) )
        zans <- ( x - mu ) / sigma
      }

  }

#######
#######
manmu <- 4313
mansigma <- 583
Leo_x <- 4948

wommu <- 5261
womsigma <- 807
Mary_x <- 5513


zcalc( x = Leo_x, mu = manmu, sigma = mansigma, positivealignment = FALSE, negpossible = FALSE)

zcalc( x = Mary_x, mu = wommu, sigma = womsigma, positivealignment = FALSE, negpossible = FALSE )



### 3.18
###
### 1 54,
2 55,
3 56,
4 56,
5 57,
6 58,
7 58,
8 59,
9 60,
10 60,
11 60,
12 61,
13 61,
14 62,
15 62,
16 63,
17 63,
18 63,
19 64,
20 65,
21 65,
22 67,
23 67,
24 69,
25 73

fheight <- read.delim(