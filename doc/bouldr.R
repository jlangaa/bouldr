## ---- warning=FALSE, message=FALSE--------------------------------------------
set.seed(2020)
library(bouldr)
library(ggplot2)
options(digits = 2)

## -----------------------------------------------------------------------------
dd <- generate_data(10,Diagnosis = c("Depression","Anxiety","ADHD"),
                          Measure = c("A","B"),
                          Informant = c("Self","Parent", "Teacher"))


## -----------------------------------------------------------------------------
head(dd)

## -----------------------------------------------------------------------------
single <- bouldr(dat = dd,
            f = Outcome ~ Score,
            test = 'delong',
            levels = c('no','yes'),
            direction = "<")

aucs(single)

## ---- fig.width = 5, fig.asp = .8---------------------------------------------
plot(single) + labs(title = "Demo of plot function using test data")

## -----------------------------------------------------------------------------
tests(single)

## -----------------------------------------------------------------------------
grouped <- bouldr(dat = dd,
            f = Outcome ~ Score + Informant,
            test = 'delong',
            levels = c('no','yes'),
            direction = "<")

## ---- fig.width = 5, fig.asp = .8---------------------------------------------
plot(grouped) + labs(title = "Demo of plot function using test data")

## -----------------------------------------------------------------------------
aucs(grouped)

## -----------------------------------------------------------------------------
tests(grouped)

## -----------------------------------------------------------------------------
faceted <- bouldr(dat = dd,
            f = Outcome ~ Score + Informant + Diagnosis,
            test = 'delong',
            levels = c('no','yes'),
            direction = "<")

## ---- fig.width = 8, fig.asp=.38----------------------------------------------
plot(faceted) + labs(title = "Demo of plot function using test data")

## -----------------------------------------------------------------------------
aucs(faceted)

## -----------------------------------------------------------------------------
tests(faceted)

