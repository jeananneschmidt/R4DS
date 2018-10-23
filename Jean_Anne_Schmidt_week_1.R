mtcars
myname <- "JeanAnneSchmidt"
mtcarsColumns <- colnames(mtcars)
mtcarsSummary <- summary(mtcars)
dratValue <- subset(mtcars, subset = mpg>21 & cyl ==6)[ ,5]
topQsec <- head(mtcars[order(-mtcars$qsec),])
