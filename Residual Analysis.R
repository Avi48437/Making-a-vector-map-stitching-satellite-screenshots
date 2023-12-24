clicks=read.csv(file.choose())
#residual_Analysis

fitx = lm(x~place+ss-1, clicks)
plot(fitx)

checkpoint=function(x,lab)
{
  plot(x)
  identify(x,lab=lab)
}

checkpoint(rstudent(fitx),c(1:length(fitx)))
[1] 12 24 50
checkpoint(dffits(fitx),c(1:length(fitx)))
[1]  5 12 24 50
checkpoint(hatvalues(fitx),c(1:length(fitx)))
[1]  1  7  8 10 21 28 30 31 34 35 36 38 42 43 47 48 53 54

fity = lm(y~place+ss-1, clicks)
plot(fity)

checkpoint=function(y,lab)
{
  plot(y)
  identify(y,lab=lab)
}

checkpoint(rstudent(fity),c(1:length(fity)))
[1]  4 12 15 17 24
checkpoint(dffits(fity),c(1:length(fity)))
[1]  4 12 15 16 24 32
checkpoint(hatvalues(fity),c(1:length(fity)))
[1]  1  7  8 10 21 28 30 31 34 35 36 38 42 43 47 49 53 54
