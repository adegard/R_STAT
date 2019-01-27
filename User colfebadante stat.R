library(readr)
usertable <- read_csv("C:/Users/degar_000/Downloads/users_201710271019th.csv")

library(ggplot2)
# counts
ggplot(data.frame(usertable$cb_provincia), aes(x=usertable$cb_provincia)) +
  geom_bar()

freqfunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), ", ")))), n)
}

mostfreq<-freqfunc(usertable$cb_provincia, 10)


