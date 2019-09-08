setwd('~/Documents/hands-on-programming-with-R')
die <- c(1,2,3,4,5,6)
m <- matrix(die, nrow=2)
m
m <- matrix(die, nrow=2, byrow=TRUE)
m

ar <- array(c(11:14, 21:34, 31:34), dim = c(2, 2, 3))
ar 
hand1 <- c("ace", 'king', 'queen', 'jack', 'ten', 'spades', 'spades', 'spades', 'spades', 'spades')
matrix(hand1, nrow = 5)
matrix(hand1, ncol = 2)
dim(hand1) <- c(5,2)

hand2 <- c('ace', 'spades', 'king', 'spades', 'queen', 'spades', 'jack', 'spades', 'ten', 'spades')
matrix(hand2, nrow = 5, byrow=TRUE)
matrix(hand2, ncol = 2, byrow=TRUE)
dim(die) <- c(2,3)
typeof(die)
class(die)
attributes(die)
Sys.time()
now <- Sys.time()
now
typeof(now)
class(now)
unclass(now)
mil <- 1000000
mil
class(mil) <- c('POSIXct', 'POSIXt')
mil
gender <- factor(c("male", "female", 'female', 'male'))
typeof(gender)
attributes(gender)
unclass(gender)
as.character(gender)
card <- c('ace', 'heart', 1)
card

list1 <- list(100:130, "R", list(TRUE, FALSE))
list1
card <- list('ace', ' heart', 1)
card

df <- data.frame(face = c('ace', 'two', 'six'), suit = c('clubs','clubs','clubs'), value=c(1,2,3))
df
list(face = 'ace', suit= 'hearts', value = 1)
c(face = 'ace', suit='gearts', value=1)
typeof(df)
class(df)
str(df)

head(deck)
write.csv(deck, file = 'cards.csv', row.names = FALSE)
deck <- read.csv("deck.csv")
head(deck)
deck[1,1]
deck[1:2, 1:2]
deck[1,1:3]
deck[1:2, 1]
deck[1:2, 1, drop = FALSE]
deck[-(2:52), 1:3]
deck[-1, 1]
deck2 <- deck[1:52,]
deck3 <- deck[c(2,1,3:52),]
head(deck3)
random <- sample(1:52, size=52)
deck4 <- deck[random,]
head(deck4)
deal <- function(cards){
  cards[1,]
}
shuffle <- function(cards){
  random <- sample(1:52, size=52)
  cards[random,]
}
deal(deck)
deck2 <- shuffle(deck)
deal(deck2)
deck$value
mean(deck$value)
median(deck$value)
lst <- list(number = c(1,2), logical = TRUE, strings = c('a','b','c'))
lst
lst[1]
sum(lst[1])
lst$number
sum(lst$number)
lst[[1]]
lst['number']
lst[['number']]

deck2 <- deck
vec <- c(0,0,0,0,0)
vec[1]
vec[1] <- 1000
vec
vec[c(1,3,5)] <- c(1,1,1)
deck2$new <- 1:52
head(deck2)
deck2$new <- NULL
head(deck2)
deck2[c(13,26,39,52),]
deck2[c(13,26,39,52), 3]
deck2$value[c(13,26,39,52)] <- 14
head(deck2)
deck3 <- shuffle(deck)
head(deck3)
deck2$face
deck2$face == 'ace'
sum(deck2$face == 'ace')
deck3$face == 'ace'
deck3$value[deck3$face == 'ace']
deck3$value[deck3$face == 'ace'] <- 14
head(deck3)
deck4 <- deck
deck4$value <- 0
head(deck4, 13)

deck4$suit == 'hearts'
deck4$value[deck4$suit == 'hearts']
deck4$value[deck4$suit == 'hearts'] <- 1
deck4[deck4$face == 'queen',]
deck4[deck4$suit == 'spades',]
deck4$face == 'queen' & deck4$suit == 'spades'
queenOfSpades <- deck4$face == 'queen' & deck4$suit == 'spades'
deck4[queenOfSpades,]
deck4$value[queenOfSpades] <- 13

deck5 <- deck
head(deck5, 13)
deck5$face %in% c('king', 'queen', 'jack')
facecard <- deck5$face %in% c('king', 'queen', 'jack')
facecard
deck5[facecard,]
deck5$value[facecard] <- 10
head(deck5, 13)
deck5$value[deck5$face == 'ace'] <- NA
head(deck5 ,13)

library(devtools)
globalenv()
baseenv()
emptyenv()
parent.env(globalenv())
ls(emptyenv())
ls(globalenv())
head(globalenv()$deck, 3)
assign('new', 'hello global', envir = globalenv())
globalenv()$new
environment()
new
new <- 'hello active'
new
show_env <- function() {
  list(ran.in = environment(),
       parent = parent.env(environment()),
       objects = ls.str(environment()))
}
show_env()
show_env()
environment(show_env)
show_env <- function() {
  a <- 1
  b <- 2
  c <- 3
  list(ran.in = environment(),
       parent = parent.env(environment()),
       objects = ls.str(environment()))
}
show_env()
foo <- 'take me to your runtime'
show_env <- function(x=foo) {
  list(ran.io = environment(),
       parent = parent.env(environment()),
       objects = ls.str(environment()))
}
show_env()
deal <- function() {
  deck[1,]
}
environment(deal)
DECK <- deck
deck <- deck[-1,]
head(deck,3)
deal <- function() {
  card <- deck[1,]
  deck <- deck[-1,]
  card
}

deal <- function() {
  card <- deck[1,]
  assign('deck', deck[-1,], envir = globalenv())
  card
}
deal()
deal()
deal()

head(deck, 3)
a <- shuffle(deck)
head(deck, 3)
head(a, 3)
shuffle <- function() {
  random <- sample(1:52, size=52)
  assign('deck', DECK[random,], envir = globalenv())
}
shuffle()
deal()
deal()

setup <- function(deck) {
  DECK <- deck
  
  DEAL <- function() {
    card <- deck[1,]
    assign('deck', deck[-1,], envir = globalenv())
    card
  }
  
  SHUFFLE <- function() {
    random <- sample(1:52, size=52)
    assign('deck', DECK[random,], envir = globalenv())
  }
  
  list(deal = DEAL, shuffle = SHUFFLE)
}
cards <- setup(deck)
cards
deal <- cards$deal
shuffle <- cards$shuffle
deal
shuffle
deal()
shuffle()
