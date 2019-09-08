get_symbols <- function() {
  wheel <- c('DD', '7', 'BBB', 'BB', 'B', 'C', '0')
  sample(wheel, size = 3, replace=TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}
get_symbols()

play <- function() {
  # 1. make symbols
  symbols <- get_symbols()
  
  # 2. print symbols
  print(symbols)
  
  # 3. score calculation
  score(symbols)
}
play()

payouts <- c('DD' = 100, '7' = 80, 'BBB' = 40, 'BB' = 5, 'B' = 10, 'C' = 10, '0' = 0)
payouts
payouts['DD']
unname(payouts['DD'])

symbols <- c('B', '0', 'B')
#어떤 경우에 해당하는지 조건을 확인
same <- symbols[1] == symbols[2] & symbols[2] == symbols[3]
bars <- symbols %in% c('B', 'BB', 'BBB')

#상금계산
if(same) {
  # 심볼이 모두 같은 경우
  payouts <- c('DD' = 100, '7' = 80, 'BBB' = 40, 'BB' = 5, 'B' = 10, 'C' = 10, '0' = 0)
  prize <- unnames(payouts[symbols[1]])
} else if (all(bars)) {
  # 심벌이 모두 바인 경우
  prize <- 5
} else {
  # 체리의 개수
  cherries <- sum(symbols == 'C')
  prize <- c(0, 2, 5)[cherries + 1]
}
# 다이아온드일 때 상금 조정
diamonds <- sum(symbols == 'DD')
prize  * 2 ^ diamonds

score <- function(symbols) {
  #어떤 경우에 해당하는지 조건을 확인
  same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
  bars <- symbols %in% c('B', 'BB', 'BBB')
  
  #상금계산
  if(same) {
    # 심볼이 모두 같은 경우
    payouts <- c('DD' = 100, '7' = 80, 'BBB' = 40, 'BB' = 5, 'B' = 10, 'C' = 10, '0' = 0)
    prize <- unname(payouts[symbols[1]])
  } else if (all(bars)) {
    # 심벌이 모두 바인 경우
    prize <- 5
  } else {
    # 체리의 개수
    cherries <- sum(symbols == 'C')
    prize <- c(0, 2, 5)[cherries + 1]
  }
  # 다이아온드일 때 상금 조정
  diamonds <- sum(symbols == 'DD')
  prize  * 2 ^ diamonds
}
play <- function() {
  # 1. make symbols
  symbols <- get_symbols()
  
  # 2. print symbols
  print(symbols)
  
  # 3. score calculation
  score(symbols)
}
play()

one_play <- play()
one_play
attr(one_play, 'symbols') <- c('B', '0', 'B')
attributes(one_play)
one_play

play <- function() {
  symbols <- get_symbols()
  prize <- score(symbols)
  attr(prize, 'symbols') <- symbols
  prize
}
play()

two_play <- play()
two_play

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols)
}
play()
three_play <- play()
three_play

slot_display <- function(prize) {
  #symbols 속성을 추출
  symbols <- attr(prize, 'symbols')
  
  #smybols 벡터를 하나의 문자열로 합침
  symbols <- paste(symbols, collapse = " ")
  
  #symbols을 정규표현식으로 prize와 결합
  string <- paste(symbols, prize, sep= '\n$')
  
  #콘솔에서 결과를 따옴표 없이 표시
  cat(string)
}
slot_display(one_play)
symbols <- attr(one_play, 'symbols')
symbols

slot_display(play())
slot_display(play())
print
class(one_play) <- 'slots'
class(one_play)
args(print)
print.slots <- function(x, ...) {
  cat("print.slots 메서드를 사용 중")
}
print(one_play)
one_play
rm(print.slots)

now <- Sys.time()
attributes(now)
print.slots <- function(x, ...) {
  slot_display(x)
}
one_play

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols, class = "slots")
}
class(play())
play()
play()

methods(class = 'factor')

die <- c(1,2,3,4,5,6)
rolls <- expand.grid(die, die)
rolls
rolls$value <- rolls$Var1 + rolls$Var2
head(rolls, 3)

prob <- c('1' = 1/8, '2' = 1/8, '3' = 1/8, '4' = 1/8, '5' = 1/8, '6' = 3/8)
prob
rolls$Var1
prob[rolls$Var1]
rolls$prob1 <- prob[rolls$Var1]
head(rolls, 3)
rolls$prob2 <- prob[rolls$Var2]
head(rolls, 3)
rolls$prob <- rolls$prob1 * rolls$prob2
head(rolls, 3)
sum(rolls$value * rolls$prob)

wheel <- c('DD', '7', 'BBB', 'BB', 'B', 'C', '0')
combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors =  FALSE)
combos

get_symbols <- function() {
  wheel <- c('DD', '7', 'BBB', 'BB', 'B', 'C', '0')
  sample(wheel, size = 3, replace = TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}
prob <- c('DD' = 0.03, '7' = 0.03, 'BBB' = 0.06, 'BB' = 0.1, 'B' = 0.25, 'C' = 0.01, '0' = 0.52)
combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]
head(combos, 3)
combos$prob <- combos$prob1 * combos$prob2 * combos$prob3
sum(combos$prob)
head(combos, 3)

symbols <- c(combos[1, 1], combos[1, 2], combos[1, 3])
symbols
score(symbols)

chars <- vector(length = 4)
words <- c('My', 'four', 'for', 'loop' )
for( i in 1:4) {
  chars[i] <- words[i]
}
chars

combos$prize <- NA
head(combos, 3)
for (i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}
head(combos, 3)
sum(combos$prize * combos$prob)

score <- function(symbols) {
  diamonds <- sum(symbols == 'DD')
  cherries <- sum(symbols == 'C')
  
  slots <- symbols[symbols != 'DD']
  same <- length(unique(slots)) == 1
  bars <- slots %in% c("B", "BB", "BBB")
  
  if(diamonds == 3) {
    prize <- 100
  } else if(same) {
    payouts <- c('7' = 80, 'BBB' = 40, 'BB' = 25, 'B' = 10, 'C' = 10, '0' = 0)
    prize <- unname(payouts[slots[1]])
  } else if (all(bars)) {
    prize <- 5
  } else if (cherries > 0) {
    prize <- c(0, 2, 5)[cherries + diamonds + 1 ]
  } else {
    prize <- 0
  }
  
  prize * 2 ^ diamonds
}

for ( i in 1:nrow(combos)) {
  symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
  combos$prize[i] <- score(symbols)
}
sum(combos$prize * combos$prob)

abs_loop <- function(vec) {
  for (i in 1:length(vec)) {
    if (vec[1] < 0) {
      vec[1] <- -vec[1]
    }
  }
  vec
}

abs_set <- function(vec) {
  negs <- vec < 0 
  vec[negs] <- vec[negs] * -1
  vec
}

long <- rep(c(-1, 1), 500000)
long
system.time(abs_loop(long))
system.time(abs_set(long))

vec <- c(1, -2, 3, -4, 5, 6, -7, -8, -9, 10)
vec < 0
vec[vec < 0]
vec[vec < 0] * -1

change_symbols <- function(vec) {
  for (i in 1:length(vec)) {
    if (vec[i] == 'DD'){
      vec[i] <- 'joker'
    } else if (vec[i] == 'C'){
      vec[i] <- 'ace'
    } else if (vec[i] == '7'){
      vec[i] <- 'king'
    } else if (vec[i] == 'B') {
      vec[i] <- 'queen'
    } else if (vec[i] == 'BB') {
      vec[i] <- 'jack'
    } else if (vec[i] == 'BBB') {
      vec[i] <- 'ten'
    } else {
      vec[i] <- 'nine'
    }
  }
  vec
}
vec <- c('DD', 'C', '7', 'B', 'BB', 'BBB', '0')
change_symbols(vec)
many <- rep(vec, 1000000)
system.time(change_symbols(many))
vec[vec == 'DD']

change_symbols <- function(vec) {
  vec[vec == 'DD'] <- 'joker'
  vec[vec == 'C'] <- 'ace'
  vec[vec == '7'] <- 'king'
  vec[vec == 'B'] <- 'queen'
  vec[vec == 'BB'] <- 'jack'
  vec[vec == 'BBB'] <- 'ten'
  vec[vec == '0'] <- 'nine'
  
  vec
}
system.time(change_symbols(many))

change2_symbols <- function(vec) {
  tb <- c('DD' = 'joker', 'C' = 'ace', '7' = 'king', 'B' = 'queen', 'BB' = 'jack', 'BBB' = 'ten', '0' = 'nine')
  unname(tb[vec])
}
change2_symbols(vec)
system.time(change2_symbols(many))

winings <- vector(length = 1000000)
for (i in 1:1000000) {
  winings[i] <- play()
}
mean(winings)

get_many_symbols <- function(n) {
  wheel <- c('DD', '7', 'BBB', 'BB', 'B', 'C', '0')
  vec <- sample(wheel, size = 3 * n, replace = TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
  matrix(vec, ncol = 3)
}
get_many_symbols(5)

play_many <- function(n) {
  symb_mat <- get_many_symbols(n = n)
  data.frame(w1 = symb_mat[,1], w2 = symb_mat[,2],
             w3 = symb_mat[,3], prize = score_many(symb_mat))
}

score_many <- function(symbols) {
  cherries <- rowSums(symbols == "C")
  diamonds <- rowSums(symbols == 'DD')
  
  prize <- c(0, 2, 5)[cherries + diamonds + 1]
  prize[!cherries] <- 0
  
  same <- symbols[, 1] == symbols[, 2] & symbols[, 2] == symbols[, 3]
  payoffs <- c('DD' = 100, '7' = 80, 'BBB' = 40, 'BB' = 25, 'B' = 10, 'C' = 10, '0' = 0)
  prize[same] <- payoffs[symbols[same, 1]]
  
  bars <- symbols == 'B' | symbols == 'BB' | symbols == 'BBB'
  all_bars <- bars[, 1] & bars[, 2] & bars[, 3] & !same
  prize[all_bars] <- 5
  
  two_wilds <- diamonds == 2
  
  one <- two_wilds & symbols[, 1] != symbols[ ,2] & symbols[, 2] == symbols[, 3]
  two <- two_wilds & symbols[, 1] != symbols[ ,2] & symbols[, 1] == symbols[, 3]
  three <- two_wilds & symbols[, 1] == symbols[ ,2] & symbols[, 2] != symbols[, 3]
  
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]]
  prize[three] <- payoffs[symbols[three, 3]]
  
  one_wild <- diamonds == 1
  
  wild_bars <- one_wild & (rowSums(bars) == 2)
  prize[wild_bars] <- 5
  
  one <- one_wild & symbols[, 1] == symbols[, 2]
  two <- one_wild & symbols[, 2] == symbols[, 3]
  three <- one_wild & symbols[, 3] == symbols[, 1]
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]]
  prize[three] <- payoffs[symbols[three, 3]]
  
  unname(prize * 2^diamonds)
}
system.time(play_many(10000000))

