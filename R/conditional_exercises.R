

fizzbuzz <- function(x) {
  mod3 <- x %% 3 == 0
  mod5 <- x %% 5 == 0
  
  if (mod3 && mod5) return('fizzbuzz')
  if (mod3) return('fizz')
  if (mod5) return('buzz')

  x
}

print_temp1 <- function(temp) {
  if (temp <= 0) {
    "freezing"
  } else if (temp <= 10) {
    "cold"
  } else if (temp <= 20) {
    "cool"
  } else if (temp <= 30) {
    "warm"
  } else {
    "hot"
  }
}

print_temp2 <- function(temp) {
  cut(temp, 
      breaks = c(-Inf, 0, 10, 20, 30, Inf), 
      labels = c('freezing', 
                 'cold',
                 'cool', 
                 'warm', 
                 'hot'), 
      right = FALSE
  )
}










