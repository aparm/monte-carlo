library(gmp)

LKG <- function(n, seeme = 777, a = 5122456, b = 0, m = 1073741789){
  tulem = rep(NA, n)
  tulem[1] <- as.numeric((as.bigz(a) * seeme + b) %% m)
  for(i in 1:(n - 1)){
    tulem[i + 1] <- as.numeric((as.bigz(a) * tulem[i] + b) %% m)
  }
  return(tulem / m)
}

n <- 50000


# ülesanne 1
u <- LKG(n)

# tekitame vektor jaotuspunktidest
c_vektor = c(-Inf, 0.1, 0.2, 0.3, 0.4, Inf)

# tõenäosuste vektor
p <- c(0.1, 0.1, 0.1, 0.1, 0.6)


sildistatud_u <- cut(u, c_vektor)

test1 <- chisq.test(table(sildistatud_u), p = p)
test1

# p-value = 0.975 > 0.1
vastus1 <- 'Võivad olla ühtlase jaotusega'



# ülesanne 2
x <- u * 4

test2 <- ks.test(x, punif, 0, 4)
test2

# p-value = 0.4701 > 0.1
vastus2 <- 'Võivad sobida'



# ülesanne 3
c_vektor = c(-Inf, 0.4, 1.2, 1.6, 2, 2.8, Inf)
p <- c(0.1, 0.2, 0.1, 0.1, 0.2, 0.3)

sildistatud_x <- cut(x, c_vektor)


n <- length(sildistatud_x)
esimene <- sildistatud_x[seq(1, n, by = 2)]
teine <- sildistatud_x[seq(2, n, by = 2)]
tabel <- table(esimene, teine)
ptabel <- p %o% p


test3 <- chisq.test(c(tabel), p = c(ptabel))
test3

# p-value = 0.04426 < 0.1
vastus3 <- 'Ei ole sõltumatud väärtused vaadeldavast jaotusest'





