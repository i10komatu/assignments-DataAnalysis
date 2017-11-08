# csvの読み込み
dat.R = read.table("winequality-red.csv", header=T, sep=";")
dat.W = read.table("winequality-white.csv", header=T, sep=";")

# 1. 品質と最も高い相関を持つ特徴量は？
highest_cor <- function(dat) {
  correlation = rep(0, 11)
  for(i in 1:11) {
    correlation[i] = cor(dat$quality, dat[i])
  }
  return (names(dat)[match(max(correlation), correlation)]);
}

highest_cor(dat.R)
highest_cor(dat.W)

# 2. 11種類すべての特徴量を説明変数とした 1次式で，品質指標を回帰
summary(lm(quality ~ ., data=dat.R))
summary(lm(quality ~ ., data=dat.W))

# 3. 特徴量同士の積も用いた2次式で回帰した場合には，回帰の質は改善するか
summary(lm(quality ~ .^2, data=dat.R))
summary(lm(quality ~ .^2, data=dat.W))
summary(aov(quality ~ .^2, data=dat.R))
summary(aov(quality ~ .^2, data=dat.W))

# 4. AICを用いた特徴量選択
res <- step(lm(quality ~ ., data=dat.R))
summary(res)
round(coefficients(res), 3)
res <- step(lm(quality ~ ., data=dat.W))
summary(res)
round(coefficients(res), 3)

summary(step(lm(quality ~ .^2, data=dat.R)))
summary(step(lm(quality ~ .^2, data=dat.W)))
