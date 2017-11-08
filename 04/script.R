# csv�̓ǂݍ���
dat.R = read.table("winequality-red.csv", header=T, sep=";")
dat.W = read.table("winequality-white.csv", header=T, sep=";")

# 1. �i���ƍł��������ւ��������ʂ́H
highest_cor <- function(dat) {
  correlation = rep(0, 11)
  for(i in 1:11) {
    correlation[i] = cor(dat$quality, dat[i])
  }
  return (names(dat)[match(max(correlation), correlation)]);
}

highest_cor(dat.R)
highest_cor(dat.W)

# 2. 11��ނ��ׂĂ̓����ʂ�����ϐ��Ƃ��� 1�����ŁC�i���w�W����A
summary(lm(quality ~ ., data=dat.R))
summary(lm(quality ~ ., data=dat.W))

# 3. �����ʓ��m�̐ς��p����2�����ŉ�A�����ꍇ�ɂ́C��A�̎��͉��P���邩
summary(lm(quality ~ .^2, data=dat.R))
summary(lm(quality ~ .^2, data=dat.W))
summary(aov(quality ~ .^2, data=dat.R))
summary(aov(quality ~ .^2, data=dat.W))

# 4. AIC��p���������ʑI��
res <- step(lm(quality ~ ., data=dat.R))
summary(res)
round(coefficients(res), 3)
res <- step(lm(quality ~ ., data=dat.W))
summary(res)
round(coefficients(res), 3)

summary(step(lm(quality ~ .^2, data=dat.R)))
summary(step(lm(quality ~ .^2, data=dat.W)))