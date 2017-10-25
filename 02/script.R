## 2-(1) 一様乱数を足し合わせた数の分布が正規分布に近づくことを確認する
# 初期化
N <- 0; data <- 0
f <- function(data, N) {
  # 描画デバイスの作成
  pdf(sprintf("02\\hist%02d.pdf", N), width=10, height=10)
  hist_dev = dev.cur()
  pdf(sprintf("02\\qq%02d.pdf", N), width=10, height=10)
  qq_dev = dev.cur()

  # データの加算とグラフ表示
  data <- data + runif(10000)
  dev.set(hist_dev)
  hist(data)
  dev.set(qq_dev)
  qqplot(qnorm(ppoints(10000)), data)

  dev.off(hist_dev)
  dev.off(qq_dev)
  return(data)
}

# p-valueが大きければ正規分布であると判断できる
N <- N + 1; data <- f(data, N); ks.test(data, "pnorm", mean(data), sd(data))

# 後処理
rm(N, f, data)

## 4(1) 和の密度関数の式が正しいことを最尤推定し確認する
data1 <-　rnorm(1000000, mean=0.4, sd=1)
data2 <- rnorm(1000000, mean=0.6, sd=2)
data <- data1 + data2

# 最尤推定法 (mle) を使用
library(stats4)
nLL <- function(mean) -sum(dnorm(data, mean, 2, log=T))
(fit <- mle(nLL, start=list(mean=0)))

# 最尤推定法 (optim) を使用
library(stats)
nLL <- function(arg) -sum(dnorm(data, mean=arg[1], sd=arg[2], log=T))
(fit <- optim(c(0,1), nLL))$par

# 後処理
rm(data1, data2, data, nLL)

## 6(1) MASSパッケージのmvrnorm()による乱数ベクトル系列作成
library(MASS)

# 乱数ベクトル系列作成関数
rgmix <- function(n) {
  y <- c();
  for(i in 1:n) {
    u <- runif(1);
    if(u < 0.2) {
      v <- mvrnorm(1, c(-5, -10), matrix(c(4, 3, 3, 9), 2, 2))
    } else if(u < 0.7) {
      v <- mvrnorm(1, c(0, 0), matrix(c(4, 0, 0, 4), 2, 2))
    } else {
      v <- mvrnorm(1, c(7, 5), matrix(c(4, 0, 0, 9), 2, 2))
    }
    y <- c(y, v)
  }
  y <- matrix(y, ncol=2, byrow=T)
  return(y)
}

# 描画デバイスの作成
pdf("02\\mvrnorm-plot.pdf", width=10, height=10)
dev = dev.cur()

# データ生成
dat <- rgmix(5000)
dat <- data.frame(x=dat[,1], y=dat[,2])

# プロット
dev.set(dev)
plot(dat, xlim=c(-20, 20), ylim=c(-20, 20))
dev.off(dev)

## 7(1) EMアルゴリズムで元の2変数3成分ガウス混合分布が推定できるか試す
#install.packages("mclust")
library(mclust)

# 描画デバイスの作成
pdf("02\\mvrnorm-density.pdf", width=10, height=10)
dev = dev.cur()

# EMアルゴリズムの適用
res <- densityMclust(dat)

# プロット
dev.set(dev)
plot(res, what="density", xlim=c(-20, 20), ylim=c(-20, 20))
dev.off(dev)

# 推測されたパラメータの確認
res$parameter$pro
res$parameter$mean
res$parameter$variance$sigma

rm(rgmix, dat, res)

## 10(1) rnorm(N, mean=0.86, sd=1.3) の平均値，分散の区間推定
f <- function(max) {
  sdkn <- c() # 既知の標準偏差を用いた信頼区間 (正規分布)
  sdun <- c() # 推定標準偏差を用いた信頼区間 (正規分布)
  sdkt <- c() # 既知の標準偏差を用いた信頼区間 (t分布)
  sdut <- c() # 推定標準偏差を用いた信頼区間 (t分布)
  vux <- c() # 不偏分散を用いた区間推定 (カイ二乗分布)

  for(N in 2:max) {
    # 初期化
    dat <- rnorm(N, mean=0.86, sd=1.3)
    mu <- mean(dat); sigma <- sd(dat)

    # 平均値の区間推定
    # 既知の標準偏差を用いた信頼区間 (正規分布)
    sdkn <- rbind(sdkn, (qnorm(c(0.025, 0.975)) * 1.3 / sqrt(N) + mu))
    
    # 推定標準偏差を用いた信頼区間 (正規分布)
    sdun <- rbind(sdun, (qnorm(c(0.025, 0.975)) * sigma /  sqrt(N) + mu))

    # 既知の標準偏差を用いた信頼区間 (t分布)
    sdkt <- rbind(sdkt, (qt(c(0.025, 0.975), df=(N-1)) * 1.3 / sqrt(N) + mu))

    # 推定標準偏差を用いた信頼区間 (t分布)
    sdut <- rbind(sdut, (qt(c(0.025, 0.975), df=(N-1)) * sigma /  sqrt(N) + mu))

    # 分散の区間推定
    # 不偏分散を用いた区間推定 (カイ二乗分布)
    vux <- rbind(vux, ((N-1) * sigma**2 / qchisq(c(0.975, 0.025), df=(N-1))))
  }

  res <- list(sdkn, sdun, sdkt, sdut, vux)
  return(res)
}

# グラフの保存
save <- function(res){
  for(i in 1:length(names)) {
    pdf(sprintf("02\\%s-%d.pdf", names[i], N), width=10, height=10)
    devnum = dev.cur()
  
    a = res[[i]]
  
    dev.set(devnum)
    plot(0, 0, type = "n", xlab = "N", ylab = names[i], xlim=c(0, N), ylim=c(tv[i]-2, tv[i]+2))
    lines(2:N-1, a[,1], col="red"); lines(2:N-1, a[,2], col="red")
    lines(c(0, N-1), c(tv[i], tv[i]), col="green4")
    dev.off(devnum)
  }
}

names <- c("sdkn", "sdun", "sdkt", "sdut", "vux")
tv <- c(0.86, 0.86, 0.86, 0.86, 1.3**2)
N <- 500; res <- f(N); save(res)
