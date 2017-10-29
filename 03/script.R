### 1. data-2d.txtのデータを主成分分析
# データ読み込み
dat <- read.csv("data-2d.txt", header=F)
dat.pca <- prcomp(dat, center = T, retx = T)

# グラフ描画
{
plot(c(), xlim=c(-10, 10), ylim=c(-10, 10), type="n")
points(dat[,1], dat[,2])
points(dat.pca$x[,1], dat.pca$x[,2], col="red")
points(-dat.pca$x[,2], dat.pca$x[,1], col="blue")
}

## (a) 回転角を推定
# 元の2変数正規分布を主成分分析すると，y軸が主軸となるはずであるため，90度引く
((atan2(dat.pca$rotation[2,1], dat.pca$rotation[1,1])/pi*180)-90)

## (b) データを主軸上に射影
hist(dat.pca$x[,1], breaks=40)

## (c) (x,y)座標系におけるx,y成分の相関係数
cor(dat[,1], dat[,2])

## (d) (p,q)座標系におけるp,q成分の相関係数
cor(dat.pca$x[,1], dat.pca$x[,2])

### 2. data-5d.txtのデータを主成分分析し，散布図を表示
dat <- read.csv("data-5d.txt", header=F)
dat.pca <- prcomp(dat, center = T, retx = T)
plot(dat.pca$x[,1], dat.pca$x[,2])

### 3. t検定
N = 10000
exist.mean = 15.1; exist.sd = 5; prop.mean = 15; prop.sd = 5;
exist.time <- rnorm(N, mean=exist.mean, sd=exist.sd)
prop.time <- rnorm(N, mean=prop.mean, sd=prop.sd)
(res = t.test(exist.time, prop.time, alternative="two.sided", var.equal=T, conf.level=0.95))
# 有意水準を5%とする
(res$p.value < 0.05) # 帰無仮説を棄却してもよいか？有意差がある(差がある)か？
