library(outliers)
library(kernlab)
library(caret)
library(dplyr)

set.seed(123)

# データの読み込み
fftD = read.csv("100-40-rndD_fft_union.csv", header=F)
fftH = read.csv("100-40-rndH_fft_union.csv", header=F)
pcaD = read.csv("pca_100-40-D.csv", header=F)
pcaH = read.csv("pca_100-40-H.csv", header=F)

# 第pcDim主成分までを取り出す
pcDim = 54;
pcaD = pcaD[, 1:pcDim];
pcaH = pcaH[, 1:pcDim];

# データ数の確認
print(dim(fftD))
print(dim(fftH))
print(dim(pcaD))
print(dim(pcaH))

# 欠損の確認; 欠損のないデータを用いているため、FALSEになるはず
anyNA(fftD) | anyNA(fftH) | anyNA(pcaD) | anyNA(pcaH)

## 各列に対して, Smirnov-Grubbs検定で外れ値かどうか見る
pdf("fftD.pdf", width=10, height=10)
device = dev.cur()
boxplot(fftD)
dev.off(device)
# for (i in 1:512) {
#   print(i)
#   print(grubbs.test(fftD[,i]))
# }

pdf("fftH.pdf", width=10, height=10)
device = dev.cur()
boxplot(fftH)
dev.off(device)
# for (i in 1:512) {
#  print(i)
#  print(grubbs.test(fftH[,i]))
# }

pdf("pcaD.pdf", width=10, height=10)
device = dev.cur()
boxplot(pcaD)
dev.off(device)
# for (i in 1:pcDim) {
#   print(i)
#   print(grubbs.test(pcaD[,i]))
# }

pdf("pcaH.pdf", width=10, height=10)
device = dev.cur()
boxplot(pcaH)
dev.off(device)
# for (i in 1:pcDim) {
#   print(i)
#   print(grubbs.test(pcaH[,i]))
# }

## one class SVMを使った外れ値検出
fftDsvm = data.frame(x=fftD, class=1)
model = ksvm(x=class~., data=fftDsvm, type="one-svc", kernel="vanilladot", nu=0.04)
OLIDfftD = numeric(dim(fftD)[1])
OLIDfftD[SVindex(model)[which(coef(model)==1.0)]] = 1

fftHsvm = data.frame(x=fftH, class=1)
model = ksvm(x=class~., data=fftHsvm, type="one-svc", kernel="vanilladot", nu=0.05)
OLIDfftH = numeric(dim(fftH)[1])
OLIDfftH[SVindex(model)[which(coef(model)==1.0)]] = 1

pcaDsvm = data.frame(x=pcaD, class=1)
model = ksvm(x=class~., data=pcaDsvm, type="one-svc", kernel="vanilladot", nu=0.04)
OLIDpcaD = numeric(dim(pcaD)[1])
OLIDpcaD[SVindex(model)[which(coef(model)==1.0)]] = 1

pcaHsvm = data.frame(x=pcaH, class=1)
model = ksvm(x=class~., data=pcaHsvm, type="one-svc", kernel="vanilladot", nu=0.05)
OLIDpcaH = numeric(dim(pcaH)[1])
OLIDpcaH[SVindex(model)[which(coef(model)==1.0)]] = 1

## DとHのデータを結合する
fftDor = data.frame(fftD, class=1, OL=OLIDfftD)
fftHor = data.frame(fftH, class=2, OL=OLIDfftH)
fftOR = rbind(fftDor, fftHor)

pcaDor = data.frame(pcaD, class=1, OL=OLIDpcaD)
pcaHor = data.frame(pcaH, class=2, OL=OLIDpcaH)
pcaOR = rbind(pcaDor, pcaHor)

# 10-CV用のインデックスリストの作成
CVIDfftlist = createFolds(fftOR[, 1], k=10)
CVIDpcalist = createFolds(pcaOR[, 1], k=10)

## 外れ値除去
pb <- txtProgressBar(min = 0, max = (length(CVIDfftlist) + length(CVIDpcalist)) * 2, style = 3)

### 元データ
errFFTOR = NULL
for(i in 1:length(CVIDfftlist)){
  setTxtProgressBar(pb, i)
  model = ksvm(class~., data=select(filter(fftOR[-CVIDfftlist[[i]],], OL==0), -OL),kernel="vanilladot")
  tmp = abs(select(filter(fftOR[CVIDfftlist[[i]],], OL==0), -OL)$class - round(predict(model, new=select(filter(fftOR[CVIDfftlist[[i]],], OL==0),-OL))))
  errFFTOR = c(errFFTOR, (length(tmp) - sum(tmp)) / length(tmp))
}

### PCA
errPCAOR = NULL
for(i in 1:length(CVIDpcalist)){
  setTxtProgressBar(pb, i + length(CVIDfftlist))
  model = ksvm(class~., data=select(filter(pcaOR[-CVIDpcalist[[i]],], OL==0), -OL),kernel="vanilladot")
  tmp = abs(select(filter(pcaOR[CVIDpcalist[[i]],], OL==0), -OL)$class - round(predict(model, new=select(filter(pcaOR[CVIDpcalist[[i]],], OL==0),-OL))))
  errPCAOR = c(errPCAOR, (length(tmp) - sum(tmp)) / length(tmp))
}

## 外れ値除去しない
### 元データ
errFFTAll = NULL
for(i in 1:length(CVIDfftlist)){
  setTxtProgressBar(pb, i + length(CVIDfftlist) + length(CVIDpcalist))
  model = ksvm(class~., data=select(fftOR[-CVIDfftlist[[i]],],-OL), kernel="vanilladot")
  tmp = abs(select(fftOR[CVIDfftlist[[i]],],-OL)$class - round(predict(model, new = select(fftOR[CVIDfftlist[[i]],], -OL))))
  errFFTAll = c(errFFTAll, (length(tmp) - sum(tmp)) / length(tmp))
}

### PCA
errPCAAll = NULL
for(i in 1:length(CVIDpcalist)){
  setTxtProgressBar(pb, i + length(CVIDfftlist) * 2 + length(CVIDpcalist))
  model = ksvm(class~., data=select(pcaOR[-CVIDpcalist[[i]],],-OL), kernel="vanilladot")
  tmp = abs(select(pcaOR[CVIDpcalist[[i]],],-OL)$class - round(predict(model, new = select(pcaOR[CVIDpcalist[[i]],], -OL))))
  errPCAAll = c(errPCAAll, (length(tmp) - sum(tmp)) / length(tmp))
}

print(errFFTAll)
print(errFFTOR)
print(errPCAAll)
print(errPCAOR)

print(t.test(errFFTAll, errFFTOR))
print(t.test(errPCAAll, errPCAOR))
print(t.test(errFFTOR, errPCAOR))