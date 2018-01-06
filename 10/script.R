library(igraph)

# データの読み込み
g = read.graph('data/apscoauthor.csv')
answer = read.table('data/citation_count.txt')

# ランキング
## 次数中心性の計算
d = degree(g)
## 固有ベクトル中心性の計算
e = evcent(g)$vector
## PageRankの計算
p = page.rank(g)$vector

## ランキングの作成
ans = sort(answer$V2, decreasing = TRUE, method = "s", index.return = TRUE)
ans_rnk = rank(-answer$V2)
res_d = sort(d, decreasing = TRUE, method = "s", index.return = TRUE)
res_d_rnk = rank(-d)
res_e = sort(e, decreasing = TRUE, method = "s", index.return = TRUE)
res_e_rnk = rank(-e)
res_p = sort(p, decreasing = TRUE, method = "s", index.return = TRUE)
res_p_rnk = rank(-p)

## 順位相関係数
### スピアマン
rho_d = (cor.test(ans_rnk, res_d_rnk, method="s"))$estimate; rho_d
rho_e = (cor.test(ans_rnk, res_e_rnk, method="s"))$estimate; rho_e
rho_p = (cor.test(ans_rnk, res_p_rnk, method="s"))$estimate; rho_p

### ケンドール
tau_d = (cor.test(ans_rnk, res_d_rnk, method="k"))$estimate; tau_d
tau_e = (cor.test(ans_rnk, res_e_rnk, method="k"))$estimate; tau_e
tau_p = (cor.test(ans_rnk, res_p_rnk, method="k"))$estimate; tau_p

## precision--recall curve
A = ans$ix[1:(length(ans$ix)*0.01)]
seg = 200

P_d = c(); R_d = c()
for (i in 0:seg) {
    q = 100*i/seg
    B = res_d$ix[1:(length(ans$ix)*q/100)]
    AandB = intersect(A, B)
    P_d[i] = length(AandB) / length(B)
    R_d[i] = length(AandB) / length(A)
}
plot(P_d, R_d, type="l", xlab="Recall", ylab="Precision")

P_e = c(); R_e = c()
for (i in 0:seg) {
    q = 100*i/seg
    B = res_e$ix[1:(length(ans$ix)*q/100)]
    AandB = intersect(A, B)
    P_e[i] = length(AandB) / length(B)
    R_e[i] = length(AandB) / length(A)
}
plot(P_e, R_e, type="l", xlab="Recall", ylab="Precision")

P_p = c(); R_p = c()
for (i in 0:seg) {
    q = 100*i/seg
    B = res_p$ix[1:(length(ans$ix)*q/100)]
    AandB = intersect(A, B)
    P_p[i] = length(AandB) / length(B)
    R_p[i] = length(AandB) / length(A)
}
plot(P_p, R_p, type="l", xlab="Recall", ylab="Precision")

## nDCG@100
k = 100

### 理想的なランキングにおける値
isum = 0
for(r in 1:k) {
    isum = isum + ans$x[r] / log(r+1)
}

dsum = 0
for(r in 1:k) {
    dsum = dsum + answer$V2[res_d$ix[r]] / log(r+1)
}

esum = 0
for(r in 1:k) {
    esum = esum + answer$V2[res_e$ix[r]] / log(r+1)
}

psum = 0
for(r in 1:k) {
    psum = psum + answer$V2[res_p$ix[r]] / log(r+1)
}

(dsum/isum)
(esum/isum)
(psum/isum)