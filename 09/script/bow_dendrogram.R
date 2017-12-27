source('indicator.R')
bow = read.table("bow.txt", sep=" ")
bow2 = bow
bow2$V1 = NULL
bow2$V2 = NULL
d = dist(bow2)
hc = hclust(d)
plot(hc)
res = cutree(hc, k=15)
ct = table(res, bow$V2)
purity(ct)
f_measure(ct)
