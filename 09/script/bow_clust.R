source('indicator.R')
bow = read.table("bow.txt", sep=" ")
bow2 = bow
bow2$V1 = NULL
bow2$V2 = NULL
km = kmeans(bow2, 15)
ct = table(km$cluster, bow$V2)
purity(ct)
f_measure(ct)
