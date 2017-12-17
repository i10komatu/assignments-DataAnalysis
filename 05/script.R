#install.packages("igraph")
library(igraph)

# “Ç‚İ‚İ
g1 <- read.graph("advice.txt")
g2 <- read.graph("friend.txt")
g3 <- read.graph("report.txt")

# •\¦
plot(g1)
plot(g2)
plot(g3)

# Ÿ”’†S«‚ğŒvZ
d1 <- degree(g1)
d2 <- degree(g2)
d3 <- degree(g3)

din1 <- degree(g1, mode="in")
din2 <- degree(g2, mode="in")
din3 <- degree(g3, mode="in")

dout1 <- degree(g1, mode="out")
dout2 <- degree(g2, mode="out")
dout3 <- degree(g3, mode="out")

# ‹ßÚ’†S«‚ğŒvZ
c1 <- closeness(g1)
c2 <- closeness(g2)
c3 <- closeness(g3)

# ”}‰î’†S«‚ğŒvZ
b1 <- betweenness(g1)
b2 <- betweenness(g2)
b3 <- betweenness(g3)

# ’†S«‡‚É•À‚×•Ï‚¦
sort(d1,index.return=T,decreasing=T)
sort(d2,index.return=T,decreasing=T)
sort(d3,index.return=T,decreasing=T)

# ‘ŠŠÖŒW”‚ÌŒvZ
cor(d1, d2) # •Œ¾-—Fl
cor(d2, d3) # —Fl-•ñ
cor(d3, d1) # •ñ-•Œ¾

cor(c1, c2) # •Œ¾-—Fl
cor(c2, c3) # —Fl-•ñ
cor(c3, c1) # •ñ-•Œ¾

cor(b1, b2) # •Œ¾-—Fl
cor(b2, b3) # —Fl-•ñ
cor(b3, b1) # •ñ-•Œ¾

cor(din2, dout2)
cor.test(din2, dout2)