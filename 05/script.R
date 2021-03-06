#install.packages("igraph")
library(igraph)

# �ǂݍ���
g1 <- read.graph("advice.txt")
g2 <- read.graph("friend.txt")
g3 <- read.graph("report.txt")

# �\��
plot(g1)
plot(g2)
plot(g3)

# �������S�����v�Z
d1 <- degree(g1)
d2 <- degree(g2)
d3 <- degree(g3)

din1 <- degree(g1, mode="in")
din2 <- degree(g2, mode="in")
din3 <- degree(g3, mode="in")

dout1 <- degree(g1, mode="out")
dout2 <- degree(g2, mode="out")
dout3 <- degree(g3, mode="out")

# �ߐڒ��S�����v�Z
c1 <- closeness(g1)
c2 <- closeness(g2)
c3 <- closeness(g3)

# �}��S�����v�Z
b1 <- betweenness(g1)
b2 <- betweenness(g2)
b3 <- betweenness(g3)

# ���S�����ɕ��וς�
sort(d1,index.return=T,decreasing=T)
sort(d2,index.return=T,decreasing=T)
sort(d3,index.return=T,decreasing=T)

# ���֌W���̌v�Z
cor(d1, d2) # ����-�F�l
cor(d2, d3) # �F�l-��
cor(d3, d1) # ��-����

cor(c1, c2) # ����-�F�l
cor(c2, c3) # �F�l-��
cor(c3, c1) # ��-����

cor(b1, b2) # ����-�F�l
cor(b2, b3) # �F�l-��
cor(b3, b1) # ��-����

cor(din2, dout2)
cor.test(din2, dout2)