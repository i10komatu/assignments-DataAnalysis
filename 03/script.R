### 1. data-2d.txt�̃f�[�^���听������
# �f�[�^�ǂݍ���
dat <- read.csv("data-2d.txt", header=F)
dat.pca <- prcomp(dat, center = T, retx = T)

# �O���t�`��
{
plot(c(), xlim=c(-10, 10), ylim=c(-10, 10), type="n")
points(dat[,1], dat[,2])
points(dat.pca$x[,1], dat.pca$x[,2], col="red")
points(-dat.pca$x[,2], dat.pca$x[,1], col="blue")
}

## (a) ��]�p�𐄒�
# ����2�ϐ����K���z���听�����͂���ƁCy�����厲�ƂȂ�͂��ł��邽�߁C90�x����
((atan2(dat.pca$rotation[2,1], dat.pca$rotation[1,1])/pi*180)-90)

## (b) �f�[�^���厲��Ɏˉe
hist(dat.pca$x[,1], breaks=40)

## (c) (x,y)���W�n�ɂ�����x,y�����̑��֌W��
cor(dat[,1], dat[,2])

## (d) (p,q)���W�n�ɂ�����p,q�����̑��֌W��
cor(dat.pca$x[,1], dat.pca$x[,2])

### 2. data-5d.txt�̃f�[�^���听�����͂��C�U�z�}��\��
dat <- read.csv("data-5d.txt", header=F)
dat.pca <- prcomp(dat, center = T, retx = T)
plot(dat.pca$x[,1], dat.pca$x[,2])

### 3. t����
N = 10000
exist.mean = 15.1; exist.sd = 5; prop.mean = 15; prop.sd = 5;
exist.time <- rnorm(N, mean=exist.mean, sd=exist.sd)
prop.time <- rnorm(N, mean=prop.mean, sd=prop.sd)
(res = t.test(exist.time, prop.time, alternative="two.sided", var.equal=T, conf.level=0.95))
# �L�Ӑ�����5%�Ƃ���
(res$p.value < 0.05) # �A�����������p���Ă��悢���H�L�Ӎ�������(��������)���H