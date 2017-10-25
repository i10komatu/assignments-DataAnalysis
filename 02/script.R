## 2-(1) ��l�����𑫂����킹�����̕��z�����K���z�ɋ߂Â����Ƃ��m�F����
# ������
N <- 0; data <- 0
f <- function(data, N) {
  # �`��f�o�C�X�̍쐬
  pdf(sprintf("02\\hist%02d.pdf", N), width=10, height=10)
  hist_dev = dev.cur()
  pdf(sprintf("02\\qq%02d.pdf", N), width=10, height=10)
  qq_dev = dev.cur()

  # �f�[�^�̉��Z�ƃO���t�\��
  data <- data + runif(10000)
  dev.set(hist_dev)
  hist(data)
  dev.set(qq_dev)
  qqplot(qnorm(ppoints(10000)), data)

  dev.off(hist_dev)
  dev.off(qq_dev)
  return(data)
}

# p-value���傫����ΐ��K���z�ł���Ɣ��f�ł���
N <- N + 1; data <- f(data, N); ks.test(data, "pnorm", mean(data), sd(data))

# �㏈��
rm(N, f, data)

## 4(1) �a�̖��x�֐��̎������������Ƃ��Ŗސ��肵�m�F����
data1 <-�@rnorm(1000000, mean=0.4, sd=1)
data2 <- rnorm(1000000, mean=0.6, sd=2)
data <- data1 + data2

# �Ŗސ���@ (mle) ���g�p
library(stats4)
nLL <- function(mean) -sum(dnorm(data, mean, 2, log=T))
(fit <- mle(nLL, start=list(mean=0)))

# �Ŗސ���@ (optim) ���g�p
library(stats)
nLL <- function(arg) -sum(dnorm(data, mean=arg[1], sd=arg[2], log=T))
(fit <- optim(c(0,1), nLL))$par

# �㏈��
rm(data1, data2, data, nLL)

## 6(1) MASS�p�b�P�[�W��mvrnorm()�ɂ�闐���x�N�g���n��쐬
library(MASS)

# �����x�N�g���n��쐬�֐�
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

# �`��f�o�C�X�̍쐬
pdf("02\\mvrnorm-plot.pdf", width=10, height=10)
dev = dev.cur()

# �f�[�^����
dat <- rgmix(5000)
dat <- data.frame(x=dat[,1], y=dat[,2])

# �v���b�g
dev.set(dev)
plot(dat, xlim=c(-20, 20), ylim=c(-20, 20))
dev.off(dev)

## 7(1) EM�A���S���Y���Ō���2�ϐ�3�����K�E�X�������z������ł��邩����
#install.packages("mclust")
library(mclust)

# �`��f�o�C�X�̍쐬
pdf("02\\mvrnorm-density.pdf", width=10, height=10)
dev = dev.cur()

# EM�A���S���Y���̓K�p
res <- densityMclust(dat)

# �v���b�g
dev.set(dev)
plot(res, what="density", xlim=c(-20, 20), ylim=c(-20, 20))
dev.off(dev)

# �������ꂽ�p�����[�^�̊m�F
res$parameter$pro
res$parameter$mean
res$parameter$variance$sigma

rm(rgmix, dat, res)

## 10(1) rnorm(N, mean=0.86, sd=1.3) �̕��ϒl�C���U�̋�Ԑ���
f <- function(max) {
  sdkn <- c() # ���m�̕W���΍���p�����M����� (���K���z)
  sdun <- c() # ����W���΍���p�����M����� (���K���z)
  sdkt <- c() # ���m�̕W���΍���p�����M����� (t���z)
  sdut <- c() # ����W���΍���p�����M����� (t���z)
  vux <- c() # �s�Ε��U��p������Ԑ��� (�J�C��敪�z)

  for(N in 2:max) {
    # ������
    dat <- rnorm(N, mean=0.86, sd=1.3)
    mu <- mean(dat); sigma <- sd(dat)

    # ���ϒl�̋�Ԑ���
    # ���m�̕W���΍���p�����M����� (���K���z)
    sdkn <- rbind(sdkn, (qnorm(c(0.025, 0.975)) * 1.3 / sqrt(N) + mu))
    
    # ����W���΍���p�����M����� (���K���z)
    sdun <- rbind(sdun, (qnorm(c(0.025, 0.975)) * sigma /  sqrt(N) + mu))

    # ���m�̕W���΍���p�����M����� (t���z)
    sdkt <- rbind(sdkt, (qt(c(0.025, 0.975), df=(N-1)) * 1.3 / sqrt(N) + mu))

    # ����W���΍���p�����M����� (t���z)
    sdut <- rbind(sdut, (qt(c(0.025, 0.975), df=(N-1)) * sigma /  sqrt(N) + mu))

    # ���U�̋�Ԑ���
    # �s�Ε��U��p������Ԑ��� (�J�C��敪�z)
    vux <- rbind(vux, ((N-1) * sigma**2 / qchisq(c(0.975, 0.025), df=(N-1))))
  }

  res <- list(sdkn, sdun, sdkt, sdut, vux)
  return(res)
}

# �O���t�̕ۑ�
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