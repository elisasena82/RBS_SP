# Superposed Histograms
load("/home/elisa/Downloads/Dados_Estacao_IAG/t19362022.RData")
told=na.omit(subset(tdia,subset=ano<1966,select=c("ano","mes","dia","tmax","tmin","tmed","amp")))
tnew=na.omit(subset(tdia,subset=ano>1992,select=c("ano","mes","dia","tmax","tmin","tmed","amp")))
hist(told$tmax)
hist(tnew$tmax)
mean(told$tmax,na.rm=T)
mean(tnew$tmax,na.rm=T)
median(told$tmax,na.rm=T)
median(tnew$tmax,na.rm=T)
sd(told$tmax,na.rm=T)
sd(tnew$tmax,na.rm=T)
hist(told$tmin)
hist(tnew$tmin)
mean(told$tmin,na.rm=T)
mean(tnew$tmin,na.rm=T)
median(told$tmin,na.rm=T)
median(tnew$tmin,na.rm=T)
sd(told$tmin,na.rm=T)
sd(tnew$tmin,na.rm=T)
mean(told$tmed,na.rm=T)
mean(tnew$tmed,na.rm=T)
sd(told$tmed,na.rm=T)
sd(tnew$tmed,na.rm=T)
mean(told$amp,na.rm=T)
mean(tnew$amp,na.rm=T)
sd(told$amp,na.rm=T)
sd(tnew$amp,na.rm=T)

qqnorm(told$tmax)
qqnorm(tnew$tmax)
qqnorm(told$tmin)
qqnorm(tnew$tmin)
qqnorm(told$tmed)
qqnorm(tnew$tmed)
qqnorm(told$amp)
qqnorm(tnew$amp)

library(HistogramTools)
pdf("/home/elisa/Downloads/Dados_Estacao_IAG/Hist_Temps.pdf",width=12,height=8)
par(mfrow=c(2,2))
hx=hist(told$tmax,plot=F)
PlotRelativeFrequency(hx,col=rgb(1,0,0,0.5), main="",xlab="Tmax (ºC)",ylim=c(0,0.2),xlim=c(5,38))
hy=hist(tnew$tmax,plot=F)
PlotRelativeFrequency(hy,col=rgb(0,0,1,0.5),add=T)
legend("topleft",c("1936-1965","1993-2022"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),bty="n")
# legend("topleft",c("1936-1965","1993-2022"),col=c("red","blue"),lwd=3,bty="n")
box()

hx=hist(told$tmin,plot=F)
PlotRelativeFrequency(hx,col=rgb(1,0,0,0.5), main="",xlab="Tmin (ºC)",ylim=c(0,0.22),xlim=c(0,25))
hy=hist(tnew$tmin,plot=F)
PlotRelativeFrequency(hy,col=rgb(0,0,1,0.5),add=T)
legend("topleft",c("1936-1965","1993-2022"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),bty="n")
box()

hx=hist(told$tmed,plot=F)
PlotRelativeFrequency(hx,col=rgb(1,0,0,0.5), main="",xlab="Tmean (ºC)",ylim=c(0,0.22),xlim=c(4,30))
hy=hist(tnew$tmed,plot=F)
PlotRelativeFrequency(hy,col=rgb(0,0,1,0.5),add=T)
legend("topleft",c("1936-1965","1993-2022"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),bty="n")
box()

hx=hist(told$amp,plot=F)
PlotRelativeFrequency(hx,col=rgb(1,0,0,0.5), main="",xlab="TDR (ºC)",ylim=c(0,0.22),xlim=c(0,27))
hy=hist(tnew$amp,plot=F)
PlotRelativeFrequency(hy,col=rgb(0,0,1,0.5),add=T)
box()
legend("topleft",c("1936-1965","1993-2022"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),bty="n")
dev.off()

vmax=tnew$tmax-(mean(tnew$tmax,na.rm=T)-mean(told$tmax,na.rm=T))
vmin=tnew$tmin-(mean(tnew$tmin,na.rm=T)-mean(told$tmin,na.rm=T))
vmed=tnew$tmed-(mean(tnew$tmed,na.rm=T)-mean(told$tmed,na.rm=T))
vamp=tnew$amp-(mean(tnew$amp,na.rm=T)-mean(told$amp,na.rm=T))
# KS test
ks.test(told$tmax,tnew$tmax)
ks.test(told$tmax,vmax)
ks.test(told$tmin,tnew$tmin)
ks.test(told$tmin,vmin)
ks.test(told$tmed,tnew$tmed)
ks.test(told$tmed,vmed)
ks.test(told$amp,tnew$amp)
ks.test(told$amp,vamp)

pold=subset(iagdia,subset=ano<1963 & altura>0.1)
pnew=subset(iagdia,subset=ano>1992 & altura>0.1)

hx=hist(pold$altura,plot=F)
PlotRelativeFrequency(hx,col=rgb(1,0,0,0.5), main="",xlab="Precipitation (mm)")
hy=hist(pnew$altura,plot=F)
PlotRelativeFrequency(hy,col=rgb(0,0,1,0.5),add=T)
box()

hx=hist(pold$duracao,plot=F)
PlotRelativeFrequency(hx,col=rgb(1,0,0,0.5), main="",xlab="Duration (min)")
hy=hist(pnew$duracao,plot=F)
PlotRelativeFrequency(hy,col=rgb(0,0,1,0.5),add=T)
box()

hx=hist(pold$index,plot=F,breaks=17)
PlotRelativeFrequency(hx,col=rgb(1,0,0,0.5), main="",xlab="Height/Duration (mm/min)")
hy=hist(pnew$index,plot=F)
PlotRelativeFrequency(hy,col=rgb(0,0,1,0.5),add=T)
box()

hx=hist(pold$maxalth,plot=F,breaks=17)
PlotRelativeFrequency(hx,col=rgb(1,0,0,0.5), main="",xlab="Maximum daily precipitation (mm)")
hy=hist(pnew$maxalth,plot=F)
PlotRelativeFrequency(hy,col=rgb(0,0,1,0.5),add=T)
box()

ks.test(pold$altura,pnew$altura)
ks.test(pold$duracao,pnew$duracao)
ks.test(pold$maxalth,pnew$maxalth)
ks.test(pold$index,pnew$index)

# By month
toldmm=subset(iagdia,subset=ano<1966 & mes==5)
tnewmm=subset(iagdia,subset=ano>1992 & mes==5)
vmin=tnewmm$tmin-(mean(tnewmm$tmin,na.rm=T)-mean(toldmm$tmin,na.rm=T))
ks.test(toldmm$tmin,tnewmm$tmin)
ks.test(toldmm$tmin,vmin)

hx=hist(toldmm$tmin,plot=F)
hy=hist(tnewmm$tmin,plot=F)
PlotRelativeFrequency(hx,col=rgb(1,0,0,0.5), main="",xlab="Tmin (ºC)",ylim=c(0,0.01+max((hx$counts/sum(hx$counts)),(hy$counts/sum(hy$counts)))),xlim=c(hx$breaks[1]-1,1+hy$breaks[length(hy$breaks)]))
PlotRelativeFrequency(hy,col=rgb(0,0,1,0.5),add=T)
box()

# Distribution parameters to analyse
# Mean, SD, Median, Mode, Kurtosis, Skewness
# KS-test

library(moments)
skewness(toldmm$tmin,na.rm=T)
skewness(tnewmm$tmin,na.rm=T)
kurtosis(toldmm$tmin,na.rm=T)
kurtosis(tnewmm$tmin,na.rm=T)

skewness(told$tmin,na.rm=T)
skewness(tnew$tmin,na.rm=T)
kurtosis(told$tmin,na.rm=T)-3
kurtosis(tnew$tmin,na.rm=T)-3

skewness(told$tmax,na.rm=T)
skewness(tnew$tmax,na.rm=T)
kurtosis(told$tmax,na.rm=T)-3
kurtosis(tnew$tmax,na.rm=T)-3

# Standard errors
nold=nrow(told)
nnew=nrow(tnew)
sesdoldtmax=sd(told$tmax,na.rm=T)/sqrt(2*nold)
sesdnewtmax=sd(tnew$tmax,na.rm=T)/sqrt(2*nnew)
sesdoldtmin=sd(told$tmin,na.rm=T)/sqrt(2*nold)
sesdnewtmin=sd(tnew$tmin,na.rm=T)/sqrt(2*nnew)
seskewold=sqrt(6/nold)
seskewnew=sqrt(6/nnew)
sekurtold=sqrt(24/nold)
sekurtnew=sqrt(24/nnew)

a=rnorm(365,25,4)
b=rnorm(365,30,4)
c=rnorm(365,35,4)
p10=quantile(a,0.1)
p90=quantile(a,0.9)
sd(a[a<p10])
sd(b[b<p10])
sd(c[c<p10])
sd(a[a>p90])
sd(b[b>p90])
sd(c[c>p90])
length(a[a<p10])
length(b[b<p10])
length(c[c<p10])
length(a[a>p90])
length(b[b>p90])
length(c[c>p90])

# Cumulative distributions KS
compare <- function(x, y) {
  n <- length(x); m <- length(y)
  w <- c(x, y)
  o <- order(w)
  z <- cumsum(ifelse(o <= n, m, -n))
  i <- which.max(abs(z))
  w[o[i]]
}

pdf("/home/elisa/Downloads/Dados_Estacao_IAG/KS_early_recent_periods_paper.pdf",width=12,height=5)
par(mfrow=c(1,2), font.lab = 1, font.axis=1, font.main=2, cex.main=0.2, cex.lab=1.5, cex.axis=1.5)
v1=na.omit(told$tmax)
v2=na.omit(tnew$tmax)
u <- compare(v1,v2)
e.x <- ecdf(v1)
e.y <- ecdf(v2)
abs(e.x(u) - e.y(u))
ks.test(v1,v2)$statistic
plot(ecdf(v1), verticals=T, col = "red",xlab="Tmax (ºC)",
     ylab="Cumulative frequency",main="") 
# mtext("c)", at=c(-7,1.1),cex=1.5)
plot(ecdf(v2), verticals=T, add = TRUE, lty = "dashed", col = "dark red") 
lines(c(u,u), c(0,1), col="Gray")
lines(c(u,u), c(e.x(u), e.y(u)), lwd=3)
legend("topleft", legend=c("1936-1965", "1993-2022"), col=c("red","dark red"), lty=1, lwd=c(3,3), cex=1.5, bty="n")
text(x=23,y=0.52,"D",col="black",cex=1.75,font=2)
text(8, 0.6, "D=0.15", cex=1.5)
text(11.6, 0.5, expression(p-value < 10^-16), cex=1.5)

v1=na.omit(told$tmin)
v2=na.omit(tnew$tmin)
u <- compare(v1,v2)
e.x <- ecdf(v1)
e.y <- ecdf(v2)
abs(e.x(u) - e.y(u))
ks.test(v1,v2)$statistic
plot(ecdf(v1), verticals=T, col = "blue",xlab="Tmin (ºC)",
     ylab="Cumulative frequency", main="") 
# mtext("d)", at=c(-7,1.1),cex=1.5)
plot(ecdf(v2), verticals=T, add = TRUE, lty = "dashed", col = "dark blue") 
lines(c(u,u), c(0,1), col="Gray")
lines(c(u,u), c(e.x(u), e.y(u)), lwd=3)
legend("topleft", legend=c("1936-1965", "1993-2022"), col=c("blue","dark blue"), lty=1, lwd=c(3,3), cex=1.5, bty="n")
text(x=13,y=0.6,"D",col="black",cex=1.75,font=2)
text(0, 0.6, "D=0.15", cex=1.5)
text(2.9, 0.5, expression(p-value < 10^-16), cex=1.5)
dev.off()

pdf("/home/elisa/Downloads/Dados_Estacao_IAG/KS_Tmax_transformed_dist_early_recent_final_v2.pdf",width=6,height=4.5)
par(mfrow=c(1,1), font.lab = 1, font.axis=1, font.main=2, cex.main=0.2, cex.lab=1.5, cex.axis=1.5,mar = c(6,6,1,1))
v1=na.omit(told$tmax)
v2=na.omit(vmax)
u <- compare(v1,v2)
e.x <- ecdf(v1)
e.y <- ecdf(v2)
abs(e.x(u) - e.y(u))
ks.test(v1,v2)$statistic
plot(ecdf(v1), verticals=T, col = "red",xlab="Tmax (ºC)",
     ylab="Cumulative frequency", main="") 
plot(ecdf(v2), verticals=T, add = TRUE, lty = "dashed", col = "dark red") 
# lines(c(u,u), c(0,1), col="Gray")
lines(c(u,u), c(e.x(u), e.y(u)), lwd=2)
legend("bottomright", legend=c("1936-1965", "1993-2022"), col=c("red","dark red"), lty=1, lwd=3, cex=1.5, bty="n")
# text(x=25,y=0.52,"DTMax",col="red",cex=1.75,font=2)
lines(c(u,18.5),c(e.x(u),0.93),col="dark green",lwd=2,lty=2)
lines(c(u,18.5),c(e.y(u),0.57),col="dark green",lwd=2,lty=2)
text(31, 0.52, "D=0.02", cex=1.6)
text(32, 0.4, "p-value=0.03", cex=1.6)

# Zoom image
par(fig = c(grconvertX(c(10, 20), from="user", to="ndc"),
            grconvertY(c(0.5, 1), from="user", to="ndc")),
    mar = c(1,0,1,1),
    new = TRUE)
plot(ecdf(v1), xlim=c(25,25.4), ylim=c(0.53,0.57), verticals=T, col = "red",xlab="",
     ylab="",main="", lty = "dashed")
plot(ecdf(v2), verticals=T, add = TRUE, lty = "dashed", col = "dark red") 
lines(c(u,u), c(e.x(u), e.y(u)), lwd=2)
dev.off()

pdf("/home/elisa/Downloads/Dados_Estacao_IAG/KS_Tmin_transformed_dist_early_recent_final_v2.pdf",width=6,height=4.5)
par(mfrow=c(1,1), font.lab = 1, font.axis=1, font.main=2, cex.main=0.2, cex.lab=1.5, cex.axis=1.5,mar = c(6,6,1,1))
v1=na.omit(told$tmin)
v2=na.omit(vmin)
u <- compare(v1,v2)
e.x <- ecdf(v1)
e.y <- ecdf(v2)
abs(e.x(u) - e.y(u))
ks.test(v1,v2)$statistic
plot(ecdf(v1), verticals=T, col = "blue",xlab="Tmin (ºC)",
     ylab="Cumulative frequency", main="") 
plot(ecdf(v2), verticals=T, add = TRUE, lty = "dashed", col = "dark blue") 
# lines(c(u,u), c(0,1), col="Gray")
lines(c(u,u), c(e.x(u), e.y(u)), lwd=2)
legend("bottomright", legend=c("1936-1965", "1993-2022"), col=c("blue","dark blue"), lty=1, lwd=3, cex=1.5, bty="n")
# text(x=10,y=0.52,"DTMin",col="red",cex=1.75,font=2)
lines(c(u,9),c(e.x(u),0.56),col="dark green",lwd=2,lty=2)
lines(c(u,9),c(e.y(u),0.93),col="dark green",lwd=2,lty=2)
text(20, 0.6, "D=0.03", cex=1.6)
text(18.5, 0.3, "p-value=0.003", cex=1.6)

# Zoom image
par(fig = c(grconvertX(c(0, 10), from="user", to="ndc"),
            grconvertY(c(0.5, 1), from="user", to="ndc")),
    mar = c(1,0,1,1),
    new = TRUE)
plot(ecdf(v1), xlim=c(17.55,17.75), ylim=c(0.84,0.89), verticals=T, col = "blue",xlab="",
     ylab="",main="", lty = "dashed")
plot(ecdf(v2), verticals=T, add = TRUE, lty = "dashed", col = "dark blue") 
lines(c(u,u), c(e.x(u), e.y(u)), lwd=2)
dev.off()

cairo_pdf("/home/elisa/Downloads/Dados_Estacao_IAG/Hist_paper_final_Tmax.pdf",width=4,height=3.5)
hx=hist(told$tmax,plot=F)
PlotRelativeFrequency(hx,col=rgb(1,0,0,0.5), main="", xlab="Tmax (ºC)",ylim=c(0,0.22),xlim=c(5,38))
hy=hist(tnew$tmax,plot=F)
PlotRelativeFrequency(hy,col=rgb(0,0,1,0.5),add=T)
legend("topleft",c("1936-1965","1993-2022"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),bty="n")
box()
text(12, 0.13, expression(Delta*bar("T")[Max]==1.5*" "^o*C), cex=1.0)
# mtext("a)", at=c(-5,0.2),cex=1.1,font=1)
dev.off()

cairo_pdf("/home/elisa/Downloads/Dados_Estacao_IAG/Hist_paper_final_Tmin.pdf",width=4,height=3.5)
hx=hist(told$tmin,plot=F)
PlotRelativeFrequency(hx,col=rgb(1,0,0,0.5), main="", xlab="Tmin (ºC)",ylim=c(0,0.24),xlim=c(-5,25))
hy=hist(tnew$tmin,plot=F)
PlotRelativeFrequency(hy,col=rgb(0,0,1,0.5),add=T)
legend("topleft",c("1936-1965","1993-2022"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),bty="n")
box()
text(1.5, 0.14, expression(Delta*bar(T)[Min]==1.5*" "^o*C), cex=1.0)
# mtext("b)", at=c(-5,0.2),cex=1.1,font=1)
dev.off()