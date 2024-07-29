library(readxl)
Tp= read_excel("TNp_Idx_SP_GRL.xlsx",sheet = "Tp")

rhtx90=tail(records(Tp$TX90,do.plot=F),n=1)
rltx90=tail(records(-Tp$TX90,do.plot=F),n=1)
rhtx10=tail(records(Tp$TX10,do.plot=F),n=1)
rltx10=tail(records(-Tp$TX10,do.plot=F),n=1)
rhtn90=tail(records(Tp$TN90,do.plot=F),n=1)
rltn90=tail(records(-Tp$TN90,do.plot=F),n=1)
rhtn10=tail(records(Tp$TN10,do.plot=F),n=1)
rltn10=tail(records(-Tp$TN10,do.plot=F),n=1)

library(evir)
notrd=function(y,x,db) {
  model=lm(y~x,data=db)
  tmednotrd=y-model$coefficients[2]*x-model$coefficients[1]
  nhfwd=tail(records(tmednotrd,do.plot=F),n=1)
  nlfwd=tail(records(-tmednotrd,do.plot=F),n=1)
  nhbwd=tail(records(rev(tmednotrd),do.plot=F),n=1)
  nlbwd=tail(records(rev(-tmednotrd),do.plot=F),n=1)
  alpha=nhfwd$number+nlfwd$number-(nhbwd$number+nlbwd$number)
  l1=list("ano"=x,"ynotd"=tmednotrd,
          "nhfwd"=nhfwd,"nlfwd"=nlfwd,
          "nhbwd"=nhbwd,"nlbwd"=nlbwd,"alpha"=alpha)
  return(l1)
}

# Novo rho para remover efeitos de heteroscedasticidade
# Tenho que olhar para indices forward e
# backwards sem remover tendência na média
rhtx90bwd=tail(records(rev(Tp$TX90),do.plot=F),n=1)
rltx90bwd=tail(records(rev(-Tp$TX90),do.plot=F),n=1)
rhtx10bwd=tail(records(rev(Tp$TX10),do.plot=F),n=1)
rltx10bwd=tail(records(rev(-Tp$TX10),do.plot=F),n=1)
rhtn90bwd=tail(records(rev(Tp$TN90),do.plot=F),n=1)
rltn90bwd=tail(records(rev(-Tp$TN90),do.plot=F),n=1)
rhtn10bwd=tail(records(rev(Tp$TN10),do.plot=F),n=1)
rltn10bwd=tail(records(rev(-Tp$TN10),do.plot=F),n=1)

# Alpha
alphatx90=notrd(Tp$TX90,Tp$YEAR,Tp)
alphatx90$alpha
alphatx10=notrd(Tp$TX10,Tp$YEAR,Tp)
alphatx10$alpha
alphatn90=notrd(Tp$TN90,Tp$YEAR,Tp)
alphatn90$alpha
alphatn10=notrd(Tp$TN10,Tp$YEAR,Tp)
alphatn10$alpha

pdf("/home/elisa/Downloads/Dados_Estacao_IAG/Plots_Extremes_temp_paper_final.pdf", width=8,height=6)
par(mfrow=c(2,2), mar=c(4, 4.1, 2, 1), font.lab = 1, font.axis=1, font.main=2, cex.main=1, cex.lab=1, cex.axis=1)
plot(Tp$YEAR,Tp$TX10,xlab="Year",ylab="Very cold days: TX10p",lwd=1.5)
r=records(Tp$TX10,do.plot=F)
points(r$trial+1935,r$record,col="red",pch=19)
r=records(-Tp$TX10,do.plot=F)
points(r$trial+1935,-r$record,col="blue",pch=4,lwd=2.5)
text(2015, 65, expression(rho == 0.00), cex=1.25)
text(2007, 60, expression(rho[bwd] == -1.01), cex=1.25)
text(2012, 55, expression(alpha == -10), cex=1.25)
mtext("a)", at=c(1915,90),cex=1.1,font=1)

plot(Tp$YEAR,Tp$TX90,xlab="Year",ylab="Very warm days: TX90p",lwd=1.5)
r=records(Tp$TX90,do.plot=F)
points(r$trial+1935,r$record,col="red",pch=19)
r=records(-Tp$TX90,do.plot=F)
points(r$trial+1935,-r$record,col="blue",pch=4,lwd=2.5)
text(1947, 93, expression(rho == 0.85), cex=1.25)
text(1950, 85, expression(rho[bwd] == 1.25), cex=1.25)
text(1943, 77, expression(alpha == 1), cex=1.25)
mtext("b)", at=c(1915,90),cex=1.1,font=1)

plot(Tp$YEAR,Tp$TN10,xlab="Year",ylab="Very cold nights: TN10p",lwd=1.5)
r=records(Tp$TN10,do.plot=F)
points(r$trial+1935,r$record,col="red",pch=19)
r=records(-Tp$TN10,do.plot=F)
points(r$trial+1935,-r$record,col="blue",pch=4,lwd=2.5)
text(2010, 80, expression(rho == -0.22), cex=1.25)
text(2007, 72, expression(rho[bwd] == -0.61), cex=1.25)
text(2013, 66, expression(alpha == -7), cex=1.25)
mtext("c)", at=c(1915,90),cex=1.1,font=1)

plot(Tp$YEAR,Tp$TN90,xlab="Year",ylab="Very warm nights: TN90p",lwd=1.5)
r=records(Tp$TN90,do.plot=F)
points(r$trial+1935,r$record,col="red",pch=19)
r=records(-Tp$TN90,do.plot=F)
points(r$trial+1935,-r$record,col="blue",pch=4,lwd=2.5)
text(1947, 80, expression(rho == 2.56), cex=1.25)
text(1950, 73, expression(rho[bwd] == 0.34), cex=1.25)
text(1944, 66, expression(alpha == 6), cex=1.25)
mtext("d)", at=c(1915,90),cex=1.1,font=1)
dev.off()
