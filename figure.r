# Figure 1 - Completeness of Death Counts

dat=read.table('cobertura_homens.txt', header=T,sep='\t')
View(dat)
names(dat)

limty=range(dat[,-1], na.rm=T)

plot(dat$Country,dat$Chile, type='h', lwd=2, lty=1, col='black', ylim=limty, ann=F)
lines(dat$Country,dat$Brazil, lwd=2, lty=2, col='black')
lines(dat$Country,dat$Argentina, lwd=2, lty=3, col='black')
lines(dat$Country,dat$Colombia, lwd=2, lty=4, col='black')
lines(dat$Country,dat$Costa.Rica, lwd=2, lty=5, col='black')
lines(dat$Country,dat$Cuba, lwd=2, lty=6, col='grey50')
lines(dat$Country,dat$Guatemala, lwd=2, lty=5, col='grey50')
lines(dat$Country,dat$Mexico, lwd=2, lty=4, col='grey50')
lines(dat$Country,dat$Panama, lwd=2, lty=3, col='grey50')
lines(dat$Country,dat$Peru, lwd=2, lty=2, col='grey50')
lines(dat$Country,dat$Puerto.Rico, lwd=2, lty=1, col='grey50')

dat$Country

png("Figura 1.png", width=13, height=7.5, unit='in', res=300)

barplot(as.matrix(dat[,-1]), beside=T, space=c(0,4), 
        xlab="Country", ylab="Completeness of Death", col=c('grey9','grey19',  
'grey29','grey39', 'grey49', 'grey59','grey69','grey79','grey89','grey99'), cex.names=.8,
names.arg=c('Argentina', 'Brazil', 'Chile', 'Colombia','Costa\n Rica','Cuba','Guatemala',
'Mexico','Panama','Peru','Puerto \n Rico','Rep.\n Dominicana','Uruguay'))
legend(160,1.60, legend=c('1920', '1930', '1940', '1950', '1960', '1970', '1980', '1990', '2000', '2010'), 
fill=c('grey9','grey19', 'grey29','grey39', 'grey49', 'grey59','grey69','grey79','grey89','grey99'),cex=.8)
abline(h=1, lty=2, col='grey', lwd=2)
abline(h=.5, lty=2, col='grey', lwd=2)
box()

dev.off()

# Figure 2 - Survival Curves - Male and Females

dados<-read.table('dados_LA_IQR_novo.txt', sep='\t', header=T)
#write.csv(dados,"data.csv",row.names = FALSE)

View(dados)
names(dados)
unique(dados$ctry)

#501 502 503 504 505 506 507 508 509 510 511 512 513
#Argentina, Brasil, Chile, Colombia, Costa Rica, Cuba, Dominican Republic, Mexico, Uruguai, Panama, Peru, Puerto Rico, Guatemala

##Separando os dados###
#dat.m<-subset(dados, sex=='m')
dat.f<-subset(dados, sex=='f')

##Retirando os missings dos dados###
#dat.m<-subset(dat.m, Sxstd!="NA")
dat.f<-subset(dat.f,Sxstd!="NA")

cod<-factor(dat.f$ctryname)
tab<-split(dat.f,cod)

#unique(tab[[]]$year)

###

png("Figura3_females_S(x)_novo.png", width=12, height=10, unit='in', res=300)

#windows(width=11, height=10)
m <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,14,14),nrow = 4,ncol = 4,byrow = TRUE)
par(mar=c(2, 2, 2, 1))
layout(mat = m, heights = c(0.5,0.5,0.5,0.5))

#Argentina
with(tab[['Argentina ']][tab[['Argentina ']]$year==1970,],
     plot(age,Sxstd,xlab='', ylab="", type='l', lwd=1, col='purple'));
with(tab[['Argentina ']][tab[['Argentina ']]$year==1980,],lines(age,Sxstd,lwd=1, col='blue'));
with(tab[['Argentina ']][tab[['Argentina ']]$year==1991,],lines(age,Sxstd,lwd=1, col='grey'));
with(tab[['Argentina ']][tab[['Argentina ']]$year==2001,],lines(age,Sxstd,lwd=1, col='brown')); 
with(tab[['Argentina ']][tab[['Argentina ']]$year==2010,],lines(age,Sxstd,lwd=1, col='black')); 
legend(3,0.8, c("1970","1980","1991","2000","2010"), lty=c(1), bty='n',  
       cex=1, col=c("purple","blue","grey",'brown',"black"), lwd=1)
mtext("Argentina", cex=1.2)

#Brasil
with(tab[['Brasil']][tab[['Brasil']]$year==1980,],
     plot(age,Sxstd,xlab='Age Group', ylab="Survival Function", type='l', lwd=1, col='blue'));
with(tab[['Brasil']][tab[['Brasil']]$year==1991,],lines(age,Sxstd,lwd=1, col='grey'));
with(tab[['Brasil']][tab[['Brasil']]$year==2000,],lines(age,Sxstd,lwd=1, col='brown'));
with(tab[['Brasil']][tab[['Brasil']]$year==2010,],lines(age,Sxstd,lwd=1, col='black')); 
legend(3,0.8, c("1980","1991","2000","2010"), lty=c(1), bty='n',  
       cex=1, col=c("forestgreen","red","blue","black"), lwd=1)
mtext("Brazil", cex=1.2)

##Chile
with(tab[['Chile']][tab[['Chile']]$year==1920,],
     plot(age,Sxstd,xlab='Age Group', ylab="Survival Function", type='l', lwd=1, col='yellow4'));
with(tab[['Chile']][tab[['Chile']]$year==1930,],lines(age,Sxstd,lwd=1, col='orange'));
with(tab[['Chile']][tab[['Chile']]$year==1940,],lines(age,Sxstd,lwd=1, col='red'));
with(tab[['Chile']][tab[['Chile']]$year==1952,],lines(age,Sxstd,lwd=1, col='green'));
with(tab[['Chile']][tab[['Chile']]$year==1960,],lines(age,Sxstd,lwd=1, col='forestgreen'));
with(tab[['Chile']][tab[['Chile']]$year==1970,],lines(age,Sxstd,lwd=1, col='purple'));
with(tab[['Chile']][tab[['Chile']]$year==1982,],lines(age,Sxstd,lwd=1, col='blue'));
with(tab[['Chile']][tab[['Chile']]$year==1992,],lines(age,Sxstd,lwd=1, col='grey'));
with(tab[['Chile']][tab[['Chile']]$year==2002,],lines(age,Sxstd,lwd=1, col='brown')); 
with(tab[['Chile']][tab[['Chile']]$year==2012,],lines(age,Sxstd,lwd=1, col='black')); 
legend(3,0.8, c("1920","1930","1940","1952","1960","1970","1982",'1992',"2002","2012"), lty=c(1), bty='n',  
       cex=1, col=c('yellow4','orange',"red",'green',"forestgreen",'purple',"blue",'grey','brown',"black"), lwd=1)
mtext("Chile", cex=1.2)

##Colombia
with(tab[['Colombia']][tab[['Colombia']]$year==1964,],
     plot(age,Sxstd,xlab='Age Group', ylab="Survival Function", type='l', lwd=1, col='forestgreen'));
with(tab[['Colombia']][tab[['Colombia']]$year==1973,],lines(age,Sxstd,lwd=1, col='purple'));
with(tab[['Colombia']][tab[['Colombia']]$year==1985,],lines(age,Sxstd,lwd=1, col='blue'));
with(tab[['Colombia']][tab[['Colombia']]$year==1993,],lines(age,Sxstd,lwd=1, col='grey'));
with(tab[['Colombia']][tab[['Colombia']]$year==2005,],lines(age,Sxstd,lwd=1, col='brown')); 
legend(3,0.8, c("1964","1973","1985",'1993',"2002"), lty=c(1), bty='n',  
       cex=1, col=c("forestgreen",'purple',"blue",'grey','brown'), lwd=1)
mtext("Colombia", cex=1.2)

##Costa Rica
with(tab[['Costa Rica']][tab[['Costa Rica']]$year==1963,],
     plot(age,Sxstd,xlab='Age Group', ylab="Survival Function", type='l', lwd=1, col='forestgreen'));
with(tab[['Costa Rica']][tab[['Costa Rica']]$year==1973,],lines(age,Sxstd,lwd=1, col='purple'));
with(tab[['Costa Rica']][tab[['Costa Rica']]$year==1984,],lines(age,Sxstd,lwd=1, col='blue'));
with(tab[['Costa Rica']][tab[['Costa Rica']]$year==2000,],lines(age,Sxstd,lwd=1, col='brown')); 
legend(3,0.8, c("1963","1973","1984","2000"), lty=c(1), bty='n',  
       cex=1, col=c("forestgreen",'purple',"blue",'brown'), lwd=1)
mtext("Costa Rica", cex=1.2)

##Cuba
with(tab[['Cuba']][tab[['Cuba']]$year==1970,],
     plot(age,Sxstd,xlab='Age Group', ylab="Survival Function", type='l', lwd=1, col='purple'));
with(tab[['Cuba']][tab[['Cuba']]$year==1981,],lines(age,Sxstd,lwd=1, col='blue'));
with(tab[['Cuba']][tab[['Cuba']]$year==1990,],lines(age,Sxstd,lwd=1, col='grey'));
with(tab[['Cuba']][tab[['Cuba']]$year==2000,],lines(age,Sxstd,lwd=1, col='brown')); 
with(tab[['Cuba']][tab[['Cuba']]$year==2010,],lines(age,Sxstd,lwd=1, col='black')); 
legend(3,0.8, c("1970","1980","1991","2001","2010"), lty=c(1), bty='n',  
       cex=1, col=c("purple","blue","grey",'brown',"black"), lwd=1)
mtext("Cuba", cex=1.2)

##Guatemala
with(tab[['Guatemala']][tab[['Guatemala']]$year==1964,],
     plot(age,Sxstd,xlab='Age Group', ylab="Survival Function", type='l', lwd=1, col='forestgreen'));
with(tab[['Guatemala']][tab[['Guatemala']]$year==1973,],lines(age,Sxstd,lwd=1, col='purple'));
with(tab[['Guatemala']][tab[['Guatemala']]$year==1981,],lines(age,Sxstd,lwd=1, col='blue'));
with(tab[['Guatemala']][tab[['Guatemala']]$year==1994,],lines(age,Sxstd,lwd=1, col='grey'));
legend(3,0.8, c("1964","1973","1981",'1994'), lty=c(1), bty='n',  
       cex=1, col=c("forestgreen",'purple',"blue",'grey'), lwd=1)
mtext("Guatemala", cex=1.2)

##Mexico 
with(tab[['Mexico']][tab[['Mexico']]$year==1940,],
     plot(age,Sxstd,xlab='Age Group', ylab="Survival Function", type='l', lwd=1, col='red'));
with(tab[['Mexico']][tab[['Mexico']]$year==1950,],lines(age,Sxstd,lwd=1, col='green'));
with(tab[['Mexico']][tab[['Mexico']]$year==1960,],lines(age,Sxstd,lwd=1, col='forestgreen'));
with(tab[['Mexico']][tab[['Mexico']]$year==1970,],lines(age,Sxstd,lwd=1, col='purple'));
with(tab[['Mexico']][tab[['Mexico']]$year==1980,],lines(age,Sxstd,lwd=1, col='blue'));
with(tab[['Mexico']][tab[['Mexico']]$year==1990,],lines(age,Sxstd,lwd=1, col='grey'));
with(tab[['Mexico']][tab[['Mexico']]$year==2000,],lines(age,Sxstd,lwd=1, col='brown')); 
with(tab[['Mexico']][tab[['Mexico']]$year==2010,],lines(age,Sxstd,lwd=1, col='black')); 
legend(3,0.8, c("1940","1950","1960","1970","1980",'1990',"2000","2010"), lty=c(1), bty='n',  
       cex=1, col=c("red",'green',"forestgreen",'purple',"blue",'grey','brown',"black"), lwd=1)
mtext("Mexico", cex=1.2)

##Panama 
with(tab[['Panama']][tab[['Panama']]$year==1950,],
     plot(age,Sxstd,xlab='Age Group', ylab="Survival Function", type='l', lwd=1, col='green'));
with(tab[['Panama']][tab[['Panama']]$year==1960,],lines(age,Sxstd,lwd=1, col='forestgreen'));
with(tab[['Panama']][tab[['Panama']]$year==1970,],lines(age,Sxstd,lwd=1, col='purple'));
with(tab[['Panama']][tab[['Panama']]$year==1980,],lines(age,Sxstd,lwd=1, col='blue'));
with(tab[['Panama']][tab[['Panama']]$year==1990,],lines(age,Sxstd,lwd=1, col='grey'));
with(tab[['Panama']][tab[['Panama']]$year==2000,],lines(age,Sxstd,lwd=1, col='brown')); 
legend(3,0.8, c("1950","1960","1970","1980",'1990',"2000"), lty=c(1), bty='n',  
       cex=1, col=c('green',"forestgreen",'purple',"blue",'grey','brown'), lwd=1)
mtext("Panama", cex=1.2)

##Peru 
with(tab[['Peru']][tab[['Peru']]$year==1972,],
     plot(age,Sxstd,xlab='Age Group', ylab="Survival Function", type='l', lwd=1, col='purple'));
with(tab[['Peru']][tab[['Peru']]$year==1981,],lines(age,Sxstd,lwd=1, col='blue'));
with(tab[['Peru']][tab[['Peru']]$year==1993,],lines(age,Sxstd,lwd=1, col='grey'));
with(tab[['Peru']][tab[['Peru']]$year==2007,],lines(age,Sxstd,lwd=1, col='brown')); 
legend(3,0.8, c("1972","1981",'1993',"2007"), lty=c(1), bty='n',  
       cex=1, col=c('purple',"blue",'grey','brown'), lwd=1)
mtext("Peru", cex=1.2)

##Puerto Rico 
with(tab[['Puerto Rico']][tab[['Puerto Rico']]$year==1960,],
     plot(age,Sxstd,xlab='Age Group', ylab="Survival Function", type='l', lwd=1, col='forestgreen'));
with(tab[['Puerto Rico']][tab[['Puerto Rico']]$year==1970,],lines(age,Sxstd,lwd=1, col='purple'));
with(tab[['Puerto Rico']][tab[['Puerto Rico']]$year==1980,],lines(age,Sxstd,lwd=1, col='blue'));
with(tab[['Puerto Rico']][tab[['Puerto Rico']]$year==1990,],lines(age,Sxstd,lwd=1, col='grey'));
with(tab[['Puerto Rico']][tab[['Puerto Rico']]$year==2000,],lines(age,Sxstd,lwd=1, col='brown')); 
legend(3,0.8, c("1960","1970","1980",'1990',"2000"), lty=c(1), bty='n',  
       cex=1, col=c("forestgreen",'purple',"blue",'grey','brown'), lwd=1)
mtext("Puerto Rico", cex=1.2)

##Dominican Republic 
with(tab[['Dominican Republic']][tab[['Dominican Republic']]$year==1960,],
     plot(age,Sxstd,xlab='Age Group', ylab="Survival Function", type='l', lwd=1, col='forestgreen'));
with(tab[['Dominican Republic']][tab[['Dominican Republic']]$year==1970,],lines(age,Sxstd,lwd=1, col='purple'));
with(tab[['Dominican Republic']][tab[['Dominican Republic']]$year==1981,],lines(age,Sxstd,lwd=1, col='blue'));
with(tab[['Dominican Republic']][tab[['Dominican Republic']]$year==1993,],lines(age,Sxstd,lwd=1, col='grey'));
legend(3,0.8, c("1960","1970","1981","1993"), lty=c(1), bty='n',  
       cex=1, col=c("forestgreen",'purple',"blue",'grey'), lwd=1)
mtext("Dominican Republic", cex=1.2)

##Uruguay 
with(tab[['Uruguai']][tab[['Uruguai']]$year==1963,],
     plot(age,Sxstd,xlab='Age Group', ylab="Survival Function", type='l', lwd=1, col='forestgreen'));
with(tab[['Uruguai']][tab[['Uruguai']]$year==1975,],lines(age,Sxstd,lwd=1, col='purple'));
with(tab[['Uruguai']][tab[['Uruguai']]$year==1985,],lines(age,Sxstd,lwd=1, col='blue'));
with(tab[['Uruguai']][tab[['Uruguai']]$year==1996,],lines(age,Sxstd,lwd=1, col='grey'));
with(tab[['Uruguai']][tab[['Uruguai']]$year==2006,],lines(age,Sxstd,lwd=1, col='brown')); 
legend(3,0.8, c("1965","1975","1985",'1996',"2006"), lty=c(1), bty='n',  
       cex=1, col=c("forestgreen",'purple',"blue",'grey','brown'), lwd=1)
mtext("Uruguay", cex=1.2)

##
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "bottomleft",inset = 0,
       legend = c("Note: Horizontal and vertical axis are,  respectively, ages and \n survival probabilities (S(x)) after age of five"), cex=1.8, horiz = TRUE, bty ='n')

dev.off()

# Figure 4 - Comparison with estimates from Lambda

##Dados###
dados<-read.table('Resultados_IQR_LA_novo.txt', sep='\t', header=T)
dat<-read.table('Resultados_IQR_Lambda.txt', sep='\t', header=T)
dat.s<-read.table('Resultados_IQR_Sweden.txt', sep='\t', header=T)
dat.r<-read.table('Resultados_IQR_Russia.txt', sep='\t', header=T)
dat.b<-read.table('Resultados_IQR_Bulgaria.txt', sep='\t', header=T)


#View(dados)
names(dados)
dados$Pais

names(dat)
dat$Pais

### Paises semelhantes nas duas bases
# Arg Bra Chi Col Crc Cub Dom Gua Mex Pan Per Uru 

##Separando os dados para LHMD
dados1<-dados[dados$Pais=='Arg',];dados2<-dados[dados$Pais=='Bra',]
dados3<-dados[dados$Pais=='Chi',];dados4<-dados[dados$Pais=='Col',]
dados5<-dados[dados$Pais=='Crc',];dados6<-dados[dados$Pais=='Cub',]
dados7<-dados[dados$Pais=='Gua',];dados8<-dados[dados$Pais=='Mex',]
dados9<-dados[dados$Pais=='Pan',];dados10<-dados[dados$Pais=='Per',]
dados11<-dados[dados$Pais=='Pur',];dados12<-dados[dados$Pais=='Dom',]
dados13<-dados[dados$Pais=='Uru',]

##Separando os dados para Lambda
dat1<-dat[dat$Pais=='Arg',];dat2<-dat[dat$Pais=='Bra',]
dat3<-dat[dat$Pais=='Chi',];dat4<-dat[dat$Pais=='Col',]
dat5<-dat[dat$Pais=='Crc',];dat6<-dat[dat$Pais=='Cub',]
dat7<-dat[dat$Pais=='Gua',];dat8<-dat[dat$Pais=='Mex',]
dat9<-dat[dat$Pais=='Pan',];dat10<-dat[dat$Pais=='Per',]
dat11<-dat[dat$Pais=='Dom',];dat12<-dat[dat$Pais=='Uru',]

### Separando por sexo ###

## Homens
##LHMD
dados.arm<-subset(dados1, Sexo=='m');dados.brm<-subset(dados2, Sexo=='m')
dados.chm<-subset(dados3, Sexo=='m');dados.com<-subset(dados4, Sexo=='m')
dados.crm<-subset(dados5, Sexo=='m');dados.cum<-subset(dados6, Sexo=='m')
dados.gum<-subset(dados7, Sexo=='m');dados.mem<-subset(dados8, Sexo=='m')
dados.pam<-subset(dados9, Sexo=='m');dados.pem<-subset(dados10, Sexo=='m')
dados.pum<-subset(dados11, Sexo=='m');dados.dom<-subset(dados12, Sexo=='m');
dados.urm<-subset(dados13, Sexo=='m')
##LAMBDA
dat.arm<-subset(dat1, Sexo=='m');dat.brm<-subset(dat2, Sexo=='m')
dat.chm<-subset(dat3, Sexo=='m');dat.com<-subset(dat4, Sexo=='m')
dat.crm<-subset(dat5, Sexo=='m');dat.cum<-subset(dat6, Sexo=='m')
dat.gum<-subset(dat7, Sexo=='m');dat.mem<-subset(dat8, Sexo=='m')
dat.pam<-subset(dat9, Sexo=='m');dat.pem<-subset(dat10, Sexo=='m')
dat.dom<-subset(dat11, Sexo=='m');dat.urm<-subset(dat12, Sexo=='m')

##Mulheres
##LHMD
dados.arf<-subset(dados1, Sexo=='f');dados.brf<-subset(dados2, Sexo=='f')
dados.chf<-subset(dados3, Sexo=='f');dados.cof<-subset(dados4, Sexo=='f')
dados.crf<-subset(dados5, Sexo=='f');dados.cuf<-subset(dados6, Sexo=='f')
dados.guf<-subset(dados7, Sexo=='f');dados.mef<-subset(dados8, Sexo=='f')
dados.paf<-subset(dados9, Sexo=='f');dados.pef<-subset(dados10, Sexo=='f')
dados.puf<-subset(dados11, Sexo=='f');dados.dof<-subset(dados12, Sexo=='f')
dados.urf<-subset(dados13, Sexo=='f')
##LAMBDA 
dat.arf<-subset(dat1, Sexo=='f');dat.brf<-subset(dat2, Sexo=='f')
dat.chf<-subset(dat3, Sexo=='f');dat.cof<-subset(dat4, Sexo=='f')
dat.crf<-subset(dat5, Sexo=='f');dat.cuf<-subset(dat6, Sexo=='f')
dat.guf<-subset(dat7, Sexo=='f');dat.mef<-subset(dat8, Sexo=='f')
dat.paf<-subset(dat9, Sexo=='f');dat.pef<-subset(dat10, Sexo=='f')
dat.dof<-subset(dat11, Sexo=='f');dat.urf<-subset(dat12, Sexo=='f')

## Outros paises nao LA
##Separando os dados###
dat.s.m<-subset(dat.s, Sexo=='m' & Ano>='1920' & Ano<='2010')
dat.s.f<-subset(dat.s, Sexo=='f' & Ano>='1920' & Ano<='2010')

dat.b.m<-subset(dat.b, Sexo=='m' & Ano>='1950' & Ano<='2010')
dat.b.f<-subset(dat.b, Sexo=='f' & Ano>='1950' & Ano<='2010')

dat.r.m<-subset(dat.r, Sexo=='m' & Ano>='1960' & Ano<='2010')
dat.r.f<-subset(dat.r, Sexo=='f' & Ano>='1960' & Ano<='2010')

### Margem dos dados, para X e Y ####
#yrange=range(dados$IQR,dat$IQR,dat.s$IQR,dat.r$IQR,dat.b$IQR)
## Como os dados foram suavizados limites colocados na mão# 
yrange=c(12,35) 

#xrange=range(dados$Ano,dat$Ano)

###########################################################################
##################### New Graphs ##########################################
###########################################################################

#####################Lambda#####################
png("Figura4_IQR_novo.png", width=12, height=10, unit='in', res=300)
split.screen(figs=rbind(c(0,0.42,0.5,1),c(0.4,0.9,0.5,1),
                        c(0,0.42,0.0,0.5),c(0.4,0.9,0.0,0.5)))

#################### LAHMD #####################

##Homens - LAHMD
screen(1)
par(las=1, family='serif')
plot(lowess(dat.s.m$Ano,dat.s.m$IQR, f=0.055), type='l', pch=10, col='black', 
     lwd=2, lty=2, xlab='Year', ylab='Interquartile Range', axes=F, ylim=yrange, xlim=xrange)
lines(lowess(dat.b.m$Ano,dat.b.m$IQR, f=0.3), col='brown', lwd=2, lty=2)
lines(lowess(dat.r.m$Ano,dat.r.m$IQR, f=0.68), col='grey50', lwd=2, lty=2)
lines(lowess(dados.chm$Ano,dados.chm$IQR, f=0.35), col='blue', lwd=2,lty=2)
lines(lowess(dados.mem$Ano,dados.mem$IQR, f=0.2),col='forestgreen', lwd=2, lty=2)
lines(lowess(dados.pam$Ano,dados.pam$IQR, f=0.1), col='purple', lwd=2, lty=2)
lines(lowess(dados.com$Ano,dados.com$IQR, f=0.8), col='orange', lwd=2, lty=2)
lines(lowess(dados.urm$Ano,dados.urm$IQR, f=0.15),col='red', lwd=2, lty=2)
lines(lowess(dados.brm$Ano,dados.brm$IQR, f=0.1), col='black', lwd=2,lty=1)
lines(lowess(dados.dom$Ano,dados.dom$IQR, f=0.1),col='forestgreen', lwd=2, lty=1)
lines(lowess(dados.cum$Ano,dados.cum$IQR, f=0.3),col='orange', lwd=2, lty=1)
lines(lowess(dados.pum$Ano,dados.pum$IQR, f=0.1),col='brown', lwd=2, lty=1)
lines(lowess(dados.crm$Ano,dados.crm$IQR, f=0.8),col='grey50', lwd=2, lty=1)
lines(lowess(dados.pem$Ano,dados.pem$IQR, f=0.15),col='red', lwd=2, lty=1)
lines(lowess(dados.arm$Ano,dados.arm$IQR, f=0.1),col='purple', lwd=2, lty=1)
lines(lowess(dados.gum$Ano,dados.gum$IQR, f=0.1),col='blue', lwd=2, lty=1)
axis(1, at=c('1910', '1920', '1930', '1940', '1950', '1960', '1970','1980','1990','2000', '2010'), cex.axis=0.6)
axis(2, cex.axis=0.6)
box()
mtext("Males - Data LAHMD and UN", cex=1.2)

### Mulheres - LAHMD
screen(2)
par(xpd=T, mar=par()$mar+c(0,0,0,4))
par(las=1, family='serif')
plot(lowess(dat.s.f$Ano,dat.s.f$IQR, f=0.055), type='l', pch=10, col='black', 
     lwd=2, lty=2, xlab='Year', ylab='Interquartile Range', axes=F, ylim=yrange, xlim=xrange)
lines(lowess(dat.b.f$Ano,dat.b.f$IQR, f=0.3), col='brown', lwd=2, lty=2)
lines(lowess(dat.r.f$Ano,dat.r.f$IQR, f=0.68), col='grey50', lwd=2, lty=2)
lines(lowess(dados.chf$Ano,dados.chf$IQR, f=0.35), col='blue', lwd=2,lty=2)
lines(lowess(dados.mef$Ano,dados.mef$IQR, f=0.2),col='forestgreen', lwd=2, lty=2)
lines(lowess(dados.paf$Ano,dados.paf$IQR, f=0.1), col='purple', lwd=2, lty=2)
lines(lowess(dados.cof$Ano,dados.cof$IQR, f=0.8), col='orange', lwd=2, lty=2)
lines(lowess(dados.urf$Ano,dados.urf$IQR, f=0.15),col='red', lwd=2, lty=2)
lines(lowess(dados.brf$Ano,dados.brf$IQR, f=0.1), col='black', lwd=2,lty=1)
lines(lowess(dados.dof$Ano,dados.dof$IQR, f=0.1),col='forestgreen', lwd=2, lty=1)
lines(lowess(dados.cuf$Ano,dados.cuf$IQR, f=0.3),col='orange', lwd=2, lty=1)
lines(lowess(dados.puf$Ano,dados.puf$IQR, f=0.1),col='brown', lwd=2, lty=1)
lines(lowess(dados.crf$Ano,dados.crf$IQR, f=0.8),col='grey50', lwd=2, lty=1)
lines(lowess(dados.pef$Ano,dados.pef$IQR, f=0.15),col='red', lwd=2, lty=1)
lines(lowess(dados.arf$Ano,dados.arf$IQR, f=0.1),col='purple', lwd=2, lty=1)
lines(lowess(dados.guf$Ano,dados.guf$IQR, f=0.1),col='blue', lwd=2, lty=1)
axis(1, at=c('1910', '1920', '1930', '1940', '1950', '1960', '1970','1980','1990','2000', '2010'), cex.axis=0.6)
axis(2, cex.axis=0.6)
box()
legend(2015,35, c("Sweden","Bulgaria","Russia","Chile","Mexico","Panama",
                  "Colombia","Uruguay","Brazil","Domicanan Rep.","Cuba","Puerto Rico",
                  "Costa Rica","Peru","Argentina","Guatemala"),
           cex=0.8, col=c("black","brown", "grey50", "blue","forestgreen","purple","orange","red","black",
              "forestgreen","orange","brown","grey50","red","purple", "blue"),
      lwd=c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2),lty=c(2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1),bty='n')
mtext("Females - Data LAHMD and UN", cex=1.2)

############### LAMBDA #######################################

##Homens - LAMBDA
screen(3)
par(las=1, family='serif')
plot(lowess(dat.chm$Ano,dat.chm$IQR, f=0.1), type='l', pch=10, col='blue', 
     lwd=2, lty=2, xlab='Year', ylab='Interquartile Range', axes=F, ylim=yrange, xlim=xrange)
lines(lowess(dat.mem$Ano,dat.mem$IQR, f=0.1), col='forestgreen', lwd=2, lty=2)
lines(lowess(dat.pam$Ano,dat.pam$IQR, f=0.1), col='purple', lwd=2,lty=2)
lines(lowess(dat.com$Ano,dat.com$IQR, f=0.1), col='orange', lwd=2,lty=2)
lines(lowess(dat.urm$Ano,dat.urm$IQR, f=0.1),col='red', lwd=2, lty=2)
lines(lowess(dat.brm$Ano,dat.brm$IQR, f=0.1), col='black', lwd=2, lty=1)
lines(lowess(dat.dom$Ano,dat.dom$IQR, f=0.1),col='forestgreen', lwd=2, lty=1)
lines(lowess(dat.cum$Ano,dat.cum$IQR, f=0.1), col='orange', lwd=2, lty=1)
lines(lowess(dat.crm$Ano,dat.crm$IQR, f=0.1),col='grey50', lwd=2, lty=1)
lines(lowess(dat.pem$Ano,dat.pem$IQR, f=0.1),col='red', lwd=2, lty=1)
lines(lowess(dat.arm$Ano,dat.arm$IQR, f=0.1), col='purple', lwd=2, lty=1)
lines(lowess(dat.gum$Ano,dat.gum$IQR, f=0.1),col='blue', lwd=2, lty=1)
axis(1, at=c('1910', '1920', '1930', '1940', '1950', '1960', '1970','1980','1990','2000', '2010'), cex.axis=0.6)
axis(2, cex.axis=0.6)
box()
mtext("Males - Data Lambda", cex=1.2)

### Mulheres - LAMBDA
screen(4)
par(xpd=T, mar=par()$mar+c(0,0,0,4))
par(las=1, family='serif')
plot(lowess(dat.chf$Ano,dat.chf$IQR, f=0.1), type='l', pch=10, col='blue', 
     lwd=2, lty=2, xlab='Year', ylab='Interquartile Range', axes=F, ylim=yrange, xlim=xrange)
lines(lowess(dat.mef$Ano,dat.mef$IQR, f=0.1), col='forestgreen', lwd=2, lty=2)
lines(lowess(dat.paf$Ano,dat.paf$IQR, f=0.1), col='purple', lwd=2,lty=2)
lines(lowess(dat.cof$Ano,dat.cof$IQR, f=0.1), col='orange', lwd=2,lty=2)
lines(lowess(dat.urf$Ano,dat.urf$IQR, f=0.1),col='red', lwd=2, lty=2)
lines(lowess(dat.brf$Ano,dat.brf$IQR, f=0.1), col='black', lwd=2, lty=1)
lines(lowess(dat.dof$Ano,dat.dof$IQR, f=0.1),col='forestgreen', lwd=2, lty=1)
lines(lowess(dat.cuf$Ano,dat.cuf$IQR, f=0.1), col='orange', lwd=2, lty=1)
lines(lowess(dat.crf$Ano,dat.crf$IQR, f=0.1),col='grey50', lwd=2, lty=1)
lines(lowess(dat.pef$Ano,dat.pef$IQR, f=0.1),col='red', lwd=2, lty=1)
lines(lowess(dat.arf$Ano,dat.arf$IQR, f=0.1), col='purple', lwd=2, lty=1)
lines(lowess(dat.guf$Ano,dat.guf$IQR, f=0.1),col='blue', lwd=2, lty=1)
axis(1, at=c('1910', '1920', '1930', '1940', '1950', '1960', '1970','1980','1990','2000', '2010'), cex.axis=0.6)
axis(2, cex.axis=0.6)
box()
mtext("Females - Data Lambda", cex=1.2)
legend(2015,35, c("Chile","Mexico","Panama",
                  "Colombia","Uruguay","Brazil","Domicanan Rep.","Cuba",
                  "Costa Rica","Peru","Argentina","Guatemala"),
cex=0.8, col=c("blue","forestgreen","purple","orange","red","black",
               "forestgreen","orange","grey50","red","purple", "blue"),  
lwd=c(2,2,2,2,2,2,2,2,2,2,2,2), lty=c(2,2,2,2,2,1,1,1,1,1,1,1),bty='n')

close.screen(all=T)
dev.off()
##################################################

# Figure 5 - Figure of C50
##Dados###
dat.s<-read.table('Resultados_C50_Sweden.txt', sep='\t', header=T)
dat.r<-read.table('Resultados_C50_Russia.txt', sep='\t', header=T)
dat.b<-read.table('Resultados_C50_Bulgaria.txt', sep='\t', header=T)
dados<-read.table('Resultados_C50_LA_novo.txt', sep='\t', header=T)

#View(dados)
names(dados)
dados$Pais

dados1<-dados[dados$Pais=='Arg',];dados2<-dados[dados$Pais=='Bra',]
dados3<-dados[dados$Pais=='Chi',];dados4<-dados[dados$Pais=='Col',]
dados5<-dados[dados$Pais=='Crc',];dados6<-dados[dados$Pais=='Cub',]
dados7<-dados[dados$Pais=='Gua',];dados8<-dados[dados$Pais=='Mex',]
dados9<-dados[dados$Pais=='Pan',];dados10<-dados[dados$Pais=='Per',]
dados11<-dados[dados$Pais=='Pur',];dados12<-dados[dados$Pais=='Dom',]
dados13<-dados[dados$Pais=='Uru',]

##Separando os dados###
dat.s.m<-subset(dat.s, Sex=='m' & Ano>='1920' & Ano<='2010')
dat.s.f<-subset(dat.s, Sex=='f' & Ano>='1920' & Ano<='2010')

dat.b.m<-subset(dat.b, Sex=='m' & Ano>='1950' & Ano<='2010')
dat.b.f<-subset(dat.b, Sex=='f' & Ano>='1950' & Ano<='2010')

dat.r.m<-subset(dat.r, Sex=='m' & Ano>='1960' & Ano<='2010')
dat.r.f<-subset(dat.r, Sex=='f' & Ano>='1960' & Ano<='2010')

dat.arm<-subset(dados1, Sexo=='m');dat.brm<-subset(dados2, Sexo=='m')
dat.chm<-subset(dados3, Sexo=='m');dat.com<-subset(dados4, Sexo=='m')
dat.crm<-subset(dados5, Sexo=='m');dat.cum<-subset(dados6, Sexo=='m')
dat.gum<-subset(dados7, Sexo=='m');dat.mem<-subset(dados8, Sexo=='m')
dat.pam<-subset(dados9, Sexo=='m');dat.pem<-subset(dados10, Sexo=='m')
dat.pum<-subset(dados11, Sexo=='m');dat.dom<-subset(dados12, Sexo=='m')
dat.urm<-subset(dados13, Sexo=='m');dat.arf<-subset(dados1, Sexo=='f')
dat.brf<-subset(dados2, Sexo=='f');dat.chf<-subset(dados3, Sexo=='f')
dat.cof<-subset(dados4, Sexo=='f');dat.crf<-subset(dados5, Sexo=='f')
dat.cuf<-subset(dados6, Sexo=='f');dat.guf<-subset(dados7, Sexo=='f')
dat.mef<-subset(dados8, Sexo=='f');dat.paf<-subset(dados9, Sexo=='f')
dat.pef<-subset(dados10, Sexo=='f');dat.puf<-subset(dados11, Sexo=='f')
dat.dof<-subset(dados12, Sexo=='f');dat.urf<-subset(dados13, Sexo=='f')

###Margem dos dados####
yrange=range(dat.s.m$C50,dat.b.m$C50, dat.r.m$C50,dat.arm$C50,dat.brm$C50,
             dat.chm$C50,dat.com$C50,dat.crm$C50,dat.cum$C50,dat.gum$C50, 
             dat.mem$C50, dat.pam$C50,dat.pem$C50,dat.pum$C50,dat.dom$C50,
             dat.urm$C50,dat.s.f$C50,dat.b.f$C50,dat.r.f$C50,dat.arf$C50,
             dat.brf$C50,dat.chf$C50,dat.cof$C50,dat.crf$C50,dat.cuf$C50,
             dat.guf$C50,dat.mef$C50,dat.paf$C50,dat.pef$C50, dat.puf$C50,
             dat.dof$C50,dat.urf$C50)

xrange=range(dat.s.m$Ano,dat.b.m$Ano,dat.r.m$Ano,dat.arm$Ano,dat.brm$Ano,dat.chm$Ano,
             dat.com$Ano,dat.crm$Ano,dat.cum$Ano,dat.gum$Ano, dat.mem$Ano, dat.pam$Ano,
             dat.pem$Ano,dat.pum$Ano,dat.dom$Ano,dat.urm$Ano,dat.s.f$Ano,dat.b.f$Ano,dat.r.f$Ano,
             dat.arf$Ano,dat.brf$Ano,dat.chf$Ano,dat.cof$Ano,dat.crf$Ano,dat.cuf$Ano,
             dat.guf$Ano,dat.mef$Ano,dat.paf$Ano,dat.pef$Ano, dat.puf$Ano, dat.dof$Ano,dat.urf$Ano)

###########################################################################
##################### New Graphs ##########################################
###########################################################################
png("Figura5_C50_novo.png", width=10, height=7, unit='in', res=300)
split.screen(figs=rbind(c(0,0.42,0,1),c(0.4,.9,0,1)))

screen(1)
### Homens ##### 
par(las=1, family='serif')
plot(lowess(dat.s.m$Ano,dat.s.m$C50, f=0.055), type='l', pch=10, col='black', lwd=2, lty=2, 
     xlab='Year', ylab='C50', axes=F, ylim=yrange, xlim=xrange)
lines(lowess(dat.b.m$Ano,dat.b.m$C50, f=0.3), col='brown', lwd=2, lty=2)
lines(lowess(dat.r.m$Ano,dat.r.m$C50, f=0.68), col='grey50', lwd=2, lty=2)
lines(lowess(dat.chm$Ano,dat.chm$C50, f=0.35), col='blue', lwd=2,lty=2)
lines(lowess(dat.mem$Ano,dat.mem$C50, f=0.2),col='forestgreen', lwd=2, lty=2)
lines(lowess(dat.pam$Ano,dat.pam$C50, f=0.1), col='purple', lwd=2, lty=2)
lines(lowess(dat.com$Ano,dat.com$C50, f=0.8), col='orange', lwd=2, lty=2)
lines(lowess(dat.urm$Ano,dat.urm$C50, f=0.15),col='red', lwd=2, lty=2)
lines(lowess(dat.brm$Ano,dat.brm$C50, f=0.1), col='black', lwd=2,lty=1)
lines(lowess(dat.dom$Ano,dat.dom$C50, f=0.1),col='forestgreen', lwd=2, lty=1)
lines(lowess(dat.cum$Ano,dat.cum$C50, f=0.3),col='orange', lwd=2, lty=1)
lines(lowess(dat.pum$Ano,dat.pum$C50, f=0.1),col='brown', lwd=2, lty=1)
lines(lowess(dat.crm$Ano,dat.crm$C50, f=0.8),col='grey50', lwd=2, lty=1)
lines(lowess(dat.pem$Ano,dat.pem$C50, f=0.15),col='red', lwd=2, lty=1)
lines(lowess(dat.arm$Ano,dat.arm$C50, f=0.1),col='purple', lwd=2, lty=1)
lines(lowess(dat.gum$Ano,dat.gum$C50, f=0.1),col='blue', lwd=2, lty=1)
axis(1, at=c('1920', '1930', '1940', '1950', '1960', '1970', '1980', '1990', '2000', '2010'), cex.axis=0.6)
axis(2, cex.axis=0.6)
box()
mtext("Males", cex=1.2)

screen(2)
### Mulheres ###
par(xpd=T, mar=par()$mar+c(0,0,0,4))
par(las=1, family='serif')
plot(lowess(dat.s.f$Ano,dat.s.f$C50, f=0.055), type='l', pch=10, col='black', lwd=2, lty=2, xlab='Year', 
     ylab='C50', axes=F, ylim=yrange, xlim=xrange)
lines(lowess(dat.b.f$Ano,dat.b.f$C50, f=0.3), col='brown', lwd=2, lty=2)
lines(lowess(dat.r.f$Ano,dat.r.f$C50, f=0.68), col='grey50', lwd=2, lty=2)
lines(lowess(dat.chf$Ano,dat.chf$C50, f=0.35), col='blue', lwd=2,lty=2)
lines(lowess(dat.mef$Ano,dat.mef$C50, f=0.2),col='forestgreen', lwd=2, lty=2)
lines(lowess(dat.paf$Ano,dat.paf$C50, f=0.1), col='purple', lwd=2, lty=2)
lines(lowess(dat.cof$Ano,dat.cof$C50, f=0.8), col='orange', lwd=2, lty=2)
lines(lowess(dat.urf$Ano,dat.urf$C50, f=0.15),col='red', lwd=2, lty=2)
lines(lowess(dat.brf$Ano,dat.brf$C50, f=0.1), col='black', lwd=2,lty=1)
lines(lowess(dat.dof$Ano,dat.dof$C50, f=0.1),col='forestgreen', lwd=2, lty=1)
lines(lowess(dat.cuf$Ano,dat.cuf$C50, f=0.3),col='orange', lwd=2, lty=1)
lines(lowess(dat.puf$Ano,dat.puf$C50, f=0.1),col='brown', lwd=2, lty=1)
lines(lowess(dat.crf$Ano,dat.crf$C50, f=0.8),col='grey50', lwd=2, lty=1)
lines(lowess(dat.pef$Ano,dat.pef$C50, f=0.15),col='red', lwd=2, lty=1)
lines(lowess(dat.arf$Ano,dat.arf$C50, f=0.1),col='purple', lwd=2, lty=1)
lines(lowess(dat.guf$Ano,dat.guf$C50, f=0.1),col='blue', lwd=2, lty=1)
axis(1, at=c('1920', '1930', '1940', '1950', '1960', '1970', '1980', '1990', '2000', '2010'), cex.axis=0.6)
axis(2, cex.axis=0.6)
box()
mtext("Females", cex=1.2)
legend(2020,27, c("Sweden","Bulgaria","Russia","Chile","Mexico",
                  "Panama","Colombia","Uruguay","Brazil","Domicanan Rep.",
                  "Cuba","Puerto Rico","Costa Rica","Peru","Argentina","Guatemala"),
       cex=0.8, col=c("black", "brown","grey50", "blue","forestgreen","purple",
                     "orange","red","black","forestgreen","orange","brown","grey50","red","purple", "blue"),
                 lwd=c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), lty=c(2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1),bty='n')

close.screen(all=T)
dev.off()

# Figures 6 and 7 - IQR and Life Expectancy at age 5

#########################################################################
dados<-read.table('Resultados_IQR_LA_novo.txt', sep='\t', header=T)
dat.f<-read.table('E5_mulheres.txt', sep='\t', header=T)
dat.m<-read.table('E5_homens.txt', sep='\t', header=T)
names(dat.m)
## Agregando vetores das esperanças de vida E5
##Muieres
data.f<-as.vector(cbind(dat.f$Argentina,dat.f$Brazil,dat.f$Chile,dat.f$Colombia,
               dat.f$Costa.Rica,dat.f$Cuba,dat.f$Rep.Dominicana,dat.f$Mexico,dat.f$Uruguay,
               dat.f$Panama,dat.f$Peru,dat.f$Puerto.Rico,dat.f$Guatemala))
dados.f <- data.f[!is.na(data.f)]

##Homis
data.m<-as.vector(cbind(dat.m$Argentina,dat.m$Brazil,dat.m$Chile,dat.m$Colombia,
               dat.m$Costa.Rica,dat.m$Cuba,dat.m$Rep.Dominicana,dat.m$Mexico,dat.m$Uruguay,
               dat.m$Panama,dat.m$Peru,dat.m$Puerto.Rico,dat.m$Guatemala))
dados.m <- data.m[!is.na(data.m)]

##juntandos os dados numa única linha e agregando ao banco antigo
dado<-cbind(dados,E5=as.vector(cbind(dados.f,dados.m)))
dado
#View(dado)
names(dado)
dado$Pais
dado.f<-subset(dado,Sexo=='f');dado.m<-subset(dado,Sexo=='m')

##Gráficos mulheres
par(mfrow=c(4,4))
par(mar=c(2,3,2,3))
#par(mai=c(1.5,1.5,1.5,1.5))
## Sequência gráfica ###

#Arg Bra Chi Col

#Arg
par(family='serif')
with(dado.f, plot(dado.f[dado.f$Pais=='Arg','Ano'], dado.f[dado.f$Pais=='Arg','IQR'], 
     type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
     lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
       #mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.f, plot(dado.f[dado.f$Pais=='Arg','Ano'], dado.f[dado.f$Pais=='Arg','E5'],
     type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
     box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Argentina')
#legend(1940,20.5,legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Bra
par(family='serif')
with(dado.f, plot(dado.f[dado.f$Pais=='Bra','Ano'], dado.f[dado.f$Pais=='Bra','IQR'], 
     type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
     lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#       mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.f, plot(dado.f[dado.f$Pais=='Bra','Ano'], dado.f[dado.f$Pais=='Bra','E5'],
     type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
     box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Brazil')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Chi
par(family='serif')
with(dado.f, plot(dado.f[dado.f$Pais=='Chi','Ano'], dado.f[dado.f$Pais=='Chi','IQR'], 
     type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
     lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#       mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.f, plot(dado.f[dado.f$Pais=='Chi','Ano'], dado.f[dado.f$Pais=='Chi','E5'],
     type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
     box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Chile')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Col
par(family='serif')
with(dado.f, plot(dado.f[dado.f$Pais=='Col','Ano'], dado.f[dado.f$Pais=='Col','IQR'], 
     type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
     lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#       mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.f, plot(dado.f[dado.f$Pais=='Col','Ano'], dado.f[dado.f$Pais=='Col','E5'],
     type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
     box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Colombia')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

############################## Continu ###########################################

#Crc Cub Gua Mex 
#Crc
par(family='serif')
with(dado.f, plot(dado.f[dado.f$Pais=='Crc','Ano'], dado.f[dado.f$Pais=='Crc','IQR'], 
     type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
     lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#       mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.f, plot(dado.f[dado.f$Pais=='Crc','Ano'], dado.f[dado.f$Pais=='Crc','E5'],
     type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
     box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Costa Rica')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Cub
par(family='serif')
with(dado.f, plot(dado.f[dado.f$Pais=='Cub','Ano'], dado.f[dado.f$Pais=='Cub','IQR'], 
     type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
     lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#       mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.f, plot(dado.f[dado.f$Pais=='Cub','Ano'], dado.f[dado.f$Pais=='Cub','E5'],
     type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
     box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Cuba')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Gua
par(family='serif')
with(dado.f, plot(dado.f[dado.f$Pais=='Gua','Ano'], dado.f[dado.f$Pais=='Gua','IQR'], 
                  type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
                  lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.f, plot(dado.f[dado.f$Pais=='Gua','Ano'], dado.f[dado.f$Pais=='Gua','E5'],
                  type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Guatemala')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Mex
par(family='serif')
with(dado.f, plot(dado.f[dado.f$Pais=='Mex','Ano'], dado.f[dado.f$Pais=='Mex','IQR'], 
                  type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
                  lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.f, plot(dado.f[dado.f$Pais=='Mex','Ano'], dado.f[dado.f$Pais=='Mex','E5'],
                  type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Mexico')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

############################## Continu ###########################################

#Pan Per Pur Dom
#Pan
par(family='serif')
with(dado.f, plot(dado.f[dado.f$Pais=='Pan','Ano'], dado.f[dado.f$Pais=='Pan','IQR'], 
                  type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
                  lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.f, plot(dado.f[dado.f$Pais=='Pan','Ano'], dado.f[dado.f$Pais=='Pan','E5'],
                  type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Panama')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Per
par(family='serif')
with(dado.f, plot(dado.f[dado.f$Pais=='Per','Ano'], dado.f[dado.f$Pais=='Per','IQR'], 
                  type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
                  lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.f, plot(dado.f[dado.f$Pais=='Per','Ano'], dado.f[dado.f$Pais=='Per','E5'],
                  type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Peru')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Pur
par(family='serif')
with(dado.f, plot(dado.f[dado.f$Pais=='Pur','Ano'], dado.f[dado.f$Pais=='Pur','IQR'], 
                  type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
                  lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.f, plot(dado.f[dado.f$Pais=='Pur','Ano'], dado.f[dado.f$Pais=='Pur','E5'],
                  type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Puerto Rico')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Dom
par(family='serif')
with(dado.f, plot(dado.f[dado.f$Pais=='Dom','Ano'], dado.f[dado.f$Pais=='Dom','IQR'], 
     type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
     lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#       mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.f, plot(dado.f[dado.f$Pais=='Dom','Ano'], dado.f[dado.f$Pais=='Dom','E5'],
     type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
     box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Dominican Republic')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

############################## Continu ###########################################

#Uru  
par(family='serif')
with(dado.f, plot(dado.f[dado.f$Pais=='Uru','Ano'], dado.f[dado.f$Pais=='Uru','IQR'], 
     type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
     lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#       mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.f, plot(dado.f[dado.f$Pais=='Uru','Ano'], dado.f[dado.f$Pais=='Uru','E5'],
     type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
     box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Uruguay')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')
       


######################################################################################################################
######################################################################################################################
######################################################################################################################

##Gráficos Homens
par(mfrow=c(4,4))
par(mar=c(2,3,2,3))
## Sequência gráfica ###

#Arg Bra Chi Col
#Arg
par(family='serif')
with(dado.m, plot(dado.m[dado.m$Pais=='Arg','Ano'], dado.m[dado.m$Pais=='Arg','IQR'], 
     type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
     lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#       mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.m, plot(dado.m[dado.m$Pais=='Arg','Ano'], dado.m[dado.m$Pais=='Arg','E5'],
     type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
     box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Argentina')
#legend(1940,20.5,legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Bra
par(family='serif')
with(dado.m, plot(dado.m[dado.m$Pais=='Bra','Ano'], dado.m[dado.m$Pais=='Bra','IQR'], 
     type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
     lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#       mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.m, plot(dado.m[dado.m$Pais=='Bra','Ano'], dado.m[dado.m$Pais=='Bra','E5'],
     type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
     box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Brazil')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Chi
par(family='serif')
with(dado.m, plot(dado.m[dado.m$Pais=='Chi','Ano'], dado.m[dado.m$Pais=='Chi','IQR'], 
     type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
     lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#       mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.m, plot(dado.m[dado.m$Pais=='Chi','Ano'], dado.m[dado.m$Pais=='Chi','E5'],
     type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
     box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Chile')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Col
par(family='serif')
with(dado.m, plot(dado.m[dado.m$Pais=='Col','Ano'], dado.m[dado.m$Pais=='Col','IQR'], 
     type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
     lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#       mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.m, plot(dado.m[dado.m$Pais=='Col','Ano'], dado.m[dado.m$Pais=='Col','E5'],
     type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
     box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Colombia')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

############################## Continu ###########################################

#Crc Cub Gua Mex 
#Crc
par(family='serif')
with(dado.m, plot(dado.m[dado.m$Pais=='Crc','Ano'], dado.m[dado.m$Pais=='Crc','IQR'], 
     type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
     lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#       mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.m, plot(dado.m[dado.m$Pais=='Crc','Ano'], dado.m[dado.m$Pais=='Crc','E5'],
     type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
     box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Costa Rica')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')
 
#Cub
par(family='serif')
with(dado.m, plot(dado.m[dado.m$Pais=='Cub','Ano'], dado.m[dado.m$Pais=='Cub','IQR'], 
     type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
     lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#       mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.m, plot(dado.m[dado.m$Pais=='Cub','Ano'], dado.m[dado.m$Pais=='Cub','E5'],
     type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
     box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Cuba')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Gua
par(family='serif')
with(dado.m, plot(dado.m[dado.m$Pais=='Gua','Ano'], dado.m[dado.m$Pais=='Gua','IQR'], 
                  type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
                  lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.m, plot(dado.m[dado.m$Pais=='Gua','Ano'], dado.m[dado.m$Pais=='Gua','E5'],
                  type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Guatemala')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Mex
par(family='serif')
with(dado.m, plot(dado.m[dado.m$Pais=='Mex','Ano'], dado.m[dado.m$Pais=='Mex','IQR'], 
                  type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
                  lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.m, plot(dado.m[dado.m$Pais=='Mex','Ano'], dado.m[dado.m$Pais=='Mex','E5'],
                  type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Mexico')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

############################## Continu ###########################################

#Pan Per Pur Dom
#Pan
par(family='serif')
with(dado.m, plot(dado.m[dado.m$Pais=='Pan','Ano'], dado.m[dado.m$Pais=='Pan','IQR'], 
                  type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
                  lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.m, plot(dado.m[dado.m$Pais=='Pan','Ano'], dado.m[dado.m$Pais=='Pan','E5'],
                  type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Panama')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Per  
par(family='serif')
with(dado.m, plot(dado.m[dado.m$Pais=='Per','Ano'], dado.m[dado.m$Pais=='Per','IQR'], 
                  type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
                  lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.m, plot(dado.m[dado.m$Pais=='Per','Ano'], dado.m[dado.m$Pais=='Per','E5'],
                  type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Peru')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Pur
par(family='serif')
with(dado.m, plot(dado.m[dado.m$Pais=='Pur','Ano'], dado.m[dado.m$Pais=='Pur','IQR'], 
                  type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
                  lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.m, plot(dado.m[dado.m$Pais=='Pur','Ano'], dado.m[dado.m$Pais=='Pur','E5'],
                  type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Puerto Rico')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

#Dom
par(family='serif')
with(dado.m, plot(dado.m[dado.m$Pais=='Dom','Ano'], dado.m[dado.m$Pais=='Dom','IQR'], 
     type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
     lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#       mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.m, plot(dado.m[dado.m$Pais=='Dom','Ano'], dado.m[dado.m$Pais=='Dom','E5'],
     type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
     box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Dominican Republic')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')

############################## Continu ###########################################

#Uru       
par(family='serif')
with(dado.m, plot(dado.m[dado.m$Pais=='Uru','Ano'], dado.m[dado.m$Pais=='Uru','IQR'], 
     type="l", col="red",ylab=NA,xlab='Year',ylim=range(dado$IQR),axes=F, 
     lwd=3, xlim=range(dado$Ano))); axis(1,cex.axis=0.9);axis(side=2,cex.axis=0.9); 
#       mtext(side=2, line=1.8,'Interquartile Range', cex=.8)
par(new = T)
with(dado.m, plot(dado.m[dado.m$Pais=='Uru','Ano'], dado.m[dado.m$Pais=='Uru','E5'],
     type='l',axes=F, xlab=NA, ylab=NA,lwd=3,xlim=range(dado$Ano),ylim=range(dado$E5))); axis(side = 4,cex.axis=0.9)
     box()
#mtext(side=4, line=2,'Life Expectancy at age 5', cex=.8)
mtext('Uruguay')
#legend("bottomleft",legend=c('Interquartile Range','Life Expectancy at age 5'),
#       lty=c(1,1),lwd=2, col=c("red", "black"),bty='n')
