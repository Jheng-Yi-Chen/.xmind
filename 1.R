####

## LOAD THE DATA
leg2016 <- read.csv(file="C:/Users/CJY/Desktop/1.csv", header=TRUE);
dim(leg2016);
leg2016[1:10,];


## SETTINGS FOR PLOTTING FIGURE
par(mfrow=c(1,1), mar=c(4,4,1,1), oma=c(2,2,1,1), family='PMingLiU'); # BiauKai


## PLOT SCATTERPLOT
plot(DPPShare ~ KMTShare, data=leg2016, type="n", xlab="KMT share", ylab="DPP share", main="", xlim=c(0, 100), ylim=c(0, 100), yaxt="n", xaxt="n", bty="n");

axis(side=2, las=1, tick=TRUE, cex.axis=1.2, at=seq(0,100,20), pos=c(-5,0) ); # Y-axis
axis(side=1, las=1, tick=TRUE, cex.axis=1.2, at=seq(0,100,20), pos=c(-5,0) ); # X-axis

abline(a=0, b=1, col="gray90");

levels(leg2016$Region);

## SET THE COLORS
col.region <- ifelse(leg2016$Region == "Center", "yellow", ifelse(leg2016$Region == "East", "red", ifelse(leg2016$Region == "North", "blue", ifelse(leg2016$Region == "South", "green", "purple"))));

## SET THE TRANSPARENCY
tans.col.region <- adjustcolor(col.region, alpha.f=0.6);

points(x=leg2016$KMTShare, y=leg2016$DPPShare, type="p", pch=16, col=tans.col.region, cex=leg2016$NoCand/2);

text(x=leg2016$KMTShare, y=leg2016$DPPShare, labels=leg2016$District, cex=0.5, col="black");

text(x=50, y=90, labels="2016", cex=4, col="gray50");



