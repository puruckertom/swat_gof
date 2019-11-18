###This script shows figures for R Figures presentation at CEESG 2/15/2012
###Author: Andrew Kramer

##First read in a package that makes several common plot variations easier, specifically axis breaks and confidence intervals
library(plotrix)

##This reads in some data on nematocysts in sea anemones (Kramer and Francis 2004)
anemone<-read.csv("anemone data.csv")
##Create some random datasets
simple <- cbind(rnorm(20,8,3),runif(20,0,10))
simple2<-cbind(rpois(20,2),runif(20,0,10))
simple3 <- cbind(rep(c(1,2,3),50),rpois(150,10))
simple4 <- cbind(rep(c(1,2,3),8),rpois(24,3))

##Start with a simple x-y plot
plot(simple[,1]~simple[,2])

##Use most common 'par' and 'plot' options to improve appearance
x11(width=5,height=5) ##Explicitly set plot size in inches, can also use win.graph() on Windows
##Aside: here is where you can save directly to a certain format using tiff() or pdf() or postscript() or win.graph() format is: postscript(file="name",width,height, onefile for adding multiple pages, pointsize for text, also res for tiff or png)
par(mar=c(3.5,4.5,0,0),mgp=c(2,0.5,0)) ##This sets margin for side 1,2,3,4 and placement of labels, tick labels and axis lines
plot(simple[,1]~simple[,2],las=1,bty="l",xlab=NA,ylab=NA) ##las changes orientation of labels, bty='L' removes box, often suppress labels to add later
mtext(side=1,text=expression(paste(italic(Species)," length (",mu*"m)")),line=2)
mtext(side=2,text=expression(paste("Critical density (",~m^-3, ")",sep="")),line=2)
mtext(side=2,text=expression(paste("Temperature (",degree~C,")",sep="")),line=3.25)
##expression and paste are used to combine math symbols and format with text for labels
##mtext is used to add text in a certain margin, need to define side and distance in lines from axis


##More complex plot including symbols, labels in plot, legend, axis break, user axis
x11(5,5)
par(mar=c(3,3,0.5,1),mgp=c(2,0.5,0))
plot(anemone[,3]~anemone[,2],pch=anemone[,1],las=1,bty="l") #The new thing here is using pch and data to display different symbols for each species
plot(anemone[,3]~log10(anemone[,2]),pch=anemone[,1],las=1,bty="l") ##Can automatically log scale with log="x", but this way is preferred)
plot(anemone[,3]~log10(anemone[,2]),pch=anemone[,1],las=1,bty="l",
     xlim=c(-1,3),xaxt="n",xlab=NA,ylab=NA) ##Set the x-axis range, suppress x-axis and labels
axis(1,at=c(-1,0,1,2,3),labels=c(0.1,1,10,100,1000)) ##Add nice x-axis
mtext(side=1,line=2,text="Wet weight (g)")
mtext(side=2,line=2,text=expression(paste("Nematocyst capsule length ( ",mu*"m)"))) ##Add axis labels, using a Greek letter here so need expression and paste again

##Add more data and an axis break from 'plotrix'
par(mar=c(3,3,0.5,1),mgp=c(2,0.5,0))
plot(anemone[,3]~log10(anemone[,2]),pch=anemone[,1],bty="l",xaxs="i",yaxs="i",
     xlim=c(-1,3),xaxt="n",ylim=c(40,90),yaxt="n",xlab=NA,ylab=NA) ##Set both axes ranges and then suppress
axis.break(axis=2,breakpos=51,style="slash") #This adds the classic axis break symbol on specified side
axis(2,at=c(40,50,55,65,75,85),labels=c(20,30,seq(55,85,10)),las=1,lwd=0,lwd.ticks=1) #Now we label the axis in user coordinates (differ from the actual because of break), the lwd parts keep the gap open
par(new=TRUE,mar=c(3,3,0.5,1),mgp=c(2,0.5,0)) #This says we are going to be adding to the active figure, and it makes sure our margins are the same so nothing is distorted
plot(anemone[,6]~log10(anemone[,5]),pch=anemone[,1],bty="l",xaxs="i",yaxs="i",
     xlim=c(-1,3),axes=FALSE,ylim=c(20,70),xlab=NA,ylab=NA) #This adds data on the other side of the break, use gap.plot is data is all one and you want a gap
axis(1,at=c(-1,0,1,2,3),labels=c(0.1,1,10,100,1000)) ##Add nice x-axis
mtext(side=1,line=2,text="Wet weight (g)")
mtext(side=2,line=2,text=expression(paste("Nematocyst capsule length ( ",mu*"m)")))

##Advised instead gap plot, implemented here with axis.break, also see gap.plot
par(mar=c(3,3,0.5,1),mgp=c(2,0.5,0))
plot(anemone[,3]~log10(anemone[,2]),pch=anemone[,1],bty="l",xaxs="i",yaxs="i",
     xlim=c(-1,3),xaxt="n",ylim=c(40,90),yaxt="n",xlab=NA,ylab=NA)
axis.break(2,51,style="gap") #This changes the appearance of the axis.break to make it more apparent
axis(2,at=c(40,50,55,65,75,85),labels=c(20,30,seq(55,85,10)),las=1,lwd=0,lwd.ticks=1)
par(new=TRUE,mar=c(3,3,0.5,1),mgp=c(2,0.5,0))
plot(anemone[,6]~log10(anemone[,5]),pch=anemone[,1],bty="l",xaxs="i",yaxs="i",
     xlim=c(-1,3),axes=FALSE,ylim=c(20,70),xlab=NA,ylab=NA)
axis(1,at=c(-1,0,1,2,3),labels=c(0.1,1,10,100,1000))
mtext(side=1,line=2,text="Wet weight (g)")
mtext(side=2,line=2,text=expression(paste("Nematocyst capsule length ( ",mu*"m)")))
text(1,30,"Tentacles") #The text command is used to put text inside the plot at x-y coord provided
text(1,65,"Acontia") #The text command is used to put text inside the plot at x-y coord provided

##Add confidence intervals using plotCI in 'plotrix'
par(mar=c(3,3,0.5,1),mgp=c(2,0.5,0))
plotCI(y=anemone[,3],x=log10(anemone[,2]),pch=ifelse(anemone[,1]==1,1,17),
     uiw=anemone[,4],bty="l",xaxs="i",yaxs="i",
     xlim=c(-1,3),xaxt="n",ylim=c(40,90),yaxt="n",xlab=NA,ylab=NA) ##This is all the same except it is 'plotCI' not plot, so the data has to be specified differently, the limits (uiw here) have to be specified. I have also changed the symbols using an ifelse()
axis.break(2,51,style="gap")
axis(2,at=c(40,50,55,65,75,85),labels=c(20,30,seq(55,85,10)),las=1,lwd=0,lwd.ticks=1)
par(new=TRUE,mar=c(3,3,0.5,1),mgp=c(2,0.5,0))
plotCI(y=anemone[,6],x=log10(anemone[,5]),pch=ifelse(anemone[,1]==1,1,17),bty="l",xaxs="i",yaxs="i",
     xlim=c(-1,3),axes=FALSE,ylim=c(20,70),xlab=NA,ylab=NA,uiw=anemone[,7]) ##Same as previous
axis(1,at=c(-1,0,1,2,3),labels=c(0.1,1,10,100,1000))
mtext(side=1,line=2,text="Wet weight (g)")
mtext(side=2,line=2,text=expression(paste("Nematocyst capsule length ( ",mu*"m)")))
text(1,30,"Tentacles")
text(1,65,"Acontia")
legend(x=.8,y=40,legend=c(expression(paste(italic(Metridium)," ",italic(farcimen),sep="")),
                  expression(paste(italic(Metridium)," ",italic(senile),sep=""))),pch=c(1,17),bty="o")
##The legend is added at coordinates within the plot, the symbols to be matched with the labels are designated by pch, lty would be used if there were lines


##Multi-panel plots
##Most basic, draw two independent plots as single stack
x11(3.5,7) ##Set up tall device region
par(mfrow=c(2,1),mar=c(3,3,0,0.5),mgp=c(2,0.5,0)) ##mfrow tells R there are two row and 1 column here
plot(simple[,1]~simple[,2],las=1,bty='l') #Goes in first row
plot(simple2[,1]~simple2[,2],las=1,bty='l') #Goes in second row

##Do the same design with layout instead of mfrow
layout(matrix(data=c(1,2),nrow=2),widths=rep(3.5,2),heights=rep(3.5,2)) #This tells where in a matrix to print figures and how big to make them
par(mar=c(3,3,0,0.5),mgp=c(2,0.5,0)) #Still need to set the margins, in this case for both figure following
plot(simple[,1]~simple[,2],las=1,bty='l') #This goes in '1'
plot(simple2[,1]~simple2[,2],las=1,bty='l') #This goes in '2'

##Combine axes
par(mfrow=c(2,1),mar=c(0,3,0,0.5),oma=c(3,0,0,0),mgp=c(2,0.5,0)) ##mfrow tells R there are two row and 1 column here, we also take out margins and then add an outer margin
Fig1<-plot(simple[,1]~simple[,2],las=1,bty='l',xaxt="n") #First plot
axis(1,Fig1,labels=FALSE,tcl=0.5) #This is an example of how to add the ticks back and make them point the opposite way
plot(simple2[,1]~simple2[,2],las=1,bty='l') #Second plot
mtext(outer=TRUE,side=1,line=2,"simple[,2]") #The two places a label can be added, outer is incorrect here
mtext(side=1,line=2,"simple[,2]") #This adds the label in the margin of active plot, properly centered here

##Add a shaded region
polygon(x=c(0,10,10,0),y=c(0,0,2,2),col=rgb(0,0,1,0.25)) #polygon connects points and can be hatched, filled, etc.

##Use layout to make more complex plot
x11(7,7)
par(mar=c(3,3,0.5,0),mgp=c(2,0.5,0))
layout(matrix(c(1,1,2,3),byrow=T,nrow=2),heights=c(2,1)) #This creates a 3 panel plot with the first figure taking up the whole width and being twice as tall, while the other two splitting the bottom third.
plot(simple[,1]~simple[,2],las=1,bty='l')
plot(simple2[,1]~simple2[,2],las=1,bty='l')
plot(simple2[,1]~simple2[,2],las=1,bty='l')

#Make barplot with inset
stacked<-table(simple3[,1],simple3[,2]) ##Creates a table of categories for the barplot
stacked.in <- table(simple4[,1],simple4[,2]) ##Creates a table of categories for inset plot
x11(7,7)
par(mar=c(3,3,0.2,0.2),mgp=c(2,0.5,0))
Fig2 <- barplot(stacked,beside=FALSE,col=c("black","grey","white"),names=rep("",18),xlab="Scores",ylab="Number of times",las=1) #Creates a stacked barplot, I have suppressed the x-labels so I can rotate them
mtext(colnames(stacked),at=Fig2,side=1,las=2,line=0.5) #Illustrates rotating axis labels using las

##Add the inset by defining new plot region inside the other one
par(fig=c(0.6,0.95,0.55,0.85),new=TRUE,mar=c(3,3,0,0),mgp=c(1.75,0.5,0)) ##This figure region is relative to the other one (values between 0 and 1) and has its own margins
plot.new() #This initiates the new plot in the region defined above
plot.window(xlim=c(-1,6),ylim=c(0,4)) #This sets up the axis ranges for the plot, not always necessary
Fig2b <- barplot(stacked.in,beside=FALSE,col=c("black","grey","white"),names=rep("",7),las=1,bty="o")
mtext("Scores",side=1,line=1)
mtext(colnames(stacked.in),at=Fig2b,side=1,las=2,line=0.2)
mtext("Number\nof times",side=2,line=1.5) #The new thing here is using "\n" to have a hard return in label
