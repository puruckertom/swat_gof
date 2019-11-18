# Scatterplot Matrices from the glus Package 
library(gclus)
#class(dta)
gofs.1500 <- top5.gofs[,1:5] # get data 
class(gofs.1500)
gofs.1500.r <- abs(cor(gofs.1500)) # get correlations
gofs.1500.col <- dmat.color(gofs.1500.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
gofs.1500.o <- order.single(gofs.1500.r) 

gof.cor.pdf <- paste(pdf_dir,"gof.cor.pdf",sep="")
pdf(file=gof.cor.pdf)
  cpairs(gofs.1500, gofs.1500.o, panel.colors=gofs.1500.col, gap=.5,
    main="GOFs Ordered and Colored by Correlation" )
dev.off()
