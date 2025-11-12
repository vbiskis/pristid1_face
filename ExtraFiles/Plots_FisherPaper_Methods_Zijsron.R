#Define parameters
x <- seq(150, 1750)                                #SRL bounds
y1 <- function(x) {TL = (13.6426*x)^0.8559}        #Whitty Equation
y2 <- function(x) {TL = 3.7536*x + 225.536}        #Peverell Equation

#Image parameters
tiff("test2.tiff", units="in", width=5, height=5, res=300)

#Plot curves
curve(y2, from=150, to=1500, xlab="Standard Rostrum Length (mm)", ylab="Total Length (mm)", col=rgb(0.8,0.4,0.1,0.7), lwd=2, main="Pristis zijsron", font.main=3)
curve(y1, add=TRUE, lwd=2, col=rgb(0.2,0.4,0.1,0.7))

#Add legend
legend("bottomright", 
       legend = c("Whitty", "Peverell"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

dev.off()
