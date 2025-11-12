#Define parameters
x <- seq(150, 850)      #SRL bounds
y1 <- function(x) {(0.1795 + (0.00036*x - 0.071208)^.5)/0.00018}    #Whitty Equation
y2 <- function(x) {5.317*x - 72.683}        #Peverell Equation

#Image parameters
tiff("test.tiff", units="in", width=5, height=5, res=300)

#Plot curves
curve(y1, from=150, to=800, xlab="Standard Rostrum Length (mm)", ylab="Total Length (mm)", col=rgb(0.2,0.4,0.1,0.7), lwd=2, main="Anoxypristis cuspidata", font.main=3)
curve(y2, add=TRUE, lwd=2, col=rgb(0.8,0.4,0.1,0.7))

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
save("test.tiff")
