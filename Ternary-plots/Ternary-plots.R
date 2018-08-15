require("vcd")  
barcode<-read.csv("C:/Users/dr382/Desktop/bar.csv")
pch <- c(1,2,3,4,5,6,7)
colors <- c("black","red","green", "orange","brown","purple","pink")
ternaryplot(#This is the actual plotting of the data to get the triangle
  barcode[,2:4],#Here I provide the file and columns to plot
  pch = pch,#This is choosing the shape of data points (already defined as pch)
  cex = .70,#This is the size of the data points  
  col = colors[as.numeric(barcode$Mouse_samples)],#coloring the points 
  main = "Relative proportion of barcodes")#Provides a title for the graph 
  
  require(ggtern)
plot <- ggtern(data = barcode, aes(x = Gr, y = CD4T, z = B)) + 
  geom_mask() +
  geom_point(size=3,aes(shape=Mouse_samples,fill=Mouse_samples,colour=Mouse_samples)) +
  scale_shape_manual(values=c(1,2,3,4,5,6,7))    +
  scale_color_manual(values=c("black","red","green", "orange","brown","purple","pink"))+
  ggtitle("Relative proportion of barcodes")
###render
plot
