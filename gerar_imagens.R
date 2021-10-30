library(magick)
library(tidyverse)
require(grDevices)

olhos <- image_read("olhos.gif")
feliz <- image_read("boca_feliz.gif")
triste <- image_read("boca_triste.gif")
indefinido <- image_read("boca_indefinido.gif")
dir.create('feliz')
dir.create('triste')
dir.create('indefinido')

position <- function(){sample(-10:10, 1)}
s <- function(){sample(1:20, 1)}

gerar_imagens <- function(titulo,quantidade){
  setwd(paste0('./',titulo))
  ajuste = 0
  if (titulo=='triste') { 
    ajuste = 60
    humor = triste
  }
  if (titulo=='feliz') { humor = feliz }
  if (titulo=='indefinido') { humor = indefinido }
  for (i in 1:quantidade){
    png(file=paste0(titulo,"_",i,".png"),width=224, height=224)
    op <- par(bg = "white",mar=c(0,0,0,0))
    plot(c(0, 250), c(0, 250), type = "n", bty = 'n',asp=0,
         main="", xlab = "", ylab = "",
         xaxt='n', yaxt='n',xaxs="i", yaxs="i"  )
    image <- as.raster(matrix(0:1, ncol = 1, nrow = 1))
    
    ######################## xleft, ybottom, xright, ytop,
    oe_h = -20 + position()
    oe_v = 50 + position()
    rasterImage(olhos[s()], oe_h, oe_v, oe_h+240, oe_v+240, interpolate = T)
    od_h = 60 + position()
    od_v = 50 + position()
    rasterImage(olhos[s()], od_h, od_v, od_h+240, od_v+240, interpolate = T)
    b_h = 0 + position()
    b_v = 0 + position()
    rasterImage(humor[s()], b_h, b_v, b_h+240, b_v+240-ajuste, interpolate = T)
    par(op)
    dev.off()
  }
  setwd(paste0('../'))
}

gerar_imagens('feliz',1000)
gerar_imagens('triste',1000)
gerar_imagens('indefinido',1000)
