#!/usr/bin/env python
# -*- coding: utf-8 -*-

'''
Back off numbers of axis
And draw black and white rectangles
correlated to each tick position.
'''
import matplotlib.pyplot as plt
import numpy as np

fig = plt.figure()
ax = fig.add_subplot(111)
ax.tick_params(direction='in', length=6, width=2, colors='g',pad=22)
plt.show()

## Codes from menugget.blogspot.com

#map.frame.R

#bar.width is the width of the frame in inches
#deg.ext is the extention of the frame segments in degrees
#other parameters from polygon can be passed to the frame
#requires PBSmapping package (function "joinPolys")
map.frame <- function(bar.width=NULL, deg.ext=1, ...){
 if(missing(bar.width)) bar.width <- mean(par()$pin)*0.02
 usr <- par()$usr
 bar.width.x <- bar.width/par()$pin[1] * (usr[2]-usr[1])
 bar.width.y <- bar.width/par()$pin[2] * (usr[4]-usr[3])
 bar.lims.x <- seq(-180,180,deg.ext)
 bar.lims.y <- seq(-90,90,deg.ext)
 is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
 bar.bottom <- data.frame(PID=rep(1, 4), POS=1:4, X=c(usr[1],usr[1]+bar.width.y,usr[2]-bar.width.y,usr[2]), Y=c(usr[3],usr[3]+bar.width.x,usr[3]+bar.width.x,usr[3]))
 bar.top <- data.frame(PID=rep(1, 4), POS=1:4, X=c(usr[1],usr[1]+bar.width.y,usr[2]-bar.width.y,usr[2]), Y=c(usr[4],usr[4]-bar.width.x,usr[4]-bar.width.x,usr[4]))
 bar.left <- data.frame(PID=rep(1, 4), POS=1:4, X=c(usr[1],usr[1],usr[1]+bar.width.y,usr[1]+bar.width.y), Y=c(usr[3],usr[4], usr[4]-bar.width.x,usr[3]+bar.width.x))
 bar.right <- data.frame(PID=rep(1, 4), POS=1:4, X=c(usr[2],usr[2]-bar.width.y,usr[2]-bar.width.y,usr[2]), Y=c(usr[3],usr[3]+bar.width.x,usr[4]-bar.width.x,usr[4]))
 
 #X axis
 for(i in seq(length(bar.lims.x)-1)){
  xs <- c(bar.lims.x[i], bar.lims.x[i], bar.lims.x[i+1], bar.lims.x[i+1])
  #bottom
  ys <- c(usr[3], usr[3]+bar.width.y, usr[3]+bar.width.y, usr[3])
  bottom <- data.frame(PID=rep(1, 4), POS=1:4, X=xs, Y=ys)
  bottom.join <- joinPolys(bottom,bar.bottom)
 
  #top
  ys <- c(usr[4]-bar.width.y, usr[4], usr[4], usr[4]-bar.width.y)
  top <- data.frame(PID=rep(1, 4), POS=1:4, X=xs, Y=ys)
  top.join <- joinPolys(top,bar.top)
 
  tmp.col <- ifelse(is.wholenumber(i/2), "black", "white")
  polygon(bottom.join$X, bottom.join$Y, col=tmp.col, ...)
  polygon(top.join$X, top.join$Y, col=tmp.col, ...)
 }
 #Y axis
 for(i in seq(length(bar.lims.y)-1)){
  ys <- c(bar.lims.y[i], bar.lims.y[i], bar.lims.y[i+1], bar.lims.y[i+1])
  #left
  xs <- c(usr[1], usr[1]+bar.width.x, usr[1]+bar.width.x, usr[1])
  left <- data.frame(PID=rep(1, 4), POS=1:4, X=xs, Y=ys)
  left.join <- joinPolys(left,bar.left)
 
  #right
  xs <- c(usr[2], usr[2]-bar.width.x, usr[2]-bar.width.x, usr[2])
  right <- data.frame(PID=rep(1, 4), POS=1:4, X=xs, Y=ys)
  right.join <- joinPolys(right,bar.right) 
  
  tmp.col <- ifelse(is.wholenumber(i/2), "black", "white")
  polygon(left.join$X, left.join$Y, col=tmp.col, ...)
  polygon(right.join$X, right.join$Y, col=tmp.col, ...)
 }
 box()
}
