
"cgi" <-  function(x = long, y = lat, z = NA, w = NA, modproj = NA, mlong = NA, 
                   mlat = NA, col = 1, plot = T)
{
  #===============================================================================
  # CENTER OF GRAVITY, INERTIA AND ISOTROPY
  #
  # Routine from Geostatistics for Estimating Fish Abundance (GEFA)
  # & EU program Fisboat, DG-Fish, STREP n° 502572
  # Authors : M.Woillez (Mines-ParisTech), N.Bez (IRD) 
  #           and J.Rivoirard (Mines-ParisTech)
  # Last update : 01 march 2008 
  #
  # Argument:
  #	x	      The x-coordinate (MUST be a vector).
  #	y	      The y-coordinates (MUST be a vector).
  #	z	      The regionalised variable in 2d (MUST be a vector). 
  #         If missing, the results of 'cgi' will concern the samples only.
  # w	      Optional. A weight or a area of influence. Set to 1 if missing
  #	modproj	Optional. Indicates the type of projection to perform.
  # mlong   mean longitude in DEGREES of the data set to be transformed
  # mlat    mean latitude in DEGREES of the data set to be transformed
  #	        See 'dg2nm' for precisions.
  #	col	    Color for representing the axes.
  #	plot	  If plot=T the principal axes of the inertia are automatically 
  #		      plotted on an ALREADY EXISTING figure.
  #
  #	The output consists in a list with :
  #	xcg, ycg	    the coordinates of the center of gravity of z
  #	I	            the value of the inertia of z around its center of gravity
  # Imax          the value of the inertia of z according to the first principal 
  #               axes of the inertia
  # Imin          the value of the inertia of z according to the second principal 
  #               axes of the inertia
  #	Iso           the value of the isotropy of z
  # xaxe1, yaxe1  the coordinates of the first principal axes of the inertia of z
  # xaxe2, yaxe2	the coordinates of the second principal axes of the inertia of z
  #
  #===============================================================================
  
  miss <- function(x){
    length(x) == 1 && is.na(x)
  }
  if(miss(z))
    z <- rep(1, length(x))
  if(miss(w))
    w <- rep(1, length(x))
  sel <- !is.na(x * y * z * w)
  x <- x[sel]
  y <- y[sel]
  z <- z[sel]
  w <- w[sel]
  if(length(x[!is.na(x)]) > 0) {
    if(!miss(modproj)) {
      bid <- dg2nm(x = x, y = y, modproj = modproj, mlong = mlong, mlat = mlat)
      x <- bid$x
      y <- bid$y
    }
    # Center of gravity coordinates
    xg <- sum(x * z * w)/sum(z * w)
    yg <- sum(y * z * w)/sum(z * w)
    
    # Inertia
    dx <- x - xg
    dy <- y - yg
    d <- sqrt(dx^2 + dy^2)
    inert <- sum(z * w * (d^2))/sum(z * w)
    I <- inert	
    
    # Weigthed PCA 
    if(!is.na(I)) {
      M11 <- sum(dx^2 * z * w)
      M22 <- sum(dy^2 * z * w)
      M21 <- sum(dx * dy * z * w)
      M12 <- M21
      M <- matrix(c(M11, M12, M21, M22), byrow = T, ncol = 2)
      x1 <- eigen(M)$vectors[1, 1]
      y1 <- eigen(M)$vectors[2, 1]
      x2 <- eigen(M)$vectors[1, 2]
      y2 <- eigen(M)$vectors[2, 2]
      r1 <- eigen(M)$values[1]/(eigen(M)$values[1] + eigen(M)$values[2])
      
      # Principal axis coordinates
      e1 <- (y1/x1)^2
      sx1 <- x1/abs(x1)
      sy1 <- y1/abs(y1)
      sx2 <- x2/abs(x2)
      sy2 <- y2/abs(y2)
      xa <- xg + sx1 * sqrt((r1 * inert)/(1 + e1))
      ya <- yg + sy1 * sqrt((r1 * inert)/(1 + (1/e1)))
      xb <- 2 * xg - xa
      yb <- 2 * yg - ya
      xc <- xg + sx2 * sqrt(((1 - r1) * inert)/(1 + (1/e1)))
      yc <- yg + sy2 * sqrt(((1 - r1) * inert)/(1 + e1))
      xd <- 2 * xg - xc
      yd <- 2 * yg - yc
      Imax <- r1*inert 
      Imin <- (1-r1)*inert
      Iso <- sqrt(Imin/Imax)
    }
    else {
      xa <- NA
      ya <- NA
      xb <- NA
      yb <- NA
      xc <- NA
      yc <- NA
      xd <- NA
      yd <- NA
      Imax <- NA
      Imin <- NA
      Iso <- NA
    }
    if(!miss(modproj)) {
      bid <- nm2dg(x = c(xg, xa, xb, xc, xd), y = c(yg, ya, yb, yc, yd), 
                   modproj = modproj, mlong = mlong, mlat = mlat)
      res <- list(xcg = bid$x[1], ycg = bid$y[1], I = I, Imax = Imax, 
                  Imin = Imin, Iso = Iso, xaxe1 = bid$x[2:3], yaxe1 = bid$y[2:3], 
                  xaxe2 = bid$x[4:5],	yaxe2 = bid$y[4:5])
    }
    else res <- list(xcg = xg, ycg = yg, I = I, Imax = Imax, Imin = Imin, 
                     Iso = Iso, xaxe1 = c(xa, xb), yaxe1 = c(ya, yb), xaxe2 = c(xc, xd), 
                     yaxe2 = c(yc, yd))
    if(plot == T) {
      segments(res$xaxe1[1], res$yaxe1[1], res$xaxe1[2], res$yaxe1[2], col = col)
      segments(res$xaxe2[1], res$yaxe2[1], res$xaxe2[2], res$yaxe2[2], col = col)
    }
  }
  else {
    res <- list(xcg = NA, ycg = NA, I = NA, Imax = NA, 
                Imin = NA, Iso = NA, xaxe1 = NA, yaxe1 = NA, xaxe2 = NA, yaxe2 = NA)
  }
  res
}
