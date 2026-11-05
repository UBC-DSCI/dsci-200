# Displays a specific video frame from a 4D array (frames × height × width × RGB).
# The function extracts the selected frame, ensures pixel values are within [0, 1],
# and renders it on a new graphics page using grid.raster()
show_frame = function(x, frame_number){
  dims   <- dim(x)
  aframe <- x[frame_number,,,]
  aframe <- pmax(0, pmin(aframe,1) ) 
  dim(aframe) <- dims[2:4] 
  grid::grid.newpage()
  grid::grid.raster(aframe,interpolate = FALSE)
}

#--------------------------------------------
# Preprocess data before analysis using classical estimates
scale_filler_c <- function(dat, est_scale,dd){
  zeropix <- which(est_scale < 1e-12)
  nzero <- length(zeropix) 
  filler <- 2 * ((1:dd[1]) - (dd[1] + 1) / 2) / (dd[1] - 1)
  filler <- 0.9 / 255 * matrix(rep(filler, nzero), ncol = nzero, byrow = F)
  centers <- apply(dat[, zeropix], 2, FUN = mean)
  centers <- pmax(0, pmin(1, centers)); 
  filler <- sweep(filler, 2, centers, "+")
  dat[, zeropix] <- filler
  est_scale[zeropix] <- colSds(dat[, zeropix])
  list(dat = dat, scale = est_scale)
}

# Preprocess data before analysis using robust estimates
scale_filler_r <- function(dat, est_scale,dd){
  zeropix <- which(est_scale < 1e-12)
  nzero <- length(zeropix) 
  filler <- 2 * ((1:dd[1]) - (dd[1] + 1) / 2) / (dd[1] - 1)
  filler <- 0.9 / 255 * matrix(rep(filler, nzero), ncol = nzero, byrow = F)
  centers <- apply(dat[, zeropix], 2, FUN = median)
  centers <- pmax(0, pmin(1, centers)); 
  filler <- sweep(filler, 2, centers, "+")
  dat[, zeropix] <- filler
  est_scale[zeropix] <- estLocScale(dat[, zeropix])$scale
  list(dat = dat, scale = est_scale)
}

#--------------------------------------------
# Prepares PCA loading matrix for visualization
prepLoading <- function(v, dims) {
  dim(v) <- dims
  v <- apply(v, c(1, 2), sum)
  maxab <-  0.5 * max(abs(c(max(v), min(v))))
  v <- pmin(pmax(v, -maxab), maxab)
  return(list(loading = v, maxab = maxab))
}

#--------------------------------------------
# Compute residuals

compute_residuals <- function(X, loadings, center, k = 3){
  Xc <- sweep(X, 2, center, "-")          
  scores <- Xc %*% loadings[, 1:k]        
  Xfit <- scores %*% t(loadings[, 1:k])   
  Xfit <- sweep(Xfit, 2, center, "+")    
  Xres <- X - Xfit
  Xres
}


# Displays which pixels in a given frame are detected as outliers
show_frame_residual <- function(i, dat, Xres, loc_est, scale_est, cutoff = 200){
  
  # --- standardized residuals ---
  restemp <- (Xres[i, ] - loc_est) / scale_est
  dim(restemp) <- c(prod(dims[2:3]), dims[4])
  
  res2 <- restemp[,1]^2 + restemp[,2]^2 + restemp[,3]^2
  
  mask <- rep(0, prod(dims[2:3]))
  mask[res2 > cutoff] <- 1
  mask <- cbind(mask, mask, mask)
  dim(mask) <- dims[2:4]
  
  # --- original frame ---
  realim <- dat[i, ]
  realim <- pmax(0, pmin(realim, 1))
  dim(realim) <- dims[2:4]
  
  # --- masked residual image ---
  masked <- realim * mask
  masked <- pmax(0, pmin(masked, 1))
  dim(masked) <- dims[2:4]
  masked[masked == 0] <- 0.5
  
  # --- side-by-side output ---
  finalim <- array(0, dim = c(dims[2], 2*dims[3], dims[4]))
  finalim[, 1:dims[3], ] <- realim
  finalim[, (dims[3]+1):(2*dims[3]), ] <- masked
  
  grid::grid.newpage()
  grid::grid.raster(finalim, interpolate = FALSE)
}




