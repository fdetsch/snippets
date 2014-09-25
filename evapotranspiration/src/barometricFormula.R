barometricFormula <- function(z, 
                              gp, 
                              ta, 
                              p.levels, 
                              ...) {
  
  diff.abs <- abs(z - gp)
  id.min <- which.min(diff.abs)
  
  if (diff.abs[id.min - 1] < diff.abs[id.min + 1]) {
    h0 <- gp[id.min]
    h1 <- z
    t <- ta[id.min]
    p0 <- p.levels[id.min]
  } else {
    h0 <- gp[id.min + 1]
    h1 <- z  
    t <- ta[id.min + 1]
    p0 <- p.levels[id.min + 1]
  }
  
  dh <- h1 - h0
  g <- 9.807
  m <- 0.02896
  r <- 8.314
  
  p1 <- p0 * exp((-1) * m * g / (r * t) * dh)
  
  return(p1)
}