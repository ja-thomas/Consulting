distanceBetweenLines <- function(ax, bx, ay, by, segments = TRUE) {
  if (length(ax) != length(bx)) {
    stop(sprintf("The length of the offset vector 'ax' (%d) and direction vector 'bx' (%d) are not equal.", length(ax), length(bx)));
  }
  
  if (length(ay) != length(by)) {
    stop(sprintf("The length of the offset vector 'ay' (%d) and direction vector 'by' (%d) are not equal.", length(ay), length(by)));
  }
  
  if (length(ax) != length(ay)) {
    stop(sprintf("The line x(s) and y(t) are of different dimensions: %d vs %d", length(ax), length(ay)));
  }
  
  if (length(ax) <= 1)
    stop(sprintf("The lines must be in two or more dimensions: %d", length(ax)));
  
  ax <- as.vector(ax);
  bx <- as.vector(bx);
  ay <- as.vector(ay);
  by <- as.vector(by);
  
  # Consider the two lines in an K-space
  #   x(s) = a_x + b_x*t    (line 1)
  #   y(t) = a_y + b_y*s    (line 2)
  # where s and t are scalars and the other vectors in R^K.
  
  # Some auxillary calculations
  A <- sum(bx*bx);
  B <- 2*(sum(bx*ax)-sum(bx*ay));
  C <- 2*sum(bx*by);
  D <- 2*(sum(by*ay)-sum(by*ax));
  E <- sum(by*by);
  F <- sum(ax*ax) + sum(ay*ay);
  
  # Shortest distance between the two lines (points)
  G <- C^2-4*A*E;
  d2 <- (B*C*D+B^2*E+C^2*F+A*(D^2-4*E*F))/G;
  d <- sqrt(d2);
  
  # The points that are closest to each other.
  t <- (2*A*D+B*C)/G;   # t is on y(t)
  s <- (C*t-B)/(2*A);   # s is on x(s)
  
  if(segments){
    t <- max(c(0, min(t, 1)))
    s <- max(c(0, min(s, 1)))
  }

  
  # Get the coordinates of the two points on x(s) and y(t) that
  # are closest to each other.
  xs <- ax + bx*s;
  yt <- ay + by*t;
  if(segments){
    d <- sqrt(sum((xs-yt)^2))
  }
  list(ax=ax, bx=bx, ay=ay, by=by, s=s, t=t, xs=xs, yt=yt, distance=d, segments = segments);
}
