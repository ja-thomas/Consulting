



par(mfrow = c(1,2))
x <- seq(0,2*pi, length.out = 100)
plot(x, sin(x), type = "l", lwd = 2, main = "# Bäume = 1")
plot(stepfun(c(0, pi, 2*pi), c(0.6, 0.6,-0.6, -0.6)), do.points = FALSE,
     add = TRUE, lwd = 2, col = "blue")




knots <- cumsum(rexp(200, rate = 5))
knots <- knots[which(knots < 2*pi)]
sin_knots <- sin(knots)+ rnorm(length(knots),0,0.001)


plot(x, sin(x), type = "l", lwd = 2, main = paste("# Bäume =", length(sin_knots)))
plot(stepfun((knots + c(0, knots[-length(knots)]))*0.5,
             c(sin_knots[1], sin_knots)),
             add = TRUE, do.points = FALSE, lwd = 2, col = "blue")
     