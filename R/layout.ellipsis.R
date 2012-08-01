layout.ellipsis <-
function(ig, a=1, b=1.5) {
    N <- length(V(ig))
    # circle angles
    ang <- seq(0, 2*pi, length.out=(N+1))[-1]
    x <- a*cos(ang)
    y <- b*sin(ang)
    cbind(x,y)
}
