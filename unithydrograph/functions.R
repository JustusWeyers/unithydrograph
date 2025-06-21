
datainput_plot = function(inp, outp, bf = NA) {
  x    = inp$t
  y    = inp$P_eff
  barW = unique(diff(inp$t))
  
  p = {
    plot(x = outp$t, y = outp$Q, "l", axes = FALSE,
         xlab = "Time t [h]",
         ylab = "P_eff [mm/h] / Q [m^3/s]",
         ylim = c(0, max(outp$Q)*1.125)
         )
    rect(
      xleft = x, 
      ybottom=0, 
      xright=x + barW, 
      ytop = y, 
      col = "lightblue",
      border = "white"
    )
    lines(x = outp$t, y = outp$Q)
    points(x = outp$t, y = outp$Q)
    text(x = outp$t, y = outp$Q, labels=round(outp$Q, 3), cex= 0.7, adj = c(-0.25, -0.25))

    if (!any(is.na(bf))) {
      segments(
        x0 = bf[1], y0 = outp$Q[outp$t == bf[1]],  
        x1 = bf[2], y1 = outp$Q[outp$t == bf[2]],
        col = "red"
      )
    }
    axis(1, at = outp$t)
    axis(2)
  }
  return(p)
}

# An ugly but (hopefully) working Function
lsq = function(QD, UHM, P, k_, m) {
  mirror = matrix(unlist(lapply(1:k_, function(k) {
    lim = (k + m - 1)
    mat = matrix(unlist(lapply(k:lim, function(i) {
      c(QD[i], -1 * UHM[i,]) * -P[i-(k-1)]
    })), nrow = m, byrow = TRUE)
    b = -1 * mat[,1]
    A = t(mat[,2:ncol(mat)])
    dFs = c(rep(0, times = k-1), sum(diag(A)))
    for (i in 2:ncol(A)) {
      A = A[-nrow(A), -1]
      dFs = c(dFs, sum(diag(A)))
    }
    dFs = c(dFs, rep(0, max(k_)-k), sum(b))
    return(dFs)
  })), nrow = k_, byrow = TRUE)
  b =  mirror[,ncol(mirror)]
  mirror = mirror[,1:k_]
  lower = t(mirror * upper.tri(mirror))
  mirror = mirror + lower
  return(list(A = mirror, b = b))
}

### Standard

empty_df = function(r, c, cn) {
  df = data.frame(matrix(NA_real_, nrow = r, ncol = c))
  colnames(df) = cn
  return(df)
}

col_1 = function(...) {
  shiny::column(1, ...)
}

col_2 = function(...) {
  shiny::column(2, ...)
}

col_3 = function(...) {
  shiny::column(3, ...)
}

col_4 = function(...) {
  shiny::column(4, ...)
}

col_5 = function(...) {
  shiny::column(5, ...)
}

col_6 = function(...) {
  shiny::column(6, ...)
}

col_8 = function(...) {
  shiny::column(8, ...)
}

col_10 = function(...) {
  shiny::column(10, ...)
}

col_12 = function(...) {
  shiny::column(12, ...)
}