#' Force field simulation for a set of points
#'
#' Force field simulation of interaction of set of points. Adapted form FField v0.1.0 package by Grigori Kapoustin on 27/04/2006
#' @param coords	matrix or data.frame consisting of two columns (x and y coordinates).
#' @param rep.fact repulsion force factor.
#' @param rep.dist.lmt repulsion distance limit.
#' @param attr.fact attraction force factor.
#' @param adj.max maximum position adjustment at each iteration.
#' @param adj.lmt position adjustment limit at which the simulation stops.
#' @param iter.max the maximum number of iterations beyond which simulation will end and a warning will be reported.
#' @export
#' @author Tokhir Dadaev
#' @seealso \url{http://cran.r-project.org/web/packages/FField}
#' @seealso \url{http://cran.r-project.org/web/packages/ggrepel}

FFieldPtRep <- function(coords, rep.fact = 20, rep.dist.lmt = 10, attr.fact = 0.2,
                         adj.max = 0.1, adj.lmt = 0.5, iter.max = 10000)
{
  # Adapted form FField package by Grigori Kapoustin
  # https://cran.r-project.org/web/packages/FField/
  # 27/04/2006

  if (length(dim(coords)) != 2) {
    stop("FFieldPtRep: dim(coords) must be 2\n")
  }
  if (ncol(coords) < 2) {
    stop("FFieldPtRep: ncol(coords) must be >= 2\n")
  }
  if (nrow(coords) < 2) {
    stop("FFieldPtRep: nrow(coords) must be >= 2\n")
  }
  coords <- as.data.frame(coords)
  colnames(coords)[(1:2)] <- c("x", "y")
  coords.orig <- coords
  FVCalc <- function(vects.x, vects.y, f.fact, f.type = "invsq") {
    d.sq <- (vects.x^2 + vects.y^2)
    d <- sqrt(d.sq)
    vects.x <- vects.x/d
    vects.y <- vects.y/d
    if (f.type == "invsq") {
      d.sq[d >= rep.dist.lmt] <- Inf
      vect.f.x <- vects.x/d.sq * f.fact
      vect.f.y <- vects.y/d.sq * f.fact
    }
    else if (f.type == "lin") {
      vect.f.x <- vects.x * d * f.fact
      vect.f.y <- vects.y * d * f.fact
    }
    else {
      stop("FFieldPtRep: Unexpected f.type\n")
    }
    vect.f.x[is.na(vect.f.x)] <- 0
    vect.f.y[is.na(vect.f.y)] <- 0
    f.vect <- cbind(colSums(vect.f.x), colSums(vect.f.y))
    return(f.vect)
  }
  iter <- 0
  repeat {
    vects.x <- apply(coords, 1, function(c) (c[1] - coords$x))
    vects.y <- apply(coords, 1, function(c) (c[2] - coords$y))
    f.rep.v <- FVCalc(vects.x = vects.x, vects.y = vects.y,
                      f.fact = rep.fact, f.type = "invsq")
    vects.orig <- coords.orig - coords
    f.attr.v <- FVCalc(vects.x = t(as.matrix(vects.orig$x)),
                       vects.y = t(as.matrix(vects.orig$y)), f.fact = attr.fact,
                       f.type = "lin")
    f.v <- f.rep.v + f.attr.v
    if (all(abs(f.v) <= adj.lmt)) {
      (break)()
    }
    mv.vect <- apply(f.v, c(1, 2), function(x) sign(x) *
                       min(abs(x), adj.max))
    coords <- coords + mv.vect
    if ((iter <- iter + 1) > iter.max) {
      warning("FFieldPtRep: Maximum iterations exceeded ",
              "without convergence.\n")
      (break)()
    }
  }
  return(coords)
}
