#' @title Calculate biomass growth using a logistic curve
#' @description The logistic curve (also called sigmoid curve) is commonly used
#' to model growth and saturation processes. For more discussion on biomass
#' growth curves see e.g. Paine et al. (2011). How to fit nonlinear plant growth
#' models and calculate growth rates: an update for ecologists.
#' https://doi.org/10.1111/j.2041-210X.2011.00155.x
#' Here the following equation is used:
#' ymin + ((ymax - ymin) / (1 + xmax * exp(-k * x))

calc_biomass_growth_curve <- function(
    x    = 1:50,
    xmax = 50,
    ymin = 0,
    ymax = 3,
    k    = 0.2
) {
  # Logistic curve
  return(ymin + ((ymax - ymin) / (1 + xmax * exp(-k * x))))
}

calc_biomass_expgrowth_curve <- function(
  x = 1:50,
  a = 0.5,
  r = 0.5
  ) {
  return(a*exp(r*x))
  }

calc_biomass_lineargrowth <- function(
    x = 1:50,
    a = 0,
    m = 5.46
    ){
  return(a+m*x)
}

calc_biomass_vonbert <- function(
    x = 1:50,
    y0 = 0,
    ymax = 250,
    b0 = 1,
    b1 = 0.064,
    b2 = 1.964
){
  pred.agb <- y0 + (ymax * (1 - b0 - exp(-b1*x))^b2)
  return(pred.agb)
}
