# 3-mathematical.functions.R

# Hill function ----
hill_fit <- function(results_array)
{ # Fittiing Hill equation (typically useful for dose reponse curves)
  
  # source: https://github.com/dritoshi/Fitting-Hill-equation/blob/master/bin/hill.r
  # Itoshi NIKAIDO <dritoshi@gmail.com>
  
  # Unpack data
  L  <- results_array$Inducer # x axis = independent variable ; L short for ligand
  y  <- results_array$value # dependent variable
  
  # # conf
  # output <- "results/hill.pdf"
  
  # initial
  y0 <- min(y)
  ymax.init <- 4e4
  n.init  <- 1
  Kd.init <- 1e-2
  
  # fitting Hill equation
  y.nls <- nlsLM(y ~ y0 + (ymax - y0) * L^n / (Kd^n + L^n), start = c(ymax = ymax.init, n = n.init, Kd = Kd.init))
  
  # # extract fitting data
  # y.nls.summary <- summary(y.nls)
  # y.nls.n       <- y.nls.summary$param[1]
  # y.nls.Kd      <- y.nls.summary$param[2]
  # y.nls.predict <- predict(y.nls)
  # results <- cbind(y, y.nls.predict)
}


# Self starter hill function ----

hill_fit.SS <- function(.data)
{
  # Resource : https://www.statforbiology.com/nonlinearregression/usefulequations#logistic_curve
  
  # Unpack data
  L  <- .data$Inducer # x axis = independent variable ; L short for ligand
  y  <- .data$value # dependent variable
  
  # fitting Hill equation
  y.nls <- drc::drm(y ~ L, fct = drc::LL.4(names = c('n', 'y0,', 'ymax', 'Kd')),
               data = .data)
  
  # y.nls <- nls(y ~ SSfpl(L, y0, ymax, Kd, n),
  #              data = .data)
  
}
