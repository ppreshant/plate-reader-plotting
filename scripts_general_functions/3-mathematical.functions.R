# 3-mathematical.functions.R

# Hill function ----
hill_fit <- function(results_array)
{ # Fittiing Hill equation (typically useful for dose reponse curves)
  
  # source: https://github.com/dritoshi/Fitting-Hill-equation/blob/master/bin/hill.r
  # Itoshi NIKAIDO <dritoshi@gmail.com>
  
  # Unpack data
  L  <- results_array$L # x axis = independent variable
  y  <- results_array$y # dependant variable
  
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
