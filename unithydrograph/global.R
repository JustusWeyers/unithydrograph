
demo_SystemInput = data.frame(
  t = c(0.0, 0.5, 1.0, 1.5, 2.0),
  P_eff = c(14.6, 19.5, 0.0, 29.3, 0.0)
)

demo_SystemOutput = data.frame(
  t = seq(0, 8, by = 0.5),
  Q = c(
    0.5, 12.9, 31.2, 25.1, 35.0, 32.5, 13.4, 5.56, 2.53, 1.43, 1.11, 0.80, 
    0.74, 0.71, 0.66, 0.65, 0.64
  )
)

demo_area = 8.762
demo_bf = c(0, 5.5)

demo_rain= data.frame(
  t = c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5),
  P_eff = c(50.0, 30.0, 20.0, 0, 0, 0, 0, 0, 0)
)

source("functions.R")
