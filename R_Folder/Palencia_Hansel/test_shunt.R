library(tidyverse)


dat <- read_csv("DATALOG_FLIGHT.TXT")

dat <- dat %>% 
  select(`Sample Number`, `NR exptime (s)`, `X Acceleration (G)`, `Y Acceleration (G)`, `Z Acceleration (G)`, `Flight Altitude (ft)`)

dat <- map(dat, as.numeric)

dat <- data.frame(dat)

# dat <- dat %>% 
#   filter(Flight.Altitude..ft. > 0,
#          NR.exptime..s. > 0)

dat %>% 
  ggplot() +
  geom_point(aes(x = NR.exptime..s. , y = abs(dat$Flight.Altitude..ft. )))

dat %>% 
  ggplot() +
  geom_point(aes(x = NR.exptime..s./3600 , dat$X.Acceleration..G. ))

dat %>% 
  ggplot() +
  geom_point(aes(x = NR.exptime..s., dat$Y.Acceleration..G.))

dat %>% 
  ggplot() +
  geom_point(aes(x = NR.exptime..s., dat$Z.Acceleration..G.))





dat %>% 
  ggplot() +
  geom_point(aes(x = Sample.Number, y = sqrt((dat$X.Acceleration..G.^2) + (dat$Y.Acceleration..G.^2) + (dat$Z.Acceleration..G.^2)))) +
  scale_x_continuous(limits = c(6500, 17500))


dat %>% 
  ggplot() +
  geom_point(aes(x = Sample.Number , y = dat$Flight.Altitude..ft., color = sqrt((dat$X.Acceleration..G.^2) + (dat$Y.Acceleration..G.^2) + (dat$Z.Acceleration..G.^2))) +
  scale_x_continuous(limits = c(6500, 17500))

















