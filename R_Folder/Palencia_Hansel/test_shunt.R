library(tidyverse)

dat <- read_csv("R_Folder/Data/DATALOG_FLIGHT.TXT")

dat <- dat %>% 
  select(`Sample Number`, `NR exptime (s)`, `X Acceleration (G)`, `Y Acceleration (G)`, `Z Acceleration (G)`, `Flight Altitude (ft)`)

dat <- map(dat, as.numeric)

dat <- data.frame(dat)

# dat <- dat %>% 
#   filter(Flight.Altitude..ft. > 0,
#          NR.exptime..s. > 0)

# dat %>% 
#   ggplot() +
#   geom_point(aes(x = NR.exptime..s. , y = abs(dat$Flight.Altitude..ft. )))
# 
# dat %>% 
#   ggplot() +
#   geom_point(aes(x = NR.exptime..s./3600 , dat$X.Acceleration..G. ))
# 
# dat %>% 
#   ggplot() +
#   geom_point(aes(x = NR.exptime..s., dat$Y.Acceleration..G.))
# 
# dat %>% 
#   ggplot() +
#   geom_point(aes(x = NR.exptime..s., dat$Z.Acceleration..G.))



dat %>% 
  ggplot() +
  geom_point(aes(x = Sample.Number, y = sqrt((dat$X.Acceleration..G.^2) + (dat$Y.Acceleration..G.^2) + (dat$Z.Acceleration..G.^2)))) +
  scale_x_continuous(limits = c(6500, 17500))


dat %>% 
  ggplot() +
  geom_point(aes(x = Sample.Number , y = dat$Flight.Altitude..ft., color = sqrt((dat$X.Acceleration..G.^2) + (dat$Y.Acceleration..G.^2) + (dat$Z.Acceleration..G.^2)))) +
  scale_x_continuous(limits = c(6500, 17500))


# Something is wrong with this time sensor, it definitely isn't working how it's supposed to.

# I figured out the time isn't in the correct order for whatever reason, and there was obviously some dropped readings....

# It might be best to do a monte carlo A/R simulation to see if we can remove the bottom mess...



p1 <- dat %>% 
  filter(Sample.Number > 3500,
         Sample.Number <  17500
         #Flight.Altitude..ft. > 5000 --- This looks pretty good in cleaning misreads... I'm sure there is a good way to tell if something is misreading...
         ) %>% 
  select(NR.exptime..s., Flight.Altitude..ft.) %>% 
  distinct() %>% 
  ggplot() +
  geom_point(aes(x = order(NR.exptime..s.), y = Flight.Altitude..ft., color = "Gradient of Shunt Flow")) +
  scale_color_manual(values = "black") +
  labs(x = "Time (s)", y = "Altitude (ft)", color = "Shunt Flow") +
  theme_minimal()

















