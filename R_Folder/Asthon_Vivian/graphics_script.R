library(tidyverse)
library(plot3D)
library(plotly)

#### Reading in the data
datalog <- read_csv("data/DATALOG_FLIGHT (1).TXT") 

View(datalog)
#dat <- read_csv("data/PLOTDATA - PLOTDATA.csv")
#dat <- read_csv("data/PLOTDATA.csv")
#View(dat)

colnames(datalog)


#### Wrangling 
dat <- datalog %>%
  select(`Sample Number`, `Nanolab elapsed time (ms)`, `NR exptime (s)`, `X Acceleration (G)`, `Y Acceleration (G)`, `Z Acceleration (G)`, `Flight Altitude (ft)`, `Flight State`) %>% 
  map(., as.numeric)

dat <- as.data.frame(dat)

dat <- dat %>% 
  mutate(total_gs = (abs(X.Acceleration..G.) + abs(Y.Acceleration..G.) + abs(Z.Acceleration..G.)))


#### Questions 

## how often is a sample being taken?

## why is some of the NR expected times negative?

## We want a graph that is Flow_rate vs G's, right?


## Is launch site and landing at 0 ft for the flight Altitude?

## Why are the sample numbers all janky? there are some sample numbers that occur multiple times but has different values for the other columns, but this doesn't happen for all of the sample numbers, what's up with that??

## Is the text file comprised of multiple launches or sensors? (in reference to the question above)



### Graphs 

## Sample number vs Flight Alt -- looks parabolic but somthing is strange with the sample number
dat %>% 
  ggplot(aes(x = `Sample.Number`, y = `Flight.Altitude..ft.`, color = total_gs)) +
  geom_point() +
  theme_bw() + 
  labs(x = "Sample Number", y = "Flight Altitude (ft)", title = "The Flight pattern for Altitude and G's dependte of Sample number") + 
  scale_color_continuous("Total G's")

## elapsed time vs Flight Alt --  is the same
dat %>% 
  ggplot(aes(x = Nanolab.elapsed.time..ms., y = `Flight.Altitude..ft.`, color = total_gs)) +
  geom_point() +
  theme_bw()

### in both of the graphs above oyu see the expected parabolic shape, but with random points at the bottom, whats up with that?

## Expected time vs Flight Alt 
## you can see our three different samples 
dat %>% 
  ggplot(aes(x = NR.exptime..s., y = `Flight.Altitude..ft.`, color = total_gs)) +
  geom_point() +
  theme_bw()



## What's up with this? going down and up agian 

## Flight Alt vs Total G's 
dat %>% 
  filter(Flight.Altitude..ft. > 0) %>% 
  ggplot(aes(x = Flight.Altitude..ft., y = total_gs, color = Sample.Number)) +
  geom_point() +
  theme_bw()

## What? Lets look at depth.

x <- dat$Sample.Number
y <- dat$Flight.Altitude..ft.
z <- dat$total_gs

scatter3D(x, y, z) ## Can't move this graph

## Flight Alt vs Sample Num vs Total G's
fig <- plot_ly(dat, 
               x = ~Sample.Number, 
               y = ~Flight.Altitude..ft., 
               z = ~total_gs, 
               color = ~Sample.Number) %>% 
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = "Sample Number"),
                      yaxis = list(tilte = "Altitude"),
                      zaxis = list(title = "Total G's")))

fig


## X g's vs Y g's vs Z g's
fig <- plot_ly(dat, 
               x = ~X.Acceleration..G., 
               y = ~Y.Acceleration..G., 
               z = ~Z.Acceleration..G., 
               color = ~total_gs) %>% 
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = "X"),
                      yaxis = list(tilte = "Y"),
                      zaxis = list(title = "Z")))

fig

####################

## Somthing is strange with time and sample


## Sample vs elapsed time -- near perfect correlation

dat %>% 
  ggplot(aes(x = Nanolab.elapsed.time..ms., y = Sample.Number)) +
  geom_point()

#### any replacing Nanolab.elapsed.time..ms. with Sample.Number would result in the same graph

## Sample Num vs expected time

dat %>% 
  ggplot(aes(x = NR.exptime..s., y = Sample.Number)) +
  geom_point()

### What??? The graph above seems strange, it's a sideways parabola 


## elapsed vs expected

dat %>% 
  ggplot(aes(x = Nanolab.elapsed.time..ms., y = NR.exptime..s.)) +
  geom_point()
  


### Spliting the data 

datalog <- datalog %>% 
  mutate(n = 1:31210)
## new set starts at 173 and 5000 with tile headers
datalog1 <- datalog[1:172,]
datalog2 <- datalog[174:4999,]
datalog3 <- datalog[5001:31210,]

## Wrangling it all

### datalog1
dat1 <- datalog1 %>%
  select(`Sample Number`, `Nanolab elapsed time (ms)`, `NR exptime (s)`, `X Acceleration (G)`, `Y Acceleration (G)`, `Z Acceleration (G)`, `Flight Altitude (ft)`, `Flight State`) %>% 
  map(., as.numeric)

dat1 <- as.data.frame(dat1)

dat1 <- dat1 %>% 
  mutate(total_gs = (abs(X.Acceleration..G.) + abs(Y.Acceleration..G.) + abs(Z.Acceleration..G.)))

### datalog2
dat2 <- datalog2 %>%
  select(`Sample Number`, `Nanolab elapsed time (ms)`, `NR exptime (s)`, `X Acceleration (G)`, `Y Acceleration (G)`, `Z Acceleration (G)`, `Flight Altitude (ft)`, `Flight State`) %>% 
  map(., as.numeric)

dat2 <- as.data.frame(dat2)

dat2 <- dat2 %>% 
  mutate(total_gs = (abs(X.Acceleration..G.) + abs(Y.Acceleration..G.) + abs(Z.Acceleration..G.)))

### datalog3

dat3 <- datalog3 %>%
  select(`Sample Number`, `Nanolab elapsed time (ms)`, `NR exptime (s)`, `X Acceleration (G)`, `Y Acceleration (G)`, `Z Acceleration (G)`, `Flight Altitude (ft)`, `Flight State`) %>% 
  map(., as.numeric)

dat3 <- as.data.frame(dat3)

dat3 <- dat3 %>% 
  mutate(total_gs = (abs(X.Acceleration..G.) + abs(Y.Acceleration..G.) + abs(Z.Acceleration..G.)))


#### Looking at graphs 

## Sample number vs Flight Alt -- looks parabolic but somthing is strange with the sample number
dat1 %>% 
  ggplot(aes(x = `Sample.Number`, y = `Flight.Altitude..ft.`, color = total_gs)) +
  geom_point() +
  theme_bw()
## dat1 is a flat line with this graph


dat2 %>% 
  ggplot(aes(x = `Sample.Number`, y = `Flight.Altitude..ft.`, color = total_gs)) +
  geom_point() +
  theme_bw()
### dat2 is also a flat line



dat3 %>% 
  ggplot(aes(x = `Sample.Number`, y = `Flight.Altitude..ft.`, color = total_gs)) +
  geom_point() +
  theme_bw()

## has a parabola but it still looks like there are repeat numbers



#############

## elapsed time vs Flight Alt --  is the same
dat %>% 
  ggplot(aes(x = Nanolab.elapsed.time..ms., y = `Flight.Altitude..ft.`, color = total_gs)) +
  geom_point() +
  theme_bw()

### in both of the graphs above oyu see the expected parabolic shape, but with random points at the bottom, whats up with that?

## Expected time vs Flight Alt 
## you can see our three different samples 
dat %>% 
  ggplot(aes(x = NR.exptime..s., y = `Flight.Altitude..ft.`, color = total_gs)) +
  geom_point() +
  theme_bw()



#############################

dat1 %>% 
  ggplot(aes(x = NR.exptime..s., y = `Flight.Altitude..ft.`, color = total_gs)) +
  geom_point() +
  theme_bw()



dat2 %>% 
  ggplot(aes(x = NR.exptime..s., y = `Flight.Altitude..ft.`, color = total_gs)) +
  geom_point() +
  theme_bw()



dat3 %>% 
  ggplot(aes(x = NR.exptime..s., y = `Flight.Altitude..ft.`, color = total_gs)) +
  geom_point() +
  theme_bw()

