
# What functions are needed?

# Function for sampling from / weighting a hitters spray distribution

# Adjustin this to any hitter


################################################################################
################################################################################
################################################################################
# First the hitter spray distribution:

# Evenly sample from a spray angle, launch angle, exit velo cube
# and reweight according to the likelihood of that ball in play actually being generated

# Use my statcast database to augment this part of the analysis:

library(DBI)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringi)

db = DBI::dbConnect(RPostgres::Postgres(),
                    dbname = ,#########, 
                    user = ,#######, 
                    password = ,##########, 
                    host = "localhost", 
                    port = 5432)


# Taking data from 2020 and on removes most of the issues from Statcasts "No-Nulls" policy
# which imputes specific values when angle/velocity aren't captured
data = tbl(db,"statcast") %>% filter(game_year >=2020,!is.na(launch_speed),!is.na(spray_angle),!is.na(launch_angle)) %>%  select(batter,stand,spray_angle,launch_angle,launch_speed,hit_distance_sc) %>% collect()


# Look at lefty and righty distributions

data_L = data %>% filter(stand=="L")
data_R = data %>% filter(stand=="R")

data_L %>% filter(spray_angle >=-50,spray_angle <=50) %>% ggplot(aes(x = spray_angle, y = launch_angle))+
  stat_bin_2d()

data_R %>% filter(spray_angle >=-50,spray_angle <=50) %>% ggplot(aes(x = spray_angle, y = launch_angle))+
  stat_bin_2d()

data %>% filter(spray_angle >=-50,spray_angle <=50) %>% ggplot(aes(x = spray_angle, y = launch_angle))+
  stat_bin_2d()+
  facet_wrap(~stand)

data %>% filter(spray_angle >=-50,spray_angle <=50) %>% ggplot(aes(x = launch_speed, y = launch_angle))+
  stat_bin_2d()+
  facet_wrap(~stand)

data %>% filter(spray_angle >=-50,spray_angle <=50) %>% ggplot(aes(x = spray_angle, y = launch_speed))+
  stat_bin_2d()+
  facet_wrap(~stand)

data %>% filter(spray_angle >=-50,spray_angle <=50) %>% ggplot(aes(x = spray_angle, y = launch_angle,z = launch_speed))+
  stat_summary_2d()+
  scale_fill_viridis_c(lim=c(60,98),oob=scales::squish)+
  facet_wrap(~stand)

# Make a 3d binning to train sampling model on

data = data %>% mutate(pull = if_else(stand=="R",spray_angle,-spray_angle))
#

# Check average pull/launch angle/ launch speed rates for LHB and RHB:

mean(filter(data,stand=="R",launch_angle <=10)$pull<=0)
mean(filter(data,stand=="R",launch_angle >=10)$pull<=0)
mean(filter(data,stand=="R")$launch_speed)
mean(filter(data,stand=="R")$launch_angle)

mean(filter(data,stand=="L",launch_angle <=10)$pull<=0)
mean(filter(data,stand=="L",launch_angle >=10)$pull<=0)
mean(filter(data,stand=="L")$launch_speed)
mean(filter(data,stand=="L")$launch_angle)

breaks1 <- seq(-45, 45, length=21)
breaks2 <- seq(-40, 80, length=41)
breaks3 <- seq(40, 115, length=31)
xints <- data.frame(
  x1=cut(data$pull, breaks=breaks1),
  x2=cut(data$launch_angle, breaks=breaks2),
  x3=cut(data$launch_speed, breaks=breaks3))
table(complete.cases(xints))
binned_results = xtabs(~ ., xints)



# Find the hit distances for batted balls in each bin

total_count = sum(binned_results)

binned_results_dframe = data.frame()
for(i in seq(20)){
  print(i)
  data_filter_1 = data %>% filter(pull>=breaks1[i],
                                  pull<=breaks1[i+1])
  for(j in seq(40)){
    data_filter_2 = data_filter_1 %>% filter(launch_angle>=breaks2[j],
                                             launch_angle<=breaks2[j+1])
    for(k in seq(30)){
      data_filter_3 = data_filter_2 %>% filter(launch_speed>=breaks3[k],
                                               launch_speed<=breaks3[k+1])
      binned_results_dframe = binned_results_dframe %>% bind_rows(data.frame(
        pull = (breaks1[i]+breaks1[i+1])/2,
        launch_angle = (breaks2[j]+breaks2[j+1])/2,
        launch_speed = (breaks3[k]+breaks3[k+1])/2,
        frac = binned_results[i,j,k] / total_count,
        hit_distance = median(data_filter_3$hit_distance_sc,na.rm=TRUE)
        
      ))
    }
  }
}

# turn hit distance to average of launch_angle/sspeed if bin is completely unpopulated
# Turn to zero if no cases of that launch angle/speed combination
# Just making sure there are no nans


binned_results_dframe = binned_results_dframe %>% group_by(launch_angle,launch_speed) %>% mutate(median_dist = median(hit_distance,na.rm=TRUE)) %>% ungroup() %>% mutate(hit_distance = if_else(is.na(hit_distance),median_dist,hit_distance)) %>% 
  select(-median_dist)

# which combinations still have no distance value?

binned_results_dframe %>% filter(is.na(hit_distance)) %>% group_by(launch_angle,launch_speed) %>% summarise(count = n(),frac = sum(frac,na.rm=TRUE)) %>% View()

# basically a bunch of groundballs and pop-ups

# give the groundballs zero hit distance and the pop-ups 175ft


binned_results_dframe = binned_results_dframe %>% mutate(hit_distance = if_else((is.na(hit_distance))&(launch_angle <=0),0,hit_distance),
                                                         hit_distance = if_else((is.na(hit_distance))&(launch_angle >=20),175,hit_distance))








# Find hang-time estimate by comparing to hang time of similar batted balls in the SMT dataset

# Ideally I'd just use physics here, but there will be spin/drag effects which are difficult
# to model and will be correlated to launch angle / spray angle / launch speed



ball_data = read.csv("SMT-Data-Challenge/ball_pos.csv")

game_data = read.csv("SMT-Data-Challenge/game_events.csv")

# Filter by balls hit into play
bip_data = game_data %>% group_by(game_str,play_id) %>% filter(event_code==4) %>% rename(hit_timestamp = timestamp)

# Normalise the time to start when the ball is hit
bip_trajectory = ball_data %>% left_join(bip_data) %>% filter(!is.na(event_code)) %>% group_by(game_str,play_id)%>%
  arrange(timestamp) %>% 
  mutate(
    time_zero = timestamp - hit_timestamp
  ) %>% filter(time_zero>=0)

# Estimate launch angle, spray angle, and speed of the batted balls at each timestamp
bip_trajectory = bip_trajectory %>% group_by(game_str,play_id)%>%
  arrange(time_zero) %>% mutate(
    speed = 1e3*sqrt((ball_position_x-lag(ball_position_x))**2+(ball_position_y-lag(ball_position_y))**2) / (timestamp - lag(timestamp)),
    launch_angle = 180 * atan((ball_position_z-lag(ball_position_z))/ (sqrt((ball_position_x-lag(ball_position_x))**2 + (ball_position_y-lag(ball_position_y))**2))) / pi,
    spray_angle = 180 * atan((ball_position_x)/(ball_position_y))/pi
  )

# get game and play ids of each ball in play
unique_plays = bip_data %>% select(game_str,play_id)


# Look at some of the data, index_in corresponds to each ball in play
index_in = 9

bip_trajectory %>% filter(game_str==unique_plays$game_str[index_in],play_id == unique_plays$play_id[index_in]) %>% select(time_zero,speed,launch_angle,spray_angle,
                                                                                                                          ball_position_x,ball_position_y,ball_position_z)

# take the trajectory after 3 timesteps, this seems to be when the data is mostly
# unaffected by the collision but is not so far as to be affected by bouncing etc.

bip_trajectory_data = bip_trajectory %>% group_by(game_str,play_id)%>%
  arrange(time_zero) %>% 
  mutate(timestep_num = seq_along(time_zero)) %>% 
  summarise(
    speed_next = first(speed[timestep_num==4]),
    launch_angle_next = first(launch_angle[timestep_num==4]),
    spray_angle_next = first(spray_angle[timestep_num==4]),
    speed = first(speed[timestep_num==3]),
    launch_angle = first(launch_angle[timestep_num==3]),
    spray_angle = first(spray_angle[timestep_num==3])
  )
  
  
bip_trajectory_data = bip_trajectory_data %>% mutate(speed = speed * 0.68)
  
  
# connect this to hang times:

outfield = game_data %>% group_by(game_str,play_id) %>% filter(sum(as.numeric((player_position %in% c(7,8,9)) & (event_code==2)),na.rm=TRUE)>=1) %>% ungroup()

# Find timestamps for when the ball hits the ground or is caught by the fielder

outfield_data = outfield %>% group_by(game_str,play_id) %>% arrange(timestamp) %>% mutate(receive_ball = (player_position %in% c(7,8,9)) & (event_code==2)) %>% summarise(
  pos_catch = first(player_position[event_code==2]),
  timestamp_hit_ground = first(timestamp[event_code %in% c(2,16,9)]),
  timestamp_get_ball = first(timestamp[event_code %in% c(2)]),
  timestamp_hit_into_play = first(timestamp[event_code==4]),
  hang_time = timestamp_hit_ground - timestamp_hit_into_play,
  .groups="drop") %>% ungroup() %>% mutate(
    catch = as.numeric(timestamp_get_ball <= timestamp_hit_ground)
  )
  
  

bip_trajectory_data = bip_trajectory_data %>% left_join(outfield_data)

# Use a GAM to predict hangtime of the ball:

bip_trajectory_data = bip_trajectory_data %>% filter(!is.na(launch_angle),!is.na(speed),!is.na(hang_time))
# ~2400 flyballs with which to measure hang time


library(mgcv)

bip_trajectory_data = bip_trajectory_data %>% filter(speed <=120,speed>=60,launch_angle<=80,launch_angle >=0)


hang_time_gam = gam(hang_time ~ s(launch_angle) + s(speed) + ti(launch_angle,speed),data = bip_trajectory_data)
summary(hang_time_gam)

library(tidymv)

plot_smooths(hang_time_gam,launch_angle)
plot_smooths(hang_time_gam,speed)

bip_trajectory_data$hang_time_pred = predict.gam(hang_time_gam,newdata = bip_trajectory_data)

ggplot(bip_trajectory_data,aes(x = speed, y = launch_angle, color = hang_time))+
  geom_point(size=3)+
  scale_color_distiller(lim = c(0,6500),oob= scales::squish,palette = "Spectral")
ggplot(bip_trajectory_data,aes(x = speed, y = launch_angle, color = hang_time_pred))+
  geom_point(size=3)+
  scale_color_distiller(lim = c(0,6500),oob= scales::squish,palette = "Spectral")

binned_results_dframe$hang_time = predict.gam(hang_time_gam,newdata = mutate(binned_results_dframe,speed = launch_speed))

# Cap and floor hang time

binned_results_dframe = binned_results_dframe %>% mutate(hang_time = if_else(hang_time <=0,0,hang_time),
                                                         hang_time = if_else(hang_time >=6500,6500,hang_time)) 

# Convert to seconds

binned_results_dframe = binned_results_dframe %>% mutate(hang_time = hang_time / 1000)

ggplot(binned_results_dframe,aes(x = launch_speed, y = hit_distance,color = hang_time))+
  geom_point(size=3)+
  scale_color_distiller(lim = c(0,6.5),oob= scales::squish,palette = "Spectral")

binned_results_dframe = binned_results_dframe %>% mutate(pos_x = hit_distance * sin(pull * pi / 180),
                                                         pos_y = hit_distance*cos(pull * pi / 180))

binned_results_dframe %>% filter(hit_distance!=0) %>% ggplot(aes(x = pos_x,y = pos_y,color = frac))+
  geom_point()+
  scale_color_viridis_c(lim=c(0,0.0001),oob = scales::squish)

binned_results_dframe %>% filter(hit_distance!=0) %>% ggplot(aes(x = pos_x,y = pos_y,color = hang_time))+
  geom_point()+
  scale_color_viridis_c(lim=c(0,7),oob = scales::squish)+
  facet_wrap(~launch_angle)



# figure out how to modify best later
# Probably just a simple launch speed multiplier, pull/spray multiplier (for GB/FB), and launch_angle shift

saveRDS(binned_results_dframe,file = "binned_batted_ball_sample_with_hang.rds")



# binned_results_dframe = readRDS("binned_batted_ball_sample_with_hang.rds")


# Tabulate changes to the distributions to quickly modify to any pull/la/ls needed


tabulate_spray_chart = function(batter_spray){
  results = data.frame()
  for(i in seq(-10,10)){
    if (i >= 0){
      batter_spray_launch = batter_spray %>% group_by(launch_speed,pull) %>% arrange(launch_angle) %>% mutate(
        frac = lag(frac,n=i,default = 0)
      ) %>% ungroup()
    }else{
      batter_spray_launch = batter_spray %>% group_by(launch_speed,pull) %>% arrange(launch_angle) %>% mutate(
        frac = lead(frac,n=as.integer(-i),default = 0)
      ) %>% ungroup()
    }
    for(j in seq(-5,5)){
      
      if(j >= 0){
        batter_spray_pull_gb = batter_spray_launch %>% group_by(launch_speed,launch_angle) %>% arrange(pull) %>% mutate(
          frac = if_else(launch_angle <=10,lag(frac,n=j,default = 0),frac)
        ) %>% ungroup()
      }else{
        batter_spray_pull_gb = batter_spray_launch %>% group_by(launch_speed,launch_angle) %>% arrange(pull) %>% mutate(
          frac = if_else(launch_angle <=10,lead(frac,n=as.integer(-j),default = 0),frac)
        ) %>% ungroup()
      }
      
      for(k in seq(-5,5)){
        
        
        if (k >= 0){
          batter_spray_pull_fb = batter_spray_pull_gb %>% group_by(launch_speed,launch_angle) %>% arrange(pull) %>% mutate(
            frac = if_else(launch_angle >10,lag(frac,n=k,default = 0),frac)
          ) %>% ungroup()
        }else{
          batter_spray_pull_fb = batter_spray_pull_gb %>% group_by(launch_speed,launch_angle) %>% arrange(pull) %>% mutate(
            frac = if_else(launch_angle >10,lead(frac,n=as.integer(-k),default = 0),frac)
          ) %>% ungroup()
        }
        
        results_out = batter_spray_pull_fb %>% filter(pull <=45,pull>=-45) %>% summarise(av_launch_angle = weighted.mean(launch_angle,frac),
                                                                                         av_launch_speed = weighted.mean(launch_speed,frac),
                                                                                         av_gb_pull = weighted.mean(pull<=0,frac * as.numeric(launch_angle<=10)),
                                                                                         av_fb_pull = weighted.mean(pull<=0,frac * as.numeric(launch_angle>10)))
        
        results = bind_rows(results,mutate(results_out,step_launch = i,
                                           step_gb_pull = j,
                                           step_fb_pull = k))
        
      }
    }
  }
  return(results)
}

batter_spray = binned_results_dframe

spray_chart_tabulate = tabulate_spray_chart(batter_spray)

spray_chart_tabulate

saveRDS(spray_chart_tabulate,"tabulated_spray_chart.rds")








###
###
###

# Finally make some nice charts that will be useful for the write-up


data = readRDS("models/binned_batted_ball_sample_with_hang.rds")

# Three graphs in a relatively cyclic arrangement

data %>% group_by("Exit Velocity" = round(launch_speed,-1),launch_angle) %>% summarise(
  relfrac = sum(frac),
  pull = weighted.mean(pull,frac,na.rm=TRUE)
) %>% ggplot(aes(x = launch_angle, y = relfrac,fill = pull))+
  geom_point(size=5,pch=21)+
  scale_fill_distiller(palette = "PuOr",lim = c(-20,20),oob = scales::squish)+
  theme_minimal()+
  theme(text = element_text(size = 20),
        legend.position = c(0.84,0.1))+
  facet_wrap(~`Exit Velocity`,labeller = label_both)+
  ylab("Relative Fraction")+
  xlab("Launch Angle /degrees")+
  labs(fill = "Spray Angle\n(Negative = Pull Side)")+
  ggtitle("Launch Angle / Exit Velocity Distributions")

ggsave("plots/launch_exit.jpg",bg="white")

data %>% group_by("Exit Velocity" = round(launch_speed,-1),pull) %>% summarise(
  relfrac = sum(frac),
  launch_angle = weighted.mean(launch_angle,frac,na.rm=TRUE)
) %>% ggplot(aes(x = pull, y = relfrac,fill = pull))+
  geom_point(size=5,pch=21)+
  scale_fill_distiller(palette = "PuOr",lim = c(-30,30),oob = scales::squish)+
  theme_minimal()+
  theme(text = element_text(size = 20),
        legend.position = c(0.84,0.1))+
  facet_wrap(~`Exit Velocity`,labeller = label_both)+
  ylab("Relative Fraction")+
  xlab("Spray Angle /degrees (Negative = Pull Side)")+
  labs(fill = "Launch Angle /degrees")+
  ggtitle("Spray Angle / Exit Velocity Distributions")

ggsave("plots/spray_exit.jpg",bg="white")

data %>% group_by("Launch Angle" = round(launch_angle,-1),pull) %>% summarise(
  relfrac = sum(frac),
  launch_speed = weighted.mean(launch_speed,frac,na.rm=TRUE)
) %>% ggplot(aes(x = pull, y = relfrac,fill = launch_speed))+
  geom_point(size=5,pch=21)+
  scale_fill_distiller(palette = "PuOr",lim = c(75,95),oob = scales::squish)+
  theme_minimal()+
  theme(text = element_text(size = 20),
        #legend.position = c(0.84,0.1)
        )+
  facet_wrap(~`Launch Angle`,labeller = label_both)+
  ylab("Relative Fraction")+
  xlab("Spray Angle\n(Negative = Pull Side)")+
  labs(fill = "Exit Velocity\n/mph")+
  ggtitle("Spray Angle / Launch Angle Distributions")


ggsave("plots/launch_spray.jpg",bg="white")

data = data %>% select(-`if_else(is.na(hit_distance), median_dist, hit_distance)`) #remove an accidental column
write.csv(data,"")