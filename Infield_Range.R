# Look at likelihood on infielder reaching the ball given it's speed / angle

# Look at the time to reach the ball too and the position at which it is fielded

# Take balls hit into play on the infield:

library(tidyverse)


position_data = read.csv("SMT-Data-Challenge/player_pos.csv")
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




# plot a histogram of the speed (converted from ft/s to mph with the factor of 0.68)
ggplot(bip_trajectory_data,aes(x = speed*0.68))+
  geom_histogram()+
  xlim(0,200)

ggplot(bip_trajectory_data,aes(x = speed_next*0.68))+
  geom_histogram()+
  xlim(0,200)


# a clear tail of wrong speeds here, in those cases take the next timestep,
# if that doesn't fix it then just max out at 160ft/s (not far under 120mph, the max for elite MLB hitters)

ggplot(bip_trajectory_data,aes(x = spray_angle))+
  geom_histogram()+
  xlim(-50,50)

ggplot(bip_trajectory_data,aes(x = spray_angle_next))+
  geom_histogram()+
  xlim(-50,50)

ggplot(bip_trajectory_data,aes(x = launch_angle))+
  geom_histogram()+
  xlim(-90,90)

ggplot(bip_trajectory_data,aes(x = launch_angle_next))+
  geom_histogram()+
  xlim(-90,90)

# spray angles look fine

# launch angle looks a little iffy, but anything close to 0 or below is similar in
# practice as they're all groundballs

# launch angle_next has very few low launch angles, best to avoid using this


# filter out extreme values
bip_trajectory_data = bip_trajectory_data %>% mutate(speed = if_else(speed >=170,speed_next,speed),
                                                     speed = if_else(speed >=170,160,speed),
                                                     spray_angle = if_else(spray_angle >=50,50,spray_angle),
                                                     spray_angle = if_else(spray_angle <=-50,-50,spray_angle),
                                                     launch_angle = if_else(launch_angle >=90,90,launch_angle),
                                                     launch_angle = if_else(launch_angle <=-90,-90,launch_angle),)

ggplot(bip_trajectory_data,aes(x = spray_angle,y = launch_angle))+
  geom_point()+
  xlim(-90,90)

# Get groundball fielding probability based on angles and ball speed:


bip_data_fielding = game_data %>% left_join(select(bip_data,game_str,play_id,hit_timestamp)) %>% filter(!is.na(hit_timestamp)) %>% 
  group_by(game_str,play_id) %>% arrange(timestamp) %>% filter(event_code==2) %>% 
  summarise(first_fielder = first(player_position),
            fielding_timestamp = first(timestamp),
            fielding_time = first(timestamp-hit_timestamp))

bip_data_fielding

# Find the likelihood of each position fielding a ball:

bip_data_position = position_data %>% left_join(bip_data_fielding) %>% filter(!is.na(first_fielder))
bip_data_position = bip_data_position %>% filter(player_position <=9) %>% group_by(game_str,play_id,player_position) %>% 
  arrange(timestamp) %>% summarise(init_pos_x  = first(field_x),
                                   init_pos_y  = first(field_y)) %>% mutate(init_angle = 180*atan(init_pos_x/init_pos_y)/pi,
                                                                            init_depth = sqrt((init_pos_x)**2+(init_pos_y)**2))

bip_data_position


# join together

bip_data_position = bip_data_position %>% left_join(bip_trajectory_data) %>% left_join(bip_data_fielding)

bip_data_position = bip_data_position %>% mutate(fielding_angle_diff = abs(init_angle - spray_angle),
                                                 fielded = as.numeric(first_fielder==player_position))

# a plot of angle/speed to see where fielding plays are made

bip_data_position %>% filter(launch_angle<=10) %>% ggplot(aes(x = fielding_angle_diff, y = speed,color = factor(fielded)))+
  geom_point()

# I tried using GAMs for this but they took too much runtime to evaluate, stick
# to linear/logistic regressions for now


# filter groundballs as those with below 10 degree launch angle
# rather arbitrary but will probably get rid of most line drives over fielder's heads
# especially since launch angle isn't very well modelled for groundballs using this
# data anyway


# What's important for modelling range?
#-- angle, reaction time (1/speed), speed, depth, launch angle
# Include a few interaction terms here, angle-speed, angle-depth, angle-time


# The linear model had some deficiencies with weaker balls hit at low angle_diff,
# and hard balls hit at larger angles, include a couple of flags to improve the
# model for these specific cases

range_model = glm(formula = fielded ~ fielding_angle_diff + speed + launch_angle+init_depth + I(1/speed) +
                 I(fielding_angle_diff*speed)+I(fielding_angle_diff*init_depth) + I(fielding_angle_diff / speed) +
                 I(as.numeric(fielding_angle_diff<=15)*as.numeric(speed <=130)*as.numeric(speed >=80)) +
                 I(as.numeric(fielding_angle_diff>=5)*as.numeric(fielding_angle_diff<=20)*as.numeric(speed <=160)*as.numeric(speed >=125)),family = "binomial",data = filter(bip_data_position,launch_angle<=10,player_position %in%c(3,4,5,6)))
summary(range_model)
bip_data_position_inf = bip_data_position %>% filter(player_position%in%c(3,4,5,6),launch_angle<=10)

bip_data_position_inf$fielded_pred = predict(range_model,newdata = bip_data_position_inf,type = "response")

data_to_save = bip_data_position_inf
write.csv(data_to_save,"data/infield_range_data.csv")

# Some general plots of the relationships between variables

bip_data_position_inf %>% ggplot(aes(x = fielding_angle_diff, y = speed,color = factor(fielded)))+
  geom_point()+
  labs(color="fielded")

bip_data_position_inf %>% ggplot(aes(x = fielding_angle_diff, y = speed,color = fielded_pred))+
  geom_point()+
  scale_color_viridis_c(lim=c(0,1),oob = scales::squish)+
  labs(color = "fielded\npred")

bip_data_position_inf %>% ggplot(aes(x = launch_angle, y = init_depth,color = fielded_pred))+
  geom_point()+
  scale_color_viridis_c(lim=c(0,1),oob = scales::squish)+
  labs(color = "fielded\npred")

# plots of residuals, see if there are any clear issues:

bip_data_position_inf %>% group_by(round_field_pred = 10*round(10*fielded_pred,0)) %>% 
  summarise(fielded_pred = mean(fielded_pred),
            fielded = mean(fielded),
            count = n()) %>% ggplot(aes(x = fielded_pred,y = fielded,size = count))+geom_point()+
  annotate("segment",x=0,y=0,xend=1,yend=1)


bip_data_position_inf %>% group_by(round_init_depth = 10*round(0.1*init_depth,0)) %>% 
  summarise(fielded_pred = mean(fielded_pred),
            fielded = mean(fielded),
            count = n()) %>% ggplot(aes(x = fielded_pred,y = fielded,size = count,color = round_init_depth))+geom_point()+
  annotate("segment",x=0,y=0,xend=0.3,yend=0.3)+
  scale_color_viridis_c()


bip_data_position_inf %>% group_by(round_angle = 4*round(0.25*fielding_angle_diff,0)) %>% 
  summarise(fielded_pred = mean(fielded_pred),
            fielded = mean(fielded),
            count = n()) %>% ggplot(aes(x = fielded_pred,y = fielded,size = count,color = round_angle))+geom_point()+
  annotate("segment",x=0,y=0,xend=0.9,yend=0.9)+
  scale_color_viridis_c()

bip_data_position_inf %>% group_by(round_speed = 10*round(0.1*speed,0)) %>% 
  summarise(fielded_pred = mean(fielded_pred),
            fielded = mean(fielded),
            count = n()) %>% ggplot(aes(x = fielded_pred,y = fielded,size = count,color = round_speed))+geom_point()+
  annotate("segment",x=0,y=0,xend=0.25,yend=0.25)+
  scale_color_viridis_c()



bip_data_position_inf %>% group_by(round_angle = 3*round(0.33*launch_angle,0)) %>% 
  summarise(fielded_pred = mean(fielded_pred),
            fielded = mean(fielded),
            count = n()) %>% ggplot(aes(x = fielded_pred,y = fielded,size = count,color = round_angle))+geom_point()+
  annotate("segment",x=0,y=0,xend=0.25,yend=0.25)+
  scale_color_viridis_c()


bip_data_position_inf %>% group_by(player_position,round_angle = 4*round(0.25*fielding_angle_diff,0)) %>% 
  summarise(fielded_pred = mean(fielded_pred),
            fielded = mean(fielded),
            count = n()) %>% ggplot(aes(x = fielded_pred,y = fielded,color = factor(player_position)))+geom_point(size=10)+
  annotate("segment",x=0,y=0,xend=0.9,yend=0.9)


bip_data_position_inf %>% group_by(round_angle = 4*round(0.25*fielding_angle_diff,0),round_depth= 10*round(0.1*init_depth,0)) %>% 
  summarise(fielded_pred = mean(fielded_pred),
            fielded = mean(fielded),
            count = n()) %>% ggplot(aes(x = round_angle,y = round_depth,size = count))+
  geom_point(aes(color = fielded_pred))+
  annotate("segment",x=0,y=0,xend=0.9,yend=0.7)+
  scale_color_viridis_c(lim = c(0,0.9),oob = scales::squish)

bip_data_position_inf %>% group_by(round_angle = 4*round(0.25*fielding_angle_diff,0),round_depth= 10*round(0.1*init_depth,0)) %>% 
  summarise(fielded_pred = mean(fielded_pred),
            fielded = mean(fielded),
            count = n()) %>% ggplot(aes(x = round_angle,y = round_depth,size = count))+
  geom_point(aes(color = fielded - fielded_pred))+
  annotate("segment",x=0,y=0,xend=0.9,yend=0.9)+
  scale_color_viridis_c(lim = c(-0.1,0.1),oob = scales::squish)


bip_data_position_inf %>% group_by(round_angle = 4*round(0.25*fielding_angle_diff,0),round_speed= 10*round(0.1*speed,0)) %>% 
  summarise(fielded_pred = mean(fielded_pred),
            fielded = mean(fielded),
            count = n()) %>% ggplot(aes(x = round_angle,y = round_speed,size = count))+
  geom_point(aes(color = fielded_pred))+
  annotate("segment",x=0,y=0,xend=0.9,yend=0.9)+
  scale_color_viridis_c(lim = c(0,0.7),oob = scales::squish)

bip_data_position_inf %>% group_by(round_angle = 4*round(0.25*fielding_angle_diff,0),round_speed= 10*round(0.1*speed,0)) %>% 
  summarise(fielded_pred = mean(fielded_pred),
            fielded = mean(fielded),
            count = n()) %>% ggplot(aes(x = round_angle,y = round_speed,size = count))+
  geom_point(aes(color = fielded - fielded_pred))+
  annotate("segment",x=0,y=0,xend=0.9,yend=0.9)+
  scale_color_distiller(lim = c(-0.2,0.2),oob = scales::squish,palette = "PuOr")

# In general I can't see any clear actionable residuals here, as stated I already
# fixed a couple with specific flags for locations in speed/angle space


# Double check fielding likelihood by position

bip_data_position_inf %>% group_by(player_position) %>% summarise(field = mean(fielded,na.rm=TRUE),
                                                                  field_pred = mean(fielded_pred,na.rm=TRUE),
                                                                  difference = field - field_pred,
                                                                  count = n())

# Interestingly no differences based on positions here. Only at the 1% level or so
# I guess the model will auto-adjust by using fielder depth as some proxy to some
# level


# Make a plot of fielding probability by speed/angle

bip_data_position_inf %>% ggplot(aes(x = fielding_angle_diff, y = speed*0.68,color = fielded_pred))+
  geom_point(size = 3)+
  scale_color_viridis_c(lim=c(0,1),oob = scales::squish,labels = scales::percent_format())+
  labs(color = "fielded\npred")+
  theme_minimal()+
  theme(text = element_text(size = 20))+
  labs(color = "Fielding\nLikelihood")+
  xlab("Spray Angle Difference to Fielder")+
  ylab("Exit Velocity mph")+
  xlim(0,40)


bip_data_position_inf %>% filter(fielding_angle_diff<40) %>% ggplot(aes(x = fielding_angle_diff, y = speed*0.68,z = fielded_pred))+
  stat_summary_2d(bins=10)+
  scale_fill_viridis_c(lim=c(0,1),oob = scales::squish,labels = scales::percent_format())+
  theme_minimal()+
  theme(text = element_text(size = 20))+
  labs(fill = "Expected\nFielding\nRate")+
  xlab("Spray Angle Difference to Fielder")+
  ylab("Exit Velocity /mph")+
  xlim(-0.01,44)

bip_data_position_inf %>% filter(fielding_angle_diff<40) %>% ggplot(aes(x = fielding_angle_diff, y = speed*0.68,z = fielded))+
  stat_summary_2d(bins=10)+
  scale_fill_viridis_c(lim=c(0,1),oob = scales::squish,labels = scales::percent_format())+
  labs(fill = "fielded\npred")+
  geom_density_2d(color="red")+
  theme_minimal()+
  theme(text = element_text(size = 20))+
  labs(fill = "Fielding\nRate")+
  xlab("Spray Angle Difference to Fielder")+
  ylab("Exit Velocity /mph")+
  xlim(-0.01,44)









# Next move onto modelling the time taken to field the ball
# This is especially important for timing up infield plays


# for modelling time:
# speed, 1/speed, initial depth

# Take only balls which were actually fielded

bip_data_infielded = filter(bip_data_position,launch_angle<=10,player_position %in%c(3,4,5,6),fielded==1)

bip_data_infielded = bip_data_infielded %>% mutate(fielding_time = if_else(fielding_time >=5000,5000,as.double(fielding_time)))

field_model_time = lm(formula = fielding_time ~ fielding_angle_diff + speed + launch_angle + init_depth + I(1/speed),data = bip_data_infielded)
summary(field_model_time)


bip_data_infielded$fielding_time_pred = field_model_time$fitted.values

data_to_save = bip_data_infielded
write.csv(data_to_save,"data/infield_timing_data.csv")
# Plots of residuals:


bip_data_infielded %>% group_by(round_field_pred = 250*round(0.004*fielding_time_pred,0)) %>% 
  summarise(fielded_pred = mean(fielding_time_pred),
            fielded = mean(fielding_time),
            count = n()) %>% ggplot(aes(x = fielded_pred,y = fielded,size = count))+geom_point()+
  annotate("segment",x=0,y=0,xend=5000,yend=5000)


bip_data_infielded %>% group_by(round_init_depth = 10*round(0.1*init_depth,0)) %>% 
  summarise(fielded_pred = mean(fielding_time_pred),
            fielded = mean(fielding_time),
            count = n()) %>% ggplot(aes(x = fielded_pred,y = fielded,size = count,color = round_init_depth))+geom_point()+
  annotate("segment",x=1000,y=1000,xend=3000,yend=3000)+
  scale_color_viridis_c()


bip_data_infielded %>% group_by(round_angle = 4*round(0.25*fielding_angle_diff,0)) %>% 
  summarise(fielded_pred = mean(fielding_time_pred),
            fielded = mean(fielding_time),
            count = n()) %>% ggplot(aes(x = fielded_pred,y = fielded,size = count,color = round_angle))+geom_point()+
  annotate("segment",x=1000,y=1000,xend=5000,yend=5000)+
  scale_color_viridis_c()

bip_data_infielded %>% group_by(round_speed = 10*round(0.1*speed,0)) %>% 
  summarise(fielded_pred = mean(fielding_time_pred),
            fielded = mean(fielding_time),
            count = n()) %>% ggplot(aes(x = fielded_pred,y = fielded,size = count,color = round_speed))+geom_point()+
  annotate("segment",x=1000,y=1000,xend=4000,yend=4000)+
  scale_color_viridis_c()



bip_data_infielded %>% group_by(round_angle = 3*round(0.33*launch_angle,0)) %>% 
  summarise(fielded_pred = mean(fielding_time_pred),
            fielded = mean(fielding_time),
            count = n()) %>% ggplot(aes(x = fielded_pred,y = fielded,size = count,color = round_angle))+geom_point()+
  annotate("segment",x=1000,y=1000,xend=4000,yend=4000)+
  scale_color_viridis_c()


# No clear residuals here with any of the factors considered here, good to move on

# make a plot of fielding time by speed/angle

bip_data_infielded %>% group_by(round_angle = 4*round(0.25*fielding_angle_diff,0),round_speed= 0.68*10*round(0.1*speed,0)) %>% 
  summarise(fielded_pred = median(fielding_time_pred)/1000,
            fielded = median(fielding_time),
            count = n()) %>% ggplot(aes(x = round_angle,y = round_speed,size = count))+
  geom_point(aes(fill = fielded_pred),pch=21)+
  annotate("segment",x=0,y=0,xend=0.9,yend=0.9)+
  scale_fill_distiller(lim = c(1.5,2.8),oob = scales::squish,palette = "PuOr")+
  xlim(0,40)+
  labs(fill = "Predicted Time\nto Field /s",size = "Number of\nEvents")+
  xlab("Spray Angle to Fielder")+
  scale_size_continuous(range = c(3,8))+
  ylab("Ball Speed /mph")+
  theme_minimal()+
  theme(text = element_text(size=20))

ggsave("plots/fielding_time.jpg",bg="white")

# make a plot by speed / initial depth
bip_data_infielded %>% group_by(round_depth = 10*round(0.1*init_depth,0),round_speed= 10*round(0.1*speed,0)) %>% 
  summarise(fielded_pred = mean(fielding_time_pred),
            fielded = mean(fielding_time),
            count = n()) %>% ggplot(aes(x = round_depth,y = round_speed,size = count))+
  geom_point(aes(fill = fielded_pred),pch = 21)+
  annotate("segment",x=0,y=0,xend=0.9,yend=0.9)+
  scale_fill_distiller(lim = c(1000,3000),oob = scales::squish,palette = "PuOr")

bip_data_infielded %>% group_by(round_depth = 10*round(0.1*init_depth,0),round_speed= 10*round(0.1*speed,0)) %>% 
  summarise(fielded_pred = mean(fielding_time_pred),
            fielded = mean(fielding_time),
            count = n()) %>% ggplot(aes(x = round_depth,y = round_speed,size = count))+
  geom_point(aes(fill = fielded),pch = 21)+
  annotate("segment",x=0,y=0,xend=0.9,yend=0.9)+
  scale_fill_distiller(lim = c(1000,3000),oob = scales::squish,palette = "PuOr")

# Looks like the model does a good job from these plots







# Finally model the depth at which the ball is fielded

# For modelling depth:
# speed, 1/speed, initial depth, (maybe launch angle)


bip_data_position = position_data %>% left_join(bip_data_fielding) %>% filter(!is.na(first_fielder))
bip_data_position = bip_data_position %>% filter(player_position <=9) %>% group_by(game_str,play_id,player_position) %>% 
  arrange(timestamp) %>% summarise(init_pos_x  = first(field_x),
                                   init_pos_y  = first(field_y),
                                   field_pos_x = first(field_x[timestamp >= fielding_timestamp]),
                                   field_pos_y = first(field_y[timestamp >= fielding_timestamp])) %>% mutate(init_angle = 180*atan(init_pos_x/init_pos_y)/pi,
                                                                                                             init_depth = sqrt((init_pos_x)**2+(init_pos_y)**2))

# join together

bip_data_position = bip_data_position %>% left_join(bip_trajectory_data) %>% left_join(bip_data_fielding)

bip_data_position = bip_data_position %>% mutate(fielding_angle_diff = abs(init_angle - spray_angle),
                                                 fielded = as.numeric(first_fielder==player_position))


bip_data_infielded = filter(bip_data_position,launch_angle<=10,player_position %in%c(3,4,5,6),fielded==1)

bip_data_infielded = bip_data_infielded %>% mutate(fielded_depth = sqrt((field_pos_x)**2+(field_pos_y)**2))

# Make some plots

bip_data_infielded %>% ggplot(aes(x = fielding_angle_diff, y = speed, color = fielded_depth))+
  geom_point()+
  scale_color_viridis_c(lim=c(50,170),oob = scales::squish)+
  facet_wrap(~player_position)

bip_data_infielded %>% ggplot(aes(x =init_depth, y = speed, color = fielded_depth))+
  geom_point()+
  scale_color_viridis_c(lim=c(50,170),oob = scales::squish)+
  facet_wrap(~player_position)

# make a linear model:

field_model_depth = lm(formula = fielded_depth ~ fielding_angle_diff + speed +I(1/speed) + launch_angle+init_depth+ 1/speed,data = bip_data_infielded)
summary(field_model_depth)


bip_data_infielded$fielding_depth_pred = field_model_depth$fitted.values


data_to_save = bip_data_infielded
write.csv(data_to_save,"data/infield_depth_data.csv")

# Plot the predictions vs actual and look for residuals


bip_data_infielded %>% group_by(round_field_pred = 10*round(0.1*fielding_depth_pred,0)) %>% 
  summarise(fielded_pred = mean(fielding_depth_pred),
            fielded = mean(fielded_depth),
            count = n()) %>% ggplot(aes(x = fielded_pred,y = fielded,size = count))+geom_point()+
  annotate("segment",x=0,y=0,xend=200,yend=200)


bip_data_infielded %>% group_by(round_init_depth = 10*round(0.1*init_depth,0)) %>% 
  summarise(fielded_pred = mean(fielding_depth_pred),
            fielded = mean(fielded_depth),
            count = n()) %>% ggplot(aes(x = fielded_pred,y = fielded,size = count,color = round_init_depth))+geom_point()+
  annotate("segment",x=50,y=50,xend=200,yend=200)+
  scale_color_viridis_c()


bip_data_infielded %>% group_by(round_angle = 4*round(0.25*fielding_angle_diff,0)) %>% 
  summarise(fielded_pred = mean(fielding_depth_pred),
            fielded = mean(fielded_depth),
            count = n()) %>% ggplot(aes(x = fielded_pred,y = fielded,size = count,color = round_angle))+geom_point()+
  annotate("segment",x=50,y=50,xend=200,yend=200)+
  scale_color_viridis_c()

bip_data_infielded %>% group_by(round_speed = 10*round(0.1*speed,0)) %>% 
  summarise(fielded_pred = mean(fielding_depth_pred),
            fielded = mean(fielded_depth),
            count = n()) %>% ggplot(aes(x = fielded_pred,y = fielded,size = count,color = round_speed))+geom_point()+
  annotate("segment",x=50,y=50,xend=200,yend=200)+
  scale_color_viridis_c()



bip_data_infielded %>% group_by(round_angle = 3*round(0.33*launch_angle,0)) %>% 
  summarise(fielded_pred = mean(fielding_depth_pred),
            fielded = mean(fielded_depth),
            count = n()) %>% ggplot(aes(x = fielded_pred,y = fielded,size = count,color = round_angle))+geom_point()+
  annotate("segment",x=50,y=50,xend=200,yend=200)+
  scale_color_viridis_c()





# This model has no clear residuals too




bip_data_infielded %>% group_by(round_depth = 10*round(0.1*init_depth,0),round_speed= 10*round(0.1*speed,0)) %>% 
  summarise(fielded_pred = mean(fielding_depth_pred),
            fielded = mean(fielded_depth),
            count = n()) %>% ggplot(aes(x = round_depth,y = 0.68*round_speed,size = count))+
  geom_point(aes(fill = fielded_pred),pch = 21)+
  annotate("segment",x=0,y=0,xend=0.9,yend=0.9)+
  scale_fill_distiller(lim = c(40,180),oob = scales::squish,palette = "PuOr")+
  labs(fill = "Predicted\nFielding Depth\n/ft",size = "Number of\nEvents")+
  theme_minimal()+
  scale_size_continuous(range = c(3,9))+
  xlab("Initial Fielder Depth /ft")+
  ylab("Exit Velocity /mph")+
  theme(text = element_text(size = 20))


bip_data_infielded %>% group_by(round_depth = 10*round(0.1*init_depth,0),round_speed= 10*round(0.1*speed,0)) %>% 
  summarise(fielded_pred = mean(fielding_depth_pred),
            fielded = mean(fielded_depth),
            count = n()) %>% ggplot(aes(x = round_depth,y = fielded_pred,fill = 0.68*round_speed,size = count))+
  annotate("segment",x = 0,y = 0,xend = 200,yend=200,color="black",size=1)+
  geom_point(aes(fill = 0.68 * round_speed),pch = 21)+
  annotate("segment",x=0,y=0,xend=0.9,yend=0.9)+
  scale_fill_distiller(lim = c(40,120),oob = scales::squish,palette = "PuOr")+
  labs(fill = "Exit Velocity\n/mph",size = "Number of\nEvents")+
  theme_minimal()+
  scale_size_continuous(range = c(3,9))+
  xlab("Initial Fielder Depth /ft")+
  ylab("Predicted Fielding Depth /ft")+
  theme(text = element_text(size = 20))

ggsave("fielding_depth.jpg",bg="white")


bip_data_infielded %>% group_by(round_depth = 10*round(0.1*init_depth,0),round_speed= 10*round(0.1*speed,0)) %>% 
  summarise(fielded_pred = mean(fielding_depth_pred),
            fielded = mean(fielded_depth),
            count = n()) %>% ggplot(aes(x = round_depth,y = round_speed,size = count))+
  geom_point(aes(fill = fielded),pch = 21)+
  annotate("segment",x=0,y=0,xend=0.9,yend=0.9)+
  scale_fill_distiller(lim = c(40,180),oob = scales::squish,palette = "PuOr")



bip_data_infielded %>% group_by(round_depth = 10*round(0.1*init_depth,0),round_angle = 4*round(0.25*fielding_angle_diff,0)) %>% 
  summarise(fielded_pred = mean(fielding_depth_pred),
            fielded = mean(fielded_depth),
            count = n()) %>% ggplot(aes(y = round_depth,x = round_angle,size = count))+
  geom_point(aes(fill = fielded_pred),pch = 21)+
  annotate("segment",x=0,y=0,xend=0.9,yend=0.9)+
  scale_fill_distiller(lim = c(40,180),oob = scales::squish,palette = "PuOr")

bip_data_infielded %>% group_by(round_depth = 10*round(0.1*init_depth,0),round_angle = 4*round(0.25*fielding_angle_diff,0)) %>% 
  summarise(fielded_pred = mean(fielding_depth_pred),
            fielded = mean(fielded_depth),
            count = n()) %>% ggplot(aes(y = round_depth,x = round_angle,size = count))+
  geom_point(aes(fill = fielded),pch = 21)+
  annotate("segment",x=0,y=0,xend=0.9,yend=0.9)+
  scale_fill_distiller(lim = c(40,180),oob = scales::squish,palette = "PuOr")








# Save all the models before moving on

saveRDS(range_model,"range_model_GLM.rds")

saveRDS(field_model_depth,"depth_model_LM.rds")

saveRDS(field_model_time,"time_model_LM.rds")











###
###

# Do some checks of infield depth (or reaction time) vs angle difference
# for infielders fielding the ball

# This model may over-rate the ability to field the ball when standing very shallow

range_model = readRDS("range_model_GLM.rds")

data = data.frame(speed = runif(10000,80,170),
                  fielding_angle_diff = runif(10000,0,30),
                  launch_angle = runif(10000,-30,0),
                  init_depth = runif(10000,50,150))

data$fielded = predict.glm(type="response",object = range_model,newdata = data)
data$fielded

# data$reaction_time = 

ggplot(data,aes(x = fielding_angle_diff,y = init_depth,color = fielded))+
  geom_point()




data_inf = filter(bip_data_position,launch_angle<=10,player_position %in%c(3,4,5,6))

data_inf$fielded_pred = predict.glm(type="response",object = range_model,newdata = data_inf)

ggplot(data_inf,aes(x = fielding_angle_diff,y = init_depth,color = fielded))+
  geom_point()
ggplot(data_inf,aes(x = fielding_angle_diff,y = init_depth/speed,z = fielded))+
#geom_point()+
  ylim(0,1.5)+
  stat_summary_hex()

data_inf %>% group_by(reac_time = 0.2*round(init_depth / speed*5,0)) %>% filter(reac_time <=2) %>% summarise(fielded = mean(fielded),
                                                                                                             fielded_pred = mean(fielded_pred),
                                                                                                             count = n()) %>% 
  ggplot(aes(x = reac_time, y = fielded, size = count))+geom_point()+geom_point(color="red",aes(y = fielded_pred))


data %>% group_by(reac_time = 0.2*round(init_depth / speed*5,0)) %>% filter(reac_time <=2) %>% summarise(fielded = mean(fielded),
                                                                                                             count = n()) %>% 
  ggplot(aes(x = reac_time, y = fielded, size = count))+geom_point()


# Clearly not enough of a punishment for standing too close (in terms of reaction time)

# Introduce a "fielding cliff" to punish these close standers

# This can simulate what isn't picked up in the data (because no one ever does it!)

# I can't find much data on how infield depth affects fielding rate in the literature
# Assume a kind of linear effect which decreases the fielding rate based on the depth of the player


data = data %>% mutate(reaction_factor = if_else(init_depth<=150,0.01*(init_depth - 50),1),
                       reaction_factor = if_else(reaction_factor <=0,0,reaction_factor),
                       fielded_reaction = fielded * reaction_factor)

data_to_plot = data %>% group_by(reac_time = 0.2*round(init_depth / speed*5,0)) %>% filter(reac_time <=2.5) %>% summarise("With Distance\nCliff" = mean(fielded_reaction),
                                                                                                         "Original" = mean(fielded),
                                                                                                         count = n()) %>%
  pivot_longer(cols = c("With Distance\nCliff","Original"))
ggplot(data_to_plot,aes(x = reac_time, y = value, color = name))+geom_point()+
  geom_point(size=8)+
  theme_minimal()+
  theme(text = element_text(size=20))+
  xlab("Estimated Reaction Time /s")+
  ylab("Modeled Fielding Rate")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(color = "Model")

ggsave("plots/Cliff_Effect.jpg",bg="white")


ggplot(data,aes(x = fielding_angle_diff,y = init_depth,color = fielded_reaction))+
  geom_point()+
  labs(color = "fielded")+
  xlab("Spray Angle to Fielder")+
  ylab("Initial Fielder Depth")+
  theme_minimal()

# Looks much more reasonable now, with short-range fielding being appropriately punished!



