# Measure infield arm strength
# Infield transfer time (collection to release)
# Batter home to first time
# What timings between ball and batter at 1B result in hits and outs


library(tidyverse)

position_data = read.csv("SMT-Data-Challenge/player_pos.csv")
ball_data = read.csv("SMT-Data-Challenge/ball_pos.csv")

game_data = read.csv("SMT-Data-Challenge/game_events.csv")

# Take all infield throws from 2B,3B,SS to 1B
infield_throws = game_data %>% group_by(game_str,play_id) %>% filter(sum(as.numeric((player_position %in% c(4,5,6)) & (event_code==3)),na.rm=TRUE)>=1) %>% 
  filter(sum(event_code==3,na.rm=TRUE)<=1) %>% # Remove double plays (2 throws)
  ungroup()

infield_throws_data = infield_throws %>% group_by(game_str,play_id) %>% mutate(throw_start = (player_position %in% c(4,5,6)) & (event_code==3),
                                                                               throw_end = (player_position %in% c(3)) & (event_code==2)) %>% summarise(
                                                                                 pos_throw = first(player_position[event_code==3]),
                                                                                 Time_0 = first(timestamp[throw_start]),
                                                                                 Time_1 = first(timestamp[throw_end]),
                                                                                 .groups="drop") %>% ungroup()

infield_throws_data = infield_throws_data %>% filter(!is.na(Time_1),!is.na(Time_0))

# Get time and speed of throws

infield_throw_balls = ball_data %>% left_join(infield_throws_data) %>% filter(!is.na(Time_1),!is.na(Time_0),
                                                                              timestamp <=Time_1,timestamp >= Time_0) %>% group_by(
                                                                                game_str,
                                                                                play_id
                                                                              ) %>% arrange(timestamp) %>% mutate(time = timestamp - first(timestamp),
                                                                                                                  speed =  1e3 * sqrt((ball_position_x-lag(ball_position_x))**2+(ball_position_y-lag(ball_position_y))**2) / (timestamp - lag(timestamp))) %>% 
  filter(max(time)<=4000) %>% ungroup() # remove really long throws (probably errors)

ggplot(infield_throw_balls,aes(x = time,y = speed,color = factor(paste(game_str,play_id))))+
  geom_line(show.legend = FALSE)

# Generally capturing the throws well, can see the effect of drag on throw speed over time


# plot the direction of all the throws, coloured by throwing position
# Looks good, generally all are going to 1B
infield_throw_balls %>% filter(pos_throw %in% c(4,5,6),time <=2000) %>% ggplot(aes(x = ball_position_x,y = ball_position_y,color = factor(pos_throw),group = paste(game_str,play_id)))+
  geom_line()+
  annotate("point",x=0,y=60.5,color="blue",size=5)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  coord_fixed()



# Get summary stats for each throw, speed, distance, time
infield_throw_balls_data = infield_throw_balls %>% group_by(game_str,play_id) %>% arrange(timestamp) %>% mutate(time_pos = seq_along(timestamp)) %>% summarise(throw_time = first(Time_1 - Time_0),
                                                                                                                                                               Time_0 = first(Time_0),
                                                                                                                                                               Time_1 = first(Time_1),
                                                                                                                                                               throw_dist = sqrt(first(ball_position_x - 90/sqrt(2))**2 + first(ball_position_y - 90/sqrt(2))**2),
                                                                                                                                                               Speed_init = median(speed[time_pos<=5],na.rm=TRUE),
                                                                                                                                                               pos_throw = first(pos_throw),
                                                                                                                                                               .groups="drop")
infield_throw_balls_data

# Plot throw speed and distance

infield_throw_balls_data %>% filter(pos_throw %in% c(4,5,6)) %>% ggplot(aes(x = Speed_init, y = throw_dist,color = factor(pos_throw)))+
  geom_point()


# Look at time saved from harder throws:

infield_throw_balls_data %>% filter(pos_throw %in% c(4,5,6),throw_time <=2000) %>% ggplot(aes(y = Speed_init, x = throw_dist,color = throw_time))+
  geom_point()+
  scale_color_viridis_c(lim=c(500,1500),oob = scales::squish)

infield_throw_balls_data %>% filter(pos_throw %in% c(4,5,6),throw_time <=2000) %>% ggplot(aes(y = throw_time, x = 1e3* throw_dist / Speed_init))+
  geom_point()+
  annotate("segment",x=400,xend=2000,y=400,yend=2000)


# Linear model for throw time based on speed and distance:
# Ideally would just use physics (SUVAT + drag) but easier here to just make
# a simple fit that would do the job
lm(throw_time ~ I(1e3 * throw_dist / Speed_init),data = infield_throw_balls_data)

# throw_time = -113.627 + 1.167 * 1000 * throw_dist / Speed_init
# Can use this formula when simulating plays

infield_throw_balls_data %>% filter(pos_throw %in% c(4,5,6),throw_time <=2000) %>% ggplot(aes(y = throw_time, x = -113.627 + (1.167 * 1e3* throw_dist / Speed_init)))+
  geom_point(aes(color = throw_dist))+
  annotate("segment",x=400,xend=2000,y=400,yend=2000)








hist(infield_throw_balls_data$Speed_init)
infield_throw_balls_data %>% group_by(pos_throw) %>% summarise(mean_speed = mean(Speed_init,na.rm=TRUE),
                                                               sd_speed = sd(Speed_init,na.rm=TRUE),
                                                               count = n())


ggplot(infield_throw_balls_data,aes(x = Speed_init,fill = factor(pos_throw,levels=c(4,6,5),labels=c("2B","SS","3B"))))+
  geom_density(alpha=0.5)+
  ylab("Relative Fraction")+
  xlab("Initial Throw Speed ft/s")+
  labs(fill = "Position")+
  theme_minimal()+
  theme(text = element_text(size=20))

ggsave("plots/throw_speed.jpg",bg="white",width=7,height=4)


data_to_save = infield_throw_balls_data
write.csv(data_to_save,"data/infield_throw_speed_data.csv")


# This gives throw speed distribution by position
# some throws aren't full effort if the play isn't super close


# averages are 2B = 89 ft/s
# ss = 106 ft/s
# 2b = 109 ft/s

# Goes down to 75-80 ft/s
# Up to ~ 130 ft/s


# Calculate transfer time

# For each infield throw, get the time from acquiring the ball to throwing it


data_transfer = data.frame()
for(i in seq(dim(infield_throw_balls_data)[1])){
  print(i)
  play = infield_throw_balls_data[i,]
  data_temp = game_data %>% filter(game_str == play$game_str,play_id == play$play_id,
                                   player_position == play$pos_throw)
  
  ball_in = filter(data_temp,event_code==2)$timestamp[1]
  ball_out = filter(data_temp,event_code==3)$timestamp[1]
  time_transfer = ball_out - ball_in
  
  dataframe_temp = data.frame(game_str = play$game_str,
                              play_id = play$play_id,
                              pos_throw = play$pos_throw,
                              ball_in = ball_in,
                              ball_out = ball_out,
                              time_transfer = time_transfer)
  data_transfer = bind_rows(data_transfer,dataframe_temp)
}
data_transfer

hist(data_transfer$time_transfer)

data_transfer %>% group_by(pos_throw) %>% summarise(mean_transfer = mean(time_transfer,na.rm=TRUE),
                                                    sd_transfer = sd(time_transfer,na.rm=TRUE),
                                                    count = n())


ggplot(data_transfer,aes(x = time_transfer/1000,fill = factor(pos_throw,levels=c(4,6,5),labels=c("2B","SS","3B"))))+
  geom_density(alpha=0.5)+
  ylab("Relative Fraction")+
  xlab("Transfer Time /s")+
  labs(fill = "Position")+
  theme_minimal()+
  xlim(0,2.5)+
  theme(text = element_text(size=20))

ggsave("plots/throw_transfer.jpg",bg="white",width=7,height=4)


data_to_save = data_transfer
write.csv(data_to_save,"data/infield_throw_transfer_data.csv")


# Average transfer time by position:
#2b = 1.08s
#ss = 1.05s
#3b = 1.31s

# Down to 0.5s, up to 2s

# Plenty of these will be in non-urgent situations
# When simulating: take 0.2s off each average value to simulate the case where a play might
#'be close




# Look at batter position and speed at ball release:
# Will give a general idea of how batters get down the line


position_data_infield = position_data %>% filter(player_position==10) %>% left_join(infield_throw_balls_data) %>% filter(!is.na(throw_time))



position_data_infield = position_data_infield %>% group_by(game_str,play_id) %>% arrange(timestamp) %>% 
  mutate(Batter_Speed = 1e3 * sqrt((field_x-lag(field_x))**2+(field_y-lag(field_y))**2) / (timestamp - lag(timestamp)),
         Batter_Dist = sqrt((field_x - 90/sqrt(2))**2 + (field_y - 90/sqrt(2))**2)) %>% ungroup()

position_data_infield = filter(position_data_infield,Batter_Speed <=40)%>% group_by(game_str,play_id) %>% arrange(timestamp) %>% 
  mutate(Time_Zero = timestamp -first(timestamp)) %>% ungroup()

position_data_infield %>% mutate(Time_throw = timestamp - Time_0) %>% ggplot(aes(x = Time_throw,y = Batter_Speed,color = factor(paste(game_str,play_id))))+
  geom_line(show.legend = FALSE)

# Can see the clear acceleration curve here

position_data_infield_timer = position_data_infield %>% mutate(Batter_Time = 1e3 * Batter_Dist / Batter_Speed) %>% group_by(game_str,play_id) %>% 
  arrange(timestamp) %>% filter(timestamp>=Time_0) %>% summarise(
    throw_time = first(throw_time),
    throw_dist = first(throw_dist),
    Speed_init = first(Speed_init),
    pos_throw = first(pos_throw),
    Batter_Speed = first(Batter_Speed),
    Batter_Dist = first(Batter_Dist),
    Batter_Time = first(Batter_Time),
    timestamp = first(timestamp),
    Time_0 = first(Time_0)
  )
position_data_infield_timer

ggplot(position_data_infield_timer,aes(x = throw_dist, y = Batter_Dist,color = Batter_Time - throw_time))+
  geom_point()+
  scale_color_distiller(palette = "PuOr",lim = c(-1000,1000),oob = scales::squish)

hist(position_data_infield_timer$Batter_Speed)


position_data_infield_timer %>% filter(pos_throw %in% c(4,5,6),throw_time <=2000) %>% ggplot(aes(y = Speed_init, x = throw_dist,color = Batter_Time - throw_time))+
  geom_point()+
  scale_color_viridis_c(lim=c(-1000,1000),oob = scales::squish)


# Looks like generally almost every throw beats the batter to the base
# maybe throws aren't made if batter is clearly going to make it

# Look at what ends up in hits and outs in a few lines' time
# First make some play animations to see how things are working


# Animate_Play

library(gganimate)

plot_play = function(throw_id_in){
  game_str_in = position_data_infield_timer$game_str[throw_id_in]
  play_id_in = position_data_infield_timer$play_id[throw_id_in]
  data_play = filter(ball_data,game_str==game_str_in,play_id==play_id_in)
  data_pos =  position_data %>% filter(game_str==game_str_in,play_id==play_id_in) %>% filter(player_position <=10)
  
  fig = ggplot(data_play,aes(x = ball_position_x,y = ball_position_y))+
    geom_point(show.legend=FALSE)+
    geom_point(data = data_pos,aes(x = field_x, y = field_y,color = factor(player_position==10)),show.legend = FALSE)+
    annotate("point",x=0,y=60.5,color="blue",size=5)+
    annotate("point",x=0,y=0,color="red",size=5)+
    annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
    annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
    annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
    coord_fixed()
  
  return(fig)
  
}

# Find a hit and animate it:
fig = plot_play(720)+transition_states(timestamp)
animate(fig,nframes = 200,fps = 30)








# Find plays where play starts with no-one on base and next play has either someone on first base or no one on base
# will tell me whether the batter had a hit or an out


position_data_infield = position_data %>% filter(player_position==10) %>% left_join(infield_throw_balls_data) %>% filter(!is.na(throw_time))



position_data_infield = position_data_infield %>% group_by(game_str,play_id) %>% arrange(timestamp) %>% 
  mutate(Batter_Speed = 1e3 * sqrt((field_x-lag(field_x))**2+(field_y-lag(field_y))**2) / (timestamp - lag(timestamp)),
         Batter_Dist = sqrt((field_x)**2 + (field_y)**2)) %>% ungroup()

position_data_infield = filter(position_data_infield,Batter_Speed <=40)%>% group_by(game_str,play_id) %>% arrange(timestamp) %>% 
  mutate(Time_Zero = timestamp -first(timestamp)) %>% ungroup()


position_data_infield_timer = position_data_infield  %>% group_by(game_str,play_id) %>% 
  arrange(timestamp) %>% mutate(Batter_Time = first(timestamp[Batter_Dist>=90] - Time_0[Batter_Dist>=90])) %>% 
   filter(timestamp>=Time_0) %>% summarise(
    throw_time = first(throw_time),
    throw_dist = first(throw_dist),
    Speed_init = first(Speed_init),
    pos_throw = first(pos_throw),
    Batter_Speed = first(Batter_Speed),
    Batter_Dist = first(Batter_Dist),
    Batter_Time = first(Batter_Time),
    timestamp = first(timestamp),
    Time_0 = first(Time_0),
    Time_1 = first(Time_1)
  )





game_data_1B = position_data %>% group_by(game_str,play_id) %>% summarise(count_1B = sum(player_position==11,na.rm=TRUE),
                                                                          count_OB = sum(player_position%in%c(11,12,13,14),na.rm=TRUE))

game_data_1B = game_data_1B %>% mutate(runner_on_1B = as.numeric(count_1B>=1),
                                       runner_on_B = as.numeric(count_OB>=1))

game_data_1B = game_data_1B %>% ungroup() %>% group_by(game_str) %>% arrange(play_id) %>% 
  mutate(Hit = as.numeric(runner_on_B==0 & lead(runner_on_1B==1)),
         Out = as.numeric(runner_on_B==0 & lead(runner_on_B==0))) %>% ungroup()
game_data_1B = game_data_1B %>% filter(Hit==1 | Out==1)
mean(game_data_1B$Hit,na.rm = TRUE)

# 13% of these plays resulted in hits

# Join this hit data onto the infield hit data

infield_throw_balls_data = infield_throw_balls_data %>% left_join(game_data_1B)

mean(infield_throw_balls_data$Hit,na.rm = TRUE)

# Find a hit and see what it looks like:
infield_throw_balls_data[32,]$Hit

fig = plot_play(32)+transition_states(timestamp)
animate(fig,nframes = 400,fps = 30)

position_data_infield_timer = position_data_infield_timer %>% left_join(game_data_1B)

ggplot(data = filter(position_data_infield_timer,Hit==0),aes(x = throw_dist, y = Batter_Dist,fill = Batter_Time - throw_time))+
  geom_point(pch=21,size=5)+
  geom_point(data = filter(position_data_infield_timer,Hit==1),size=5,pch=22)+
  scale_fill_distiller(palette = "PuOr",lim = c(-1000,1000),oob = scales::squish)

# Find a model for whether a hit is made based on batter time & throw time?

model_hit = glm(formula = Hit~I(Batter_Time - throw_time),family="binomial",data = filter(position_data_infield_timer,Hit==1 | Out==1))
summary(model_hit)
model_hit$coefficients[2]*1000

hist(model_hit$fitted.values)

# This didn't work, try something else




# find histogram of batter home/first times



bip_data = game_data %>% group_by(game_str,play_id) %>% filter(event_code==4) %>% rename(hit_timestamp = timestamp) %>% select(
  game_str,play_id,hit_timestamp
)

throw_data = game_data %>% group_by(game_str,play_id) %>% filter(event_code==2&player_position==3) %>% rename(throw_timestamp = timestamp) %>% select(
  game_str,play_id,throw_timestamp
)

position_data_infield = position_data %>% filter(player_position==10) %>% left_join(throw_data) %>% filter(!is.na(throw_timestamp))



position_data_infield = position_data_infield %>% group_by(game_str,play_id) %>% arrange(timestamp) %>% 
  mutate(Batter_Speed = 1e3 * sqrt((field_x-lag(field_x))**2+(field_y-lag(field_y))**2) / (timestamp - lag(timestamp)),
         Batter_Dist = sqrt((field_x)**2 + (field_y)**2)) %>% ungroup()

position_data_infield = position_data_infield %>% left_join(bip_data)

position_data_infield = position_data_infield %>% group_by(game_str,play_id) %>% arrange(timestamp) %>% 
  mutate(Time_Zero = timestamp - hit_timestamp) %>% ungroup()


position_data_infield_timer = position_data_infield  %>% group_by(game_str,play_id) %>% 
  arrange(timestamp) %>% mutate(Batter_Time_HF = first(Time_Zero[Batter_Dist>=90])) %>% summarise(
    Batter_Time_HF = first(Batter_Time_HF),
    throw_time = first(throw_timestamp - hit_timestamp)
  )

position_data_infield_timer %>% ggplot(aes(x = Batter_Time_HF))+geom_histogram()+xlim(3500,6000)

data_to_save = position_data_infield_timer
write.csv(data_to_save,"data/infield_batter_HF_speed_data.csv")


# A roughly normal distribution peaking around 4500ms or so


# Do the same trick and look at whether a runner appears on 1B after the play with 
# a throw to first

game_data_1B = position_data %>% group_by(game_str,play_id) %>% summarise(count_1B = sum(player_position==11,na.rm=TRUE),
                                                                          count_OB = sum(player_position%in%c(11,12,13,14),na.rm=TRUE))

game_data_1B = game_data_1B %>% mutate(runner_on_1B = as.numeric(count_1B>=1),
                                       runner_on_B = as.numeric(count_OB>=1))

# game_data_1B = game_data_1B %>% ungroup() %>% group_by(game_str) %>% arrange(play_id) %>% 
#   mutate(Hit = as.numeric(runner_on_B==0 & lead(runner_on_1B==1)),
#          Out = as.numeric(runner_on_B==0 & lead(runner_on_B==0))) %>% ungroup()

game_data_1B = game_data_1B %>% ungroup() %>% group_by(game_str) %>% arrange(play_id) %>% 
  mutate(Hit = as.numeric(runner_on_1B==0 & lead(runner_on_1B==1)),
         Out = as.numeric(runner_on_1B==0 & lead(runner_on_1B==0))) %>% ungroup()


game_data_1B = game_data_1B %>% filter(Hit==1 | Out==1)

position_data_infield_timer = position_data_infield_timer %>% left_join(game_data_1B)


position_data_infield_timer %>% filter(!is.na(Hit)) %>% ggplot(aes(x = Batter_Time_HF,y = throw_time,fill = factor(Hit)))+
  geom_point(size=5,pch=21)+
  annotate("segment",x=3500,y=3500,xend=5000,yend=5000)+
  xlim(3500,5000)+
  ylim(3500,5000)

# This looks more like it can see where the hits arrive when using these timings

# A slight shift needed from just the +ve/-ve value, also need to find width of logistic
# step used to model the hit likelihood

position_data_infield_timer = position_data_infield_timer %>% mutate(diff = Batter_Time_HF - throw_time)

Hit_Data = position_data_infield_timer %>% filter(!is.na(Hit),Batter_Time_HF<=5000,throw_time<=5000,
                                                  Batter_Time_HF>=3500,throw_time>=3500)



hit_model = function(diff,step,width){
  return(1 / (1 + exp((diff - step) / width)))
}

Hit_Data$hit_pred = hit_model(Hit_Data$diff,step=100,width=25)
library(caret)
RMSE(Hit_Data$Hit,Hit_Data$hit_pred)

ggplot(Hit_Data,aes(x = diff, y = Hit))+geom_point()+geom_line(aes(y = hit_pred))

results_out = data.frame()
for(i in seq(-500,500,10)){
  for(j in seq(0,1000,10)){
    Hit_Data$hit_pred = hit_model(Hit_Data$diff,step=i,width=j)
    rmse_step = RMSE(Hit_Data$Hit,Hit_Data$hit_pred)
    results_out = bind_rows(results_out,data.frame(step=i,width=j,rmse = rmse_step))
    
  }
}
results_out$step[results_out$rmse==min(results_out$rmse,na.rm = TRUE)]
results_out$width[results_out$rmse==min(results_out$rmse,na.rm = TRUE)]

results_out %>% filter(rmse==min(rmse,na.rm = TRUE))

ggplot(results_out,aes(x = step, y = width,color = rmse))+geom_point()



Hit_Data$hit_pred = hit_model(Hit_Data$diff,step=80,width=60)
RMSE(Hit_Data$Hit,Hit_Data$hit_pred)

ggplot(Hit_Data,aes(x = diff, y = Hit))+geom_line(aes(y = hit_pred),size=2)+
  geom_point(aes(color=factor(Hit)),size=4,show.legend = FALSE,alpha = 0.5)+
  xlab("Batter Time - Throw Time /ms")+
  ylab("Hit Likelihood")+
  scale_y_continuous(labels= scales::percent_format())+
  theme_minimal()+
  theme(text = element_text(size = 20))

ggsave("plots/hit_timing.jpg",width=8,height=4,bg="white")

# This informs us of how to quantify hit likelihood based on play timings
# Use this logistic curve inside the overall algorithm


data_to_save = Hit_Data
write.csv(data_to_save,"data/infield_batter_HF_Hit_data.csv")
