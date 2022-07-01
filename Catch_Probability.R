# Look at catch probability in the outfield based on hang-time and distance to travel

library(tidyverse)

position_data = read.csv("SMT-Data-Challenge/player_pos.csv")
ball_data = read.csv("SMT-Data-Challenge/ball_pos.csv")

game_data = read.csv("SMT-Data-Challenge/game_events.csv")

# Get all plays where the ball is collected by an outfielder
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

# Average number of balls reaching the outfield which get caught?
mean(outfield_data$catch)

# Now get ball and player position data on each play

outfield_ball_data = ball_data %>% left_join(outfield_data) %>% filter(!is.na(catch)) %>% group_by(game_str,play_id) %>% 
  arrange(timestamp) %>% summarise(ball_landing_x = first(ball_position_x[timestamp>=timestamp_hit_ground]),
                                   ball_landing_y = first(ball_position_y[timestamp>=timestamp_hit_ground]),
                                   ball_landing_z = first(ball_position_z[timestamp>=timestamp_hit_ground]))
print(head(outfield_ball_data))


# Join the ball timestamp data
outfield_ball_data = left_join(outfield_ball_data,outfield_data)

# filter to the outfield only
outfield_ball_data = outfield_ball_data %>% filter(ball_landing_y>=100,ball_landing_y<=420)

# Plot the positions and catch rates

ggplot(outfield_ball_data,aes(x = ball_landing_x, y = ball_landing_y,color = factor(catch)))+
  geom_point()+
  annotate("point",x=0,y=60.5,color="blue",size=5)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  coord_fixed()

# Looks like it makes sense so far, a hotspot around the starting points for the outfielders

#  Filter to only take the position of the player who retrieves the ball:

outfield_position_data = position_data %>% left_join(select(outfield_ball_data,game_str,play_id,pos_catch)) %>% filter(player_position == pos_catch)


# Find player starting position
outfield_position_data = outfield_position_data %>% group_by(game_str,play_id) %>% arrange(timestamp) %>% summarise(
  player_start_x = first(field_x),
  player_start_y = first(field_y)
)

total_outfield_data = outfield_ball_data %>% left_join(outfield_position_data)


# Find the distance from the starting point to the ball landing position
total_outfield_data = total_outfield_data %>% mutate(distance_from_start = sqrt((ball_landing_x - player_start_x)**2 + (ball_landing_y - player_start_y)**2))

# Plot hang time vs distance to travel
# A pretty clear pattern emerges
ggplot(total_outfield_data,aes(x = distance_from_start,y = hang_time,color = factor(catch)))+
  geom_point()+
  xlim(0,140)+
  ylim(3000,7500)


# filter the NA values before training a model
total_outfield_data = filter(total_outfield_data,!is.na(distance_from_start),!is.na(hang_time))

# make a train-test split

set.seed(0)
samples = sample(seq(dim(total_outfield_data)[1]), size = ,as.integer(0.8 * dim(total_outfield_data)[1]))



train_data = total_outfield_data[samples,]
test_data = total_outfield_data[-samples,]

# GLM logistic regression seems ideal for this problem

catch_prob = glm(catch ~ distance_from_start+hang_time,family="binomial",data = train_data)
summary(catch_prob)

# Look at the difference between the train and test sets to make sure there's no overfitting
# This shouldn't be an issue with a simple logistic regression anyway but good to check

train_data$catch_pred = predict.glm(catch_prob,type="response",newdata = train_data)

test_data$catch_pred = predict.glm(catch_prob,type="response",newdata = test_data)

library(caret)

RMSE(train_data$catch,train_data$catch_pred)

RMSE(test_data$catch,test_data$catch_pred)

# RMSE is lower on the test set (lol, must be the small-ish sample size)
# no evidence of overfitting anyway so we're clear

total_outfield_data$catch_prob = predict.glm(catch_prob,type="response",newdata = total_outfield_data)

# Make some plots of catch_pred and catch rate

ggplot(total_outfield_data,aes(x = distance_from_start,y = hang_time,color = catch_prob))+
  geom_point()+
  xlim(0,140)+
  ylim(3000,7500)


total_outfield_data %>% group_by(round_prob = 0.1*round(catch_prob*10,0)) %>% summarise(
  catch_pred = mean(catch_prob),
  catch = mean(catch),
  count = n()
) %>% ggplot(aes(x = catch_pred, y = catch,size = count))+geom_point()+
  annotate("segment",x=0,y=0,xend=1,yend=1)

total_outfield_data %>%  mutate(round_prob = factor(0.1*round(catch_prob*10,0),levels = seq(0,1,0.1))) %>% 
  ggplot(aes(x = round_prob, y = catch,order = round_prob))+geom_violin()+
  annotate("segment",x=0,y=0,xend=1,yend=1)

# A clear small residual when using the mean here, but this is because the model targets minimising
# the squared error so I don't consider this a large problem

# Plot of catch probability with some lines of constant probability


summary(catch_prob)
int_factor = -catch_prob$coefficients[1]
dist_factor = -catch_prob$coefficients[2]
time_factor = catch_prob$coefficients[3]

# get logistic factors for 1%, 10%, 25%, 50%, 75%, 90%, 99%

step1 = log((1/0.01)-1)
step2 =log((1/0.1)-1)
step3 =log((1/0.25)-1)
step4 =log((1/0.5)-1)
step5 =log((1/0.75)-1)
step6 =log((1/0.9)-1)
step7 =log((1/0.99)-1)

windowsFonts("bahn" = windowsFont("bahnschrift"))
ggplot(total_outfield_data,aes(x = distance_from_start,y = hang_time,fill = catch_prob,shape=factor(catch,levels=c(0,1),labels=c("No Catch","Catch"))))+
  geom_point(size = 4)+
  scale_shape_manual(values = c(21,22))+
  coord_cartesian(xlim=c(0,144),ylim=c(2000,8000))+
  annotate("segment",x=0,y=(int_factor+step1)/ time_factor,xend=137,yend=((int_factor+step1)+(dist_factor*137))/ time_factor,size=2,linetype="dashed")+
  annotate("segment",x=0,y=(int_factor+step2)/ time_factor,xend=137,yend=((int_factor+step2)+(dist_factor*137))/ time_factor,size=2,linetype="dashed")+
  annotate("segment",x=0,y=(int_factor+step3)/ time_factor,xend=137,yend=((int_factor+step3)+(dist_factor*137))/ time_factor,size=2,linetype="dashed")+
  annotate("segment",x=0,y=(int_factor+step4)/ time_factor,xend=137,yend=((int_factor+step4)+(dist_factor*137))/ time_factor,size=2,linetype="dashed")+
  annotate("segment",x=0,y=(int_factor+step5)/ time_factor,xend=137,yend=((int_factor+step5)+(dist_factor*137))/ time_factor,size=2,linetype="dashed")+
  annotate("segment",x=0,y=(int_factor+step6)/ time_factor,xend=137,yend=((int_factor+step6)+(dist_factor*137))/ time_factor,size=2,linetype="dashed")+
  annotate("segment",x=0,y=(int_factor+step7)/ time_factor,xend=137,yend=((int_factor+step7)+(dist_factor*137))/ time_factor,size=2,linetype="dashed")+
  theme_minimal()+
  scale_fill_distiller(palette = "Greens",lim=c(0,1),labels=scales::percent_format(),direction = 1)+
  theme(text = element_text(size=20,family="bahn"))+
  xlab("Distance From Starting Position /ft")+
  ylab("Hang Time /ms")+
  labs(shape = "",fill = "Catch Probability")+
  annotate("text",family="bahn",label="1%",x=142,y = 5000,size=8)+
  annotate("text",family="bahn",label="10%",x=142,y = 5900,size=8)+
  annotate("text",family="bahn",label="25%",x=142,y = 6300,size=8)+
  annotate("text",family="bahn",label="50%",x=142,y = 6700,size=8)+
  annotate("text",family="bahn",label="75%",x=142,y = 7000,size=8)+
  annotate("text",family="bahn",label="90%",x=142,y = 7300,size=8)+
  annotate("text",family="bahn",label="99%",x=142,y = 7900,size=8)
# Save this as plot_catch_prob

ggsave("plots/plot_catch_prob.jpg",bg="white",width=13,height=6,dpi=200,units = "in")


# Look at residuals by direction of travel

total_outfield_data %>% mutate(direction = "none",
                               direction = if_else(distance_from_start>=50&((ball_landing_x - player_start_x)>=abs((ball_landing_y - player_start_y))),"right",direction),
                               direction = if_else(distance_from_start>=50&((-ball_landing_x + player_start_x)>=abs((ball_landing_y - player_start_y))),"left",direction),
                               direction = if_else(distance_from_start>=50&((ball_landing_y - player_start_y)>abs((ball_landing_x - player_start_x))),"back",direction),
                               direction = if_else(distance_from_start>=50&((-ball_landing_y + player_start_y)>abs((ball_landing_x - player_start_x))),"forward",direction)) %>% 
  group_by(direction) %>% summarise(catch = mean(catch,na.rm=TRUE),
                                    catch_pred = mean(catch_prob,na.rm=TRUE),
                                    residual = 100*(catch - catch_pred))


# Some very small residuals, probably not worth correcting for
# Left here is left as you view the field from above (so actually to the players right)
# More right handed people = glove in left hand = harder to catch a ball moving to your right?


# finally save the model:

saveRDS(catch_prob,"catch_probability_glm.rds")

# Save data used in tge model:
data_to_save = total_outfield_data
write.csv(data_to_save,"data/catch_probability_data.csv")

# I will assume that infield catch probability is similar to outfield catch probability, 
# most flyballs caught on the infield are popups which will have close to 100%
# catch probability anyway

