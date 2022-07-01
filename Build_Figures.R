# A script which can rebuild all the necessary data/figures for all the tests
# when underlying models are changed/updated
getwd()
# Saves everything in the plots directory

# Means I don't have to fiddle with much and can just recreate this script when
# necessary
library(tidyverse)
library(sigmoid)
library(mgcv)
library(ggrepel)

# Same as fielding model but with GLMs/LMs instead of GAMs on the infield

# Join the components together to get a model for effectiveness of a given defensive alignment

# Get the average batted ball distribution

binned_results_dframe = readRDS(file = "models/binned_batted_ball_sample_with_hang.rds")

# catch probability model:

catch_prob = readRDS("models/catch_probability_glm.rds")

# infield range model:

infield_range = readRDS("models/models/range_model_GLM.rds")

# infield time model

infield_time_model = readRDS("time_model_LM.rds")

# infield depth model

infield_depth_model = readRDS("models/depth_model_LM.rds")


spray_chart_tabulate = readRDS("models/tabulated_spray_chart.rds")


binned_results_dframe_filtered = binned_results_dframe %>% filter(frac>0)

batter_spray = binned_results_dframe_filtered

# Use league average numbers vs RHB with no shift
player_position_data = data.frame(
  match = c(1,1,1,1,1,1,1),
  player_position = c(3,4,5,6,7,8,9),
  depth = c(110,151,118,145,298,322,294),
  angle = c(34,11,-29,-11,-27,0,27),
  catch_factor = c(-3,-3,-3,-3,0,0,0)
)


player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                       field_y = depth * cos(angle * pi / 180))




infield_ability = data.frame(
  player_position = c(3,4,5,6),
  range_factor = c(0,0,0,0), # extra ability of reaching a groundball
  transfer_time = c(1.1,0.9,1.1,0.9),
  throw_speed = c(80,90,110,105)
)



find_batter_spray = function(batter_spray,avg_speed,average_launch,gb_pull,fb_pull,handedness,tabulated_spray){
  
  results = tabulated_spray
  
  results = results %>% mutate(diff_to_input = sqrt((0.01*average_launch - 0.01*av_launch_angle)**2 +
                                                      (gb_pull - av_gb_pull)**2 +
                                                      (fb_pull - av_fb_pull)**2
  )
  )
  
  
  best_results = results %>% filter(diff_to_input==min(diff_to_input))
  
  i = best_results$step_launch
  j = best_results$step_gb_pull
  k = best_results$step_fb_pull
  
  
  if (i >= 0){
    batter_spray_adjust= batter_spray %>% group_by(launch_speed,pull) %>% arrange(launch_angle) %>% mutate(
      frac = lag(frac,n=i,default = 0)
    ) %>% ungroup()
  }else{
    batter_spray_adjust = batter_spray %>% group_by(launch_speed,pull) %>% arrange(launch_angle) %>% mutate(
      frac = lead(frac,n=as.integer(-i),default = 0)
    ) %>% ungroup()
  }
  
  if(j >= 0){
    batter_spray_adjust = batter_spray_adjust %>% group_by(launch_speed,launch_angle) %>% arrange(pull) %>% mutate(
      frac = if_else(launch_angle <=10,lag(frac,n=j,default = 0),frac)
    ) %>% ungroup()
  }else{
    batter_spray_adjust = batter_spray_adjust %>% group_by(launch_speed,launch_angle) %>% arrange(pull) %>% mutate(
      frac = if_else(launch_angle <=10,lead(frac,n=as.integer(-j),default = 0),frac)
    ) %>% ungroup()
  }
  
  if (k >= 0){
    batter_spray_adjust = batter_spray_adjust %>% group_by(launch_speed,launch_angle) %>% arrange(pull) %>% mutate(
      frac = if_else(launch_angle >10,lag(frac,n=k,default = 0),frac)
    ) %>% ungroup()
  }else{
    batter_spray_adjust = batter_spray_adjust %>% group_by(launch_speed,launch_angle) %>% arrange(pull) %>% mutate(
      frac = if_else(launch_angle >10,lead(frac,n=as.integer(-k),default = 0),frac)
    ) %>% ungroup()
  }
  
  
  batter_spray_adjust = batter_spray_adjust %>% filter(pull<=45,pull>=-45)
  # Finally adjust speed / distance using this profile
  
  speed_diff = avg_speed - weighted.mean(batter_spray_adjust$launch_speed,batter_spray_adjust$frac)
  
  batter_spray_speed = batter_spray_adjust %>% mutate(launch_speed = launch_speed + speed_diff)
  
  batter_spray_speed = batter_spray_speed %>% mutate(hit_distance = hit_distance + 12 * speed_diff / 2.5)
  
  batter_spray_speed = batter_spray_speed %>% mutate(pos_x = hit_distance * sin(pull * pi / 180),
                                                     pos_y = hit_distance*cos(pull * pi / 180))
  if(handedness=="L"){
    batter_spray_speed = batter_spray_speed %>% mutate(pull = -pull) %>% 
      mutate(pos_x = hit_distance * sin(pull * pi / 180),
             pos_y = hit_distance*cos(pull * pi / 180))
  }
  
  return(batter_spray_speed)
  
  
}


fielding_model_function_variable = function(hitter_spray,batter_time_in,player_position,infield_ability_in = infield_ability,max_prob = FALSE){
  time1 = timeDate::timeDate()
  binned_results_dframe_filtered = hitter_spray
  
  total_frac = sum(binned_results_dframe_filtered$frac)
  
  binned_results_dframe_joined = binned_results_dframe_filtered %>% mutate(match = 1) %>% filter(launch_angle >=10) %>% left_join(player_position,by = "match")
  
  outfield_frac = sum(filter(binned_results_dframe_filtered,launch_angle >=10)$frac) / total_frac
  
  binned_results_dframe_joined = binned_results_dframe_joined %>% mutate(distance_from_start = sqrt((pos_x - field_x)**2 + (pos_y - field_y)**2),
                                                                         hang_time = hang_time *1000)
  
  binned_results_dframe_joined$catch_prob = predict.glm(catch_prob,binned_results_dframe_joined,type = "response")
  
  binned_results_dframe_joined = binned_results_dframe_joined %>% mutate(catch_prob = if_else(is.na(catch_prob),0,catch_prob))
  
  
  binned_results_dframe_joined =  binned_results_dframe_joined %>% mutate(catch_prob_delogistic = log(1e-10 + catch_prob/(1 - catch_prob))) %>% mutate(
    catch_prob_delogistic = catch_prob_delogistic + catch_factor,
    catch_prob= 1/(1 + exp(-(catch_prob_delogistic)))
  )
  
  
  time2 = timeDate::timeDate()
  # Now average over positions
  if(max_prob){
    binned_results_dframe_joined_summary = binned_results_dframe_joined %>% group_by(pull,launch_angle,launch_speed,frac,hit_distance,pos_x,pos_y) %>% 
      mutate(no_catch_prob = 1 - catch_prob) %>% 
      summarise(
        catch_prob_sum = sum(catch_prob,na.rm=TRUE),
        #catch_prob = 1 - Reduce("*",no_catch_prob),
        catch_prob = max(catch_prob,na.rm=TRUE),
        .groups="drop"
      ) %>% 
      mutate(catch_prob = if_else(catch_prob>=1,1,catch_prob))
  }
  if(!max_prob){
    binned_results_dframe_joined_summary = binned_results_dframe_joined %>% group_by(pull,launch_angle,launch_speed,frac,hit_distance,pos_x,pos_y) %>% 
      mutate(no_catch_prob = 1 - catch_prob) %>% 
      summarise(
        catch_prob_sum = sum(catch_prob,na.rm=TRUE),
        catch_prob = 1 - Reduce("*",no_catch_prob),
        #catch_prob = max(catch_prob,na.rm=TRUE),
        .groups="drop"
      ) %>% 
      mutate(catch_prob = if_else(catch_prob>=1,1,catch_prob))
  }
  
  
  time3 = timeDate::timeDate()
  
  # For the groundballs need to work out the probability of a successful infield play
  # Work this out by finding the time to complete a play with a given fielding arrangement
  
  infield_position_data = filter(player_position,player_position %in% c(3,4,5,6))
  
  infield_plays = binned_results_dframe_filtered %>% filter(launch_angle <=10)
  
  infield_frac = sum(infield_plays$frac) / total_frac
  
  infield_plays = infield_plays %>% mutate(match=1) %>% left_join(infield_position_data,by = "match") %>% mutate(
    speed = launch_speed / 0.68,
    init_depth = sqrt(field_x**2 + field_y**2),
    init_angle = 180*atan(field_x / field_y)/pi,
    fielding_angle_diff = abs(init_angle - pull) # Can reduce angle to simulate better range
  )
  
  time4 = timeDate::timeDate()
  
  infield_plays$fielded = predict.glm(infield_range,infield_plays,type = "response")
  
  infield_plays = infield_plays %>% mutate(reaction_factor = if_else(init_depth<=150,0.01*(init_depth - 50),1),
                                           reaction_factor = if_else(reaction_factor <=0,0,reaction_factor),
                                           fielded = fielded * reaction_factor)
  
  infield_plays$field_depth = predict(infield_depth_model,infield_plays)
  
  infield_plays$field_time = predict(infield_time_model,mutate(infield_plays,launch_angle = if_else(launch_angle <=-10,-10,launch_angle)))*0.8
  
  time5 = timeDate::timeDate()
  
  
  infield_plays = infield_plays %>% left_join(infield_ability_in, by = "player_position")
  
  
  # Adjust fielding likelihood by range factor
  
  infield_plays = infield_plays %>% mutate(fielded_delogistic = log(1e-10 + fielded/(1 - fielded))) %>% mutate(
    fielded_delogistic = fielded_delogistic + range_factor,
    fielded = 1/(1 + exp(-(fielded_delogistic)))
  )
  
  throw_time = function(dist,speed){
    return(-113.320 + 1.167 * 1000 * dist / speed)
  }
  
  infield_plays = infield_plays %>% mutate(throw_dist = sqrt((field_depth * sin(pull*pi / 180) -(90/sqrt(2)))**2 + (field_depth * cos(pull*pi / 180) -(90/sqrt(2)))**2)) %>% 
    mutate(throw_time = throw_time(throw_dist,throw_speed),
           throw_time = if_else(throw_time<=0,0,throw_time))
  
  infield_plays$batter_time = batter_time_in
  
  # Filter out "impossible plays on the infield
  # where the combination of fielding time and location implies a sprint speed of > 25ft/s through the play
  # This may be achievable at peak speed, but including acceleration time this is 
  # very unrealistic to be achieved in reality
  
  # Actually have to use the cosine rule here, lol I guess all those maths lessons paid off
  
  
  infield_plays = infield_plays %>% mutate(dist_to_receive = sqrt(init_depth**2 + field_depth**2 - 2 * init_depth * field_depth *cos(fielding_angle_diff * pi / 180)),
                                           implied_speed = dist_to_receive / (field_time/1000))
  
  
  
  infield_plays = infield_plays %>% mutate(play_time = field_time + (1000*transfer_time) + throw_time) %>% mutate(
    # out_if_field = sigmoid(5*(batter_time - play_time)/1000),#1 / (1 + exp(((batter_time - play_time) - 80) / 60)), 
    out_if_field = 1 / (1 + exp(((-batter_time + play_time) + 80) / 60)), 
    out_if_field = if_else(implied_speed >=25,0,out_if_field),
    fielded = if_else(hit_distance >= depth+10,0,fielded),# don't catch anything landing behind the player's starting position (will be a sharply hit line drive, unrealistic to field)
    success_field = fielded * out_if_field,
    fail_field = 1 - success_field,
    fail_reach_ball = 1 - fielded
  )
  
  
  
  if(max_prob){
    infield_plays_summary = infield_plays %>% group_by(pull,launch_angle,launch_speed,frac,hit_distance,pos_x,pos_y) %>% summarise(
      out_prob_sum = sum(success_field,na.rm=TRUE),
      #out_prob = 1- Reduce("*",fail_field),
      out_prob = max(success_field,na.rm=TRUE),
      infield_prob = 1 - Reduce("*",fail_reach_ball),
      .groups="drop"
    ) %>% 
      mutate(out_prob = if_else(out_prob>=1,1,out_prob))
  }
  if(!max_prob){
    infield_plays_summary = infield_plays %>% group_by(pull,launch_angle,launch_speed,frac,hit_distance,pos_x,pos_y) %>% summarise(
      out_prob_sum = sum(success_field,na.rm=TRUE),
      out_prob = 1- Reduce("*",fail_field),
      #out_prob = max(success_field,na.rm=TRUE),
      infield_prob = 1 - Reduce("*",fail_reach_ball),
      .groups="drop"
    ) %>% 
      mutate(out_prob = if_else(out_prob>=1,1,out_prob))
  }
  
  
  infield_out_frac = (infield_plays_summary %>% ungroup() %>% summarise(out_prob = weighted.mean(out_prob,frac)))
  catch_out_frac = binned_results_dframe_joined_summary %>% ungroup() %>% summarise(catch_prob = weighted.mean(catch_prob,frac))
  
  
  total_babip = 1 - (infield_plays_summary %>% ungroup() %>% summarise(out_prob = weighted.mean(out_prob,frac)) * infield_frac) - 
    ((binned_results_dframe_joined_summary %>% ungroup() %>% summarise(catch_prob = weighted.mean(catch_prob,frac))) * outfield_frac)
  # print(total_babip)
  
  time6 = timeDate::timeDate()
  
  
  return(c(as.numeric(infield_out_frac),as.numeric(catch_out_frac),as.numeric(total_babip)))
  
}


iterate_step = function(player_position,batter_spray,batter_speed){
  
  movements = c("u","l","r","d") #u,l,d,r directions, n = unchanged
  
  temp_results = data.frame()
  
  temp_results = bind_rows(temp_results,data.frame(
    player_position = c(3,4,5,6,7,8,9),
    movement_dir = "n",
    babip = fielding_model_function_variable(batter_spray,batter_speed,player_position,infield_ability,max_prob = TRUE)[3]
  ))
  
  for(p in c(3,4,5,6,7,8,9)){ # Don't move 1B for now, realistic limit on how far they can move given they need to be near the bag
    #print(p)
    for(m in movements){
      player_position_data_temp = player_position %>% mutate(
        field_x = if_else((m == "l")&(player_position==p), field_x - 5, field_x),
        field_x = if_else((m == "r")&(player_position==p), field_x + 5, field_x),
        field_y = if_else((m == "d")&(player_position==p), field_y - 5, field_y),
        field_y = if_else((m == "u")&(player_position==p), field_y + 5, field_y),
      ) %>% 
        mutate(depth = sqrt((field_x)**2 + (field_y)**2))
      
      babip_in = fielding_model_function_variable(batter_spray,batter_speed,player_position_data_temp,infield_ability,max_prob = TRUE)[3]
      
      
      dist_1b_bag = player_position_data_temp %>% filter(player_position==3) %>% 
        mutate(dist = sqrt((field_x - 45*sqrt(2))**2+(field_y - 45*sqrt(2))**2))
      
      
      
      if(dist_1b_bag$dist >=40){
        babip_in = 1 # don't let the 1B too far out
      }
      
      temp_results = bind_rows(temp_results,data.frame(
        player_position = p,
        movement_dir = m,
        babip = babip_in
      ))
      
      
      
    }
  }
  
  
  movement_direction = temp_results %>% group_by(player_position) %>% summarise(movement_dir = first(movement_dir[babip == min(babip)]))
  
  
  
  
  
  
  new_player_position = player_position %>% left_join(movement_direction) %>% mutate(
    field_x = if_else((movement_dir == "l"), field_x - 5, field_x),
    field_x = if_else((movement_dir == "r"), field_x + 5, field_x),
    field_y = if_else((movement_dir == "d"), field_y - 5, field_y),
    field_y = if_else((movement_dir == "u"), field_y + 5, field_y),
  ) %>% 
    mutate(depth = sqrt((field_x)**2 + (field_y)**2)) %>% select(-movement_dir)
  
  return(new_player_position)
}




multiple_iterations = function(player_position,batter_spray,batter_speed,max_iter){
  iter_counter = 0
  player_position_counter = data.frame()
  player_position_new = player_position
  while(iter_counter < max_iter){
    print(iter_counter)
    player_position_new_temp = iterate_step(player_position_new,batter_spray,batter_speed)
    if(all((arrange(player_position_new,player_position)$field_x==arrange(player_position_new_temp,player_position)$field_x)&
           (arrange(player_position_new,player_position)$field_y==arrange(player_position_new_temp,player_position)$field_y))){
      return(player_position_counter)
    }
    else{
      player_position_counter = bind_rows(player_position_counter,mutate(player_position_new_temp,iter_counter = iter_counter))
    }
    iter_counter = iter_counter + 1
    player_position_new = player_position_new_temp
    
    # if(sum(duplicated(select(player_position_counter,-iter_counter)))>=1){
    #   return(player_position_counter)
    # }
  }
  return(player_position_counter)
}



# Define a new function that returns more detailed outputs for a given alignement:





fielding_model_all_info_function = function(hitter_spray,batter_time_in,player_position,infield_ability_in = infield_ability,max_prob = FALSE){
  time1 = timeDate::timeDate()
  binned_results_dframe_filtered = hitter_spray
  
  total_frac = sum(binned_results_dframe_filtered$frac)
  
  binned_results_dframe_joined = binned_results_dframe_filtered %>% mutate(match = 1) %>% filter(launch_angle >=10) %>% left_join(player_position,by = "match")
  
  outfield_frac = sum(filter(binned_results_dframe_filtered,launch_angle >=10)$frac) / total_frac
  
  binned_results_dframe_joined = binned_results_dframe_joined %>% mutate(distance_from_start = sqrt((pos_x - field_x)**2 + (pos_y - field_y)**2),
                                                                         hang_time = hang_time *1000)
  
  binned_results_dframe_joined$catch_prob = predict.glm(catch_prob,binned_results_dframe_joined,type = "response")
  
  binned_results_dframe_joined = binned_results_dframe_joined %>% mutate(catch_prob = if_else(is.na(catch_prob),0,catch_prob))
  
  
  binned_results_dframe_joined =  binned_results_dframe_joined %>% mutate(catch_prob_delogistic = log(1e-10 + catch_prob/(1 - catch_prob))) %>% mutate(
    catch_prob_delogistic = catch_prob_delogistic + catch_factor,
    catch_prob= 1/(1 + exp(-(catch_prob_delogistic)))
  )
  
  
  time2 = timeDate::timeDate()
  # Now average over positions
  if(max_prob){
    binned_results_dframe_joined_summary = binned_results_dframe_joined %>% group_by(pull,launch_angle,launch_speed,frac,hit_distance,pos_x,pos_y) %>% 
      mutate(no_catch_prob = 1 - catch_prob) %>% 
      summarise(
        catch_prob_sum = sum(catch_prob,na.rm=TRUE),
        #catch_prob = 1 - Reduce("*",no_catch_prob),
        catch_prob = max(catch_prob,na.rm=TRUE),
        .groups="drop"
      ) %>% 
      mutate(catch_prob = if_else(catch_prob>=1,1,catch_prob))
  }
  if(!max_prob){
    binned_results_dframe_joined_summary = binned_results_dframe_joined %>% group_by(pull,launch_angle,launch_speed,frac,hit_distance,pos_x,pos_y) %>% 
      mutate(no_catch_prob = 1 - catch_prob) %>% 
      summarise(
        catch_prob_sum = sum(catch_prob,na.rm=TRUE),
        catch_prob = 1 - Reduce("*",no_catch_prob),
        #catch_prob = max(catch_prob,na.rm=TRUE),
        .groups="drop"
      ) %>% 
      mutate(catch_prob = if_else(catch_prob>=1,1,catch_prob))
  }
  
  
  time3 = timeDate::timeDate()
  
  # For the groundballs need to work out the probability of a successful infield play
  # Work this out by finding the time to complete a play with a given fielding arrangement
  
  infield_position_data = filter(player_position,player_position %in% c(3,4,5,6))
  
  infield_plays = binned_results_dframe_filtered %>% filter(launch_angle <=10)
  
  infield_frac = sum(infield_plays$frac) / total_frac
  
  infield_plays = infield_plays %>% mutate(match=1) %>% left_join(infield_position_data,by = "match") %>% mutate(
    speed = launch_speed / 0.68,
    init_depth = sqrt(field_x**2 + field_y**2),
    init_angle = 180*atan(field_x / field_y)/pi,
    fielding_angle_diff = abs(init_angle - pull) # Can reduce angle to simulate better range
  )
  
  time4 = timeDate::timeDate()
  
  infield_plays$fielded = predict.glm(infield_range,infield_plays,type = "response")
  
  infield_plays = infield_plays %>% mutate(reaction_factor = if_else(init_depth<=150,0.01*(init_depth - 50),1),
                                           reaction_factor = if_else(reaction_factor <=0,0,reaction_factor),
                                           fielded = fielded * reaction_factor)
  
  infield_plays$field_depth = predict(infield_depth_model,infield_plays)
  
  infield_plays$field_time = predict(infield_time_model,mutate(infield_plays,launch_angle = if_else(launch_angle <=-10,-10,launch_angle)))*0.8
  
  time5 = timeDate::timeDate()
  
  
  infield_plays = infield_plays %>% left_join(infield_ability_in, by = "player_position")
  
  
  # print(infield_ability_in)
  
  # Adjust fielding likelihood by range factor
  
  infield_plays = infield_plays %>% mutate(fielded_delogistic = log(1e-10 + fielded/(1 - fielded))) %>% mutate(
    fielded_delogistic = fielded_delogistic + range_factor,
    fielded = 1/(1 + exp(-(fielded_delogistic)))
  )
  
  throw_time = function(dist,speed){
    return(-113.320 + 1.167 * 1000 * dist / speed)
  }
  
  infield_plays = infield_plays %>% mutate(throw_dist = sqrt((field_depth * sin(pull*pi / 180) -(90/sqrt(2)))**2 + (field_depth * cos(pull*pi / 180) -(90/sqrt(2)))**2)) %>% 
    mutate(throw_time = throw_time(throw_dist,throw_speed),
           throw_time = if_else(throw_time<=0,0,throw_time))
  
  infield_plays$batter_time = batter_time_in
  
  # Filter out "impossible plays on the infield
  # where the combination of fielding time and location implies a sprint speed of > 25ft/s through the play
  # This may be achievable at peak speed, but including acceleration time this is 
  # very unrealistic to be achieved in reality
  
  # Actually have to use the cosine rule here, lol I guess all those maths lessons paid off
  
  
  infield_plays = infield_plays %>% mutate(dist_to_receive = sqrt(init_depth**2 + field_depth**2 - 2 * init_depth * field_depth *cos(fielding_angle_diff * pi / 180)),
                                           implied_speed = dist_to_receive / (field_time/1000))
  
  
  
  infield_plays = infield_plays %>% mutate(play_time = field_time + (1000*transfer_time) + throw_time) %>% mutate(
    # out_if_field = sigmoid(5*(batter_time - play_time)/1000),#1 / (1 + exp(((batter_time - play_time) - 80) / 60)), 
    out_if_field = 1 / (1 + exp(((-batter_time + play_time) + 80) / 60)), 
    out_if_field = if_else(implied_speed >=25,0,out_if_field),
    fielded = if_else(hit_distance >= depth+10,0,fielded),# don't catch anything landing behind the player's starting position (will be a sharply hit line drive, unrealistic to field)
    success_field = fielded * out_if_field,
    fail_field = 1 - success_field,
    fail_reach_ball = 1 - fielded
  )
  
  
  
  if(max_prob){
    infield_plays_summary = infield_plays %>% group_by(pull,launch_angle,launch_speed,frac,hit_distance,pos_x,pos_y) %>% summarise(
      out_prob_sum = sum(success_field,na.rm=TRUE),
      #out_prob = 1- Reduce("*",fail_field),
      out_prob = max(success_field,na.rm=TRUE),
      infield_prob = 1 - Reduce("*",fail_reach_ball),
      .groups="drop"
    ) %>% 
      mutate(out_prob = if_else(out_prob>=1,1,out_prob))
  }
  if(!max_prob){
    infield_plays_summary = infield_plays %>% group_by(pull,launch_angle,launch_speed,frac,hit_distance,pos_x,pos_y) %>% summarise(
      out_prob_sum = sum(success_field,na.rm=TRUE),
      out_prob = 1- Reduce("*",fail_field),
      #out_prob = max(success_field,na.rm=TRUE),
      infield_prob = 1 - Reduce("*",fail_reach_ball),
      .groups="drop"
    ) %>% 
      mutate(out_prob = if_else(out_prob>=1,1,out_prob))
  }
  
  
  infield_out_frac = (infield_plays_summary %>% ungroup() %>% summarise(out_prob = weighted.mean(out_prob,frac)))* infield_frac
  catch_out_frac = binned_results_dframe_joined_summary %>% ungroup() %>% summarise(catch_prob = weighted.mean(catch_prob,frac))* outfield_frac
  
  
  total_babip = 1 - (infield_plays_summary %>% ungroup() %>% summarise(out_prob = weighted.mean(out_prob,frac)) * infield_frac) - 
    ((binned_results_dframe_joined_summary %>% ungroup() %>% summarise(catch_prob = weighted.mean(catch_prob,frac))) * outfield_frac)
  # print(total_babip)
  
 
  
  return(list("BABIP" = total_babip,
              "GO%" = infield_out_frac,
              "FO%" = catch_out_frac,
              "infield_data" = infield_plays,
              "catch_data" = binned_results_dframe_joined))
  
}








# What about just a normal batter with normal speed, only change handedness



batter_R = find_batter_spray(batter_spray,avg_speed = 87,average_launch = 12,gb_pull = 0.7,fb_pull = 0.45,handedness = "R",tabulated_spray = spray_chart_tabulate)




batter_L = find_batter_spray(batter_spray,avg_speed = 87,average_launch = 12,gb_pull = 0.7,fb_pull = 0.45,handedness = "L",tabulated_spray = spray_chart_tabulate)


player_position_data = data.frame(
  match = c(1,1,1,1,1,1,1),
  player_position = c(3,4,5,6,7,8,9),
  depth = c(110,151,118,145,298,322,294),
  angle = c(30,-1,-34,-15,-23,4,31),
  catch_factor = c(-1,-1,-1,-1,0,0,0)
)

batter_speed = 4500


player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                       field_y = depth * cos(angle * pi / 180))


infield_ability = data.frame(
  player_position = c(3,4,5,6),
  range_factor = c(-0.5,0,0,0), # extra ability of reaching a groundball
  transfer_time = c(1.1,0.9,1.1,0.9),
  throw_speed = c(80,90,110,105)
)




test_outputs_R = multiple_iterations(player_position_data,batter_R,4500,40)

test_outputs_L = multiple_iterations(player_position_data,batter_L,4500,40)
print(fielding_model_function_variable(batter_R,batter_speed,select(filter(test_outputs_R,iter_counter == max(iter_counter)),-iter_counter),infield_ability))
print(fielding_model_function_variable(batter_L,batter_speed,select(filter(test_outputs_L,iter_counter == max(iter_counter)),-iter_counter),infield_ability))

print(fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs_R,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`)
print(fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs_R,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`)
print(fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs_R,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`)

print(fielding_model_all_info_function(batter_L,batter_speed,select(filter(test_outputs_L,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`)
print(fielding_model_all_info_function(batter_L,batter_speed,select(filter(test_outputs_L,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`)
print(fielding_model_all_info_function(batter_L,batter_speed,select(filter(test_outputs_L,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`)

saveRDS(test_outputs_R,"plots/R_Fielding.rds")
saveRDS(test_outputs_L,"plots/L_Fielding.rds")


test_outputs_R %>% filter(iter_counter == max(iter_counter)) %>% ggplot(aes(x = field_x, y = field_y))+
  annotate("point",x=0,y=60.5,color="blue",size=5)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  geom_point(size=3)+
  ggtitle("Right Handed Batter")
ggsave("plots/Default_R.jpg",bg="white",width=9,height=5)

test_outputs_L %>% filter(iter_counter == max(iter_counter)) %>% ggplot(aes(x = field_x, y = field_y))+
  annotate("point",x=0,y=60.5,color="blue",size=5)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  geom_point(size=3)+
  ggtitle("Left Handed Batter")
ggsave("plots/Default_L.jpg",bg="white",width=9,height=5)







# Next look at modifying fielder abilities:




# How does a rangier center fielder change the ideal outfield positioning?


batter_R = find_batter_spray(batter_spray,avg_speed = 87,average_launch = 12,gb_pull = 0.7,fb_pull = 0.45,handedness = "R",tabulated_spray = spray_chart_tabulate)




batter_L = find_batter_spray(batter_spray,avg_speed = 87,average_launch = 12,gb_pull = 0.7,fb_pull = 0.45,handedness = "L",tabulated_spray = spray_chart_tabulate)



results = data.frame()

for(i in seq(-2,2,0.5)){
  
  player_position_data = data.frame(
    match = c(1,1,1,1,1,1,1),
    player_position = c(3,4,5,6,7,8,9),
    depth = c(110,151,118,145,298,322,294),
    angle = c(30,-1,-34,-15,-23,4,31),
    catch_factor = c(-1,-1,-1,-1,0,0,0)
  )
  
  batter_speed = 4500
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  
  infield_ability = data.frame(
    player_position = c(3,4,5,6),
    range_factor = c(-0.5,0,0,0), # extra ability of reaching a groundball
    transfer_time = c(1.1,0.9,1.1,0.9),
    throw_speed = c(80,90,110,105)
  )
  
  
  player_position_data$catch_factor = c(-1,-1,-1,-1,0,i,0)
  #data.frame(
  #match = c(1,1,1,1,1,1,1),
  #player_position = c(3,4,5,6,7,8,9),
  #depth = c(110,151,118,145,298,322,294),
  #angle = c(30,-1,-34,-15,-23,4,31),
  #catch_factor = c(0,0,0,0,0,i,0)# extra ability of catching a flyball
  #)
  
  
  
  batter_speed = 4500
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  test_outputs = multiple_iterations(player_position_data,batter_R,4500,30)
  
  results = bind_rows(results,mutate(test_outputs,cf_range = i,
                                     babip = fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
  
  print(fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`)
  print(fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`)
  print(fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`)
  
  
}





results %>% group_by(cf_range) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = cf_range))+
  #geom_point(size=2)+
  scale_color_viridis_c()+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=5)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
            ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "CF Fielding\nAbility")+
  geom_point(size=3)+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,cf_range),iter_counter==max(iter_counter))),cf_range==2),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("")
ggsave("plots/CF_range.jpg",bg="white")

saveRDS(results,"plots/results_fit_CF_ability.rds")


results$babip = results$babip$out_prob
ggplot(results,aes(x = cf_range,y = babip))+geom_point()

results$GO = results$GO$out_prob
ggplot(results,aes(x = cf_range,y = GO))+geom_point()

results$FO = results$FO$catch_prob
ggplot(results,aes(x = cf_range,y = FO))+geom_point()







# What about improving the range / throw speed / transfer time on the 3B?

batter_R = find_batter_spray(batter_spray,avg_speed = 87,average_launch = 12,gb_pull = 0.7,fb_pull = 0.45,handedness = "R",tabulated_spray = spray_chart_tabulate)
batter_L = find_batter_spray(batter_spray,avg_speed = 87,average_launch = 12,gb_pull = 0.7,fb_pull = 0.45,handedness = "L",tabulated_spray = spray_chart_tabulate)

results = data.frame()

for(i in seq(-2,2,0.5)){
 
  
  player_position_data = data.frame(
    match = c(1,1,1,1,1,1,1),
    player_position = c(3,4,5,6,7,8,9),
    depth = c(110,151,118,145,298,322,294),
    angle = c(30,-1,-34,-15,-23,4,31),
    catch_factor = c(-1,-1,-1,-1,0,0,0)# extra ability of catching a flyball
  )
  
  
  
  batter_speed = 4500
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  
  infield_ability = data.frame(
    player_position = c(3,4,5,6),
    range_factor = c(-0.5,0,i,0), # extra ability of reaching a groundball
    transfer_time = c(1.1,0.9,1.1-(i * 0.2),0.9),
    throw_speed = c(80,90,110 + (i * 10) ,105)
  )
  
  
  
  test_outputs = multiple_iterations(player_position_data,batter_R,4500,30)
  results = bind_rows(results,mutate(test_outputs,cf_range = i,
                                     babip = fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
  print(fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`)
}




results %>% group_by(cf_range) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = cf_range))+
  scale_color_viridis_c()+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=5)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "3B Fielding\nAbility")+
  geom_point(size=3)+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,cf_range),iter_counter==max(iter_counter))),cf_range==2),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("Right Handed Batter")

ggsave("plots/3B_range_R.jpg",bg="white")

# source("plot_fits.R")


saveRDS(results,"plots/results_fit_3b_R_ability.rds")

results$babip = results$babip$out_prob
ggplot(results,aes(x = cf_range,y = babip))+geom_point()

results$GO = results$GO$out_prob
ggplot(results,aes(x = cf_range,y = GO))+geom_point()

results$FO = results$FO$catch_prob
ggplot(results,aes(x = cf_range,y = FO))+geom_point()





# Same for a LHB



results = data.frame()

for(i in seq(-2,2,0.5)){
  
  player_position_data = data.frame(
    match = c(1,1,1,1,1,1,1),
    player_position = c(3,4,5,6,7,8,9),
    depth = c(110,151,118,145,298,322,294),
    angle = c(30,-1,-34,-15,-23,4,31),
    catch_factor = c(-1,-1,-1,-1,0,0,0)# extra ability of catching a flyball
  )
  
  
  
  batter_speed = 4500
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  
  infield_ability = data.frame(
    player_position = c(3,4,5,6),
    range_factor = c(-0.5,0,i,0), # extra ability of reaching a groundball
    transfer_time = c(1.1,0.9,1.1-(i * 0.2),0.9),
    throw_speed = c(80,90,110 + (i * 10) ,105)
  )
  
  
  
  test_outputs = multiple_iterations(player_position_data,batter_L,4500,30)
  results = bind_rows(results,mutate(test_outputs,cf_range = i,
                                     babip = fielding_model_all_info_function(batter_L,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_L,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_L,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
  
}




results %>% group_by(cf_range) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = cf_range))+
  scale_color_viridis_c()+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=5)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "3B Fielding\nAbility")+
  geom_point(size=3)+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,cf_range),iter_counter==max(iter_counter))),cf_range==2),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("Left Handed Batter")

ggsave("plots/3B_range_L.jpg",bg="white")

# source("plot_fits.R")


saveRDS(results,"plots/results_fit_3b_L_ability.rds")



results$babip = results$babip$out_prob
ggplot(results,aes(x = cf_range,y = babip))+geom_point()

results$GO = results$GO$out_prob
ggplot(results,aes(x = cf_range,y = GO))+geom_point()

results$FO = results$FO$catch_prob
ggplot(results,aes(x = cf_range,y = FO))+geom_point()








# Try the same for other positions, 2B, SS

# First 2B

results = data.frame()

for(i in seq(-2,2,0.5)){
  
  
  player_position_data = data.frame(
    match = c(1,1,1,1,1,1,1),
    player_position = c(3,4,5,6,7,8,9),
    depth = c(110,151,118,145,298,322,294),
    angle = c(30,-1,-34,-15,-23,4,31),
    catch_factor = c(-1,-1,-1,-1,0,0,0)# extra ability of catching a flyball
  )
  
  
  
  batter_speed = 4500
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  
  infield_ability = data.frame(
    player_position = c(3,4,5,6),
    range_factor = c(-.5,i,0,0), # extra ability of reaching a groundball
    transfer_time = c(1.1,0.9-(i * 0.2),1.1,0.9),
    throw_speed = c(80,90+ (i * 10),110  ,105)
  )
  
  
  
  test_outputs = multiple_iterations(player_position_data,batter_R,4500,30)
  results = bind_rows(results,mutate(test_outputs,cf_range = i,
                                     babip = fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
  print(fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`)
}




results %>% group_by(cf_range) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = cf_range))+
  scale_color_viridis_c()+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=5)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "2B Fielding\nAbility")+
  geom_point(size=3)+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,cf_range),iter_counter==max(iter_counter))),cf_range==2),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("Right Handed Batter")

ggsave("plots/2B_range_R.jpg",bg="white")

# source("plot_fits.R")


saveRDS(results,"plots/results_fit_2b_R_ability.rds")

results$babip = results$babip$out_prob
ggplot(results,aes(x = cf_range,y = babip))+geom_point()

results$GO = results$GO$out_prob
ggplot(results,aes(x = cf_range,y = GO))+geom_point()

results$FO = results$FO$catch_prob
ggplot(results,aes(x = cf_range,y = FO))+geom_point()





# Same for a LHB



results = data.frame()

for(i in seq(-2,2,0.5)){
  
  player_position_data = data.frame(
    match = c(1,1,1,1,1,1,1),
    player_position = c(3,4,5,6,7,8,9),
    depth = c(110,151,118,145,298,322,294),
    angle = c(30,-1,-34,-15,-23,4,31),
    catch_factor = c(-1,-1,-1,-1,0,0,0)# extra ability of catching a flyball
  )
  
  
  
  batter_speed = 4500
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  
  infield_ability = data.frame(
    player_position = c(3,4,5,6),
    range_factor = c(-.5,i,0,0), # extra ability of reaching a groundball
    transfer_time = c(1.1,0.9-(i * 0.2),1.1,0.9),
    throw_speed = c(80,90+ (i * 10),110  ,105)
  )
  
  
  
  test_outputs = multiple_iterations(player_position_data,batter_L,4500,30)
  results = bind_rows(results,mutate(test_outputs,cf_range = i,
                                     babip = fielding_model_all_info_function(batter_L,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_L,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_L,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
  
}




results %>% group_by(cf_range) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = cf_range))+
  scale_color_viridis_c()+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=5)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "2B Fielding\nAbility")+
  geom_point(size=3)+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,cf_range),iter_counter==max(iter_counter))),cf_range==2),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("Left Handed Batter")

ggsave("plots/2B_range_L.jpg",bg="white")

# source("plot_fits.R")


saveRDS(results,"plots/results_fit_2b_L_ability.rds")



results$babip = results$babip$out_prob
ggplot(results,aes(x = cf_range,y = babip))+geom_point()

results$GO = results$GO$out_prob
ggplot(results,aes(x = cf_range,y = GO))+geom_point()

results$FO = results$FO$catch_prob
ggplot(results,aes(x = cf_range,y = FO))+geom_point()









# Next SS

results = data.frame()

for(i in seq(-2,2,0.5)){
  
  
  player_position_data = data.frame(
    match = c(1,1,1,1,1,1,1),
    player_position = c(3,4,5,6,7,8,9),
    depth = c(110,151,118,145,298,322,294),
    angle = c(30,-1,-34,-15,-23,4,31),
    catch_factor = c(-1,-1,-1,-1,0,0,0)# extra ability of catching a flyball
  )
  
  
  
  batter_speed = 4500
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  
  infield_ability = data.frame(
    player_position = c(3,4,5,6),
    range_factor = c(-.5,0,0,i), # extra ability of reaching a groundball
    transfer_time = c(1.1,0.9,1.1,0.9-(i * 0.2)),
    throw_speed = c(80,90,110  ,105+ (i * 10))
  )
  
  
  
  test_outputs = multiple_iterations(player_position_data,batter_R,4500,30)
  results = bind_rows(results,mutate(test_outputs,cf_range = i,
                                     babip = fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
  print(fielding_model_all_info_function(batter_R,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`)
}




results %>% group_by(cf_range) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = cf_range))+
  scale_color_viridis_c()+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=5)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "SS Fielding\nAbility")+
  geom_point(size=3)+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,cf_range),iter_counter==max(iter_counter))),cf_range==2),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("Right Handed Batter")

ggsave("plots/SS_range_R.jpg",bg="white")

# source("plot_fits.R")


saveRDS(results,"plots/results_fit_SS_R_ability.rds")

results$babip = results$babip$out_prob
ggplot(results,aes(x = cf_range,y = babip))+geom_point()

results$GO = results$GO$out_prob
ggplot(results,aes(x = cf_range,y = GO))+geom_point()

results$FO = results$FO$catch_prob
ggplot(results,aes(x = cf_range,y = FO))+geom_point()





# Same for a LHB



results = data.frame()

for(i in seq(-2,2,0.5)){
  
  player_position_data = data.frame(
    match = c(1,1,1,1,1,1,1),
    player_position = c(3,4,5,6,7,8,9),
    depth = c(110,151,118,145,298,322,294),
    angle = c(30,-1,-34,-15,-23,4,31),
    catch_factor = c(-1,-1,-1,-1,0,0,0)# extra ability of catching a flyball
  )
  
  
  
  batter_speed = 4500
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  
  infield_ability = data.frame(
    player_position = c(3,4,5,6),
    range_factor = c(-.5,0,0,i), # extra ability of reaching a groundball
    transfer_time = c(1.1,0.9,1.1,0.9-(i * 0.2)),
    throw_speed = c(80,90,110  ,105+ (i * 10))
  )
  
  
  
  test_outputs = multiple_iterations(player_position_data,batter_L,4500,30)
  results = bind_rows(results,mutate(test_outputs,cf_range = i,
                                     babip = fielding_model_all_info_function(batter_L,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_L,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_L,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
  
}




results %>% group_by(cf_range) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = cf_range))+
  scale_color_viridis_c()+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=5)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "SS Fielding\nAbility")+
  geom_point(size=3)+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,cf_range),iter_counter==max(iter_counter))),cf_range==2),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("Left Handed Batter")

ggsave("plots/SS_range_L.jpg",bg="white")

# source("plot_fits.R")


saveRDS(results,"plots/results_fit_SS_L_ability.rds")



results$babip = results$babip$out_prob
ggplot(results,aes(x = cf_range,y = babip))+geom_point()

results$GO = results$GO$out_prob
ggplot(results,aes(x = cf_range,y = GO))+geom_point()

results$FO = results$FO$catch_prob
ggplot(results,aes(x = cf_range,y = FO))+geom_point()





###
###
###

# Finally try varying some of the batter tendencies in a similar way to these fielder tendencies
# And save figures in the same way as before:

# Start with groundball pull

results = data.frame()

for(i in seq(30,90,10)){
    
    batter_spray_L_pull = find_batter_spray(batter_spray,avg_speed = 87,average_launch = 12,gb_pull = i/100,fb_pull = 0.5,handedness = "L",tabulated_spray = spray_chart_tabulate)
    print(i)
    gb_pull_actual = sum(filter(batter_spray_L_pull,pull>=0,launch_angle<=10)$frac,na.rm=TRUE) / sum(filter(batter_spray_L_pull,launch_angle<=10)$frac,na.rm=TRUE)
    print(gb_pull_actual)
    player_position_data = data.frame(
      match = c(1,1,1,1,1,1,1),
      player_position = c(3,4,5,6,7,8,9),
      depth = c(110,151,118,145,298,322,294),
      angle = c(30,-1,-34,-15,-23,4,31),
      catch_factor = c(-1,-1,-1,-1,0,0,0)
    )

    batter_speed = 4500


    player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                           field_y = depth * cos(angle * pi / 180))

    infield_ability = data.frame(
      player_position = c(3,4,5,6),
      range_factor = c(-0.5,0,0,0), # extra ability of reaching a groundball
      transfer_time = c(1.1,0.9,1.1,0.9),
      throw_speed = c(80,90,110,105)
    )


  test_outputs = multiple_iterations(player_position_data,batter_spray_L_pull,4500,40)

  babip = fielding_model_function_variable(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)[3]
  print(babip)
  # results = bind_rows(results,mutate(test_outputs,gb_pull = gb_pull_actual,gb_pull_target = i,babip = babip))
  results = bind_rows(results,mutate(test_outputs,gb_pull = gb_pull_actual,
                                     gb_pull_target = i,
                                     babip = fielding_model_all_info_function(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
  

}

saveRDS(results,"plots/results_fit_gb_pull_L.rds")



results %>% group_by(gb_pull) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = gb_pull))+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=7)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "GB Pull%")+
  geom_point(size=3)+
  scale_color_viridis_c(labels=scales::percent_format())+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,gb_pull),iter_counter==max(iter_counter))),gb_pull_target==30),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("Left Handed Batter")
ggsave("plots/gb_pull_L.jpg",bg="white",width=9,height=5)






results = data.frame()

for(i in seq(30,90,10)){
  
  batter_spray_R_pull = find_batter_spray(batter_spray,avg_speed = 87,average_launch = 12,gb_pull = i/100,fb_pull = 0.5,handedness = "R",tabulated_spray = spray_chart_tabulate)
  print(i)
  gb_pull_actual = sum(filter(batter_spray_R_pull,pull<=0,launch_angle<=10)$frac,na.rm=TRUE) / sum(filter(batter_spray_R_pull,launch_angle<=10)$frac,na.rm=TRUE)
  print(gb_pull_actual)
  player_position_data = data.frame(
    match = c(1,1,1,1,1,1,1),
    player_position = c(3,4,5,6,7,8,9),
    depth = c(110,151,118,145,298,322,294),
    angle = c(30,-1,-34,-15,-23,4,31),
    catch_factor = c(-1,-1,-1,-1,0,0,0)
  )
  
  batter_speed = 4500
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  infield_ability = data.frame(
    player_position = c(3,4,5,6),
    range_factor = c(-0.5,0,0,0), # extra ability of reaching a groundball
    transfer_time = c(1.1,0.9,1.1,0.9),
    throw_speed = c(80,90,110,105)
  )
  
  
  test_outputs = multiple_iterations(player_position_data,batter_spray_R_pull,4500,40)
  
  babip = fielding_model_function_variable(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)[3]
  print(babip)
  results = bind_rows(results,mutate(test_outputs,gb_pull = gb_pull_actual,
                                     gb_pull_target = i,
                                     babip = fielding_model_all_info_function(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
  
}

saveRDS(results,"plots/results_fit_gb_pull_R.rds")



results %>% group_by(gb_pull) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = gb_pull))+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=7)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "GB Pull%")+
  geom_point(size=3)+
  scale_color_viridis_c(labels=scales::percent_format())+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,gb_pull),iter_counter==max(iter_counter))),gb_pull_target==30),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("Right Handed Batter")
ggsave("plots/gb_pull_R.jpg",bg="white",width=9,height=5)










# Move onto flyball pull:





results = data.frame()

for(i in seq(20,80,10)){
  
  batter_spray_L_pull = find_batter_spray(batter_spray,avg_speed = 87,average_launch = 12,gb_pull = 0.7,fb_pull = i/100,handedness = "L",tabulated_spray = spray_chart_tabulate)
  print(i)
  fb_pull_actual = sum(filter(batter_spray_L_pull,pull>=0,launch_angle>=10)$frac,na.rm=TRUE) / sum(filter(batter_spray_L_pull,launch_angle>=10)$frac,na.rm=TRUE)
  print(fb_pull_actual)
  player_position_data = data.frame(
    match = c(1,1,1,1,1,1,1),
    player_position = c(3,4,5,6,7,8,9),
    depth = c(110,151,118,145,298,322,294),
    angle = c(30,-1,-34,-15,-23,4,31),
    catch_factor = c(-1,-1,-1,-1,0,0,0)
  )
  
  batter_speed = 4500
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  infield_ability = data.frame(
    player_position = c(3,4,5,6),
    range_factor = c(-0.5,0,0,0), # extra ability of reaching a groundball
    transfer_time = c(1.1,0.9,1.1,0.9),
    throw_speed = c(80,90,110,105)
  )
  
  
  test_outputs = multiple_iterations(player_position_data,batter_spray_L_pull,4500,40)
  
  babip = fielding_model_function_variable(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)[3]
  print(babip)
  results = bind_rows(results,mutate(test_outputs,fb_pull = fb_pull_actual,
                                     fb_pull_target = i,
                                     babip = fielding_model_all_info_function(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
}

saveRDS(results,"plots/results_fit_fb_pull_L.rds")



results %>% group_by(fb_pull) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = fb_pull))+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=7)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "FB Pull%")+
  geom_point(size=3)+
  scale_color_viridis_c(labels=scales::percent_format())+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,fb_pull),iter_counter==max(iter_counter))),fb_pull_target==20),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("Left Handed Batter")
ggsave("plots/fb_pull_L.jpg",bg="white",width=9,height=5)






results = data.frame()

for(i in seq(20,80,10)){
  
  batter_spray_R_pull = find_batter_spray(batter_spray,avg_speed = 87,average_launch = 12,gb_pull = 0.7,fb_pull = i/100,handedness = "R",tabulated_spray = spray_chart_tabulate)
  print(i)
  fb_pull_actual = sum(filter(batter_spray_R_pull,pull<=0,launch_angle>=10)$frac,na.rm=TRUE) / sum(filter(batter_spray_R_pull,launch_angle>=10)$frac,na.rm=TRUE)
  print(fb_pull_actual)
  player_position_data = data.frame(
    match = c(1,1,1,1,1,1,1),
    player_position = c(3,4,5,6,7,8,9),
    depth = c(110,151,118,145,298,322,294),
    angle = c(30,-1,-34,-15,-23,4,31),
    catch_factor = c(-1,-1,-1,-1,0,0,0)
  )
  
  batter_speed = 4500
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  infield_ability = data.frame(
    player_position = c(3,4,5,6),
    range_factor = c(-0.5,0,0,0), # extra ability of reaching a groundball
    transfer_time = c(1.1,0.9,1.1,0.9),
    throw_speed = c(80,90,110,105)
  )
  
  
  test_outputs = multiple_iterations(player_position_data,batter_spray_R_pull,4500,40)
  
  babip = fielding_model_function_variable(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)[3]
  print(babip)
  results = bind_rows(results,mutate(test_outputs,fb_pull = fb_pull_actual,
                                     fb_pull_target = i,
                                     babip = fielding_model_all_info_function(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
  
}

saveRDS(results,"plots/results_fit_fb_pull_R.rds")



results %>% group_by(fb_pull) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = fb_pull))+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=7)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "FB Pull%")+
  geom_point(size=3)+
  scale_color_viridis_c(labels=scales::percent_format())+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,fb_pull),iter_counter==max(iter_counter))),fb_pull_target==20),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("Right Handed Batter")
ggsave("plots/fb_pull_R.jpg",bg="white",width=9,height=5)







# Next: launch angle


results = data.frame()

for(i in seq(-5,30,5)){
  
  batter_spray_L_pull = find_batter_spray(batter_spray,avg_speed = 87,average_launch = i,gb_pull = 0.7,fb_pull = 0.45,handedness = "L",tabulated_spray = spray_chart_tabulate)
  print(i)
  la_actual = weighted.mean(batter_spray_L_pull$launch_angle,batter_spray_L_pull$frac,na.rm=TRUE)
  print(la_actual)
  player_position_data = data.frame(
    match = c(1,1,1,1,1,1,1),
    player_position = c(3,4,5,6,7,8,9),
    depth = c(110,151,118,145,298,322,294),
    angle = c(30,-1,-34,-15,-23,4,31),
    catch_factor = c(-1,-1,-1,-1,0,0,0)
  )
  
  batter_speed = 4500
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  infield_ability = data.frame(
    player_position = c(3,4,5,6),
    range_factor = c(-0.5,0,0,0), # extra ability of reaching a groundball
    transfer_time = c(1.1,0.9,1.1,0.9),
    throw_speed = c(80,90,110,105)
  )
  
  
  test_outputs = multiple_iterations(player_position_data,batter_spray_L_pull,4500,40)
  
  babip = fielding_model_function_variable(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)[3]
  print(babip)
  results = bind_rows(results,mutate(test_outputs,la = la_actual,
                                     la_target = i,
                                     babip = fielding_model_all_info_function(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
  
}

saveRDS(results,"plots/results_fit_la_L.rds")



results %>% group_by(la) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = la))+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=7)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "Average\nLaunch Angle")+
  geom_point(size=3)+
  scale_color_viridis_c()+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,la),iter_counter==max(iter_counter))),la_target==-5),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("Left Handed Batter")
ggsave("plots/la_L.jpg",bg="white",width=9,height=5)






results = data.frame()

for(i in seq(-5,30,5)){
  
  batter_spray_R_pull = find_batter_spray(batter_spray,avg_speed = 87,average_launch = i,gb_pull = 0.7,fb_pull = 0.45,handedness = "R",tabulated_spray = spray_chart_tabulate)
  print(i)
  la_actual = weighted.mean(batter_spray_R_pull$launch_angle,batter_spray_R_pull$frac,na.rm=TRUE)
  print(la_actual)
  player_position_data = data.frame(
    match = c(1,1,1,1,1,1,1),
    player_position = c(3,4,5,6,7,8,9),
    depth = c(110,151,118,145,298,322,294),
    angle = c(30,-1,-34,-15,-23,4,31),
    catch_factor = c(-1,-1,-1,-1,0,0,0)
  )
  
  batter_speed = 4500
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  infield_ability = data.frame(
    player_position = c(3,4,5,6),
    range_factor = c(-0.5,0,0,0), # extra ability of reaching a groundball
    transfer_time = c(1.1,0.9,1.1,0.9),
    throw_speed = c(80,90,110,105)
  )
  
  
  test_outputs = multiple_iterations(player_position_data,batter_spray_R_pull,4500,40)
  
  babip = fielding_model_function_variable(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)[3]
  print(babip)
  results = bind_rows(results,mutate(test_outputs,la = la_actual,
                                     la_target = i,
                                     babip = fielding_model_all_info_function(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
  
}

saveRDS(results,"plots/results_fit_la_R.rds")



results %>% group_by(la) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = la))+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=7)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "Average\nLaunch Angle")+
  geom_point(size=3)+
  scale_color_viridis_c()+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,la),iter_counter==max(iter_counter))),la_target==20),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("Right Handed Batter")
ggsave("plots/la_R.jpg",bg="white",width=9,height=5)








# exit velocity





results = data.frame()

for(i in seq(75,95,2)){
  
  batter_spray_L_pull = find_batter_spray(batter_spray,avg_speed = i,average_launch = 12,gb_pull = 0.7,fb_pull = 0.45,handedness = "L",tabulated_spray = spray_chart_tabulate)
  print(i)
  ev_actual = weighted.mean(batter_spray_L_pull$launch_speed,batter_spray_L_pull$frac,na.rm=TRUE)
  print(ev_actual)
  player_position_data = data.frame(
    match = c(1,1,1,1,1,1,1),
    player_position = c(3,4,5,6,7,8,9),
    depth = c(110,151,118,145,298,322,294),
    angle = c(30,-1,-34,-15,-23,4,31),
    catch_factor = c(-1,-1,-1,-1,0,0,0)
  )
  
  batter_speed = 4500
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  infield_ability = data.frame(
    player_position = c(3,4,5,6),
    range_factor = c(-0.5,0,0,0), # extra ability of reaching a groundball
    transfer_time = c(1.1,0.9,1.1,0.9),
    throw_speed = c(80,90,110,105)
  )
  
  
  test_outputs = multiple_iterations(player_position_data,batter_spray_L_pull,4500,40)
  
  babip = fielding_model_function_variable(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)[3]
  print(babip)
  results = bind_rows(results,mutate(test_outputs,ev = ev_actual,
                                     ev_target = i,
                                     babip = fielding_model_all_info_function(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
  
}

saveRDS(results,"plots/results_fit_ev_L.rds")



results %>% group_by(ev) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = ev))+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=7)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "Exit Velocity\n/mph")+
  geom_point(size=3)+
  scale_color_viridis_c()+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,ev),iter_counter==max(iter_counter))),ev_target==-5),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("Left Handed Batter")
ggsave("plots/ev_L.jpg",bg="white",width=9,height=5)






results = data.frame()

for(i in seq(75,95,2)){
  
  batter_spray_R_pull = find_batter_spray(batter_spray,avg_speed = i,average_launch = 12,gb_pull = 0.7,fb_pull = 0.45,handedness = "R",tabulated_spray = spray_chart_tabulate)
  print(i)
  ev_actual = weighted.mean(batter_spray_R_pull$launch_speed,batter_spray_R_pull$frac,na.rm=TRUE)
  print(ev_actual)
  player_position_data = data.frame(
    match = c(1,1,1,1,1,1,1),
    player_position = c(3,4,5,6,7,8,9),
    depth = c(110,151,118,145,298,322,294),
    angle = c(30,-1,-34,-15,-23,4,31),
    catch_factor = c(-1,-1,-1,-1,0,0,0)
  )
  
  batter_speed = 4500
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  infield_ability = data.frame(
    player_position = c(3,4,5,6),
    range_factor = c(-0.5,0,0,0), # extra ability of reaching a groundball
    transfer_time = c(1.1,0.9,1.1,0.9),
    throw_speed = c(80,90,110,105)
  )
  
  
  test_outputs = multiple_iterations(player_position_data,batter_spray_R_pull,4500,40)
  
  babip = fielding_model_function_variable(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)[3]
  print(babip)
  results = bind_rows(results,mutate(test_outputs,ev = ev_actual,
                                     ev_target = i,
                                     babip = fielding_model_all_info_function(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
  
}

saveRDS(results,"plots/results_fit_ev_R.rds")



results %>% group_by(ev) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = ev))+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=7)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "Exit Velocity\n/mph")+
  geom_point(size=3)+
  scale_color_viridis_c()+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,ev),iter_counter==max(iter_counter))),ev_target==95),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("Right Handed Batter")
ggsave("plots/ev_R.jpg",bg="white",width=9,height=5)













# Speed


results = data.frame()

for(i in seq(4000,5000,100)){
  
  batter_spray_L_pull = find_batter_spray(batter_spray,avg_speed = 87,average_launch = 12,gb_pull = 0.7,fb_pull = 0.45,handedness = "L",tabulated_spray = spray_chart_tabulate)
  print(i)
  speed_actual = i
  print(speed_actual)
  player_position_data = data.frame(
    match = c(1,1,1,1,1,1,1),
    player_position = c(3,4,5,6,7,8,9),
    depth = c(110,151,118,145,298,322,294),
    angle = c(30,-1,-34,-15,-23,4,31),
    catch_factor = c(-1,-1,-1,-1,0,0,0)
  )
  
  batter_speed = i
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  infield_ability = data.frame(
    player_position = c(3,4,5,6),
    range_factor = c(-0.5,0,0,0), # extra ability of reaching a groundball
    transfer_time = c(1.1,0.9,1.1,0.9),
    throw_speed = c(80,90,110,105)
  )
  
  
  test_outputs = multiple_iterations(player_position_data,batter_spray_L_pull,batter_speed,40)
  
  babip = fielding_model_function_variable(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)[3]
  
  print(babip)
  results = bind_rows(results,mutate(test_outputs,speed = speed_actual,
                                     speed_target = i,
                                     babip = fielding_model_all_info_function(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_spray_L_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
  
}

saveRDS(results,"plots/results_fit_speed_L.rds")



results %>% group_by(speed) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = speed))+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=7)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "Batter\nHome-to-First\nTime /ms")+
  geom_point(size=3)+
  scale_color_viridis_c()+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,speed),iter_counter==max(iter_counter))),speed_target==4000),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("Left Handed Batter")
ggsave("plots/speed_L.jpg",bg="white",width=9,height=5)






results = data.frame()

for(i in seq(4000,5000,100)){
  
  batter_spray_R_pull = find_batter_spray(batter_spray,avg_speed = 87,average_launch = 12,gb_pull = 0.7,fb_pull = 0.45,handedness = "R",tabulated_spray = spray_chart_tabulate)
  print(i)
  speed_actual = i
  print(speed_actual)
  player_position_data = data.frame(
    match = c(1,1,1,1,1,1,1),
    player_position = c(3,4,5,6,7,8,9),
    depth = c(110,151,118,145,298,322,294),
    angle = c(30,-1,-34,-15,-23,4,31),
    catch_factor = c(-1,-1,-1,-1,0,0,0)
  )
  
  batter_speed = i
  
  
  player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                         field_y = depth * cos(angle * pi / 180))
  
  infield_ability = data.frame(
    player_position = c(3,4,5,6),
    range_factor = c(-0.5,0,0,0), # extra ability of reaching a groundball
    transfer_time = c(1.1,0.9,1.1,0.9),
    throw_speed = c(80,90,110,105)
  )
  
  
  test_outputs = multiple_iterations(player_position_data,batter_spray_R_pull,batter_speed,40)
  
  babip = fielding_model_function_variable(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)[3]
  
  print(babip)
  results = bind_rows(results,mutate(test_outputs,speed = speed_actual,
                                     speed_target = i,
                                     babip = fielding_model_all_info_function(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`BABIP`,
                                     GO = fielding_model_all_info_function(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`GO%`,
                                     FO = fielding_model_all_info_function(batter_spray_R_pull,batter_speed,select(filter(test_outputs,iter_counter == max(iter_counter)),-iter_counter),infield_ability)$`FO%`))
  
}

saveRDS(results,"plots/results_fit_speed_R.rds")



results %>% group_by(speed) %>% filter(iter_counter == max(iter_counter)) %>% ungroup() %>% ggplot(aes(x = field_x, y = field_y,color = speed))+
  #scale_fill_viridis_c()+
  annotate("point",x=0,y=60.5,color="blue",size=7)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  labs(color = "Batter\nHome-to-First\nTime /ms")+
  geom_point(size=3)+
  scale_color_viridis_c()+
  geom_path(aes(group = player_position),color="black",size=1)+
  geom_text_repel(data = filter(ungroup(filter(group_by(results,speed),iter_counter==max(iter_counter))),speed_target==4000),aes(label = player_position),nudge_x = -5,nudge_y =5 ,size=4,color="black",min.segment.length = 10)+
  ggtitle("Right Handed Batter")
ggsave("plots/speed_R.jpg",bg="white",width=9,height=5)

















# Any other examinations?
# Maybe a graphic for which part of the field a particular position has "control" over


# get example case

player_position_data = data.frame(
  match = c(1,1,1,1,1,1,1),
  player_position = c(3,4,5,6,7,8,9),
  depth = c(110,151,118,145,298,322,294),
  angle = c(30,-1,-34,-15,-23,4,31),
  catch_factor = c(-1,-1,-1,-1,0,0,0)
)

batter_speed = 4500


player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                       field_y = depth * cos(angle * pi / 180))

infield_ability = data.frame(
  player_position = c(3,4,5,6),
  range_factor = c(-0.5,0,0,0), # extra ability of reaching a groundball
  transfer_time = c(1.1,0.9,1.1,0.9),
  throw_speed = c(80,90,110,105)
)



results_summary <- fielding_model_all_info_function(batter_spray,4500,player_position_data,infield_ability)

results_summary_total = results_summary$infield_data %>% bind_rows(rename(results_summary$catch_data,success_field = catch_prob))

results_summary_total = results_summary_total %>% group_by(launch_angle,launch_speed,pull) %>% mutate(total_out_prob = sum(success_field,na.rm = TRUE)) %>% 
  ungroup()

results_summary_total = results_summary_total %>% mutate(total_out_prob = if_else(total_out_prob<=1,1,total_out_prob))

# results_summary_total = results_summary_total %>% mutate(success_field = if_else(success_field<=1e-1,0,success_field))

# results_summary_total = results_summary_total %>% filter(successful_field>=0.1)

results_summary_total %>% ggplot(aes(x = pos_x, y = pos_y,alpha = success_field / total_out_prob,fill = factor(player_position,levels =c(3,4,5,6,7,8,9),labels=c(3,4,5,6,7,8,9)),size=frac))+
  annotate("point",x=0,y=60.5,color="blue",size=7)+
  annotate("point",x=0,y=0,color="red",size=5)+
  annotate("point",x=90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=-90/sqrt(2),y=90/sqrt(2),color="red",size=5)+
  annotate("point",x=0,y=90*sqrt(2),color="red",size=5)+
  annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
  annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
  coord_fixed(xlim = c(-200,200),
              ylim = c(0,370))+
  theme_minimal()+
  theme(text = element_text(size=15),
        axis.title.y = element_text(angle = 0,vjust = 0.5))+
  xlab("ft")+
  ylab("ft")+
  geom_point(aes(color =  factor(player_position,levels =c(3,4,5,6,7,8,9),labels=c(3,4,5,6,7,8,9))),position = "jitter",pch=21,show.legend = FALSE)+
  geom_point(data = player_position_data,aes(x = field_x, y =field_y,fill = factor(player_position,levels =c(3,4,5,6,7,8,9),labels=c(3,4,5,6,7,8,9)),color=NULL),size=6,alpha=1,pch=21)+
  scale_alpha_continuous(range = c(0,0.5))+
  scale_size_continuous(range = c(0,4))+
  guides("color" = "none")+
  # scale_size_continuous(range = c(-1,6))+
  labs(fill = "Position",size = "Out%")+
  facet_wrap(~launch_angle)
  #facet_wrap(~round(launch_angle,-1))

ggsave("plots/field_control.jpg",bg="white",width=20,height=17)


# Any other examinations?

# If not then onto the shiny app!











# Build a little figure for logistic shifts:

data = data.frame(x = seq(-3,3,0.01),shift = 0)
data$value = 1/(1+exp(-data$x))


data_total = data.frame()
for(i in seq(2,-2,-0.5)){
  data = data.frame(x = seq(-5,5,0.01),shift = i)
  data$value = 1/(1+exp(-data$x - i))
  data$value0 = 1/(1+exp(-data$x))
  data_total = bind_rows(data_total,data)
}

ggplot(data_total,aes(x=value0,y=value,color = factor(shift)))+
  geom_point()+
  labs(color = "Fielding\nAbility\nShift")+
  xlab("Original Probability")+
  ylab("Updated Probability")+
  theme_minimal()+
  theme(text = element_text(size = 20))+
  scale_x_continuous(breaks = seq(0,1,0.1),labels = scales::percent_format())+
  scale_y_continuous(breaks = seq(0,1,0.1),labels = scales::percent_format())+
  coord_fixed()

ggsave("plots/logistic_shift.jpg",width=9,height=6,bg="white")














# Files to latex tables

library(xtable)


data = readRDS("plots/results_fit_speed_L.rds")
data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot = data %>% group_by(speed) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip_L = first(babip),FO_L = first(FO), GO_L = first(GO))

data = readRDS("plots/results_fit_speed_R.rds")
data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot2 = data %>% group_by(speed) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip_R = first(babip),FO_R = first(FO), GO_R = first(GO)) %>% select(-speed)

data_to_plot = bind_cols(data_to_plot,data_to_plot2)

data_to_plot = data_to_plot %>% mutate("Home-to-First Time /ms" = speed,
                                       "BABIP (LHB)" = babip_L,
                                       "BABIP (RHB)" = babip_R,
                                       #"Groundout% (LHB)" = GO_L,
                                       #"Groundout (RHB)" = GO_R,
                                       #"Flyout% (LHB)" = FO_L,
                                       #"Flyout% (RHB)" = FO_R,
                                       ) %>% 
  select(-speed,-babip_L,-FO_L,-GO_L,-babip_R,-FO_R,-GO_R)

print(xtable(data_to_plot, type = "latex",digits = 3), file = "plots/speed_table.tex",include.rownames = FALSE)


# EV

data = readRDS("plots/results_fit_ev_L.rds")
data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot = data %>% group_by(ev) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip_L = first(babip),FO_L = first(FO), GO_L = first(GO))

data = readRDS("plots/results_fit_ev_R.rds")
data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot2 = data %>% group_by(ev) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip_R = first(babip),FO_R = first(FO), GO_R = first(GO)) %>% select(-ev)

data_to_plot = bind_cols(data_to_plot,data_to_plot2)

data_to_plot = data_to_plot %>% mutate("Average Exit Velocity /mph" = ev,
                                       "BABIP (LHB)" = babip_L,
                                       "BABIP (RHB)" = babip_R,
                                       #"Flyout% (LHB)" = FO_L,
                                       #"Flyout% (RHB)" = FO_R,
) %>% 
  select(-ev,-babip_L,-FO_L,-GO_L,-babip_R,-FO_R,-GO_R)

print(xtable(data_to_plot, type = "latex",digits = 3), file = "plots/ev_table.tex",include.rownames = FALSE)



# LA

data = readRDS("plots/results_fit_la_L.rds")
data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot = data %>% group_by(la) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip_L = first(babip),FO_L = first(FO), GO_L = first(GO))

data = readRDS("plots/results_fit_la_R.rds")
data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot2 = data %>% group_by(la) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip_R = first(babip),FO_R = first(FO), GO_R = first(GO)) %>% select(-la)

data_to_plot = bind_cols(data_to_plot,data_to_plot2)

data_to_plot = data_to_plot %>% mutate("Average Launch Angle" = la,
                                       "BABIP (LHB)" = babip_L,
                                       "BABIP (RHB)" = babip_R,
                                       #"Flyout% (LHB)" = FO_L,
                                       #"Flyout% (RHB)" = FO_R,
) %>% 
  select(-la,-babip_L,-FO_L,-GO_L,-babip_R,-FO_R,-GO_R)

print(xtable(data_to_plot, type = "latex",digits = 3), file = "plots/la_table.tex",include.rownames = FALSE)





# GB Pull

data = readRDS("plots/results_fit_gb_pull_L.rds")
data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot = data %>% group_by(gb_pull) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip_L = first(babip),FO_L = first(FO), GO_L = first(GO))

data = readRDS("plots/results_fit_gb_pull_R.rds")
data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot2 = data %>% group_by(gb_pull) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip_R = first(babip),FO_R = first(FO), GO_R = first(GO)) %>% select(-gb_pull)

data_to_plot = bind_cols(data_to_plot,data_to_plot2)

data_to_plot = data_to_plot %>% mutate("Groundball Pull%" = gb_pull,
                                       "BABIP (LHB)" = babip_L,
                                       "BABIP (RHB)" = babip_R,
                                       #"Flyout% (LHB)" = FO_L,
                                       #"Flyout% (RHB)" = FO_R,
) %>% 
  select(-gb_pull,-babip_L,-FO_L,-GO_L,-babip_R,-FO_R,-GO_R)

print(xtable(data_to_plot, type = "latex",digits = 3), file = "plots/gb_pull_table.tex",include.rownames = FALSE)




# FB Pull

data = readRDS("plots/results_fit_fb_pull_L.rds")
data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot = data %>% group_by(fb_pull) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip_L = first(babip),FO_L = first(FO), GO_L = first(GO))

data = readRDS("plots/results_fit_fb_pull_R.rds")
data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot2 = data %>% group_by(fb_pull) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip_R = first(babip),FO_R = first(FO), GO_R = first(GO)) %>% select(-fb_pull)

data_to_plot = bind_cols(data_to_plot,data_to_plot2)

data_to_plot = data_to_plot %>% mutate("Flyball Pull%" = fb_pull,
                                       "BABIP (LHB)" = babip_L,
                                       "BABIP (RHB)" = babip_R,
                                       #"Flyout% (LHB)" = FO_L,
                                       #"Flyout% (RHB)" = FO_R,
) %>% 
  select(-fb_pull,-babip_L,-FO_L,-GO_L,-babip_R,-FO_R,-GO_R)

print(xtable(data_to_plot, type = "latex",digits = 3), file = "plots/fb_pull_table.tex",include.rownames = FALSE)




# 3B Ability

data = readRDS("plots/results_fit_3b_L_ability.rds")
data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot = data %>% group_by(cf_range) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip_L = first(babip),FO_L = first(FO), GO_L = first(GO))

data = readRDS("plots/results_fit_3b_R_ability.rds")
data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot2 = data %>% group_by(cf_range) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip_R = first(babip),FO_R = first(FO), GO_R = first(GO)) %>% select(-cf_range)

data_to_plot = bind_cols(data_to_plot,data_to_plot2)

data_to_plot = data_to_plot %>% mutate("3B Ability" = cf_range,
                                       "BABIP (LHB)" = babip_L,
                                       "BABIP (RHB)" = babip_R,
                                       #"Flyout% (LHB)" = FO_L,
                                       #"Flyout% (RHB)" = FO_R,
) %>% 
  select(-cf_range,-babip_L,-FO_L,-GO_L,-babip_R,-FO_R,-GO_R)

print(xtable(data_to_plot, type = "latex",digits = 3), file = "plots/3b_table.tex",include.rownames = FALSE)





# 2B Ability

data = readRDS("plots/results_fit_2b_L_ability.rds")
data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot = data %>% group_by(cf_range) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip_L = first(babip),FO_L = first(FO), GO_L = first(GO))

data = readRDS("plots/results_fit_2b_R_ability.rds")
data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot2 = data %>% group_by(cf_range) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip_R = first(babip),FO_R = first(FO), GO_R = first(GO)) %>% select(-cf_range)

data_to_plot = bind_cols(data_to_plot,data_to_plot2)

data_to_plot = data_to_plot %>% mutate("2B Ability" = cf_range,
                                       "BABIP (LHB)" = babip_L,
                                       "BABIP (RHB)" = babip_R,
                                       #"Flyout% (LHB)" = FO_L,
                                       #"Flyout% (RHB)" = FO_R,
) %>% 
  select(-cf_range,-babip_L,-FO_L,-GO_L,-babip_R,-FO_R,-GO_R)

print(xtable(data_to_plot, type = "latex",digits = 3), file = "plots/2b_table.tex",include.rownames = FALSE)






# SS Ability

data = readRDS("plots/results_fit_SS_L_ability.rds")
data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot = data %>% group_by(cf_range) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip_L = first(babip),FO_L = first(FO), GO_L = first(GO))

data = readRDS("plots/results_fit_SS_R_ability.rds")
data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot2 = data %>% group_by(cf_range) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip_R = first(babip),FO_R = first(FO), GO_R = first(GO)) %>% select(-cf_range)

data_to_plot = bind_cols(data_to_plot,data_to_plot2)

data_to_plot = data_to_plot %>% mutate("SS Ability" = cf_range,
                                       "BABIP (LHB)" = babip_L,
                                       "BABIP (RHB)" = babip_R,
                                       #"Flyout% (LHB)" = FO_L,
                                       #"Flyout% (RHB)" = FO_R,
) %>% 
  select(-cf_range,-babip_L,-FO_L,-GO_L,-babip_R,-FO_R,-GO_R)

print(xtable(data_to_plot, type = "latex",digits = 3), file = "plots/SS_table.tex",include.rownames = FALSE)






# CF Range


data$babip = data$babip$out_prob
data$GO = data$GO$out_prob
data$FO = data$FO$catch_prob
data_to_plot = data %>% group_by(cf_range) %>% 
  filter(iter_counter == max(iter_counter),player_position==5) %>% 
  summarise(babip = first(babip),FO = first(FO), GO = first(GO))


data_to_plot = data_to_plot %>% mutate("CF Ability" = cf_range,
                                       "BABIP" = babip,
                                       #"Flyout% (LHB)" = FO_L,
                                       #"Flyout% (RHB)" = FO_R,
) %>% 
  select(-cf_range,-babip,-FO,-GO)

print(xtable(data_to_plot, type = "latex",digits = 3), file = "plots/cf_table.tex",include.rownames = FALSE)














# Finally convert some of the rds files to csv files

data = readRDS("plots/results_fit_CF_ability.rds")
data_to_save = data %>% group_by(cf_range) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO
    ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_CF_ability.csv")

data = readRDS("plots/results_fit_2b_L_ability.rds")
data_to_save = data %>% group_by(cf_range) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% rename("2b_range" = cf_range) %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO
  ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_2B_L_ability.csv")

data = readRDS("plots/results_fit_2b_R_ability.rds")
data_to_save = data %>% group_by(cf_range) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% rename("2b_range" = cf_range) %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO
  ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_2B_R_ability.csv")



data = readRDS("plots/results_fit_3b_L_ability.rds")
data_to_save = data %>% group_by(cf_range) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% rename("3b_range" = cf_range) %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO
  ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_3B_L_ability.csv")

data = readRDS("plots/results_fit_3b_R_ability.rds")
data_to_save = data %>% group_by(cf_range) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% rename("3b_range" = cf_range) %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO
  ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_3B_R_ability.csv")




data = readRDS("plots/results_fit_SS_L_ability.rds")
data_to_save = data %>% group_by(cf_range) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% rename("SS_range" = cf_range) %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO
  ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_SS_L_ability.csv")

data = readRDS("plots/results_fit_SS_R_ability.rds")
data_to_save = data %>% group_by(cf_range) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% rename("SS_range" = cf_range) %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO
  ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_SS_R_ability.csv")



data = readRDS("plots/results_fit_speed_L.rds")
data_to_save = data %>% group_by(speed) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO,-speed_target
  ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_speed_L.csv")


data = readRDS("plots/results_fit_speed_R.rds")
data_to_save = data %>% group_by(speed) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO,-speed_target
  ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_speed_R.csv")




data = readRDS("plots/results_fit_ev_L.rds")
data_to_save = data %>% group_by(ev) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO,-ev_target
  ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_ev_L.csv")


data = readRDS("plots/results_fit_la_L.rds")
data_to_save = data %>% group_by(la) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO,-la_target
  ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_la_L.csv")


data = readRDS("plots/results_fit_gb_pull_L.rds")
data_to_save = data %>% group_by(gb_pull) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO,-gb_pull_target
  ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_gb_pull_L.csv")

data = readRDS("plots/results_fit_fb_pull_L.rds")
data_to_save = data %>% group_by(fb_pull) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO,-fb_pull_target
  ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_fb_pull_L.csv")






data = readRDS("plots/results_fit_ev_R.rds")
data_to_save = data %>% group_by(ev) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO,-ev_target
  ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_ev_R.csv")


data = readRDS("plots/results_fit_la_R.rds")
data_to_save = data %>% group_by(la) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO,-la_target
  ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_la_R.csv")


data = readRDS("plots/results_fit_gb_pull_R.rds")
data_to_save = data %>% group_by(gb_pull) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO,-gb_pull_target
  ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_gb_pull_R.csv")

data = readRDS("plots/results_fit_fb_pull_R.rds")
data_to_save = data %>% group_by(fb_pull) %>% 
  filter(iter_counter == max(iter_counter)) %>% ungroup() %>% select(
    -match,-angle,-catch_factor,-iter_counter,-GO,-FO,-fb_pull_target
  ) %>% mutate(babip = babip$out_prob)
data_to_save
write.csv(data_to_save,"data/results_fit_fb_pull_R.csv")
