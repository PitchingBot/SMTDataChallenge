
binned_results_dframe = readRDS(file = "binned_batted_ball_sample_with_hang.rds")

# catch probability model:

catch_prob = readRDS("catch_probability_glm.rds")

# infield range model:

infield_range = readRDS("range_model_GLM.rds")

# infield time model

infield_time_model = readRDS("time_model_LM.rds")

# infield depth model

infield_depth_model = readRDS("depth_model_LM.rds")


spray_chart_tabulate = readRDS("tabulated_spray_chart.rds")


max_prob_fitting = FALSE

binned_results_dframe_filtered = binned_results_dframe# %>% filter(frac>0)

batter_spray = binned_results_dframe_filtered

arc_data = data.frame(x = 95*sin(seq(-pi/2.5,pi/2.5,0.001)), y=60.5+95*cos(seq(-pi/2.5,pi/2.5,0.001)))

# player_position_data = data.frame(
#   match = c(1,1,1,1,1,1,1),
#   player_position = c(3,4,5,6,7,8,9),
#   depth = c(110,151,118,145,298,322,294),
#   angle = c(34,11,-29,-11,-27,0,27),
#   catch_factor = c(0,0,0,0,0,0,0)
# )
# 
# 
# player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
#                                                        field_y = depth * cos(angle * pi / 180))
# 


# 
# infield_ability = data.frame(
#   player_position = c(3,4,5,6),
#   range_factor = c(0,0,0,0), # extra ability of reaching a groundball
#   transfer_time = c(1.3,1.1,1.3,1.1),
#   throw_speed = c(80,90,110,105)
# )



# For a given desired spray chart, get the distribution which fits:

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
  
  if(handedness=="L"){
    batter_spray_speed = batter_spray_speed %>% mutate(pull = -pull) %>% 
      mutate(pos_x = hit_distance * sin(pull * pi / 180),
             pos_y = hit_distance*cos(pull * pi / 180))
  }
  
  batter_spray_speed = batter_spray_speed %>% filter(frac>=0)
  
  return(batter_spray_speed)
  
  
}






adjust_batter_spray = function(batter_spray,avg_speed,average_launch,gb_pull,fb_pull,handedness){
  
  # First find the combination of index shifts which produces the closest match to the desired launch angle, gb pull and fb pull amounts
  
  #av_launch_angle av_launch_speed av_gb_pull av_fb_pull
  #1        14.02765        89.99399  0.7319683  0.4656625
  
  results = data.frame()
  
  if(average_launch <=14){
    launch_fac = -1
  }else{
    launch_fac = 1
  }
  
  if(gb_pull <=0.73){
    gb_pull_fac = -1
  }else{
    gb_pull_fac = 1
  }
  
  if(fb_pull <=0.46){
    fb_pull_fac = -1
  }else{
    fb_pull_fac = 1
  }
  
  for(i in seq(0,10)){
    if (launch_fac >= 0){
      batter_spray_launch = batter_spray %>% group_by(launch_speed,pull) %>% arrange(launch_angle) %>% mutate(
        frac = lag(frac,n=i,default = 0)
      ) %>% ungroup()
    }else{
      batter_spray_launch = batter_spray %>% group_by(launch_speed,pull) %>% arrange(launch_angle) %>% mutate(
        frac = lead(frac,n=i,default = 0)
      ) %>% ungroup()
    }
    for(j in seq(0,5)){
      
      if(gb_pull_fac <= 0){
        batter_spray_pull_gb = batter_spray_launch %>% group_by(launch_speed,launch_angle) %>% arrange(pull) %>% mutate(
          frac = if_else(launch_angle <=10,lag(frac,n=j,default = 0),frac)
        ) %>% ungroup()
      }else{
        batter_spray_pull_gb = batter_spray_launch %>% group_by(launch_speed,launch_angle) %>% arrange(pull) %>% mutate(
          frac = if_else(launch_angle <=10,lead(frac,n=as.integer(j),default = 0),frac)
        ) %>% ungroup()
      }
      
      for(k in seq(0,5)){
        
        
        if (fb_pull_fac <= 0){
          batter_spray_pull_fb = batter_spray_pull_gb %>% group_by(launch_speed,launch_angle) %>% arrange(pull) %>% mutate(
            frac = if_else(launch_angle >10,lag(frac,n=k,default = 0),frac)
          ) %>% ungroup()
        }else{
          batter_spray_pull_fb = batter_spray_pull_gb %>% group_by(launch_speed,launch_angle) %>% arrange(pull) %>% mutate(
            frac = if_else(launch_angle >10,lead(frac,n=as.integer(k),default = 0),frac)
          ) %>% ungroup()
        }
        
        results_out = batter_spray_pull_fb %>% summarise(av_launch_angle = weighted.mean(launch_angle,frac),
                                                         av_launch_speed = weighted.mean(launch_speed,frac),
                                                         av_gb_pull = weighted.mean(pull<=0,frac * as.numeric(launch_angle<=10)),
                                                         av_fb_pull = weighted.mean(pull<=0,frac * as.numeric(launch_angle>10)))
        
        results = bind_rows(results,mutate(results_out,step_launch = i,
                                           step_gb_pull = j,
                                           step_fb_pull = k))
        
      }
    }
  }
  
  results = results %>% mutate(diff_to_input = sqrt((0.01*average_launch - 0.01*av_launch_angle)**2 +
                                                      (gb_pull - av_gb_pull)**2 +
                                                      (fb_pull - av_fb_pull)**2
  )
  )
  
  
  best_results = results %>% filter(diff_to_input==min(diff_to_input))
  
  i = best_results$step_launch
  j = best_results$step_gb_pull
  k = best_results$step_fb_pull
  
  
  if (launch_fac >= 0){
    batter_spray_adjust= batter_spray %>% group_by(launch_speed,pull) %>% arrange(launch_angle) %>% mutate(
      frac = lag(frac,n=i,default = 0)
    ) %>% ungroup()
  }else{
    batter_spray_adjust = batter_spray %>% group_by(launch_speed,pull) %>% arrange(launch_angle) %>% mutate(
      frac = lead(frac,n=i,default = 0)
    ) %>% ungroup()
  }
  
  if(gb_pull_fac <= 0){
    batter_spray_adjust = batter_spray_adjust %>% group_by(launch_speed,launch_angle) %>% arrange(pull) %>% mutate(
      frac = if_else(launch_angle <=10,lag(frac,n=j,default = 0),frac)
    ) %>% ungroup()
  }else{
    batter_spray_adjust = batter_spray_adjust %>% group_by(launch_speed,launch_angle) %>% arrange(pull) %>% mutate(
      frac = if_else(launch_angle <=10,lead(frac,n=as.integer(j),default = 0),frac)
    ) %>% ungroup()
  }
  
  if (fb_pull_fac <= 0){
    batter_spray_adjust = batter_spray_adjust %>% group_by(launch_speed,launch_angle) %>% arrange(pull) %>% mutate(
      frac = if_else(launch_angle >10,lag(frac,n=k,default = 0),frac)
    ) %>% ungroup()
  }else{
    batter_spray_adjust = batter_spray_adjust %>% group_by(launch_speed,launch_angle) %>% arrange(pull) %>% mutate(
      frac = if_else(launch_angle >10,lead(frac,n=as.integer(k),default = 0),frac)
    ) %>% ungroup()
  }
  
  
  
  # Finally adjust speed / distance using this profile
  
  batter_spray_adjust = batter_spray_adjust %>% filter(pull<=45,pull>=-45)
  
  speed_diff = avg_speed - weighted.mean(batter_spray_adjust$launch_speed,batter_spray_adjust$frac)
  
  batter_spray_speed = batter_spray_adjust %>% mutate(launch_speed = launch_speed + speed_diff)
  
  batter_spray_speed = batter_spray_speed %>% mutate(hit_distance = hit_distance + 12 * speed_diff / 2.5)
  
  if(handedness=="L"){
    batter_spray_speed = batter_spray_speed %>% mutate(pull = -pull) %>% 
      mutate(pos_x = hit_distance * sin(pull * pi / 180),
             pos_y = hit_distance*cos(pull * pi / 180))
  }
  
  return(batter_spray_speed)
}



fielding_model_function_variable = function(hitter_spray,batter_time_in,player_position,infield_ability_in = infield_ability,max_prob = FALSE){
  time1 = timeDate::timeDate()
  binned_results_dframe_filtered = hitter_spray %>% mutate(launch_angle = if_else(launch_angle <=-10,-10,launch_angle))
  
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
                                           #reaction_factor = if_else(init_depth<=120,0.01428571*(init_depth - 50),1),
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
  # print((infield_plays_summary %>% ungroup() %>% summarise(infield_prob = weighted.mean(infield_prob,frac)))* infield_frac)
  
  time6 = timeDate::timeDate()
  
  
  return(c(as.numeric(infield_out_frac),as.numeric(catch_out_frac),as.numeric(total_babip)))
  
}

iterate_step = function(player_position,batter_spray,batter_speed){
  
  movements = c("u","l","r","d") #u,l,d,r directions, n = unchanged
  
  temp_results = data.frame()
  
  temp_results = bind_rows(temp_results,data.frame(
    player_position = c(3,4,5,6,7,8,9),
    movement_dir = "n",
    babip = fielding_model_function_variable(batter_spray,batter_speed,player_position,infield_ability,max_prob = max_prob_fitting)[3]
  ))
  
  for(p in c(3,4,5,6,7,8,9)){ # Don't move 1B for now, realistic limit on how far they can move given they need to be near the bag
    #print(p)
    for(m in movements){
      player_position_data_temp = player_position %>% mutate(
        field_x = if_else((m == "l")&(player_position==p), field_x - 10, field_x),
        field_x = if_else((m == "r")&(player_position==p), field_x + 10, field_x),
        field_y = if_else((m == "d")&(player_position==p), field_y - 10, field_y),
        field_y = if_else((m == "u")&(player_position==p), field_y + 10, field_y),
      ) %>% 
        mutate(depth = sqrt((field_x)**2 + (field_y)**2))
      
      babip_in = fielding_model_function_variable(batter_spray,batter_speed,player_position_data_temp,infield_ability,max_prob = max_prob_fitting)[3]
      
      
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
    field_x = if_else((movement_dir == "l"), field_x - 10, field_x),
    field_x = if_else((movement_dir == "r"), field_x + 10, field_x),
    field_y = if_else((movement_dir == "d"), field_y - 10, field_y),
    field_y = if_else((movement_dir == "u"), field_y + 10, field_y),
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

    
    shinybusy::update_modal_spinner(text = paste0("Done Fitting Iteration ",iter_counter," (Max. 15)"))
  }
  return(player_position_counter)
}



iterate_step_fast = function(player_position,batter_spray,batter_speed,infield_ability,positions_stable){
  
  # print(player_position)
  movements = c("u","l","r","d") #u,l,d,r directions, n = unchanged
  
  temp_results = data.frame()
  
  move_positions = c(3,4,5,6,7,8,9)
  
  move_positions = move_positions[!move_positions %in% positions_stable]
  
  temp_results = bind_rows(temp_results,data.frame(
    player_position = c(3,4,5,6,7,8,9),
    movement_dir = "n",
    babip = fielding_model_function_variable(batter_spray,batter_speed,player_position,infield_ability,max_prob = max_prob_fitting)[3]
  ))
  
  for(p in move_positions){ # Don't move 1B for now, realistic limit on how far they can move given they need to be near the bag
    #print(p)
    for(m in movements){
      player_position_data_temp = player_position %>% mutate(
        field_x = if_else((m == "l")&(player_position==p), field_x - 10, field_x),
        field_x = if_else((m == "r")&(player_position==p), field_x + 10, field_x),
        field_y = if_else((m == "d")&(player_position==p), field_y - 10, field_y),
        field_y = if_else((m == "u")&(player_position==p), field_y + 10, field_y),
      ) %>% 
        mutate(depth = sqrt((field_x)**2 + (field_y)**2))
      
      babip_in = fielding_model_function_variable(batter_spray,batter_speed,player_position_data_temp,infield_ability,max_prob = max_prob_fitting)[3]
      
      
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
  
  
  
  #print(player_position)
  
  
  new_player_position = player_position %>% left_join(movement_direction) %>% mutate(
    field_x = if_else((movement_dir == "l"), field_x - 10, field_x),
    field_x = if_else((movement_dir == "r"), field_x + 10, field_x),
    field_y = if_else((movement_dir == "d"), field_y - 10, field_y),
    field_y = if_else((movement_dir == "u"), field_y + 10, field_y),
  ) %>% 
    mutate(depth = sqrt((field_x)**2 + (field_y)**2)) %>% select(-movement_dir)
  
  return(new_player_position)
}











multiple_iterations_fast = function(player_position,batter_spray,batter_speed,infield_ability,max_iter){
  iter_counter = 0
  positions_stable = c()
  player_position_counter = data.frame()
  player_position_new = player_position
  while(iter_counter < max_iter){
    #print(iter_counter)
    player_position_new_temp = iterate_step_fast(player_position_new,batter_spray,batter_speed,infield_ability,positions_stable)
    if(all((arrange(player_position_new,player_position)$field_x==arrange(player_position_new_temp,player_position)$field_x)&
           (arrange(player_position_new,player_position)$field_y==arrange(player_position_new_temp,player_position)$field_y))){
      player_position_counter = bind_rows(player_position_counter,mutate(player_position_new_temp,iter_counter = iter_counter))
      return(player_position_counter)
    }
    else{
      player_position_counter = bind_rows(player_position_counter,mutate(player_position_new_temp,iter_counter = iter_counter))
    }
    iter_counter = iter_counter + 1
    player_position_new = player_position_new_temp
    
    # Update the stable_positions list
    
    for(position in c(3,4,5,6,7,8,9)){
      position_data = player_position_counter %>% filter(player_position==position) %>% arrange(-iter_counter)
      if(dim(position_data)[1]>=3){
        if((position_data$field_x[1]==position_data$field_x[2])&
           (position_data$field_x[1]==position_data$field_x[3])&
           (position_data$field_y[1]==position_data$field_y[2])&
           (position_data$field_y[1]==position_data$field_y[3])&
           !(position %in% positions_stable)){
          positions_stable = append(positions_stable,position)
        }
      }
    }
    shinybusy::update_modal_spinner(text = paste0("Done Fitting Iteration ",iter_counter," (Max. 15)"))
    # if(sum(duplicated(select(player_position_counter,-iter_counter)))>=1){
    #   return(player_position_counter)
    # }
  }
  return(player_position_counter)
}


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
                                           #reaction_factor = if_else(init_depth<=120,0.01428571*(init_depth - 50),1),
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
  
  

  if(!max_prob){
    infield_plays_summary_no_group = infield_plays %>% group_by(pull,launch_angle,launch_speed,frac,hit_distance,pos_x,pos_y) %>% mutate(
      out_prob_sum = sum(success_field,na.rm=TRUE),
      out_prob = 1- Reduce("*",fail_field),
      fielded_sum = sum(fielded,na.rm=TRUE),
      fielded_agg = 1 - Reduce("*",1-fielded),
      #out_prob = max(success_field,na.rm=TRUE),
      infield_prob = 1 - Reduce("*",fail_reach_ball),
      .groups="drop"
    ) %>% ungroup() %>% 
      mutate(out_prob = if_else(out_prob>=1,1,out_prob))%>% 
      mutate(
        fraction_player = success_field * out_prob / out_prob_sum,
        fraction_player_field = fielded * fielded_agg / fielded_sum
      )
  }
  
  if(!max_prob){
    binned_results_dframe_joined_summary_no_group = binned_results_dframe_joined %>% group_by(pull,launch_angle,launch_speed,frac,hit_distance,pos_x,pos_y) %>% 
      mutate(no_catch_prob = 1 - catch_prob) %>% 
     mutate(
        catch_prob_sum = sum(catch_prob,na.rm=TRUE),
        catch_prob_total = 1 - Reduce("*",no_catch_prob),
        #catch_prob = max(catch_prob,na.rm=TRUE),
        .groups="drop"
      ) %>% ungroup() %>% 
      mutate(fraction_player = catch_prob * catch_prob_total / catch_prob_sum)
  }
  
  
  
  infield_out_frac = (infield_plays_summary %>% ungroup() %>% summarise(out_prob = weighted.mean(out_prob,frac)))* infield_frac
  catch_out_frac = binned_results_dframe_joined_summary %>% ungroup() %>% summarise(catch_prob = weighted.mean(catch_prob,frac))* outfield_frac
  
  
  total_babip = 1 - (infield_plays_summary %>% ungroup() %>% summarise(out_prob = weighted.mean(out_prob,frac)) * infield_frac) - 
    ((binned_results_dframe_joined_summary %>% ungroup() %>% summarise(catch_prob = weighted.mean(catch_prob,frac))) * outfield_frac)
  # print(total_babip)
  
  
  
  # functions to get positional summary
  
  print(total_frac)
  
  infield_summary = (infield_plays_summary_no_group %>% group_by(player_position) %>% summarise(out_made = sum(fraction_player*frac,na.rm=TRUE)/total_frac,
                                                                                                infield_hit = sum((fraction_player_field - fraction_player)*frac,na.rm=TRUE)/total_frac))
  
  # catch_summary = binned_results_dframe_joined %>% group_by(player_position) %>% summarise(catch = sum(catch_prob*frac))
  
  catch_summary = (binned_results_dframe_joined_summary_no_group %>% group_by(player_position) %>% summarise(catch = sum(fraction_player*frac,na.rm=TRUE)/total_frac))
  
  
  #print(head(infield_plays))
  #print(colnames(infield_plays))
  
  return(list("BABIP" = total_babip,
              "GO%" = infield_out_frac,
              "FO%" = catch_out_frac,
              "Frac_Under_10_Degrees" = infield_frac,
              "Frac_Over_10_Degrees" = outfield_frac,
              "infield_data" = infield_plays,
              "catch_data" = binned_results_dframe_joined,
              "infield_summary" = infield_summary,
              "catch_summary" = catch_summary))
  
}




EV_hist = function(batter_spray_in){
  fig = batter_spray_in %>% group_by(launch_speed) %>% summarise(frac = sum(frac)) %>% ggplot(aes(x = launch_speed,y=frac))+
    geom_point(size=4)+
    geom_line(size=2)+
    theme_minimal()+
    xlab("Exit Velocity /mph")+
    ylab("Relative Frequency")+
    theme(text = element_text(size = 20))+
    xlim(40,120)
  return(fig)
}

LA_hist = function(batter_spray_in){
  fig = batter_spray_in %>% group_by(launch_angle) %>% summarise(frac = sum(frac)) %>% ggplot(aes(x = launch_angle,y=frac))+
    geom_point(size=4)+
    geom_line(size=2)+
    theme_minimal()+
    xlab("Launch Angle /degrees")+
    ylab("Relative Frequency")+
    theme(text = element_text(size = 20))
  return(fig)
}

Pull_hist  = function(batter_spray_in){
  # fig = batter_spray_in %>% group_by(pull,launch_angle = round(launch_angle*0.1,0)*10) %>% summarise(frac = sum(frac)) %>% ggplot(aes(x = pull,y=frac))+
  #   geom_point(size=4)+
  #   geom_line(size=2)+
  #   theme_minimal()+
  #   xlab("Spray Angle /degrees")+
  #   ylab("Relative Frequency")+
  #   facet_wrap(~paste("LA:",(round(launch_angle*0.1,0)*10),sep=" "))+
  #   theme(text = element_text(size = 20))
  
  fig = batter_spray_in %>% group_by(pull,launch_angle = round(launch_angle*0.1,0)*10) %>% summarise(frac = sum(frac)) %>% filter(launch_angle <=75) %>%  ggplot(aes(x = pull,y=frac))+
    geom_point(size=4)+
    geom_line(size=2)+
    theme_minimal()+
    xlab("Spray Angle /degrees")+
    ylab("Relative Frequency")+
    facet_wrap(~launch_angle)+
    theme(text = element_text(size = 20))
  return(fig)
}

plot_positions = function(player_position_data_in){
  fig = player_position_data_in %>% ggplot(aes(x = field_x, y = field_y))+
    geom_point(size=4,color="black")+
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
    theme(axis.text.x = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          axis.ticks.x = element_blank())+
    theme(text = element_text(size = 20),
          axis.text.x = element_blank())+
    geom_label(aes(label = player_position),nudge_x = 10,nudge_y = 10)+
    geom_line(data = arc_data,aes(x=x,y=y),color="black",size=1)+
    xlab("")+
    ylab("")
  
  return(fig)
}


plot_output_catch = function(binned_results_dframe_joined,player_position_data){
  
  binned_results_dframe_joined_summary = binned_results_dframe_joined %>% group_by(pull,launch_angle,launch_speed,frac,hit_distance,pos_x,pos_y) %>% 
    mutate(no_catch_prob = 1 - catch_prob) %>% 
    summarise(
      catch_prob_sum = sum(catch_prob,na.rm=TRUE),
      catch_prob = 1 - Reduce("*",no_catch_prob),
    ) %>% 
    mutate(catch_prob = if_else(catch_prob>=1,1,catch_prob))
  
  #total_catch_prob  = 
  binned_results_dframe_joined_summary %>% ungroup() %>% summarise(catch_prob = weighted.mean(catch_prob,frac))
  
  
  fig = binned_results_dframe_joined_summary %>% filter(hit_distance!=0,launch_angle>=0,launch_angle <=74) %>% ggplot(aes(x = pos_x,y = pos_y,color = catch_prob))+
    geom_point()+
    scale_color_viridis_c(lim=c(0,1),oob = scales::squish,labels = scales::percent_format())+
    labs(color = "Catch%")+
    theme_minimal()+
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          axis.ticks = element_blank())+
    xlab("")+
    ylab("")+
    facet_wrap(~(round(launch_angle*0.1,0)*10),ncol = 2)+
    annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
    annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
    annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
    annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
    annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
    coord_fixed()+
    geom_line(data = arc_data,aes(x=x,y=y),color="black",size=1)+
    geom_point(data =  player_position_data,aes(x = field_x,y=field_y),color="red",size=4)+
    theme(text = element_text(size = 20),
          axis.text = element_blank())
  return(fig)
}
plot_output_infield = function(infield_plays_in,infield_position_data_in){
  
  infield_plays_summary = infield_plays_in %>% group_by(pull,launch_angle,launch_speed,frac,hit_distance,pos_x,pos_y) %>% summarise(
    out_prob_sum = sum(success_field,na.rm=TRUE),
    field_depth = field_depth[success_field == max(success_field)],
    out_prob = 1- Reduce("*",fail_field),
    infield_prob = 1 - Reduce("*",fail_reach_ball)
  ) %>% 
    mutate(out_prob = if_else(out_prob>=1,1,out_prob))
  
  
  infield_plays_summary %>% filter(hit_distance>=0) %>% ggplot(aes(x = hit_distance*sin(pull*pi/180),y = hit_distance*cos(pull*pi/180),color = out_prob,size=frac))+
    guides(size = "none")+
    scale_color_viridis_c(lim=c(0,1),oob = scales::squish)+
    labs(color = "GroundOut%")+
    theme_minimal()+
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          axis.ticks = element_blank())+
    xlab("")+
    ylab("")+
    coord_fixed()+
    geom_point()+
    annotate("segment",x=0,y=0,xend=-200,yend=200,color="black",size=1)+
    annotate("segment",x=0,y=0,xend=200,yend=200,color="black",size=1)+
    annotate("segment",x=0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
    annotate("segment",x=-0.5*sqrt(2)*90,y=0.5*sqrt(2)*90,xend=0,yend=sqrt(2)*90,color="black",size=1)+
    geom_line(data = arc_data,aes(x=x,y=y),color="black",size=1)+
    geom_point(data =  filter(infield_position_data_in,player_position <=6),aes(x = field_x,y=field_y,size=NULL),color="red",size=4)+
    theme(text = element_text(size = 20),
          axis.text = element_blank())
    
  
}

output_stats = function(){}



