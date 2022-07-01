# Test a new faster fitting method

# Once a position hasn't moved in a few iterations (3) consider it fixed
# and do not move it any more
# should speed up the evaluations that way




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
        field_x = if_else((m == "l")&(player_position==p), field_x - 10, field_x),
        field_x = if_else((m == "r")&(player_position==p), field_x + 10, field_x),
        field_y = if_else((m == "d")&(player_position==p), field_y - 10, field_y),
        field_y = if_else((m == "u")&(player_position==p), field_y + 10, field_y),
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
    
    
    # shinybusy::update_modal_spinner(text = paste0("Done Fitting Iteration ",iter_counter," (Max. 15)"))
  }
  return(player_position_counter)
}







# Now write the new functions



iterate_step_fast = function(player_position,batter_spray,batter_speed,infield_ability,positions_stable){
  
  movements = c("u","l","r","d") #u,l,d,r directions, n = unchanged
  
  temp_results = data.frame()
  
  move_positions = c(3,4,5,6,7,8,9)
  
  move_positions = move_positions[!move_positions %in% positions_stable]
  
  temp_results = bind_rows(temp_results,data.frame(
    player_position = c(3,4,5,6,7,8,9),
    movement_dir = "n",
    babip = fielding_model_function_variable(batter_spray,batter_speed,player_position,infield_ability,max_prob = TRUE)[3]
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
    print(iter_counter)
    player_position_new_temp = iterate_step_fast(player_position_new,batter_spray,batter_speed,infield_ability,positions_stable)
    if(all((arrange(player_position_new,player_position)$field_x==arrange(player_position_new_temp,player_position)$field_x)&
           (arrange(player_position_new,player_position)$field_y==arrange(player_position_new_temp,player_position)$field_y))){
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
    # shinybusy::update_modal_spinner(text = paste0("Done Fitting Iteration ",iter_counter," (Max. 15)"))
    # if(sum(duplicated(select(player_position_counter,-iter_counter)))>=1){
    #   return(player_position_counter)
    # }
  }
  return(player_position_counter)
}




# run the shiny app to get all the relevant data in the environment
library(shiny)
# just exit once it has loaded
runApp("Shift_Tester_App/")



player_position_data = data.frame(
  match = c(1,1,1,1,1,1,1),
  player_position = c(3,4,5,6,7,8,9),
  depth = c(110,151,118,145,298,322,294),
  angle = c(30,-1,-34,-15,-23,4,31),
  catch_factor = c(-1,-1,-1,-1,0,0,0)
)

batter_speed = 4300


player_position_data = player_position_data %>% mutate(field_x = depth * sin(angle * pi / 180),
                                                       field_y = depth * cos(angle * pi / 180))

infield_ability = data.frame(
  player_position = c(3,4,5,6),
  range_factor = c(-0.5,0,0,0), # extra ability of reaching a groundball
  transfer_time = c(1.3,1.1,1.3,1.1),
  throw_speed = c(80,90,110,105)
)

time1 = timeDate::timeDate()
test1 = multiple_iterations(player_position_data,batter_spray,batter_speed,25)
time2 = timeDate::timeDate()
print(time2 - time1)




time3 = timeDate::timeDate()
test2 = multiple_iterations_fast(player_position_data,batter_spray,batter_speed,25)
time4 = timeDate::timeDate()
print(time4 - time3)


print(as.numeric(time4-time3)/as.numeric(60*(time2-time1)))
# Speed up of 15 seconds (over 30%!)




# Now try with a really weird hitting distribution (further for fielders to move):

batter_spray_new = find_batter_spray(batter_spray,95,28,0.8,0.7,"L",spray_chart_tabulate)


time1 = timeDate::timeDate()
test1 = multiple_iterations(player_position_data,batter_spray_new,batter_speed,25)
time2 = timeDate::timeDate()
print(time2 - time1)




time3 = timeDate::timeDate()
test2 = multiple_iterations_fast(player_position_data,batter_spray_new,batter_speed,25)
time4 = timeDate::timeDate()
print(time4 - time3)


print(as.numeric((time4-time3))/as.numeric((time2-time1)))

# Almost 50% speedup!

test1 %>% filter(iter_counter==max(iter_counter)) %>% ggplot(aes(x = field_x, y = field_y))+geom_point()+
  xlim(-200,200)+ylim(0,400)

test2 %>% filter(iter_counter==max(iter_counter)) %>% ggplot(aes(x = field_x, y = field_y))+geom_point()+
  xlim(-200,200)+ylim(0,400)

# Slightly different positions but not a huge change given the extra speedup









test1 = fielding_model_all_info_function(batter_spray,batter_speed,player_position_data,infield_ability,FALSE)
test1$BABIP
test1$`GO%`
test1$`FO%`

test1$Frac_Under_10_Degrees
test1$Frac_Over_10_Degrees

test1$infield_summary
test1$catch_summary

sum(test1$infield_summary$fielded)
sum(test1$infield_summary$out_made)
sum(test1$infield_summary$infield_hit)
sum(test1$catch_summary$catch)
