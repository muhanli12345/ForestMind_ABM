####### FUNCTION MASTERFILE ###########
###### LAST UPDATED : 25/05/2024 #######

# required libraries

# install.packages("msm")
library(msm)

#### ============ INDEX

# 1. UPDATING AGENT LOCATION USING SOFTMAX RULE FOR CHOOSING MOVEMENT STRATEGIES
# 2. AVATAR MOVEMENT FUNCTION (1 STEP, GENERAL)
# 3. TARGET-BASED MOVEMENT
# 4. AVATAR-BASED MOVEMENT
# 5. RANDOM MOVEMENT
# 6. UPDATING TREE VALUE AND CHOICE PROBABILITIES FOR SOFTMAX LEARNING FUNCTION
# 7. UPDATING AVATAR INFORMATION FOR SOFTMAX LEARNING FUNCTION
# 8. UPDATING MOVEMENT STRATEGY VALUE AND CHOICE PROBABILITIES
# 9. CALCULATING AVERAGE SPEED FOR A TRACK
# 10. CALCULATING WHETHER A POINT IS WITHIN THE FIELD OF VIEW
# 11. VECTOR NORMALIZATION

# ============================================================ #
# ============================================================ #
# ============================================================ #

# 1. UPDATING AGENT LOCATION USING SOFTMAX RULE FOR CHOOSING MOVEMENT STRATEGIES ----

update_agent_location_softmax <- function(agent, agent_track, agent_track_trial, in_trial = in_trial, target = trees, avatar, prob_switch, step_length, grid, initial_choice_probs, strategy_values, beta){
  
  # This function governs agent decision making in the learning model. It works both when the agent is in trial or not in trial.
  # The decision-making in this function is based on a simple version of the softmax function.
  # It takes as arguments the following variables:
  # agent: dataset containing agent information
  # agent_track: cumulative agent track dataset for all simulations
  # agent_track_trial: cumulative agent track data the current trial
  # in_trial: variable stating whether the agent is in trial or not (in trial = 1; not in trial = 0)
  # target: dataset containing tree information
  # avatar: dataset containing avatar information
  # prob_switch: probability of switching from current movement strategy to another
  # step_length: agent's step length
  # grid: dataset containing grid boundaries
  # initial_choice_probs: vector containing probabilities of choosing each movement strategy at the start of a simulation
  # strategy_values: dataset containing strategy values and choice probabilities
  # beta: variable that determines how much preference will be given to choices with higher values (the higher, the more likely agent is to choose highly-valued probabilities)
  
  # get current agent x and y locations
  current_agent_x <- as.numeric(agent[,"x"])
  current_agent_y <- as.numeric(agent[,"y"])
  current_agent_x_vel <- as.numeric(agent[,"x_vel"])
  current_agent_y_vel <- as.numeric(agent[,"y_vel"])
  
  # get choice probabilities for each movement strategy
  strat_probs <- as.vector(strategy_values$choice_prob) # define probability vector
  
  ### IF IN TRIAL
  # record distance to closest knowledgeable avatar
  knowledgeable_avatars <- avatar[which(avatar$avt_type == "knowledgeable"),]
  smallest_dist <- knowledgeable_avatars[which(knowledgeable_avatars$dist_to_agent == min(knowledgeable_avatars$dist_to_agent)),]
  agent[,"dist_to_knowledgeable"] <- as.numeric(smallest_dist$dist_to_agent)
  
  # Defining movement mode probabilities in case no avatars in sight 
  if(nrow(avatar[which(avatar$in_sight == 1),]) == 0){ # if no avatars in sight
    # choice of following avatar is reduced to 0 and distributed evenly across the other 2 strategies
    initial_choice_probs <- c((initial_choice_probs[1]+initial_choice_probs[2]/2),(initial_choice_probs[2]+initial_choice_probs[1]/2))
    # strategy choice probability 
    strat_probs <- c((strat_probs[1]+strat_probs[2]/2),(strat_probs[2]+strat_probs[1]/2))
  } # end of "in trial" condition
  
  ### MOVEMENT STRATEGY CHOICE
  if(nrow(agent_track_trial) == 0){ # if on the first step
    # choose movement mode based on initial probabilities
    mov_strategy <- sample(size = 1, x = c("avatar", "random"), prob = initial_choice_probs)
  }else{
    if(ps_number == 0){ # if on the first trial
      if((agent_track_trial[nrow(agent_track_trial),"movement_mode"] == "random" &&
          !is.null(any(trees_visited)) &&
          !is.na(any(trees_visited)))|
         (agent_track_trial[nrow(agent_track_trial),"movement_mode"] == "avatar" &&
          any(as.numeric(unlist(strsplit(
            as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
            , split = ","))) %in% 
            trees[trees$fruiting == 1,]$target_id))){
        # choose movement mode based on initial probabilities
        mov_strategy <- sample(size = 1, x = c("avatar", "random"), prob = initial_choice_probs)
      }else{
        mov_strategy <- agent_track_trial[nrow(agent_track_trial),"movement_mode"]
      }
      
    }else{ # if not on the first trial
      if((agent_track_trial[nrow(agent_track_trial),"movement_mode"] == "random" &&
          !is.null(any(trees_visited)) &&
          !is.na(any(trees_visited)))|
         (agent_track_trial[nrow(agent_track_trial),"movement_mode"] == "avatar" &&
          any(as.numeric(unlist(strsplit(
            as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
            , split = ","))) %in% 
            trees[trees$fruiting == 1,]$target_id))){
        switch <- runif(1) # sample a number from 0 to 1
        if(switch < prob_switch){ # if it falls within the criteria for switching strategies
          if(ps_number < 1 | any(is.na(strategy_values$choice_prob))){
            # choose movement mode based on initial probabilities in the first trial
            mov_strategy <- sample(size = 1, x = c("avatar", "random"), prob = initial_choice_probs)
          }else{ # choose movement mode based on updated strategy probabilities in the following trials
            mov_strategy <- sample(size = 1, x = c("avatar", "random"), prob = strat_probs)
          }
        }else{ # if number does not fall within criteria to switch strategies
          mov_strategy <- agent_track_trial[nrow(agent_track_trial),"movement_mode"] # stick to last movement mode
        }}else{ # if logic does not fall within criteria to switch strategies
          mov_strategy <- agent_track_trial[nrow(agent_track_trial),"movement_mode"]
        }
    }
  }
  
  #Updating the move strategy if the risk of following is higher than self-exploring
  if(nrow(trees[trees$in_sight == 1,]) > 0){ # make sure the landmark in sight is more than 1
    closest_landmark_id <- trees[trees$in_sight == 1 &
                                   trees$dist_to_agent == min(trees[trees$in_sight == 1,]$dist_to_agent),]$target_id
    if(trees[trees$target_id == closest_landmark_id, ]$reward > 0 &&
       trees[trees$target_id == closest_landmark_id, ]$reward > 
       1 - strategy_values[strategy_values$strategy == "avatar","risk"]){
      mov_strategy <- sample(size = 1, x = c("avatar", "random"), prob = strat_probs)
    }
  }
  
  ### CHOOSE RANDOM MOVE WHEN THE AVATAR IS MISSING
  if(is.na(avatars[,'x']) | (avatars$in_sight == 0 & 
     (agent$dist_to_knowledgeable > 40 | is.na(agent$dist_to_knowledgeable)))){
    mov_strategy <- "random"
  }
  
  ### AVATAR MOVEMENT
  
  if(mov_strategy == "avatar"){
    # choose avatar to follow based on choice prob
    avatar_sampled <- sample(size = 1, x = 1:nrow(avatar))
    avatar_chosen <- avatar[avatar_sampled,]
    # move towards chosen avatar
    agent_update <- avt_move(
      current_agent_x = current_agent_x, 
      current_agent_y = current_agent_y, 
      current_avt_x = as.numeric(avatar_chosen[,"x"]), 
      current_avt_y = as.numeric(avatar_chosen[,"y"]), 
      avt_type = avatar_chosen[,"avt_type"], 
      step_length = step_length)
  } # end of avatar movement
  
  ### RANDOM MOVEMENT
  
  if(mov_strategy == "random"){
    # get value for each random movement mode
    mov_type <- "FAM" # sample with pre-defined probabilities
    # move using selected strategy
    agent_update <- random_move(
      random_mov_type = mov_type, 
      current_agent_x = current_agent_x,
      current_agent_y = current_agent_y,
      current_agent_x_vel = current_agent_x_vel, 
      current_agent_y_vel = current_agent_y_vel, 
      step_length = step_length, 
      landmarks_df = trees,
      agent_state = agent_state,
      FAM_memory_vectors_x = FAM_memory_vectors_x,
      FAM_memory_vectors_y = FAM_memory_vectors_y,
      field_of_view = fov)
  } # end of random movement
  
  # check if there's a rock
  for (i in 1:nrow(rocks)) {
    dist <- sqrt((agent_update$x - rocks[i, "x"])^2 + (agent_update$y - rocks[i, "y"])^2)
    if (dist < 1.5) { # collide radius
      # turn
      agent_update <- random_move(random_mov_type = "turn", current_agent_x, current_agent_y,
                                  current_agent_x_vel, current_agent_y_vel, step_length, field_of_view = fov,
                                  landmarks_df = trees,
                                  FAM_memory_vectors_x = FAM_memory_vectors_x,
                                  FAM_memory_vectors_y = FAM_memory_vectors_y,
                                  agent_state = agent_state)
      # update new position and velocity
      agent[,"x"] <- agent_update[,"x"]
      agent[,"y"] <- agent_update[,"y"]
      agent[,"x_vel"] <- agent_update[,"x_vel"]
      agent[,"y_vel"] <- agent_update[,"y_vel"]
      agent[,"movement_mode"] <- agent_update[,"movement_mode"]
      agent[,"movement_mode_mod"] <- agent_update[,"movement_mode_mod"]
    }
  }
  
  # new location 
  if(agent_update[,"x"] < grid$x_max &
     agent_update[,"y"] < grid$y_max &
     agent_update[,"x"] > grid$x_min &
     agent_update[,"y"] > grid$y_min){ # if new x and y positions are within grid
    agent[,"x"] <- agent_update[,"x"]
    agent[,"y"] <- agent_update[,"y"]
    agent[,"x_vel"] <- agent_update[,"x_vel"]
    agent[,"y_vel"] <- agent_update[,"y_vel"]
    agent[,"movement_mode"] <- agent_update[,"movement_mode"]
    agent[,"movement_mode_mod"] <- agent_update[,"movement_mode_mod"]
  }else{
    # turn
    agent_update <- random_move(random_mov_type = "turn", current_agent_x, current_agent_y,
                                current_agent_x_vel, current_agent_y_vel, step_length, field_of_view = fov,
                                landmarks_df = trees,
                                FAM_memory_vectors_x = FAM_memory_vectors_x,
                                FAM_memory_vectors_y = FAM_memory_vectors_y,
                                agent_state = agent_state)
    # update new position and velocity
    agent[,"x"] <- agent_update[,"x"]
    agent[,"y"] <- agent_update[,"y"]
    agent[,"x_vel"] <- agent_update[,"x_vel"]
    agent[,"y_vel"] <- agent_update[,"y_vel"]
    agent[,"movement_mode"] <- agent_update[,"movement_mode"]
    agent[,"movement_mode_mod"] <- agent_update[,"movement_mode_mod"]
  }
  
  return(agent)
} # end of function

# ============================================================ #
# ============================================================ #
# ============================================================ #

# 2. AVATAR MOVEMENT FUNCTION (1 STEP) ----

update_avatar_location <- function(avatar = avatars, step_length){
  
  # This function provides the rules governing avatar movement
  # (both knowledgeable and ignorant). It takes as arguments:
  # 1. avatar: dataset containing the avatar information
  # 2. step_length: avatar step length (in m); this is the same as the agent's
  
  # get currrent avatar position
  current_avt_x <- avatar[,"x"]
  current_avt_y <- avatar[,"y"]
  # get target position
  target_x <- avatar[,"target_x"]
  target_y <- avatar[,"target_y"]
  # Calculate the vector from avatar to target
  avt_to_target_x <- target_x - current_avt_x
  avt_to_target_y <- target_y - current_avt_y
  # find the unit vector pointing towards the target
  vector_magnitude <- sqrt(avt_to_target_x^2 + avt_to_target_y^2)
  unit_vector_x <- avt_to_target_x / vector_magnitude
  unit_vector_y <- avt_to_target_y / vector_magnitude
  # Calculate the new position according to direction and step length
  new_x <- current_avt_x + step_length*unit_vector_x
  new_y <- current_avt_y + step_length*unit_vector_y
  # new location
  avatar[,"x"] <- new_x
  avatar[,"y"] <- new_y
  
  return(avatar)
}

# ============================================================ #
# ============================================================ #
# ============================================================ #

# 3. AVATAR-BASED MOVEMENT ----

avt_move <- function(current_agent_x, current_agent_y, current_avt_x, current_avt_y, avt_type, step_length){
  
  # This function updates the agent's location when moving towards a selected (target) avatar.
  # It takes as arguments the following variables:
  # 1. current_agent_x: agent's current x location on the grid
  # 2. current_agent_y: agent's current y location on the grid  
  # 3. current_avt_x: target avatar x location
  # 4. current_avt_y: target avatar y location
  # 5. avt_type: target avatar type
  # 6. step_length: agent's step length 
  
  # Calculate the vector from agent to avatar
  agent_to_avatar_x <- current_avt_x - current_agent_x
  agent_to_avatar_y <- current_avt_y - current_agent_y
  
  # find the unit vector pointing towards the avatar
  vector_magnitude <- sqrt(agent_to_avatar_x^2 + agent_to_avatar_y^2)
  if(vector_magnitude != 0){
    unit_vector_x <- agent_to_avatar_x / vector_magnitude
    unit_vector_y <- agent_to_avatar_y / vector_magnitude
    # Calculate the new position according to direction and step length
    new_x <- current_agent_x + step_length*unit_vector_x
    new_y <- current_agent_y + step_length*unit_vector_y
    # calculate new velocity vector
    new_x_vel <- step_length*unit_vector_x
    new_y_vel <- step_length*unit_vector_y
  }else{
    new_x <- current_agent_x 
    new_y <- current_agent_y
    new_x_vel <- 0
    new_y_vel <- 0
  }
  
  # dataframe with updated agent info
  agent_update <- data.frame(
    "x" = new_x,
    "y" = new_y,
    "x_vel" = new_x_vel,
    "y_vel" = new_y_vel,
    "movement_mode" = "avatar",
    "movement_mode_mod" = avt_type)
  return(agent_update) 
}

# ============================================================ #
# ============================================================ #
# ============================================================ #

# 4. RANDOM MOVEMENT ----

random_move <- function(random_mov_type = "FAM", current_agent_x, current_agent_y,
                        current_agent_x_vel, current_agent_y_vel, step_length,
                        field_of_view = fov, visual_weight = 0.3,
                        landmarks_df, # Pass current landmark data
                        FAM_memory_vectors_x, FAM_memory_vectors_y,
                        agent_state # Pass agent's current learning state
) {
  
  if (random_mov_type == "FAM") {
    N_landmarks <- nrow(landmarks_df)
    nav_vec_x <- 0
    nav_vec_y <- 0
    
    # --- Get Navigation Vector from FAM (Memory Vectors) ---
    # If agent is at a known landmark, try to find a good next step from FAM
    current_loc_id <- agent_state$current_landmark_id
    if (!is.na(current_loc_id) && N_landmarks > 0 && nrow(trees[trees$in_sight == 1,]) > 0) {
      best_next_landmark_pool <- trees[trees$in_sight == 1 &
                                       trees$reward == max(trees[trees$in_sight == 1,]$reward),]
      
      best_next_landmark_id <- best_next_landmark_pool[best_next_landmark_pool$dist_to_agent == min(best_next_landmark_pool$dist_to_agent), ]$target_id
      
      max_expected_value <- -Inf
      
      if (!is.na(best_next_landmark_id)) {
        nav_vec_x <- FAM_memory_vectors_x[current_loc_id, best_next_landmark_id]
        nav_vec_y <- FAM_memory_vectors_y[current_loc_id, best_next_landmark_id]
        # Normalize
        norm_factor <- sqrt(nav_vec_x^2 + nav_vec_y^2)
        if (norm_factor > 0) {
          nav_vec_x <- nav_vec_x / norm_factor
          nav_vec_y <- nav_vec_y / norm_factor
        }
      }
    }
    memory_vec <- c(nav_vec_x, nav_vec_y)
    
    # if memory_vec equals to 0, fallback to direction perturbation
    if (all(memory_vec == 0)) {
      # add direction perturbation to the movement
      theta <- runif(1, -field_of_view/2, field_of_view/2)
      x_rot <- current_agent_x_vel * cos(theta) - current_agent_y_vel * sin(theta)
      y_rot <- current_agent_x_vel * sin(theta) + current_agent_y_vel * cos(theta)
      memory_vec <- vector_norm(x_rot, y_rot)
    }
    
    # --- Get Visual Gradient Vector (towards salient/eligible landmarks) ---
    # It used eligibility and associative reward.
    # For simplicity, I'm roughly sketching its integration.
    visual_gradient_x <- 0
    visual_gradient_y <- 0
    if (N_landmarks > 0) {
      for (i in 1:N_landmarks) {
        if (landmarks_df[i, "in_sight"] == 1) {
          dx <- landmarks_df$x[i] - current_agent_x
          dy <- landmarks_df$y[i] - current_agent_y
          dist_sq <- dx^2 + dy^2
          if (dist_sq == 0) dist_sq <- 0.01 # Avoid division by zero
          
          for (j in 1:nrow(fruit_trees)) {
            fx <- landmarks_df$x[i] - fruit_trees$x[j]
            fy <- landmarks_df$y[i] - fruit_trees$y[j]
            d_fruit <- fx^2 + fy^2
            if(d_fruit < 35 && fruit_trees$target_id[j] %in% tree_unvisit){
              # Weight by eligibility trace and reward from that landmark
              weight <- (landmarks_df$eligibility[i] + landmarks_df$reward[i]) / dist_sq
              
            }else{ # Reverse the weight if the fruit tree was visited during current game
              weight <- 1 - (landmarks_df$eligibility[i] + landmarks_df$reward[i]) / dist_sq
            }
          }
          
          norm_dist <- sqrt(dist_sq)
          visual_gradient_x <- visual_gradient_x + weight * (dx / norm_dist)
          visual_gradient_y <- visual_gradient_y + weight * (dy / norm_dist)
        }
      }
    }
    
    norm_factor_vg <- sqrt(visual_gradient_x^2 + visual_gradient_y^2)
    if (norm_factor_vg > 0) {
      visual_vec <- c(visual_gradient_x / norm_factor_vg, visual_gradient_y / norm_factor_vg)
    } else {
      visual_vec <- c(0, 0)
    }
    
    # --- Combine Vectors & Move ---
    if (all(memory_vec == 0) && all(visual_vec == 0)) { # If no guidance, use perturbation
      theta <- runif(1, -field_of_view / 2, field_of_view / 2)
      # Normalize current velocity to get direction, or use a default if vel is zero
      norm_vel <- sqrt(current_agent_x_vel^2 + current_agent_y_vel^2)
      dir_x_vel <- ifelse(norm_vel > 0, current_agent_x_vel / norm_vel, 1)
      dir_y_vel <- ifelse(norm_vel > 0, current_agent_y_vel / norm_vel, 0)
      
      final_dir_x <- dir_x_vel * cos(theta) - dir_y_vel * sin(theta)
      final_dir_y <- dir_x_vel * sin(theta) + dir_y_vel * cos(theta)
    } else {
      # Combine memory (goal-directed from FAM) and visual (immediate stimuli)
      comb_x <- (1 - visual_weight) * memory_vec[1] + visual_weight * visual_vec[1]
      comb_y <- (1 - visual_weight) * memory_vec[2] + visual_weight * visual_vec[2]
      norm_comb <- sqrt(comb_x^2 + comb_y^2)
      
      if (norm_comb > 0) {
        final_dir_x <- comb_x / norm_comb
        final_dir_y <- comb_y / norm_comb
      } else { # Fallback if combination results in zero vector (e.g. they perfectly cancel)
        final_dir_x <- memory_vec[1] + visual_vec[1] # simple sum, then re-normalize or perturb
        final_dir_y <- memory_vec[2] + visual_vec[2]
        re_norm <- sqrt(final_dir_x^2 + final_dir_y^2)
        if(re_norm > 0) {
          final_dir_x <- final_dir_x / re_norm
          final_dir_y <- final_dir_y / re_norm
        } else { # Ultimate fallback: random direction
          theta_rand <- runif(1, 0, 2*pi)
          final_dir_x <- cos(theta_rand)
          final_dir_y <- sin(theta_rand)
        }
        
      }
      
    }
    
    new_x_vel <- step_length * final_dir_x
    new_y_vel <- step_length * final_dir_y
    new_x <- current_agent_x + new_x_vel
    new_y <- current_agent_y + new_y_vel
    
    agent_update <- data.frame(
      x = new_x, y = new_y, x_vel = new_x_vel, y_vel = new_y_vel,
      movement_mode = "random", movement_mode_mod = "FAM"
    )
    
  } 
  
  if (random_mov_type == "turn") {
    # Generate an angle from a normal distribution centered at 180 degrees
    theta <- rtnorm(n = 1, mean = pi, sd = field_of_view/5, lower = pi-(field_of_view/2), upper = pi+field_of_view/2) # Assuming rtnorm exists
    # Perform velocity vector rotation around its origin
    x_vel_rotate <- current_agent_x_vel*cos(theta) - current_agent_y_vel*sin(theta)
    y_vel_rotate <- current_agent_x_vel*sin(theta) + current_agent_y_vel*cos(theta)
    # Normalize rotated vector
    norm_factor_rot <- sqrt(x_vel_rotate^2 + y_vel_rotate^2)
    norm_x_vel <- ifelse(norm_factor_rot > 0, x_vel_rotate / norm_factor_rot, 0)
    norm_y_vel <- ifelse(norm_factor_rot > 0, y_vel_rotate / norm_factor_rot, 0)
    
    # Record new x and y components of the velocity vector
    new_x_vel <- norm_x_vel*step_length # Agent does not change location, only heading for next step
    new_y_vel <- norm_y_vel*step_length
    # Dataframe with updated agent info
    agent_update <- data.frame(
      "x" = current_agent_x, "y" = current_agent_y,
      "x_vel" = new_x_vel, "y_vel" = new_y_vel,
      "movement_mode" = "random", "movement_mode_mod" = "turn")
    
  }
  
  return(agent_update)
}

# 5. UPDATING LANDMARKS INFORMATION AND FEATURE ASSOCIATION MATRIX ----

update_landmark_and_fam_simple <- function(agent, trees,
                                                agent_state,  FAM_memory_vectors_x, 
                                                FAM_memory_vectors_y) {
  
  # --- Part 1: Update Landmark Properties & Stimulus Intensity (S_i) ---
  current_agent_x <- as.numeric(agent[, "x"])
  current_agent_y <- as.numeric(agent[, "y"])
  agent_speed <- sqrt(as.numeric(agent[, "x_vel"])^2 + as.numeric(agent[, "y_vel"])^2)
  
  for (i in 1:nrow(trees)) {
    current_landmark_x <- as.numeric(trees[i, "x"])
    current_landmark_y <- as.numeric(trees[i, "y"])
    dist_current_landmark <- sqrt((current_agent_x - current_landmark_x)^2 + (current_agent_y - current_landmark_y)^2)
    trees[i, "dist_to_agent"] <- dist_current_landmark
    
    in_dd <- dist_current_landmark <= agent[, "dd"]
    in_fov_val <- is_in_fov(agent, object = trees[i, ]) # Assuming is_in_fov exists
    
    trees[i, "in_dd"] <- as.integer(in_dd)
    trees[i, "in_fov"] <- as.integer(in_fov_val)
    
    # record the visual situation of each 
    if((as.numeric(trees[i,"in_dd"]) + as.numeric(trees[i,"in_fov"])) == 2){
      trees[i, "in_sight"] <- 1 # mark as in sight
    }else{
      trees[i, "in_sight"] <- 0 # mark as not in sight
    }
  } # end of "for each tree" loop
  
  # --- Part 2: Update Eligibility Traces (E_i) based on Eq. 1 ---
  for(i in 1:nrow(trees)){
    # Update eligibility trace based on FAM model
    if (trees[i, "in_sight"] == 1) {
      trees[i, "time_insight"] <- trees[i, "time_insight"] + 1
      
      if(trees[i,'pv'] > 0){
        trees[i, "sensory"] <- as.numeric(trees[i, "time_insight"])*(as.numeric(trees[i, "is_block"]) + 1)*as.numeric(trees[i, "color"])*as.numeric(trees[i, "size"])
      }else{
        trees[i, "sensory"] <- as.numeric(trees[i, "time_insight"])*as.numeric(trees[i,"color"])*as.numeric(trees[i, "size"])
      }
      
      trees[i, "eligibility"] <- k_E + (1 - k_E) * (1 / (1 + exp(a_E * trees[i, "sensory"])))
      
    } else {# Calculate E_Decay
      total_S <- sum(trees[,"in_sight"])  # Sum of sense input
      E_Decay <- exp(1-total_S)
      trees[i, "eligibility"] <- trees[i, "eligibility"] * E_Decay  # trace decay based on the FAM model
    }
  }
  
  # --- Part 3: Update FAM - Strength_ij and Order_ij ---
  
  # calculate the association if there's fruit found
  
  if (exists('visited_fruit') && nrow(visited_fruit) > 0) {
    
    for (i in 1:nrow(trees)) {
      d_fruit <- sqrt((trees[i, "x"] - visited_fruit[, "x"])^2 + 
                        (trees[i, "y"] - visited_fruit[, "y"])^2)
      
      if(d_fruit < 35){
        C <- k_c*(as.numeric(trees[i, "eligibility"])*as.numeric(visited_fruit[, "eligibility"]) - E_threshold
              + k_a*trees[i, "reward"])*(trees[i, "eligibility"] + visited_fruit[, "eligibility"]) +
          k_d*(trees[i, "association"] - trees[i, "coccurrence"])
        trees[i, "coccurrence"] <- trees[i, "coccurrence"] + C
        trees[i, "association"] <- 1 /(1 + exp(a_s*trees[i, "coccurrence"]))
      }
      
    }
    
  }
  
  # --- Part 4: Path Integration & Memory Vector Assignment ---
  # This part needs to be triggered when the agent is considered to have "arrived" at a new landmark.
  # agent_state$current_landmark_id is updated when agent is very close to a landmark.
  # agent_state$last_landmark_id holds the previously visited one.
  
  # Logic to determine if agent arrived at a NEW distinct landmark:
  newly_arrived_at_landmark_id <- NA
  min_dist_to_arrived_landmark <- 0.25 # Example threshold for "arrival"
  # Find closest landmark if agent is very close to any
  if(any(trees$dist_to_agent < min_dist_to_arrived_landmark)){
    closest_landmark_idx <- which.min(trees$dist_to_agent)
    if(trees$dist_to_agent[closest_landmark_idx] < min_dist_to_arrived_landmark){
      if(is.na(agent_state$current_landmark_id) || closest_landmark_idx != agent_state$current_landmark_id){
        newly_arrived_at_landmark_id <- closest_landmark_idx
      }
    }
  }
  
  
  if (!is.na(newly_arrived_at_landmark_id)) {
    if (!is.na(agent_state$current_landmark_id) && agent_state$current_landmark_id != newly_arrived_at_landmark_id) {
      # Agent moved from 'agent_state$current_landmark_id' to 'newly_arrived_at_landmark_id'
      from_id <- agent_state$current_landmark_id
      to_id <- newly_arrived_at_landmark_id
      
      # Store accumulated path vector as memory vector for this pair (L_old -> L_new)
      if (agent_state$path_integrator_distance > 0) { # Only if actually moved
        FAM_memory_vectors_x[from_id, to_id] <- agent_state$path_integrator_vector_x
        FAM_memory_vectors_y[from_id, to_id] <- agent_state$path_integrator_vector_y
        # Could also store for (L_new -> L_old) with inverted vector if appropriate for your model
      }

      agent_state$last_landmark_id <- from_id # Update last distinct landmark
  }
  
    agent_state$current_landmark_id <- newly_arrived_at_landmark_id
    # Reset path integrator for the next leg
    agent_state$path_integrator_distance <- 0
    agent_state$path_integrator_vector_x <- 0
    agent_state$path_integrator_vector_y <- 0
    
  } else {
    # Agent is moving between landmarks (or not near any)
    # Accumulate path integration vector: sum of heading vectors
    agent_state$path_integrator_distance <- agent_state$path_integrator_distance + sqrt(agent[,"x_vel"]^2 + agent[,"y_vel"]^2)
    agent_state$path_integrator_vector_x <- agent_state$path_integrator_vector_x + agent[,"x_vel"]
    agent_state$path_integrator_vector_y <- agent_state$path_integrator_vector_y + agent[,"y_vel"]
  }
  
  return(list(trees = trees,
              FAM_memory_vectors_x = FAM_memory_vectors_x, FAM_memory_vectors_y = FAM_memory_vectors_y,
              agent_state = agent_state))
}

# 6. UPDATING AVATAR INFORMATION FOR SOFTMAX LEARNING FUNCTION ----

update_avatar_information <- function(agent, avatar){
  
  # This function updates avatar data to inform decision making when applying the softmax function.
  # It first calculates the value attributed to each avatar and subsequently, based on that, it calculates
  # the probability of the agent selecting any of the avatars as targets to move toward.
  # It takes as arguments the following variables:
  # 1. agent: dataset containing agent information
  # 2. avatar: dataset containing avatar information
  
  # calculate and store whether any avatars are in sight
  for(i in 1:nrow(avatar)){
    # get agent x and y locations 
    current_agent_x <- as.numeric(agent[,"x"])
    current_agent_y <- as.numeric(agent[,"y"])
    # get avatar x and y locations
    current_avt_x <- as.numeric(avatar[i,"x"])
    current_avt_y <- as.numeric(avatar[i,"y"])
    # get distance to avatar
    dist_avatar <- sqrt((current_agent_x-current_avt_x)^2 + (current_agent_y-current_avt_y)^2)
    # record distance between avatar and agent
    avatar[i, "dist_to_agent"] <- dist_avatar
    # check whether in dd and fov
    if(dist_avatar <= agent[,"dd"]){
      avatar[i,"in_dd"] <- 1 # mark as within detection distance
    }
    if(is_in_fov(agent, object = avatar[i,])){
      avatar[i,"in_fov"] <- 1 # mark as within field of view
    }
    if(avatar[i,"in_dd"] == 1 & avatar[i,"in_fov"] == 1){
      avatar[i, "in_sight"] <- 1 # mark as in sight
    }else{
      avatar[i, "in_sight"] <- 0 # mark as not in sight
    }
  }
  
  return(avatar)
} # end of function

# ============================================================ #
# ============================================================ #
# ============================================================ #

# 7. UPDATING MOVEMENT STRATEGY VALUE AND CHOICE PROBABILITIES ----

update_strategy_information_peirs <- function(strategy_values, ps_memory, agent){
  # This function updates the values of movement strategies based on the agent's previous experience,
  # then it calculates the probability of the agent choosing each movement strategy.
  # It takes as arguments the following variables:
  # 1. strategy_values : dataset containing the values and choice probabilities of each movement strategy
  # 2. ps_memory : dataset containing information on the agent's previous successes
  # if no previous memory
  if(ps_number == 0){ # if there are no previous successes
    strategy_values$value <- 1
  }
  
  # if no previous successes
  if(ps_number == 0){
    strategy_values$value <- 1
  }
  
  #calculate the cognitive load
  if(nrow(agent_target_pool) > 0 && max(agent_target_pool$reward) != 0){
    agent_target_pool$P_i <- agent_target_pool$reward/sum(trees$reward)
    intrinsic_load <- -sum(agent_target_pool$P_i * log2(as.numeric(agent_target_pool$P_i)), na.rm = TRUE)
  }else{
    if(nrow(trees[trees$in_sight == 1,]) > 0){
    intrinsic_load <- log2(nrow(trees[trees$in_sight == 1,]))
    }else{ # choose all landmarks in detection range if no item in sight
      intrinsic_load <- log2(nrow(trees[trees$in_dd == 1,]))
    }
  }
  
  strategy_values[strategy_values$strategy == "random","cognitive_load"] <- intrinsic_load
  
  # if previous successes are present
  if(sum(ps_memory$trial_successful) > 0){ # if there are previous successes
    # update the prior bayesian success probability of each move strategy
    successful_trials <- ps_memory[which(ps_memory$trial_successful == 1),]
    fail_trials <- ps_memory[which(ps_memory$trial_successful == 0),]
    
    # update the value for success and failure (with a discounting factor lambda)
    strategy_values[strategy_values$strategy == "avatar", "alpha"] <- 
      lambda * strategy_values[strategy_values$strategy == "avatar", "alpha"] +
      successful_trials[nrow(successful_trials), "n_choices_knowledgeable"]
    
    strategy_values[strategy_values$strategy == "random", "alpha"] <- 
      lambda * strategy_values[strategy_values$strategy == "random", "alpha"] +
      successful_trials[nrow(successful_trials), "n_choices_random_normal"]
    
    if(nrow(fail_trials) > 0){
      strategy_values[strategy_values$strategy == "avatar", "beta"] <- 
        lambda * strategy_values[strategy_values$strategy == "avatar", "beta"] + 
        fail_trials[nrow(fail_trials), "n_choices_knowledgeable"]
      
      strategy_values[strategy_values$strategy == "random", "beta"] <- 
        lambda * strategy_values[strategy_values$strategy == "random", "beta"] + 
        fail_trials[nrow(fail_trials), "n_choices_random_normal"]
    }else{
      strategy_values[strategy_values$strategy == "avatar", "beta"] <- 
        lambda * strategy_values[strategy_values$strategy == "avatar", "beta"]
      
      strategy_values[strategy_values$strategy == "random", "beta"] <- 
        lambda * strategy_values[strategy_values$strategy == "random", "beta"]
    }
    
    # update strategy value with bayesian rule
    strategy_values$bayesian_prob <- strategy_values$alpha/(strategy_values$alpha + strategy_values$beta)
    if(ps_number > 0){
      strategy_values$bayesian_prob <- strategy_values$alpha/(strategy_values$alpha + strategy_values$beta)
    }else{
      strategy_values[strategy_values$strategy == "avatar", "bayesian_prob"] <- initial_choice_probs[1] * strategy_values[strategy_values$strategy == "avatar", "bayesian_prob"] / 
        (initial_choice_probs[1] * strategy_values[strategy_values$strategy == "avatar", "bayesian_prob"] + 
           (1 - initial_choice_probs[1]) * (1 - strategy_values[strategy_values$strategy == "avatar", "bayesian_prob"]))
      strategy_values[strategy_values$strategy == "random", "bayesian_prob"] <- initial_choice_probs[2] * strategy_values[strategy_values$strategy == "random", "bayesian_prob"] / 
        (initial_choice_probs[2] * strategy_values[strategy_values$strategy == "random", "bayesian_prob"] + 
           (1 - initial_choice_probs[2]) * (1 - strategy_values[strategy_values$strategy == "random", "bayesian_prob"]))
    }
  }
  
  # calculate the value of landmark-targeted move
  if(agent_track_trial[nrow(agent_track_trial), "movement_mode"] == 'random' &&
     (any(as.numeric(unlist(strsplit(
       as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
       , split = ","))) %in% 
       trees[trees$fruiting == 1,]$target_id)|!is.null(any(trees_visited)) &&
      !is.na(any(trees_visited)))){
    strategy_values[which(strategy_values$strategy == "random"), "value"] <- 
      strategy_values[which(strategy_values$strategy == "random"), "value"] + 
      ks*(reward - strategy_values[which(strategy_values$strategy == "random"), "value"])*
      strategy_values[strategy_values$strategy == "random","bayesian_prob"]
  }
  
  # calculate the value of avatar-targeted move
  if(any(as.numeric(unlist(strsplit(
    as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
    , split = ","))) %in% 
    trees[trees$fruiting == 1,]$target_id) &&
    agent_track_trial[nrow(agent_track_trial), 'trees_visited'] %in% 
    avatar_track_trial[nrow(avatar_track_trial),'trees_visited']){
    strategy_values[which(strategy_values$strategy == "avatar"), "value"] <- 
      strategy_values[which(strategy_values$strategy == "avatar"), "value"] + 
      ks*(follow_result - strategy_values[which(strategy_values$strategy == "avatar"), "value"])*
      strategy_values[strategy_values$strategy == "avatar","bayesian_prob"]
  }
  
  # calculate the risk of avatar
  if(any(as.numeric(unlist(strsplit(
    as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
    , split = ","))) %in% 
    trees[trees$fruiting == 1,]$target_id) &&
    any(as.numeric(unlist(strsplit(
      as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
      , split = ","))) %in% 
      avatar_track_trial[nrow(avatar_track_trial),'trees_visited'])){
    
    if(expect_reward == 'Update'){
    sigma <- follow_result - strategy_values[which(strategy_values$strategy == "avatar"), "value"] # if choose the PEIRS model
    }else{
    sigma <- follow_result - 1 # if choose the risk aversion model
    }
    
    risk <- strategy_values[strategy_values$strategy == "avatar","risk"]
    zeta <- sigma^2 - risk
    strategy_values[strategy_values$strategy == "avatar","risk"] <- risk + k*zeta
  }else{
    sigma <- 0
  }
  
  if(ps_number < 1){
    strategy_values[strategy_values$strategy == "avatar","risk"] <- 0.01
  } 
  
  # calculate probabilities of choosing each strategy
  for(i in 1:nrow(strategy_values)){
    if(strategy_values[i,]$strategy == "random"){
      current_strategy_value <- as.numeric(strategy_values[i,"value"]) - b*sqrt(strategy_values[strategy_values$strategy == "random","cognitive_load"])
    }else{
      
      if(model == 'PEIRS'){
      current_strategy_value <- as.numeric(strategy_values[i,"value"]) + tanh(sigma*a)*strategy_values[strategy_values$strategy == "avatar","risk"] # if choose the PEIRS model
      }else{
      current_strategy_value <- as.numeric(strategy_values[i,"value"]) - a*sqrt(strategy_values[strategy_values$strategy == "avatar","risk"]) # if choose the risk aversion model
      }
      
    }
    
    num <- beta*current_strategy_value
    den <- max(beta*current_strategy_value) + log(sum(exp(beta*strategy_values[,"value"] - max(beta*strategy_values[,"value"]))))
    prob_log <- num - den
    strategy_values[i,"choice_prob"] <- exp(prob_log)
  }
  return(strategy_values)
} # end of function

# ============================================================ #
# ============================================================ #
# ============================================================ #

# 8. CALCULATING AVERAGE SPEED FOR A TRACK ----

get_avg_speed <- function(track){
  # This function calculates the average speed for any given track file (x and y locations)
  # It takes as arguments:
  # 1. track: track for which the average speed will be calculated
  all_steps <- c()
  for(i in 2:nrow(track)){
    step_length <- sqrt((track$x[i]-track$x[i-1])^2 + (track$y[i]-track$y[i-1])^2)
    all_steps <- c(all_steps,step_length)
  }
  speed_summary <- summary(all_steps)
  return(speed_summary)
}

# ============================================================ #
# ============================================================ #
# ============================================================ #

# 9. CALCULATING WHETHER A POINT IS WITHIN THE FIELD OF VIEW ----

is_in_fov <- function(agent, object) {
  
  # function for calculating whether a point (or object, or anything holding an x and y location) 
  # in the grid is in the agent's field of view. it takes as arguments:
  # 1. agent: the dataset containing the agent's information
  # 2. object: the dataset containing the information on the object we want to determine whether in the fov
  
  object_x <- as.numeric(object[,"x"])
  object_y <- as.numeric(object[,"y"])
  current_agent_x <- as.numeric(agent[,"x"])
  current_agent_y <- as.numeric(agent[,"y"])
  # get agent velocity vector
  current_agent_x_vel <- as.numeric(agent[,"x_vel"])
  current_agent_y_vel <- as.numeric(agent[,"y_vel"])
  current_vel_vec <- c(current_agent_x_vel, current_agent_y_vel)
  # Calculate the vector from subject to object
  agent_to_object_x <- object_x - current_agent_x
  agent_to_object_y <- object_y - current_agent_y
  agent_to_object_vec <- c(agent_to_object_x,agent_to_object_y)
  # Calculate the angle between the agent's velocity and the vector towards the object
  angle_radians <- as.numeric(vecAngle(current_vel_vec,agent_to_object_vec, degree = FALSE))
  # Check whether the object lies within the field of view
  if(current_agent_x_vel == 0 & current_agent_y_vel == 0){
    return(TRUE)
  }else{
    if(agent_to_object_x == 0 & agent_to_object_y == 0){
      return(TRUE)
    }else{
      if (angle_radians <= fov/2) {
        return(TRUE)}
      else{return(FALSE)}
    }
  }
}

# ============================================================ #
# ============================================================ #
# ============================================================ #

# 10. FUNCTION FOR NORMALIZING A VECTOR TO HAVE MAGNITUDE 1 ----

vector_norm <- function(new_x_vel, new_y_vel) {
  # Function to normalize a vector to have unit magnitude
  vector_mag <- sqrt(new_x_vel^2 + new_y_vel^2)
  if (vector_mag == 0) {
    new_x_vel <- 0 
    new_y_vel <- 0
  } else {
    new_x_vel <- new_x_vel / vector_mag
    new_y_vel <- new_y_vel / vector_mag
  }
  vel_components <- c("new_x_vel" = new_x_vel, "new_y_vel" = new_y_vel)
  return(vel_components)
}

