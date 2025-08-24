##### ===== 1. LOAD PACKAGES AND ENVIRONMENT DATA ----

# install.packages("msm")
library(msm) # used function: rtnorm()
# install.packages("celltrackR")
library(celltrackR) # used function: vecAngle()
library(tidyverse)
library(data.table)
library(Matrix)

# load the data
env_landmark <- read.csv("E:/ABM/ABM Files/ABM_final/envlandmark.csv")

##### ===== 2. SETTING MODEL RUN PARAMETERS ----

####### SET SEED
global_seed <- 1

####### SPATIAL PARAMETERS
grid_size <- 70 # length of grid side (square grid by default); adjust based on environment option
trial_radius <- 70 # radius from target at which trial starts

####### TEMPORAL PARAMETERS
num_iterations <- 600 # max number of iterations for a given trial (controls running time; can be set up based on the time extracted from game)
num_trials <- 7 # number of trials per simulation (used for discover the relationship between finished trials and finding fruits)
num_simulations <- 11 # number of simulations

####### AGENT PARAMETERS
step_length <- 2.15 # step length (in m), can be set up based on the velocity extracted from game the proportion between the grid scale and game map
start_position <- "center" # if set to "center", new agent spawns in the center of the grid at the start of each simulation; if set to "random", new agent spawns in a random position in the grid at the start of a simulation
fov <- 2*pi/3 # field of view in radians (here, equivalent to 120 degrees)
dd <- 45 # detection distance (in m)

####### AVATAR PARAMETERS
avt_maxdist <- 1 # maximum distance avatar can be from the subject at the start (in x and y directions)
num_avatars <- 1 # number of avatars to spawn
remove_visited_trees <- 0 # if 1, removes all previously visited trees from avatar target sampling (i.e. avatar will pick target tree from trees that havent been visited, until all trees are visited; last visited tree is always removed by default)

####### ENVIRONMENT PARAMETERS
tree_setup <- "fruits" # the option = "map" creates the environment from the Tai botanic map; if choosing "map", botanic map needs to be loaded into environment before running the code. the other option = "fruits" creates the environment from the fruit trees location extracted from game.
number_of_trees <- 7 # the number of fruiting trees in the environment
map_stable <- 1 # the environment is stable

####### BEHAVIORAL PARAMETERS
initial_choice_probs <- c(0.65, 0.35) # vector with probabilities of choosing a given strategy out of the 2 available ones at the first step of a trial (e.g. c(0.6, 0.4)); must add up to 1; set to NULL if probabilities are to be equal
prob_switch <- 0.95 # probability of re-sampling a strategy at a given step (as opposed to keeping the current choice)
beta <- 2 # this parameters regulates exploration of options when making a decision; if 0, all decisions are equally likely; the higher beta is, the more privileged will be the options with higher perceived value over those with lower value
in_trial <- 1 # in_trial: variable stating whether the agent is in trial or not (in trial = 1; not in trial = 0)
k <- 0.6 # this parameters regulates learning rate of the risk of following avatar
ks <- 0.75 # this parameters regulates learning rate of the new experience
b <- 0.1 # this parameters regulates sensitivity of the cognitive load
a <- 0.8 # this parameters regulates sensitivity of the risk
lambda <- 0.8 # forgetting factor for decay of past experiences
model <- 'PEIRS' # type of risk perception model (PEIRS OR Aversion)
expect_reward <- 'Update' # type of risk learning model (update expected reward or continue to be 1)
k_E <- 0.2 # Activation constant for Eligibility
a_E <- -0.25 # Logistic param for Eligibility
k_c <- 0.8 # Learning rate for Co-occurrence
k_a <- 1 # Reward influence on Co-occurrence threshold
k_d <- 0.001 # Decay of Co-occurrence towards Strength
a_s <- -0.25 # Logistic param for Strength
E_threshold <- 0.04 # Eligibility product threshold for Co-occurrence

####### SET REPEAT
ps_number <- 0
re <- 1
repeat {
  ### IF USING FRUITS TREE MAP EXTRACTED FROM GAME
  if(tree_setup == "fruits"){
    # create tree dataframe
    trees <- data.frame(
      "x" = env_landmark$x, # tree x positions
      "y" = env_landmark$y, # tree y positions
      "type" = env_landmark$type, # type of landmarks
      "color" = env_landmark$color, # color vibrancy of landmarks
      "is_tree" = env_landmark$is_tree, # the information about if the landmark is tree or not
      "is_block" = env_landmark$is_block, # the information about if the landmark can be acrossed or not
      "size" = env_landmark$size, # size of landmarks
      "target_id" = 1:nrow(env_landmark)) # individual target ID
    
    # set the value for trees
    trees$fruiting <- ifelse(trees$type == "Fruits", 1, 0) # fruiting state (1 = fruiting, 0 = no fruit)
    trees$in_dd <- 0 # whether landmark is in detection distance at a given time step
    trees$in_fov <- 0 # whether landmark is in field of view for a given time step 
    trees$sim_seed <- NA # simulation seed
    trees$pv <- 0 # number of previous visits to the landmark
    trees$time_insight <- 0 # number of iterations the landmark has been in sight
    trees$dist_to_agent <- NA # distance of landmark to agent at current time-step
    trees$last_visited <- 0 # records iteration of current simulation when landmark was last visited
    trees$in_sight <- 0 # whether landmark is in sight (in dd and in fov)
    trees$global_seed <- global_seed # simulation seed
    trees$sensory <- 0 # the sensory input of landmarks to agent, default to 0
    trees$eligibility <- 0 # the eligibility trace of agent to landmarks, default to 0
    trees$coccurrence <- 0 # the co-occurrence of fruit-bearing trees and landmarks, default to 0
    trees$association <- 0 # the association between the landmarks and the fruit trees
    trees$reward <- 0 # expected reward of the landmark associate with fruit-bearing trees
    
    # INSPECTING DATASET
    summary(trees) # summary
    nrow(trees) # number of trees
    max(trees$x)-min(trees$x) # x-axis grid size
    max(trees$y)-min(trees$y) # y-axis grid size
    
    # set grid boundaries
    grid <- data.frame(       # grid boundaries
      "x_max" = 150,
      "x_min" = 0,
      "y_max" = 150,
      "y_min" = 0)
    
  }
  
  # set dataframe for fruit trees
  fruit_trees <- trees[trees$fruiting == 1, ]
  
  # set dataframe for trees
  tree <- trees[trees$is_tree == 1, ]
  
  # set dataframe for rocks
  rocks <- trees[trees$type == 'PedestalRock' |
                   trees$type == 'Rocks' |
                   trees$type == 'TropicalRock',]
  
  ### CREATING AGENT
  agent <- data.frame(
    "x" = NA, # x position on the grid
    "y" = NA, # y position on the grid
    "x_vel" = NA, # velocity x component
    "y_vel" = NA, # velocity y component
    "dd" = dd, # detection distance (in meters)
    "sim_seed" = NA, #simulation seed
    "trial_number" = 0, # trial seed
    "iteration" = 0, # trial iteration number
    "dist_to_target" = NA, # distance to trial target
    "trees_visited" = NA, # trees visited by agent
    "movement_mode" = NA, # movement mode chosen for a given time step
    "movement_mode_mod" = NA, # movement mode modifier (e.g. for "avatar" movement mode, "knowledgeable" modifier)
    "dist_to_knowledgeable" = NA, # distance to the knowledgeable avatar
    "risk" = 0.01, # learned risk of following avatar, start with 0.01
    "intrinsic_load" = NA # intrinsic load of agent to utilize the explore strategy
  )
  
  ##### ===== 3. CREATE DATAFRAME FOR SIMULATION ----
  
  ### CREATE AGENT TRACK FOR ALL SIMULATIONS
  agent_track <- agent[-1,]
  
  ### DESIGNATE AGENT TRACK FOR EACH TRIAL
  agent_track_trial <- agent[-1,]
  
  ### CREATE AGENT STATE FOR EACH SIMULATIONS
  agent_state <- list(
    current_landmark_id = NA,        # Current visited landmark
    last_landmark_id = NA,           # Last visited landmark
    path_integrator_distance = 0,    # Cumulative path length
    path_integrator_vector_x = 0,    # Total velocity of direction x
    path_integrator_vector_y = 0   # Total velocity of direction y
  )
  
  ### CREATE AVATAR TRACK FOR ALL SIMULATIONS
  avatar_track <- data.frame(
    "x" = NA,
    "y" = NA,
    "avt_type" = "knowledgeable",
    "sim_seed" = NA,
    "trial_number" = NA,
    "target_id" = NA, # avatar target tree id
    "target_x" = NA, # avatar target tree x coordinate
    "target_y" = NA, # avatar target tree y coordinate
    "dist_to_agent" = NA, # avatar's distance to agent
    "in_dd" = 0, # whether avatar is in agent's detection distance (0 if not, 1 if yes)
    "in_fov" = 0, # whether avatar is in agent's field of view (0 if not, 1 if yes)
    "in_sight" = 0, # whether avatar is in agent's sight (in detection distance AND field of view)
    "iteration" = 0, # trial iteration number
    "trees_visited" = NA # trees visited by avatar
  )
  
  avatar_track <- avatar_track[-1,]
  
  ### CREATE TREE VISIT DATAFRAME FOR AVATAR
  trees_visited_avatar <- NULL
  
  ### CREATE A DATASET FOR TRACKING STRATEGY VALUES
  strategy_value_tracking <- data.frame(
    "sim_seed" = NA,
    "trial_number" = NA,
    "iteration" = NA,
    "random_value" = NA,
    "random_choice_prob" = NA,
    "bayesian_prob_random" = NA,
    "avatar_value" = NA,
    "avatar_choice_prob" = NA,
    "bayesian_prob_avatar" = NA
  ) 
  
  strategy_value_tracking <- strategy_value_tracking[-1,] # remove first row, keep only headers
  
  ### SET PREVIOUS SUCCESSES MEMORY TRACKING DATASET
  ps_memory_tracking <- data.frame(
    "sim_seed" = NA,
    "trial_number" = NA, # avatar type
    "trial_successful" = NA, # whether current trial was successful
    "ps_number" = NA, # previous successes
    "n_choices_total" = NA, # number of steps in the simulation
    "n_choices_tree" = NA, # number of times agent chose tree movement strategy
    "n_choices_knowledgeable" = NA, # number of times agent chose to follow knowledgeable avatar
    "n_choices_random_normal" = NA, # n choices random normal
    "n_choices_random_corner" = NA, # n choices random corner
    "n_choices_random_turn" = NA) # n choices random turn
  ps_memory_tracking <- ps_memory_tracking[-1,] # remove first row, keep only headers
  
  ### SET FEATURE ASSOCIATION MATRIX FOR SPATIAL LEARNING
  FAM_memory_vectors_x <- Matrix(0, nrow = nrow(trees), ncol = nrow(trees),
                                 dimnames = list(trees$target_id, trees$target_id))
  FAM_memory_vectors_y <- Matrix(0, nrow = nrow(trees), ncol = nrow(trees),
                                 dimnames = list(trees$target_id, trees$target_id))

  ### DEFINE SIMULATION NUMBER
  sim_seed <- 1
  
  ### DEFINE INITIAL TRIAL NUMBER
  trial_number <- 1
  
  ##### ===== 4. RUN SIMULATION ----
  
  repeat { # simulation number loop
    ### CREATE A DATASET FOR STRATEGY VALUES
    strategy_values <- data.frame(
      "strategy" = c("avatar", "random"),
      "value" = 1,
      "alpha" = 1,
      "beta" = 1,
      "choice_prob" = NA,
      "cognitive_load" = NA,
      "risk" = 0.01,
      "bayesian_prob" = 0.5
    )
    
    ### SET NUMBER OF PREVIOUS SUCCESSES TO 0
    ps_number <- 0
    
    ### SET PREVIOUS SUCCESSES MEMORY FOR THE SIMULATION
    ps_memory <- data.frame(
      "sim_seed" = NA,
      "trial_number" = NA, # avatar type
      "trial_successful" = NA, # whether current trial was successful
      "ps_number" = NA, # previous successes
      "n_choices_total" = NA, # number of steps in the simulation
      "n_choices_tree" = NA, # number of times agent chose tree movement strategy
      "n_choices_knowledgeable" = NA, # number of times agent chose to follow knowledgeable avatar
      "n_choices_random_normal" = NA, # n choices random normal
      "n_choices_random_turn" = NA) # n choices random turn
    ps_memory <- ps_memory[-1,]
    
    ### AGENT'S INITIAL POSITION
    angle <- runif(min = 0, max = 2*pi, 1)
    agent$x <- 75
    agent$y <- 25
    agent$x_vel <- step_length*(cos(angle)) # velocity x component
    agent$y_vel <- step_length*(sin(angle)) # velocity y component
    
    
    #RESET AVATAR COORDS
    last_avatar_coords <- NULL
    
    ### RECORD START POINT
    start_point <- data.frame(start_x = agent$x,
                              start_y = agent$y,
                              sim_seed = sim_seed)
    
    # If there are no previous avatar coordinates, generate avatars around the agent
    if (is.null(last_avatar_coords)){ 
      avatars <- data.frame(
        "x" = sample((as.numeric(agent[,"x"])-avt_maxdist):(as.numeric(agent[,"x"])+avt_maxdist), 
                     size = num_avatars, replace = FALSE),
        "y" = sample((as.numeric(agent[,"y"])-avt_maxdist):(as.numeric(agent[,"y"])+avt_maxdist), 
                     size = num_avatars, replace = FALSE),
        "avt_id" = (((sim_seed-1)*num_trials*num_avatars)+(((trial_number-1)*num_avatars)+1)):(((sim_seed-1)*num_trials*num_avatars)+(trial_number*num_avatars)),
        "avt_type" = "knowledgeable",
        "sim_seed" = sim_seed,
        "trial_number" = trial_number,
        "target_id" = NA, # avatar target tree id
        "target_x" = NA, # avatar target tree x coordinate
        "target_y" = NA, # avatar target tree y coordinate
        "dist_to_agent" = NA, # avatar's distance to agent
        "in_dd" = 0, # whether avatar is in agent's detection distance (0 if not, 1 if yes)
        "in_fov" = 0, # whether avatar is in agent's field of view (0 if not, 1 if yes)
        "in_sight" = 0, # whether avatar is in agent's sight (in detection distance AND field of view)
        "iteration" = 0, # trial iteration number
        "trees_visited" = NA # trees visited by agent
      )}else{
        avatars <- data.frame(
          "x" = last_avatar_coords[,"x"],
          "y" = last_avatar_coords[,"y"],
          "avt_id" = (((sim_seed-1)*num_trials*num_avatars)+(((trial_number-1)*num_avatars)+1)):(((sim_seed-1)*num_trials*num_avatars)+(trial_number*num_avatars)),
          "avt_type" = "knowledgeable",
          "sim_seed" = sim_seed,
          "trial_number" = trial_number,
          "target_id" = NA, # avatar target tree id
          "target_x" = NA, # avatar target tree x coordinate
          "target_y" = NA, # avatar target tree y coordinate
          "dist_to_agent" = NA, # avatar's distance to agent
          "in_dd" = 0, # whether avatar is in agent's detection distance (0 if not, 1 if yes)
          "in_fov" = 0, # whether avatar is in agent's field of view (0 if not, 1 if yes)
          "in_sight" = 0, # whether avatar is in agent's sight (in detection distance AND field of view)
          "iteration" = 0, # trial iteration number
          "trees_visited" = NA # trees visited by agent
        )
      }
    
    ### AVATAR TRACK DATA FRAMES
    avatar_track_trial <- avatars[-1,] # creates one track data frame for avatar
    
    ### RECORD SIMULATION SEED
    agent[,"sim_seed"] <- sim_seed
    avatars[,"sim_seed"] <- sim_seed
    trees[,"sim_seed"] <- sim_seed
    s <- NA
    
    ### EMPTY LIST OF VISITED TREES/LANDMARKS
    trees_visited_list <- c()
    rocks_visited_list <- c()
    trees_visited_list_avatar <- c()
    trees$fruiting <- ifelse(trees$type == "Fruits", 1, 0)
    trees$last_visited <- 0
    
    ### LIST OF UNVISITED FRUIT TREES
    tree_unvisit <- trees[trees$fruiting == 1,]
    
    ### Set up agent trial number and avatar trial number
    agent_trial_number <- 1
    trial_number <- 1
    
    ### Simulation Iteration Loop
    it <- 1
    repeat { # Combined iteration loop
      ### avatar simulation
      avatar_target_pool <- trees[trees$fruiting == 1,]
      
      # remove last visited tree
      if(exists("trees_visited_avatar")){
        if(length(which(avatar_target_pool$target_id == trees_visited_avatar[length(trees_visited_avatar)])) != 0){
          avatar_target_pool <- avatar_target_pool[-(which(avatar_target_pool$target_id == trees_visited_avatar[length(trees_visited_avatar)])),]
        }}
      
      # only change the target when the avatar meet the fruit tree
      if(!is.null(trees_visited_avatar) && any(trees_visited_avatar %in% trees[trees$fruiting == 1,]$target_id)|
         avatars$iteration == 0){
        # SET AVATAR TRIAL TARGET
        avatar_target <- avatar_target_pool[sample(x = 1:nrow(avatar_target_pool), size = 1),]
        
        # Update avatar locations
        # assign target x and y positions
        avatars[, "target_id"] <- avatar_target[,"target_id"] # assign trial target id
        avatars[, "target_x"] <- as.numeric(avatar_target[,"x"]) # assign x
        avatars[, "target_y"] <- as.numeric(avatar_target[,"y"]) # assign y
      }
      
      # Set target as NA if the avatar is waiting
      if(is.na(avatars$x)){
        avatars[, "target_id"] <- NA
        avatars[, "target_x"] <- NA
        avatars[, "target_y"] <- NA
      }else{
        # assign target x and y positions
        avatars[, "target_id"] <- avatar_target[,"target_id"] # assign trial target id
        avatars[, "target_x"] <- as.numeric(avatar_target[,"x"]) # assign x
        avatars[, "target_y"] <- as.numeric(avatar_target[,"y"]) # assign y
      }
      
      
      # UPDATE AVATAR LOCATIONS
      if(nrow(agent_track_trial) > 0 && any(as.numeric(unlist(strsplit(
        as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
        , split = ","))) %in% 
        trees[trees$fruiting == 1,]$target_id)){
        # restart to move when agent meet a fruit tree
        avatars <- update_avatar_location(
          avatar = avatars, 
          step_length =  step_length)
      }else{
        if(!is.na(avatars$dist_to_agent) && avatars$dist_to_agent < 1.5*step_length && 
           nrow(avatar_track_trial) > 0 &&
           !avatar_track_trial[nrow(avatar_track_trial), "trees_visited"] %in% 
           avatar_target_pool$target_id){
          # ensure the movement after regeneration works in the expected way
          avatars <- update_avatar_location(
            avatar = avatars, 
            step_length =  step_length)
        }else{
          if(nrow(avatar_track_trial) > 0){
            if(!avatar_track_trial[nrow(avatar_track_trial), "trees_visited"] %in% 
               trees[trees$fruiting == 1,]$target_id){
              # move to the targeted fruit tree until visited it
              avatars <- update_avatar_location(
                avatar = avatars, 
                step_length =  step_length)
            }else{
              # disappear when the targeted fruit tree was visited
              avatars <- avatars
              avatars[,'x'] <- NA
              avatars[,'y'] <- NA
            }
          }else{
            # ensure the movement at the begining of the simulation works in the expected way
            avatars <- update_avatar_location(
              avatar = avatars, 
              step_length =  step_length)
          }
        }
      }
      
      last_avatar_coords <- avatars
      
      # UPDATE AVATAR INFORMATION
      if(!is.na(avatars[,'x'])){
        avatars <- update_avatar_information(
          agent = agent,
          avatar = avatars)
      }else{
        avatars$in_sight <- 0
      }
      
      # Record all avatar movements in one data frame
      avatar_track_trial <- rbind(avatar_track_trial, avatars)
      
      # RECORD TREE VISITED SITUATION
      trees_visited_avatar <- c()
      
      if(!is.na(avatars[,'x'])){
        for (tg in 1:nrow(avatar_target_pool)) {
          d <- sqrt((avatar_target_pool[tg, "x"] - avatars[,"x"])^2 + (avatar_target_pool[tg, "y"] - avatars[,"y"])^2) # calculate distance to each tree
          if (d < 0.5*step_length) { # check if within step length
            trees_visited_avatar <- c(trees_visited_avatar, trees[tg, "target_id"])
          }
        }
        
        trees_visited_list_avatar <- unique(c(trees_visited_list_avatar, trees_visited_avatar))
        
        if (length(trees_visited_avatar) != 0) {
          # Convert visited tree IDs to a comma-separated string
          avatar_track_trial[nrow(avatar_track_trial), "trees_visited"] <- paste(trees_visited_avatar, collapse = ",")
        } else {
          avatar_track_trial[nrow(avatar_track_trial), "trees_visited"] <- NA
        }
      }else{
        avatar_track_trial[nrow(avatar_track_trial), "trees_visited"] <- avatar_track_trial[nrow(avatar_track_trial) - 1, "trees_visited"]
      }
      
      # RECORD ITERATION
      avatars[,"iteration"] <- it
      avatar_track_trial[nrow(avatar_track_trial), "iteration"] <- it
      
      # RECORD AVATAR TRIAL
      if (!is.null(trees_visited_avatar) && any(trees_visited_avatar %in% trees[trees$fruiting == 1,]$target_id)) {
        avatar_trial_number <- trial_number + 1
      }
      
      # UPDATE LANDMARK INFORMATION AND FAM
      update_result <- update_landmark_and_fam_simple(
        agent = agent,
        trees = trees,
        agent_state = agent_state,
        FAM_memory_vectors_x = FAM_memory_vectors_x,
        FAM_memory_vectors_y = FAM_memory_vectors_y)
      
      # Get the results
      trees <- update_result$trees
      agent_state <- update_result$agent_state
      FAM_memory_vectors_x <- update_result$FAM_memory_vectors_x
      FAM_memory_vectors_y <- update_result$FAM_memory_vectors_y
      
      ### Get the Items can be seen by Agent 
      agent_target_pool <- trees[trees$in_sight == 1,]
      
      ### UPDATING STRATEGY INFORMATION
      if(nrow(agent_track_trial) > 0){
        strategy_values <- update_strategy_information_peirs(
          strategy_values = strategy_values,
          ps_memory = ps_memory,
          agent = agent)
      }
      
      ### record the value of updated risk
      agent[,"risk"] <- strategy_values[strategy_values$strategy == 'avatar', "risk"]
      
      ### RECORDING STRATEGY INFO FOR CURRENT ITERATION
      strategy_value_iteration <- data.frame(
        "sim_seed" = agent[,"sim_seed"],
        "trial_number" = agent[,"trial_number"],
        "iteration" = it,
        "avatar_value" = strategy_values[which(strategy_values$strategy == "avatar"), "value"],
        "avatar_choice_prob" = strategy_values[which(strategy_values$strategy == "avatar"), "choice_prob"],
        "random_value" = strategy_values[which(strategy_values$strategy == "random"), "value"],
        "random_choice_prob" = strategy_values[which(strategy_values$strategy == "random"), "choice_prob"],
        "bayesian_prob_avatar" = strategy_values[which(strategy_values$strategy == "avatar"), "bayesian_prob"],
        "bayesian_prob_random" = strategy_values[which(strategy_values$strategy == "random"), "bayesian_prob"]
      ) 
      
      # ADDING INFORMATION TO STRATEGY VALUE TRACKING DATASET
      strategy_value_tracking <- rbind(strategy_value_tracking, strategy_value_iteration)
      
      # UPDATE AGENT LOCATION
      agent <- update_agent_location_softmax(
        agent = agent, 
        agent_track = agent_track,
        agent_track_trial = agent_track_trial,
        in_trial = in_trial,
        target = trees,
        avatar = avatars,
        step_length = step_length,
        grid = grid,
        initial_choice_probs = initial_choice_probs,
        strategy_values = strategy_values,
        prob_switch = prob_switch,
        beta = beta)
      
      # Record agent trial data
      agent_track_trial <- rbind(agent_track_trial,agent)
      
      # CHECK IF ANY FRUIT TREES WERE VISITED
      trees_visited <- c()
      
      for (tg in 1:nrow(tree)) {
        d <- sqrt((tree[tg, "x"] - agent[,"x"])^2 + (tree[tg, "y"] - agent[,"y"])^2) # calculate distance to each tree
        if (d < 5) { # check if within fruits falling range
          trees_visited <- c(trees_visited, trees[tg, "target_id"])
        }
      }
      
      trees_visited_list <- unique(c(trees_visited_list, trees_visited))
      
      # CHECK IF ANY ROCKS WERE VISITED
      rocks_visited <- c()
      
      for (tg in 1:nrow(rocks)) {
        d <- sqrt((rocks[tg, "x"] - agent[,"x"])^2 + (rocks[tg, "y"] - agent[,"y"])^2) # calculate distance to each tree
        if (d < 1.5) { # check if within rock size
          rocks_visited <- c(rocks_visited, trees[tg, "target_id"])
        }
      }
      
      rocks_visited_list <- unique(c(rocks_visited_list, rocks_visited))
      
      # Record which landmarks were found
      if (length(trees_visited) != 0) {
        # Convert visited tree IDs to a comma-separated string
        agent_track_trial[nrow(agent_track_trial), "trees_visited"] <- paste(trees_visited, collapse = ",")
        trees[trees_visited, "pv"] <- trees[trees_visited, "pv"] + 1
        trees[trees_visited, "last_visited"] <- nrow(agent_track_trial)
      } else {
        agent_track_trial[nrow(agent_track_trial), "trees_visited"] <- NA
      }
      
      if (length(rocks_visited) != 0) {
        #update the visiting situation
        trees[rocks_visited, "pv"] <- trees[rocks_visited, "pv"] + 1
        trees[rocks_visited, "last_visited"] <- nrow(agent_track_trial)
      }
      
      # CHECK TRIAL COMPELETAION
      if (any(as.numeric(unlist(strsplit(
        as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
        , split = ","))) %in% 
        trees[trees$fruiting == 1,]$target_id)|!is.null(any(trees_visited)) &&
        !is.na(any(trees_visited))) {
        agent_trial_number <- agent_trial_number + 1 # Agent trials complete
      }
      
      # RECORD THE REWARD OF CURRENT TRIAL
      if(any(as.numeric(unlist(strsplit(
        as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
        , split = ","))) %in% 
        tree_unvisit$target_id)){
        reward <- 1
      }else{
        reward <- 0
      }
      
      #update the cognitive load of agent
      if(agent$movement_mode == "avatar"){
        agent$intrinsic_load <- 0
      }else{
        agent$intrinsic_load <- strategy_values[strategy_values$strategy == "random","cognitive_load"]
      }
      
      # record the sim_seed for following state update
      s <- agent_track_trial[nrow(agent_track_trial),"sim_seed"]
      
      # RECORD ITERATION
      agent[,"iteration"] <- it
      agent_track_trial[nrow(agent_track_trial), "iteration"] <- it
      
      # Create frame for visited fruit in current trial
      visited_fruit <- tree_unvisit[which(tree_unvisit$target_id %in% as.numeric(unlist(strsplit(
        as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
        , split = ",")))),]
      
      # Update the record for visited fruit of agent
      if(any(as.numeric(unlist(strsplit(
        as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
        , split = ","))) %in% 
        tree_unvisit$target_id)){
        if(!exists('visited_fruit_trial') || nrow(visited_fruit_trial == 0)){
          visited_fruit_trial <- visited_fruit
        }else{
          visited_fruit_trial <- rbind(visited_fruit_trial, visited_fruit)
        }
      }
      
      # CHECK SUCCESS OF FOLLOWING SITUATION
      if(any(as.numeric(unlist(strsplit(
        as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
        , split = ","))) %in% 
        tree_unvisit$target_id) &&
        any(as.numeric(unlist(strsplit(
          as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
          , split = ","))) %in% 
          avatar_track_trial[nrow(avatar_track_trial),'trees_visited'])){
        follow_result <- 1
      }else{
        follow_result <- 0
      }
      
      # CHECK TRIAL SUCCESS SITUATION
      if(any(as.numeric(unlist(strsplit(
        as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
        , split = ","))) %in% 
        tree_unvisit$target_id)){
        trial_success <- 1
        ps_number <- ps_number + 1
        
        # Calculate the expected reward for landmarks near to fruit trees
        if (nrow(visited_fruit) > 0) {
          for (i in 1:nrow(trees)) {
            d_fruit <- sqrt((trees[i, "x"] - visited_fruit[, "x"])^2 + 
                              (trees[i, "y"] - visited_fruit[, "y"])^2)
            
            if (d_fruit < 35) {
              trees[i, "reward"] <- 1 * trees[i, "association"]
            }
          }
        }
        
        if (ps_number > num_trials) break
      }else{
        trial_success <- 0
      }
      
      # Remove visited fruit tree from the list of fruit-bearing trees
      if(any(as.numeric(unlist(strsplit(
        as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
        , split = ","))) %in% 
        tree_unvisit$target_id)){
        tree_unvisit <- tree_unvisit[-(which(tree_unvisit$target_id %in% as.numeric(unlist(strsplit(
          as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
          , split = ","))))),]
      }
      
      # Simulate the generated situation of avatar when the agent visited fruit trees in the game
      if((is.na(avatars$dist_to_agent)|!avatars$dist_to_agent < 5) && 
         is.na(avatars$target_id) &&
         any(as.numeric(unlist(strsplit(
           as.character(agent_track_trial[nrow(agent_track_trial), "trees_visited"])
           , split = ","))) %in% 
           trees[trees$fruiting == 1,]$target_id)) {
        avatars <- data.frame(
          "x" = sample((as.numeric(agent[,"x"])-avt_maxdist):(as.numeric(agent[,"x"])+avt_maxdist), 
                       size = num_avatars, replace = FALSE),
          "y" = sample((as.numeric(agent[,"y"])-avt_maxdist):(as.numeric(agent[,"y"])+avt_maxdist), 
                       size = num_avatars, replace = FALSE),
          "avt_id" = (((sim_seed-1)*num_trials*num_avatars)+(((trial_number-1)*num_avatars)+1)):(((sim_seed-1)*num_trials*num_avatars)+(trial_number*num_avatars)),
          "avt_type" = "knowledgeable",
          "sim_seed" = sim_seed,
          "trial_number" = trial_number,
          "target_id" = NA, # avatar target tree id
          "target_x" = NA, # avatar target tree x coordinate
          "target_y" = NA, # avatar target tree y coordinate
          "dist_to_agent" = NA, # avatar's distance to agent
          "in_dd" = 0, # whether avatar is in agent's detection distance (0 if not, 1 if yes)
          "in_fov" = 0, # whether avatar is in agent's field of view (0 if not, 1 if yes)
          "in_sight" = 0, # whether avatar is in agent's sight (in detection distance AND field of view)
          "iteration" = 0, # trial iteration number
          "trees_visited" = NA # trees visited by agent
        )
      }
      
      ##### ===== 5. RECORD RESULTS ----
      
      if(agent_trial_number > 1){
        # UPDATE AGENT MEMORY FOR TRIAL
        memory_trial <- ps_memory[-(1:nrow(ps_memory)),] # create trial memory
        memory_trial[1,"sim_seed"] <- agent_track_trial[nrow(agent_track_trial), "sim_seed"]
        memory_trial[1,"trial_number"] <- agent_track_trial[nrow(agent_track_trial), "trial_number"]
        memory_trial[1,"trial_successful"] <- trial_success
        memory_trial[1,"ps_number"] <- ps_number
        memory_trial[1,"n_choices_total"] <- nrow(agent_track_trial)
        memory_trial[1,"n_choices_knowledgeable"] <- nrow(agent_track_trial[which(agent_track_trial$movement_mode_mod == "knowledgeable"),])
        memory_trial[1,"n_choices_random_normal"] <- nrow(agent_track_trial[which(agent_track_trial$movement_mode_mod == "FAM"),])
        memory_trial[1,"n_choices_random_turn"] <- nrow(agent_track_trial[which(agent_track_trial$movement_mode_mod == "turn"),])
        ps_memory <- rbind(ps_memory,memory_trial)
        
        # TRACK OVERALL PREVIOUS SUCCESS MEMORY
        ps_memory_tracking <- rbind(ps_memory_tracking,memory_trial)
        
        if(it == 600 | ps_number >= num_trials){
          # recording overall agent track and reset the trial track dataframe
          agent_track <- rbind(agent_track, agent_track_trial)
          agent_track_trial <- agent[-1,]
          
          # recording overall avatar track and reset the trial track dataframe
          avatar_track <- rbind(avatar_track, avatar_track_trial) 
          avatar_track_trial <- avatars[-1,]
        }
      }
      
      ### End Simulation Iteration
      if (it >= num_iterations | ps_number >= num_trials) break
      it <- it + 1
    }
    
    ### Simulation End Conditions
    sim_seed <- sim_seed + 1
    if (sim_seed > num_simulations) break
  }
  
  # Record agent track for each simulation
  if (!exists("fruits_visited")) {
    fruits_visited <- data.table(agent_track)
    fruits_visited[, re := re]
  } else {
    new_data <- data.table(agent_track)
    new_data[, re := re]
    fruits_visited <- rbindlist(list(fruits_visited, new_data), use.names = TRUE, fill = TRUE)
  }
  
  # Record agent memory for each simulation
  if (!exists("agent_memory")) {
    agent_memory <- data.table(ps_memory_tracking)
    agent_memory[, re := re]
  } else {
    new_data <- data.table(ps_memory_tracking)
    new_data[, re := re]
    agent_memory <- rbindlist(list(agent_memory, new_data), use.names = TRUE, fill = TRUE)
  }
  
  # Record strategy values for each simulation
  if (!exists("strategy_memory")) {
    strategy_memory <- data.table(strategy_value_tracking)
    strategy_memory[, re := re]
  } else {
    new_data <- data.table(strategy_value_tracking)
    new_data[, re := re]
    strategy_memory <- rbindlist(list(strategy_memory, new_data), use.names = TRUE, fill = TRUE)
  }
  
  re <- re + 1
  if (re > 30) break
}

# save the dataframe for further analysis
write.csv(fruits_visited,"E:/ABM/ABM Files/data/FAM_results/fruits_visited.csv")
write.csv(agent_memory,"E:/ABM/ABM Files/data/FAM_results/agent_memory.csv")
write.csv(strategy_memory,"E:/ABM/ABM Files/data/FAM_results/strategy_memory.csv")

# removing the dataframe if needed 
rm(fruits_visited)
rm(agent_memory)
rm(strategy_memory)