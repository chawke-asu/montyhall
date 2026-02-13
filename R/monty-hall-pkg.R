#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Contestant selects first door.
#'   
#' @description
#'   `select_door()` randomly returns a value for one door (1-3)
#'   
#' @details
#'   this function first creates a vector of doors numbered 1-3, then randomly selects one of the doors, then returns the selection. this mimics a gameshow contestant randomly picking one of three closed doors
#'
#' @param ... no arguments are used by the function.
#' 
#' @return
#'   the output will be a vector
#'   
#' @examples
#'   select_door()   # randomly chooses door
#'   
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Host Opens Goat Door
#'   
#' @description
#'   `open_goat_door()` randomly returns a value for one of the unopened, unpicked doors that has a goat behind it
#'   
#' @details
#'   starts by pulling data from arguments: `game` and `a.pick`. creates a doors vector with 3 doors. creates a logical argument where if `a.pick` argument was car, it will set `goat.doors` as the two remaining unopened doors and randomly choose one of them. if `a.pick` was a goat, then it sets the opened door as the remaining unopened door that does not have a car behind it. lastly, it returns the value for `opened.door` (1-3) as the result of the logical argument.
#' 
#' @param game  establishes the random scenario that create_game() created: sample( x = c("goat","goat","car"), size=3, replace=F )
#' @param a.pick  integer (1,2,3) that aligns with the door that the contestant chose first
#' 
#' @return
#'   number between 1 and 3
#'   
#' @examples
#'   this.game <- c("goat","car","goat")
#'   my.initial.pick <- 1
#'   open_goat_door( this.game, my.initial.pick ) # should be 3
#'   
#'   #'   this.game <- c("goat","car","goat")
#'   my.initial.pick <- 2
#'   open_goat_door( this.game, my.initial.pick ) # should be 1 or 3
#'   
#'   #'   this.game <- c("goat","car","goat")
#'   my.initial.pick <- 3
#'   open_goat_door( this.game, my.initial.pick ) # should be 1
#'   
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#'
#' @title
#'   Change Doors
#'   
#' @description
#'   `change_door()` simulates the contestant either switching to a new door or staying with the door they originally picked
#'   
#' @details
#'   starts with `stay=T` meaning the contestant stays with their first pick, or `stay=F` meaning the contestant decides to switch to one of the unopened doors. the function also pulls from earlier arguments: `opened.door` and `a.pick`. next it creates a `doors` vector with 3 doors, then starts the following logical argument: if `stay=T` then `final.pick` becomes their `a.pick` value or their first choice. if `stay=F`, then `final.pick` becomes the unopened, unpicked door. lastly, this function will return the `final.pick` value which is a number between 1 and 3.
#' 
#' @param stay         logical vector that is either T or F
#' @param opened.door  number between 1 and 3, reflecting the goat door that the host opened
#' @param a.pick       integer (1,2,3) that aligns with the door that the contestant chose first
#' 
#' @return
#'   number between 1 and 3
#'   
#' @examples
#'   change_door( stay=T, opened.door=1, a.pick=3 ) # should be 3
#'   change_door( stay=F, opened.door=1, a.pick=3 ) # should be 2
#'   
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}




#' @title
#'   Determine if Contestant has Won
#'   
#' @description
#'   determine_winner() puts the previous arguments together to see if the contestant wins or loses depending on the starting scenario set by create_game() and the contestants choice to stay or switch doors
#' 
#' @details
#'   this function pulls from previous arguments 'final.pick' and 'game' to establish original scenario and contestant choice. the function itself is comprised of a simple logical argument: if 'final.pick' in this 'game' resulted in a car, the contestant wins, if not, they lose. the function will return "WIN" or "LOSE" depending on whether or not the contestant wins.
#' 
#' @param final.pick  number between 1 and 3
#' @param game        establishes the random scenario that create_game() created: sample( x = c("goat","goat","car"), size=3, replace=F )
#'
#' @return
#'   characters "WIN" or "LOSE"
#'   
#' @examples
#'   this.game <- c("goat","car","goat")
#'   determine_winner( final.pick=1, game=this.game ) # should be "LOSE" 
#'   
#'   this.game <- c("goat","car","goat")
#'   determine_winner( final.pick=2, game=this.game ) # should be "WIN" 
#'   
#'   this.game <- c("goat","car","goat")
#'   determine_winner( final.pick=3, game=this.game ) # should be "LOSE" 
#'   
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Play the Monty Hall Game
#'   
#' @description
#'   play_game() puts all the previous functions together so you can easily play a full Monty Hall game
#'   
#' @details
#'   this function creates multiple arguments using the previous functions in order to nest them all together under one function. it also creates 'final.pick.stay' and 'final.pick.switch' for the two outcomes of 'change_door', as well as 'outcome.stay' and 'outcome.switch' for the two outcomes of the contestant's final pick depending on the 'create_game' scenario. the final part of this function creates a vector called 'strategy' that has either "stay" or "switch", allowing the following code line to create a data frame that has the strategy vectors as row names, making it easier to vizualize the game results. The last part of this function calls the 'game.results' which returns the data frame that makes it easy to read all the game results in one spot.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return
#'   a dataframe table with two columns: strategy and outcome, and two rows: the 'stay' and 'switch'. the two rows will have the outcome "WIN" or "LOSE" depending on the scenario and the contestant choices
#' 
#' @examples
#'   play_game() 
#'   
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Play 100 Monty Hall Games
#'   
#' @description
#'   play_n_games() utilizes play_game() and loops to cycle through play_game() n times. by default, n is set to 100
#'   
#' @details
#'   this function starts by setting n to equal 100, then loads the dplyr package, sets up a 'results.list' collector, and loop count of 1. Then the loop portion iterates one loop n times, which is by default set to be 100, it will do the following: create the argument game.outcome to hold the play_game() function, set that with the collector, and add an additional +1 to the loop count. next it sets the collector to become 'results.df', creates a table with a pipe operator for row proportions, number rounding, and prints it. finally, this function returns the results.df table
#'
#' @param n, integer which is set to 100 here as default
#' 
#' @return
#'   returns a data frame with n rows that have the game outcome.
#' 
#' @examples
#'   play_n_games( n=100 )
#'   
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
