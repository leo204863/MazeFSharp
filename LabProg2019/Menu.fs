module LabProg2019.Menu

open System
open Engine
open Gfx
open System.Media
open LabProg2019.MazeGenerator
open System.Diagnostics
open Config

///Status of the game
type Status = Menu|InGame|Victory|ShowSolution|KeysMenu|SelectMode|Lose

///Action of the buttons in the menu
type ButtonAction = StartGame|Quit|KeysMenu|Arcade|Blind|PacMan

///Game Modes
type Mode = Arcade|Blind|PacMan


//variable for the game score
let mutable score = 0

//variable for cool down of the drill
let mutable cooldown = 15

// variable for the drill time
let mutable drillActiveTime = 5

// variable used to decide whether the player can use the drill or not 
let mutable canUseDrill = true



//struct for enemy
type enemy (enemy_sprite: sprite , direction:Vector , max_idle : int) = 
    
   let e:sprite = enemy_sprite
   let mutable d:Vector = direction
   let maxidl: int = max_idle
   let mutable idl: int = 0

   ///If false, the cell is a way in the maze
   member this.enemy_sprite with get() = e

   member this.direction with get() = d and set(value) = d <- value
   
   //the max time the player stays still
   member this.max_idle with get() = maxidl

   //the current time the player stays still
   member this.idle with get() = idl and set(value) = idl <- value


//enemies sprite list
let mutable enemiesList: enemy list = []

///Engine state
type state = {
    player: sprite
    enemies: sprite list
    menuArrow : sprite
    maze : sprite
    mutable mode: Mode
    mutable status: Status
    mutable drill: bool
}

///Class for buttons. Contains a label and a corresponding action to be executed on button press
type Button (label:string, actionCode:ButtonAction) =
    let mutable y = 0.
    member this.label = label
    member this.actionCode:ButtonAction = actionCode
    member this.Y with get() = y and set(value) = y <- value

//function that generates the enemies on the maze 
let generateEnemies(min:int,max:int,eng:engine,maze: Maze) =  

                                        //generate the number of enemies given the maximun number and the minimum number
                                        let n_enemies= myRandom.Next(min,max)

                                        let mutable isMuro = true

                                        //random coordinates to generate the enemy
                                        let mutable rand_x= myRandom.Next( 0, maze.W)
                                        let mutable rand_y= myRandom.Next( 0, maze.H)

                                        let mutable try_cell:Vector = new Vector (rand_x,rand_y)
                                        
                                        //random direction for the enemy
                                        let mutable rand_dir = myRandom.Next(0,3)
                                        
                                        let mutable dx1 = 0.0
                                        let mutable dy1 = 0.0

                                        for i=0 to n_enemies do
                                            isMuro <- true

                                            //pick random direction for the enemy
                                            match rand_dir with
                                            | 0 ->  dx1 <- 0.
                                                    dy1 <- -1.
                                            | 1 ->  dx1 <- 0.
                                                    dy1 <- 1.
                                            | 2 ->  dx1 <- 2.
                                                    dy1 <- 0.
                                            | 3 ->  dx1 <- -2.
                                                    dy1 <- 0.
                                            | _ ->  dx1 <- 0.
                                                    dy1 <- 0.
                                            
                                            //vector with the direction of the enemy sprite
                                            let v = new Vector(int(dx1),int(dy1))

                                            //for how long the enemy waits to move
                                            let idle_time = myRandom.Next(4, 5)

                                            //finding the the first cell that is not a wall
                                            while isMuro do
                                                rand_x <- myRandom.Next( 0, maze.W)
                                                rand_y <- myRandom.Next( 0, maze.H)
                                                try_cell <- new Vector (rand_x,rand_y)
                                                
                                                if not((maze.getCell(try_cell)).isWall) then isMuro <- false
                                                
                                            //generate the sprite and add the enemy to the list of enemies
                                            let enemy_sp = eng.create_and_register_sprite (image.rectangle (2,1, pixel.create('\219', Color.DarkBlue)), rand_x*2, rand_y, 2)
                                            let new_enemy = new enemy(enemy_sp,v,idle_time)
                                            enemiesList <-  new_enemy :: enemiesList


                                                   
type MySoundPlayer () =
    inherit SoundPlayer()
    member private this.privateSetSounds (status:bool) (loop:bool) (mode:SoundpathName) (soundpaths: (SoundpathName * string)list) = match soundpaths with
                                                                                                                                                    |[]-> ()
                                                                                                                                                    |(name,path)::xs -> if (name = mode) then
                                                                                                                                                                            if(status) then 
                                                                                                                                                                                this.SoundLocation <- path
                                                                                                                                                                                this.Load()

                                                                                                                                                                                if(loop) then this.PlayLooping() 
                                                                                                                                                                                            else ()
                                                                                                                                                                            else this.Stop()
                                                                                                                                                                        else this.privateSetSounds status loop mode xs
    
    member this.SetSounds (status:bool) (loop:bool) (mode:SoundpathName) = this.privateSetSounds status loop mode soundpaths
///Return to menu, Status <- Menu
let returnToMenu (st:state) (screen:wronly_raster)(sp:MySoundPlayer) =
    sp.SetSounds false false SPArcade
    sp.SetSounds false false SPBlind
    sp.SetSounds false false SPTimed
    sp.SetSounds true false SPMenu
    st.player.clear
    st.maze.clear
    score <- 0
    canUseDrill <- true
    drillActiveTime <- 5
    cooldown <-15
    st.drill <- false
    List.iter(fun (e:enemy) -> e.enemy_sprite.clear) enemiesList
    //List.iter (fun (en:enemy) -> engine.removeSprite(en.enemy_sprite)) enemiesList
    enemiesList <- []
    st.status <- Status.Menu
    st.menuArrow.drawSprite(pixel.create('>', Color.White))


//Draw the buttons on the screen
let drawButtons (buttonList: Button list) (menuYstart:float) (menuYIncrease:float) (screen:wronly_raster)=
    for i=0 to buttonList.Length - 1 do
        buttonList.[i].Y <- menuYstart + menuYIncrease * float(i)
        screen.draw_text(buttonList.[i].label, 70, int(buttonList.[i].Y), Color.White)
      

///Execute the specific action of the button if it is pressed
let executeIfButtonPressed (buttonList: Button list) (menuYstart:float) (menuYIncrease:float) (sp: MySoundPlayer) (keyo:ConsoleKeyInfo option) (st:state) (matchPressed: ButtonAction -> unit)=
    let enter = char (13)
    //If w or s are pressed, then the menuArrow is moved vertically
    let dy =
        match keyo with
        None -> 0.
        |Some key -> sp.Play()
                     match key.KeyChar with 
                          'w' -> -4.
                        | 's' -> 4.
                        //If enter pressed, the correct ButtonAction is executed
                        | _ when key.KeyChar = enter -> let clickedButton: Button option = List.tryFind (fun (button:Button)-> button.Y = st.menuArrow.y) buttonList
                                                        in match clickedButton with
                                                             None -> ignore()
                                                            |Some button -> matchPressed button.actionCode
                                                        st.menuArrow.y <- 11.
                                                        0.
                        | _   -> 0.
    st.menuArrow.move_by(0., dy)
    //x position is constant
    st.menuArrow.x <- 67.0
    //Implementing menuArrow warping
    if (st.menuArrow.y < 11.) then st.menuArrow.y <- menuYstart + menuYIncrease * float(buttonList.Length - 1)
        else if st.menuArrow.y > menuYstart + menuYIncrease * float(buttonList.Length - 1)
                then st.menuArrow.y <- 11.


///Initialize the menu
let init ()  =
            //Size of the engine window
            let W = 150
            let H = 35
            

            //Choosing the Start and End position
            let startPosition: Vector = new Vector (0,1)
            let endPosition: Vector = new Vector ((W/2)-1,H-4)

            //SameDirectionMin and sameDirectionMax are used to set the minimum and the maximum length of the corridors of the maze 
            let sameDirectionMin = 2
            let sameDirectionMax = 5

            //If set to true, the maze will have multiple paths to the exit
            let linkPaths = true;

            //MenuYstart sets the initial position of the pointer in the menu, 
            //MenuYIncrease allows you to move to the other buttons of the menu
            let menuYstart = 11.
            let menuYIncrease = 4.

            //Time for the Timed mode
            let maxTime = 100

            //List of buttons in main menu
            let arr_options:Button list = [ new Button ("Start", ButtonAction.StartGame);
                                            new Button ("Keys!", ButtonAction.KeysMenu);
                                            new Button ("Exit!", ButtonAction.Quit)
                                          ]
            //List of buttons in SelectMode
            let modeButtons:Button list = [ new Button ("Easy!", ButtonAction.Arcade);
                                            new Button ("Blind", ButtonAction.Blind);
                                            new Button ("PacMan", ButtonAction.PacMan)

                                          ]
            //Generate the Engine
            let engine = new engine (W, H)     

            let sp:MySoundPlayer = new MySoundPlayer()                       
            sp.SetSounds true true SPIntro 
            
            //Printing the logo of the maze
            Console.ForegroundColor <- ConsoleColor.Green
            printfn "%s" Config.startingScreenLogo
            ignore(Console.ReadKey())
            //intro_game.Stop()
            Console.Clear()


            //Used for deleting all the \r caracters (new line fix)
            Config.instructionMenu <- String.map (fun (ch:char)-> if ch = '\r' then char(0) else ch) Config.instructionMenu
            Config.menuScreen <- String.map (fun (ch:char)-> if ch = '\r' then char(0) else ch) Config.menuScreen
            Config.victory <- String.map (fun (ch:char)-> if ch = '\r' then char(0) else ch) Config.victory
            Config.lose <- String.map (fun (ch:char)-> if ch = '\r' then char(0) else ch) Config.lose

            //Contains the instantiated Maze while playing
            let mutable myMaze: Maze option = None

            //Creating sprite "player"
            let player = engine.create_and_register_sprite (image.rectangle (2,1, pixel.create('\219', Color.Cyan)), endPosition.X*2-1, endPosition.Y, 2)
            player.clear

            //Generate the lists for the solution
            let mutable mazeSolution: MazeCell list = []
            let mutable spriteSolution: sprite list = []
            
            ///Used for timing
            let stopWatch = new Stopwatch();

            //Engine loop function
            let myLoop (keyo : ConsoleKeyInfo option) (screen : wronly_raster) (st : state) =
                    
                    //Player can choose what to do
                    if (st.status = Status.Menu) then
                                //Draws the main menu logo on the screen
                                screen.draw_text(Config.menuScreen, 39, 4, Color.Green)
                                //Draws the main menu options on the screen
                                drawButtons arr_options menuYstart menuYIncrease screen
                                sp.SetSounds true false SPMenu
                                executeIfButtonPressed arr_options menuYstart menuYIncrease sp keyo st (fun (buttonAction:ButtonAction) ->
                                    match buttonAction with
                                        ButtonAction.StartGame -> st.status <- Status.SelectMode
                                        |ButtonAction.Quit -> Environment.Exit(0)
                                        |ButtonAction.KeysMenu -> st.status <- Status.KeysMenu
                                        |_ -> ignore())
                                st, false
                    //If the game status is InGame
                    elif st.status = Status.InGame then
                         //If the gameMode status is Blind
                         if(st.mode = Mode.Blind) then 
                             
                             //Delete the whole maze and print only the walls around the player
                             st.maze.clear
                             List.iter (fun (cell:MazeCell) -> if ((cell.isWall && distanceBetweenPoints (cell.position.X, cell.position.Y, int(st.player.x*(0.5)), int(st.player.y)) <= 6.)) then st.maze.draw_line(cell.position.X * 2, cell.position.Y, cell.position.X * 2 + 1, cell.position.Y, pixel.create('\219', Color.DarkGray))) myMaze.Value.maze 
                             if (distanceBetweenPoints (endPosition.X * 2, endPosition.Y, int(st.player.x), int(st.player.y))<=7.) then screen.draw_text("\219\219", endPosition.X*2, endPosition.Y, Color.DarkRed)
                             if (distanceBetweenPoints (startPosition.X * 2, startPosition.Y, int(st.player.x), int(st.player.y))<=7.) then screen.draw_text("\219\219", startPosition.X*2, startPosition.Y, Color.Green)

                         else    
                             List.iter (fun (cell:MazeCell) -> if cell.isWall then st.maze.draw_line(cell.position.X * 2, cell.position.Y, cell.position.X * 2 + 1, cell.position.Y, pixel.create('\219', Color.DarkGray))) myMaze.Value.maze
                             //Start
                             screen.draw_text("\219\219", startPosition.X*2, startPosition.Y, Color.DarkGreen)
                             //Finish
                             screen.draw_text("\219\219", endPosition.X*2, endPosition.Y, Color.DarkRed)
                             
                             //If the gameMode status is Timed   
                             if (st.mode = Mode.PacMan) then

                                //change player color to pacman color
                                st.player.clear
                                st.player.drawSprite (pixel.create ('\219',Color.DarkYellow))

                                //Draw the food on the maze
                                List.iter (fun (cell:MazeCell) -> if cell.isFood then st.maze.draw_line(cell.position.X * 2, cell.position.Y, cell.position.X * 2 + 1, cell.position.Y, pixel.create('\234', Color.Red))) myMaze.Value.maze
                                

                                List.iter(fun (en:enemy) ->
                                                           en.idle <- en.idle - 1
                                                           if(en.idle <= 0) then
                                                                    let current_position = new Vector ( int(en.enemy_sprite.x)/2,int(en.enemy_sprite.y) )

                                                                    //get the previous and the next directions
                                                                    let next_vector = new Vector(current_position.X + en.direction.X / 2, current_position.Y + en.direction.Y)
                                                                    let previous_vector = new Vector(current_position.X - en.direction.X / 2, current_position.Y - en.direction.Y)
                                                                    
                                                                    //get the previos and next cells
                                                                    let mutable next_cell = myMaze.Value.getCell(next_vector)
                                                                    let previous_cell = myMaze.Value.getCell(previous_vector)

                                                                    //get the adiacent cells of the enemy
                                                                    let mutable cells_list = myMaze.Value.getAdiacentCells(myMaze.Value.getCell(current_position))
                                                                    cells_list <- List.filter (fun (cell:MazeCell) -> not(cell.position.isSameAs(next_vector)) && not(cell.position.isSameAs(previous_vector))) cells_list

                                                                    cells_list <- List.filter (fun (cell:MazeCell) -> not(cell.isWall)) cells_list

                                                                    //if the enemy can't move it returns back or it chooses a random adiacent cell
                                                                    if(next_cell.isWall || cells_list.Length > 0) then
                                                                        
                                                                        //enemy is stuck
                                                                        if(cells_list.Length = 0) then
                                                                            next_cell <- previous_cell
                                                                        else
                                                                            next_cell <- cells_list.[myRandom.Next(cells_list.Length)]

                                                                    //create the new cells coordinates
                                                                    let dx1 = float((next_cell.position.X - current_position.X)*2)
                                                                    let dy1 = float(next_cell.position.Y - current_position.Y)
                                                                    //set direction of the enemy so it follows that paths
                                                                    en.direction <- new Vector(int(dx1),int(dy1))
                                                                    
                                                                    //move the enemy and reset the idle time
                                                                    en.enemy_sprite.move_by(dx1,dy1)
                                                                    en.idle <- en.max_idle

                                                                    //control used to check if the enemy and the player 
                                                                    if(en.enemy_sprite.x = st.player.x && en.enemy_sprite.y = st.player.y) then 
                                                                                                                                                sp.SetSounds true false SPLose
                                                                                                                                                sp.Play()
                                                                                                                                                List.iter(fun (e:enemy) -> e.enemy_sprite.clear) enemiesList
                                                                                                                                                enemiesList <- []
                                                                                                                                                st.maze.clear
                                                                                                                                                st.player.clear
                                                                                                                                                //List.iter (fun (mySprite:sprite) -> engine.removeSprite(mySprite)) spriteSolution
                                                                                                                                                List.iter (fun (en:enemy) -> engine.removeSprite(en.enemy_sprite)) enemiesList
                                                                                                                                                
                                                                                                                                                st.status <- Status.Lose
                                                                                                                                                
                                                                    ) enemiesList


                                if(drillActiveTime = -1) then 
                                                             screen.draw_text("Score: " + score.ToString()+ "  is_Drill_Active:" + st.drill.ToString() + "  Drill Time: "+ "0" + "  CoolDown: " + cooldown.ToString(), 0, H - 1, Color.Black, Color.Blue)
                                else 
                                                             screen.draw_text("Score: " + score.ToString()+ "  is_Drill_Active:" + st.drill.ToString() + "  Drill Time: "+ drillActiveTime.ToString() + "  CoolDown: " + cooldown.ToString(), 0, H - 1, Color.Black, Color.Blue)
                                
                                ///Remainig time for the drill
                                if(st.drill = true) then 
                                                  drillActiveTime <- 5 - stopWatch.Elapsed.Seconds
                                                  canUseDrill <- false

                                // when drill time is over,the cooldown time starts
                                if drillActiveTime <= 0 then
                                    st.drill <- false
          
                                    if(drillActiveTime = 0) then 
                                                                 stopWatch.Restart()
                                                                 drillActiveTime <- -1

                                    //calculate the drill time
                                    cooldown <- 15 - stopWatch.Elapsed.Seconds
                                    
                                    //reset the cooldown and makes the drill available
                                    if (cooldown = 0) then st.drill <- false
                                                           cooldown <- 15
                                                           canUseDrill <- true
                                                           drillActiveTime <- 5
                                    

                         //dx and dy contains the values of the movement in the x-axis and y-axis
                         let dx, dy =
                             match keyo with
                             None -> 0., 0.
                             |Some key -> //match dx and dy to a keyboard button (letter -> dx,dy)
                                          match key.KeyChar with 
                                               'w' -> 0., -1.
                                             | 'a' -> -2.,0.
                                             | 's' -> 0., 1.
                                             | 'd' -> 2.,0.
                                             //Show solution of the maze
                                             | 'e' -> st.status <- Status.ShowSolution
                                                      stopWatch.Restart()
                                                      st.player.clear

                                                      List.iter (fun (en:enemy) -> engine.removeSprite(en.enemy_sprite)) enemiesList
                                                      List.iter(fun (e:enemy) -> e.enemy_sprite.clear) enemiesList
                                                      enemiesList <- []
                                                      List.iter(fun (c:MazeCell) -> if(c.isFood) then st.maze.draw_line(c.position.X * 2, c.position.Y, c.position.X * 2 + 1, c.position.Y, pixel.create(' ', Color.Black)) ) myMaze.Value.maze

                                                      0.,0.
                                             //Ends the game and return to main memu
                                             | 'q' -> stopWatch.Restart()
                                                      returnToMenu st screen sp
                                                      0., 0.
                                             | ' ' -> if (st.mode = Mode.PacMan && canUseDrill = true ) then st.drill <- true  
                                                                                                             stopWatch.Restart()
                                                      0.,0.

                                             | _   -> 0., 0.
                         let nextPosition: Vector = new Vector(int(st.player.x + dx) / 2, int(st.player.y + dy))
                         let nextCell: MazeCell = myMaze.Value.getCell(nextPosition)

                         //The player moves only in the corridors not in the walls but if drill move is active, it destroys the wall
                         if not(nextCell.isWall) then st.player.move_by(dx, dy)
                         else if(st.mode = Mode.PacMan && st.drill = true && nextCell.position.isInsideMaze myMaze.Value.W myMaze.Value.H ) then 
                                                                                                                                                nextCell.isWall <- false
                                                                                                                                                st.maze.draw_line(nextCell.position.X * 2, nextCell.position.Y, nextCell.position.X * 2 + 1, nextCell.position.Y, pixel.create(' ', Color.Black))
                                                            
                         List.iter(fun (en:enemy) ->
                              
                                      if(en.enemy_sprite.x = st.player.x && en.enemy_sprite.y = st.player.y) then 
                                                                                                                  sp.SetSounds true false SPLose
                                                                                                                  List.iter(fun (e:enemy) -> e.enemy_sprite.clear) enemiesList
                                                                                                                  enemiesList <- []
                                                                                                                  st.maze.clear
                                                                                                                  st.player.clear
                                                                                                                  List.iter (fun (en:enemy) -> engine.removeSprite(en.enemy_sprite)) enemiesList
                                                                                                                  
                                                                                                                  st.status <- Status.Lose                                                                    
                                      ) enemiesList

                         //score of fruit
                         if(nextCell.isFood) then nextCell.isFood <- false
                                                  st.maze.draw_line(nextCell.position.X * 2, nextCell.position.Y, nextCell.position.X * 2 + 1, nextCell.position.Y, pixel.create(' ', Color.Black))
                                                  score <- score + 1
                                                       

                         //If the player position is in the ending position of the maze then the player has won the game 
                         if(nextCell.position.isSameAs(endPosition)) then sp.SetSounds true true SPVictory
                                                                          st.status <- Status.Victory
                         st, false
                    //If the player wins the game status <- Victory
                    elif st.status = Status.Victory then 
                         st.maze.clear
                         st.player.clear

                         List.iter (fun (en:enemy) -> engine.removeSprite(en.enemy_sprite)) enemiesList
                         List.iter(fun (e:enemy) -> e.enemy_sprite.clear) enemiesList
                         enemiesList <- []

                         screen.draw_text(Config.victory, 0, 0, Color.of_rgb(byte(myRandom.Next 255),byte(myRandom.Next 255),byte(myRandom.Next 255)))

                         //At any key pressed, returning to main menu
                         if(keyo.IsSome) then returnToMenu st screen sp
                         st,false

                    //When player loses status <- Lose
                    elif(st.status = Status.Lose) then
                         
                         screen.draw_text(Config.lose, 5, 0, Color.DarkRed)
                         //Returning to main menu on any key
                         if(keyo.IsSome) then sp.SetSounds true false SPLose
                                              sp.Play()
                                              returnToMenu st screen sp
                         st,false

                    //Showing solution to player
                    elif st.status = Status.ShowSolution then
                        //Solution is visible for three seconds
                        let remainingTime = 3 - stopWatch.Elapsed.Seconds
                        //If in blind mode, all the maze needs to be displayed
                        if st.mode = Mode.Blind then List.iter (fun (cell:MazeCell) ->
                            if cell.isWall then st.maze.draw_line(cell.position.X * 2, cell.position.Y, cell.position.X * 2 + 1, cell.position.Y, pixel.create('\219', Color.DarkGray))) myMaze.Value.maze
                        //Requesting the shortest solution only once
                        if mazeSolution.Length = 0 then let startCell = myMaze.Value.getCell(startPosition)
                                                        //Initial cell weight is zero
                                                        startCell.weight <- 0
                                                        //mazeSolution contains the list of MazeCell of the solution
                                                        mazeSolution <- myMaze.Value.weightedSolution([startCell])
                                                        //spriteSolution contains all the sprites intantiatiated to display the solution
                                                        spriteSolution <- []      
                                                        //For each cell in mazeSolution, a new sprite is created and added to spriteSolution
                                                        List.iter (fun (cell:MazeCell) -> (spriteSolution <- (engine.create_and_register_sprite (image.rectangle(2, 1, pixel.create('\219', Color.DarkCyan)), cell.position.X * 2, cell.position.Y, 1)) :: spriteSolution)) mazeSolution
                        
                        //If the remaining time is elapsed the player loses status <- Lose
                        if(remainingTime <= 0) then st.status <- Status.Lose
                                                    sp.SetSounds true false SPLose
                                                    sp.Play()
                                                    st.maze.clear

                                                    List.iter(fun (e:enemy) -> e.enemy_sprite.clear) enemiesList
                                                    enemiesList <- []
                                                    List.iter (fun (en:enemy) -> engine.removeSprite(en.enemy_sprite)) enemiesList

                                                    //all sprites in spriteSolution are removed
                                                    List.iter (fun (mySprite:sprite) -> engine.removeSprite(mySprite)) spriteSolution
                                                    //mazeSolutin is cleared
                                                    mazeSolution <- []
                        st, false

                    //When player clicks on the "Keys" button on the menu the status <- KeysMenu
                    elif st.status = Status.KeysMenu then 
                        st.menuArrow.clear
                        screen.draw_text(Config.instructionMenu, 19, 4, Color.Green)
                        //At any key pressed returning to main menu
                        if(keyo.IsSome) then sp.SetSounds false false SPIntro 
                                             returnToMenu st screen sp
                        st, false

                    //If in SelectMode, player has to choose the gamemode
                    elif st.status = Status.SelectMode then 
                        
                        //Draw the game menu screen on the screen 
                        screen.draw_text(Config.menuScreen, 39, 4, Color.Green)
                        
                        //Draw the game menu buttons on the screen
                        drawButtons modeButtons menuYstart menuYIncrease screen



                        sp.SetSounds true false SPMenu
                        //Handling button actions
                        executeIfButtonPressed modeButtons menuYstart menuYIncrease sp keyo st (fun (buttonAction:ButtonAction) ->
                            //Creating a new instance of Maze
                            myMaze <- Some(new Maze(W / 2, H-2, startPosition, endPosition, sameDirectionMin, sameDirectionMax, linkPaths))
                            match buttonAction with
                                ButtonAction.Arcade -> sp.SetSounds true true SPArcade 
                                                       st.mode <- Mode.Arcade
                                                       st.status <- Status.InGame                                              
                                |ButtonAction.Blind -> sp.SetSounds true true SPBlind
                                                       st.mode <- Mode.Blind
                                                       st.status <- Status.InGame
                                                       
                                |ButtonAction.PacMan ->sp.SetSounds true true SPTimed 
                                                       st.mode <- Mode.PacMan
                                                       st.status <- Status.InGame

                                                       //generate enemies in random positions of the maze given the maximun and the minimum number of enemies
                                                       let max_enemies=7                                                       
                                                       let min_enemies=4
                                                       generateEnemies(min_enemies,max_enemies,engine,myMaze.Value)    
                                |_ -> ignore()
                            


   
                            st.player.drawSprite (pixel.create ('\219',Color.Cyan))
                            //Reset position to the starting position
                            st.player.x <- (float(startPosition.X*2))
                            st.player.y <- float(startPosition.Y)
                            st.menuArrow.clear
                            
                            )
                        st, false
                    else st, false

            //Creating the sprite menuArrow        
            let menuArrow = engine.create_and_register_sprite (image.rectangle (1,1,pixel.create('>', Color.White)), 25, 11, 1)
            //Creating the sprite Maze
            let Maze = engine.create_and_register_sprite (image.rectangle (W,H, pixel.create(' ',Color.DarkBlue)),0,0,1)

            //initialize engine state
            let st0 = {
                    player = player
                    enemies = []
                    menuArrow = menuArrow
                    maze = Maze
                    mode = Mode.Arcade
                    status = Status.KeysMenu
                    drill = false
                    }
            engine.show_fps <- false
            //start engine
            engine.loop myLoop st0