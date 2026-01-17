module LabProg2019.MazeGenerator

open System

///Instance of Random
let myRandom = new Random()

[<NoComparison;NoEquality>]
///Type used for the direction 
type Vector (x:int, y:int) =
    let mutable x = x
    let mutable y = y

    ///Return the X coord (read-only)
    member this.X with get() = x
    ///Return the Y coord (read-only)
    member this.Y with get() = y

    ///Return if this position is inside a maze
    member this.isInsideMaze (W:int) (H:int):bool =
        this.X >= 1 && this.X < (W-1) && this.Y >= 1 && this.Y < (H-1)

    ///Return a new Vector with translated coords
    member this.getTranslated (direction:Vector) = new Vector(this.X + direction.X, this.Y + direction.Y)

    ///Checks for equality on X and Y coords
    member this.isSameAs (other:Vector) = this.X = other.X && this.Y = other.Y

    member this.oppsiteDirection() = new Vector(this.X * (-1) , this.Y * (-1) )

    ///Returns a random new Vector not opposite
    member this.compatibleDirection () =
        let mutable newX = myRandom.Next(3)  - 1
        let mutable newY = myRandom.Next(3)  - 1
        while (newX + this.X) = 0 && (newY + this.Y) = 0 do
            newX <- myRandom.Next(3)  - 1
            newY <- myRandom.Next(3)  - 1
        new Vector (newX, newY)

///Single cell class
type MazeCell (x:int, y:int, isWall:bool) =
    
    let mutable mutableIsWall:bool = isWall
    let mutable mutableVisited: bool = false
    let mutable mutableBlocked: bool = false
    let mutable mutablePosition: Vector = new Vector(x, y)
    let mutable mutableWeight:int = -1
    let mutable isFruit:bool=false

    ///If false, the cell is a way in the maze
    member this.isWall with get() = mutableIsWall and set(value) = mutableIsWall <- value
    ///Marks if the cell has been elaborated by any alghoritm. An unvisited cell has the highest priority in the maze generation alghoritm
    member this.isVisited with get() = mutableVisited and set(value) = mutableVisited <- value
    ///Marks if the cell is temporarily considered wall. A blocked cell (unvisited) comes second for priority in the maze generation alghoritm
    member this.isBlocked with get() = mutableBlocked and set(value) = mutableBlocked <- value
    ///Reurns the position Vector of the cell (read-only)
    member this.position with get() = mutablePosition
    
    ///getter and setter of the fruits
    member this.isFood with get() = isFruit and set(value) = isFruit <- value   
    
    //Weight indicates how far from the start of the maze. Used to find the shortest solution
    member this.weight with get() = mutableWeight and set(value) = mutableWeight <- value

///Return the distance between points
let distanceBetweenPoints (x1:int, y1:int, x2:int, y2:int) =
    sqrt (float(((pown (x1 - x2) 2) + pown (y1 - y2) 2)))



///Class for maze. Requires a starting position, an end position, min and max corridor length
type Maze (W:int, H:int, startPosition:Vector, endPosition:Vector, sameDirectionIntervalMin:int, sameDirectionIntervalMax:int, linkSomePaths:bool) =
    let w = W
    let h = H
    let mutable mutableMaze = List.init (W * H) (fun (cellIndex) -> new MazeCell(cellIndex % w, cellIndex / w, true))
    
    let privateGetCell (position:Vector):MazeCell = mutableMaze.[(position.Y * w) + position.X]

    ///Return all adiacent cells (E/N/W/S) if they're inside the maze
    let privateGetAdiacentCells (cell:MazeCell) (endPosition:Vector)=
        let mutable resultCells: MazeCell list = []

        let addIfInsideMaze (position:Vector) =
            if position.isInsideMaze w h || position.isSameAs(endPosition) then resultCells <- resultCells @ [privateGetCell position]
        
        //Est cell
        let direction = new Vector(-1, 0)
        addIfInsideMaze (cell.position.getTranslated(direction))
        //Noth cell
        let direction = new Vector(0, 1)
        addIfInsideMaze (cell.position.getTranslated(direction))
        //West cell
        let direction = new Vector(1, 0)
        addIfInsideMaze (cell.position.getTranslated(direction))
        //South cell
        let direction = new Vector(0, -1)
        addIfInsideMaze (cell.position.getTranslated(direction))

        resultCells

    ///At the end of the maze generation, all blocked cells needs to be considered wall
    let makeWallIfBlocked () =
        List.iter (fun (cell:MazeCell) -> if cell.isBlocked then cell.isWall <- true) mutableMaze

    ///Reset of isBlocked and isVisited properties of all cells (cell.isBlocked <- false / cell.isVisited <- false)
    let resetCellsStatus () =
        List.iter (fun (cell:MazeCell) -> cell.isBlocked <- false
                                          cell.isVisited <- false) mutableMaze

    ///Links the exit cell with the path in case of unreached exit
    let rec linkExit (cell:MazeCell) (direction:Vector) =
        if not(cell.isVisited) then
            cell.isWall <- false
            cell.isVisited <- true
            linkExit (privateGetCell (cell.position.getTranslated(direction))) direction

    ///Links to aligned cells separated by a single wall. Used to obtain multiple paths
    let linkPaths () =
        let percentageOfLink = 10
        List.iter (fun (cell:MazeCell) -> if cell.isWall then let adiacentCells: MazeCell list = privateGetAdiacentCells cell endPosition
                                                              let notWallCells:MazeCell list = List.filter (fun (adjCell:MazeCell) -> not(adjCell.isWall)) adiacentCells
                                                              let sameX:bool = notWallCells.Length = 2 && notWallCells.[0].position.X = notWallCells.[1].position.X
                                                              let sameY:bool = notWallCells.Length = 2 && notWallCells.[0].position.Y = notWallCells.[1].position.Y
                                                              if (sameX || sameY) && myRandom.Next(100) <= percentageOfLink then cell.isWall <- false
        ) mutableMaze

    //function that scans the maze cell and randomly set them as fruits
    let decideIfFruit() = List.iter(fun (cell:MazeCell) -> if not(cell.isWall || cell.position.isSameAs(endPosition) 
                                                                              || cell.position.isSameAs(startPosition) ) then let food=myRandom.Next(0,100)
                                                                                                                              if(food>90)then cell.isFood <- true) mutableMaze
    

    ///Generation of the maze
    let rec generateMaze (startPosition:Vector, endPosition:Vector, sameDirectionIntervalMin:int, sameDirectionIntervalMax:int, linkSomePaths:bool, isPacMan:bool) =
        //Init
        let startCell = privateGetCell startPosition
        startCell.isVisited <- true
        startCell.isWall <- false
        //Starting cell added to mazePath
        let mutable mazePath:MazeCell list = [startCell]

        ///Used to create corridors of different lenght
        let mutable mantainDirectionFor = myRandom.Next(sameDirectionIntervalMin, sameDirectionIntervalMax)
        let mutable currentDirection = new Vector(1, 0)

        //MazePath will be empty if all the maze has been explored
        while mazePath.Length > 0 do
            ///Returns all adiacent cells
            let adiacentCells:MazeCell list = privateGetAdiacentCells(privateGetCell mazePath.Head.position) (endPosition)
            ///Sublist of not visited cells
            let notVisitedCells = List.filter (fun (cell:MazeCell) -> not(cell.isVisited)) adiacentCells
            ///Sublist of not blocked cells (these will be explored first)
            let notBlockedList = List.filter (fun (cell:MazeCell) -> not(cell.isBlocked))  notVisitedCells
                    
            //Not blocked (and not visited) cells have highest priority
            if notBlockedList.Length > 0 then
                //Init nextCell
                let mutable nextCell = notBlockedList.[0]

                //Check if current direction can be mantained by the alghoritm
                let canMantainDirection = List.exists (fun (cell:MazeCell) -> (cell.position.X - mazePath.Head.position.X) = currentDirection.X && (cell.position.Y - mazePath.Head.position.Y) = currentDirection.Y) notBlockedList

                //If needed, change direction
                if mantainDirectionFor <= 0 || not(canMantainDirection) then
                    //Choose rendom direction
                    nextCell <- notBlockedList.[myRandom.Next(notBlockedList.Length)]
                    currentDirection <- new Vector(nextCell.position.X - mazePath.Head.position.X, nextCell.position.Y - mazePath.Head.position.Y)
                    //Reset countdown for new change of direction
                    mantainDirectionFor <- myRandom.Next(sameDirectionIntervalMin, sameDirectionIntervalMax)
                //Otherwise, mantain same direction
                else nextCell <- List.find (fun (cell:MazeCell) -> (cell.position.X - mazePath.Head.position.X) = currentDirection.X && (cell.position.Y - mazePath.Head.position.Y) = currentDirection.Y) notBlockedList
                
                //Decrease countdown for direction change
                mantainDirectionFor <- mantainDirectionFor - 1
                //Set isBlocked to all adiacent cells except the next one
                List.iter (fun (cell:MazeCell) -> if (cell <> nextCell) then cell.isBlocked <- true) notVisitedCells
                //Set nextCell visited and not wall
                nextCell.isVisited <- true
                nextCell.isWall <- false
                //Proceed with iteration
                mazePath <- nextCell::mazePath

            //Otherwise, if cell has no not blocked cells, blocked cells have second priority
            else
                //Check if a blocked cell can be unblocked
                let existsUnblockableCell = List.tryFind (fun (cell:MazeCell) -> 
                            (*Requirement for a blocked cell in order to be unblocked
                            is that it can't have more than 1 not wall cell adiacent*)
                            let adiacentOfBlockedCell = privateGetAdiacentCells cell endPosition
                            let visitedOfAdiacent = List.filter (fun (cell:MazeCell)-> cell.isVisited) adiacentOfBlockedCell
                            visitedOfAdiacent.Length < 2 ) notVisitedCells

                match existsUnblockableCell with
                //If a cell meets the requirements, unblock it
                Some value ->
                    value.isVisited <- true
                    value.isWall <- false
                    value.isBlocked <- false
                    //And proceed with iterations
                    mazePath <- value::mazePath
                //Otherwise this current cell is a dead-end, so turn back
                |None ->
                    mazePath <- mazePath.Tail

        //Post-generations adjustments:
        //Blocked cells are wall
        ignore(makeWallIfBlocked())
        //Link exit
        let linkToDirection = match (endPosition.X, endPosition.Y) with
                              (0, _) -> new Vector(1, 0)
                              |(_, 0) -> new Vector(0, 1)
                              |(_, _) when endPosition.Y = (H - 1) -> new Vector(0, -1)
                              |(_, _) -> new Vector(-1, 0)
        ignore(linkExit (privateGetCell endPosition) linkToDirection)
        //Reset cell status
        ignore(resetCellsStatus())
        //Link two paths
        if linkSomePaths then ignore(linkPaths())

        //chooses random not wall or blocked positions and makes them fruits
        decideIfFruit()

         
    //On instantiation, generate the maze
    do generateMaze(startPosition, endPosition, sameDirectionIntervalMin, sameDirectionIntervalMax, linkSomePaths,false)




    ///Getter for maze width
    member this.W with get() = w
    ///Getter for maze height
    member this.H with get() = h
    ///Getter for all cells in the maze
    member this.maze with get() = mutableMaze

    ///Returns the cell in a specified position
    member this.getCell (position:Vector):MazeCell = privateGetCell position

    ///Returns the adiacent cells in a specified position
    member this.getAdiacentCells (cell:MazeCell):MazeCell list = privateGetAdiacentCells cell endPosition

    ///Assigns to every cell a weight and follows the lower weighted path
    member this.weightedSolution (currentProcessCells:MazeCell list) =
        //Cell that will be evaluated in next recursion
        let mutable nextCellToProcess = []
        //For each cell to evaluate
        List.iter (fun (currentCell:MazeCell) -> 
            //Get a list of all adiacent cells not wall
            let adiacentNotWallCells = List.filter (fun (cell:MazeCell) -> not(cell.isWall)) (privateGetAdiacentCells currentCell endPosition)
            
            List.iter (fun (cell:MazeCell) -> 
                //For each of them, if the cell has more weight of the current cell it needs to be revaluated
                if cell.weight > currentCell.weight + 1 || cell.weight = -1 then
                    //So change its weight
                    cell.weight <- currentCell.weight + 1
                    //And add to next cell to process
                    nextCellToProcess <- cell::nextCellToProcess
            ) adiacentNotWallCells
        ) currentProcessCells
        //Repeat if more cells need to be processed
        if nextCellToProcess.Length > 0 then this.weightedSolution nextCellToProcess
        //Else start backtracking alghoritm
        else let rec backTracking (cell:MazeCell) =
                //Selecting all adiacent not walls cells
                let adiacentNotWallCells = List.filter (fun (cell:MazeCell) -> not(cell.isWall)) (privateGetAdiacentCells cell endPosition)
                //Get the cell with lowest weight
                let lowestCell = List.minBy (fun (adiacentCell:MazeCell) -> adiacentCell.weight) adiacentNotWallCells
                //If adiacent cell ha lower weight add current cell to solution and repeat recursively
                if lowestCell.weight < cell.weight then
                    cell::backTracking lowestCell
                //Otherwise the path is finished 

                else cell::[privateGetCell startPosition]
             //Starting from the end
             in backTracking (privateGetCell endPosition)

             

let rec sottr n =
    if (n>=0) then
        n::(sottr(n-1))
    else
        []
                               