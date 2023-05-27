open System
open Tetris
open Tetris.Figure
open Tetris.GameContext
open FSharpPlus.Lens
open Microsoft.FSharp.Core

module Game =
    let speedCoefficient = 1m;
    let fallingSpeed = 0.20m * speedCoefficient;
    let playerSpeed = 1m * speedCoefficient;
    
    let gameLoop gameContext =
        let returnChangedIfValid (validation:bool) (oldGC:GameContext) (newGC:GameContext): GameContext =
            match validation with
            | true -> newGC
            | false -> oldGC
        
        let goDown gc distance=
            let fastDownGC =
                gc
                |> setl _y_position (gc.ActiveFigure.Position.y + distance)
            if isGroundedPosition fastDownGC.ActiveFigure fastDownGC.Map then
                let newMap = addGroundToMap fastDownGC.Map (fastDownGC.ActiveFigure)
                let mapCompleteLine =
                    checkAndReturnLinesComplete newMap
                    |> List.fold markLineToDelete newMap
                let returnGC =
                    fastDownGC
                    |> setl _changeMap mapCompleteLine
                    |> setl _changeFigure (Figure.getNewFigure())
                if isGameOverPosition returnGC.ActiveFigure returnGC.Map then
                    returnGC |> setl _exitGame true
                else
                    returnGC
            else
                fastDownGC
        
        let map =
            if hasLineToDelete gameContext.Map then
                checkAndReturnLinesComplete gameContext.Map
                 |> List.fold removeLineFromMap gameContext.Map
            else
                gameContext.Map

        let cleanGC = gameContext |> setl _changeMap map
        
        let playerMovedGC = 
            match cleanGC.Action with
            | Left ->
                let movedLeftGC =
                    cleanGC 
                    |> setl _x_position (cleanGC.ActiveFigure.Position.x - playerSpeed)
                returnChangedIfValid (isValidPosition movedLeftGC.ActiveFigure movedLeftGC.Map) cleanGC movedLeftGC
            | Right ->
                let movedRightGC =
                    cleanGC
                    |> setl _x_position (cleanGC.ActiveFigure.Position.x + playerSpeed)
                returnChangedIfValid (isValidPosition movedRightGC.ActiveFigure movedRightGC.Map) cleanGC movedRightGC
            | Rotate ->
                let rotatedGC =
                    cleanGC
                    |> setl rotate ()
                returnChangedIfValid (isValidPosition rotatedGC.ActiveFigure rotatedGC.Map) cleanGC rotatedGC
            | FastDown ->
                goDown cleanGC playerSpeed
            | NoAction -> cleanGC
        

        
        // let finalGC =
        goDown playerMovedGC fallingSpeed
        |> setl _changeAction GameAction.NoAction
        
        
        
        
let rec main gameContext =
    
    if gameContext.ExitGame then
        Renderer.drawGameOver ()
        Environment.Exit 0
    Console.Clear()
    Renderer.renderMap gameContext
    Threading.Thread.Sleep(150)
    
    if Console.KeyAvailable then
        // while(Console.KeyAvailable) do
        //     Console.ReadKey(false) |> ignore // skips previous input chars
        match Console.ReadKey().Key with
        | ConsoleKey.Q -> Renderer.drawGameOver (); Environment.Exit 0
        | ConsoleKey.UpArrow
        | ConsoleKey.W ->
            main(Game.gameLoop({ gameContext with Action = GameAction.Rotate }))
        | ConsoleKey.DownArrow
        | ConsoleKey.S ->
            main(Game.gameLoop({ gameContext with Action = GameAction.FastDown }))
        | ConsoleKey.LeftArrow
        | ConsoleKey.A ->
            main(Game.gameLoop({ gameContext with Action = GameAction.Left }))
        | ConsoleKey.RightArrow
        | ConsoleKey.D ->
            main(Game.gameLoop({ gameContext with Action = GameAction.Right }))
        | ConsoleKey.P ->
            Console.Clear()
            Renderer.drawPause ();
            let _ = Console.ReadKey()
            main(Game.gameLoop(gameContext))
        | _ -> main(Game.gameLoop(gameContext))
    else
        main(Game.gameLoop(gameContext))
    
    
main (init())