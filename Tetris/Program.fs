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
        
        let playerMovedGC = 
            match gameContext.Action with
            | Left ->
                let movedLeftGC =
                    gameContext 
                    |> setl _x_position (gameContext.ActiveFigure.Position.x - playerSpeed)
                returnChangedIfValid (Figure.isValidPosition movedLeftGC.ActiveFigure movedLeftGC.Map) gameContext movedLeftGC
            | Right ->
                let movedRightGC =
                    gameContext
                    |> setl _x_position (gameContext.ActiveFigure.Position.x + playerSpeed)
                returnChangedIfValid (Figure.isValidPosition movedRightGC.ActiveFigure movedRightGC.Map) gameContext movedRightGC
            | Rotate ->
                let rotatedGC =
                    gameContext
                    |> setl rotate ()
                returnChangedIfValid (Figure.isValidPosition rotatedGC.ActiveFigure rotatedGC.Map) gameContext rotatedGC
            | FastDown ->
                let fastDownGC =
                    gameContext
                    |> setl _y_position (gameContext.ActiveFigure.Position.y + playerSpeed)
                if Figure.isGroundedPosition fastDownGC.ActiveFigure fastDownGC.Map then
                    let newMap = addGroundToMap fastDownGC.Map (fastDownGC.ActiveFigure)
                    let mapCompleteLine =
                        checkAndReturnLinesComplete newMap
                        |> List.fold removeLineFromMap newMap
                    fastDownGC
                    |> setl _changeMap mapCompleteLine
                    |> setl _changeFigure (Figure.getNewFigure())
                else
                    fastDownGC
            | NoAction -> gameContext
        
        
        // TODO: It is repeating the FastDown
        let gravityMovedGC = playerMovedGC |> setl _y_position (playerMovedGC.ActiveFigure.Position.y + fallingSpeed)
        if Figure.isGroundedPosition gravityMovedGC.ActiveFigure gravityMovedGC.Map then
            let mapAddGround = addGroundToMap gravityMovedGC.Map (gravityMovedGC.ActiveFigure)
            // 
            let mapCompleteLine =
                checkAndReturnLinesComplete mapAddGround
                |> List.fold removeLineFromMap mapAddGround
            let returnGC =
                gravityMovedGC
                |> setl _changeMap mapCompleteLine
                |> setl _changeFigure (Figure.getNewFigure())
                |> setl _changeAction GameAction.NoAction
            
            if Figure.isGameOverPosition returnGC.ActiveFigure returnGC.Map then
                returnGC |> setl _exitGame true
            else
                returnGC
        else
            gravityMovedGC
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