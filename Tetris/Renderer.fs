module Tetris.Renderer

open System
open Tetris.Figure
open Tetris.GameContext
open FSharpPlus
open FSharpPlus.Lens

let drawGameOver () =
    printf @"
    ■ ■ ■   ■ ■ ■   ■   ■   ■ ■ ■
    ■       ■   ■   ■ ■ ■   ■ ■
    ■   ■   ■ ■ ■   ■   ■   ■
    ■ ■ ■   ■   ■   ■   ■   ■ ■ ■
    
    ■ ■ ■   ■   ■   ■ ■ ■   ■ ■ ■
    ■   ■   ■   ■   ■ ■     ■ ■ ■
    ■   ■   ■   ■   ■       ■ ■
    ■ ■ ■     ■     ■ ■ ■   ■   ■
    "

let drawPause ()=
    printf @"
    ■ ■ ■   ■ ■ ■   ■   ■   ■ ■ ■   ■ ■ ■
    ■ ■ ■   ■   ■   ■   ■   ■ ■ ■   ■ ■
    ■       ■ ■ ■   ■   ■       ■   ■
    ■       ■   ■   ■ ■ ■   ■ ■ ■   ■ ■ ■
    "


let printColor (figureType: FigureType) (pixelForm: string)=
    match figureType with
    | I ->
        Console.ForegroundColor <- ConsoleColor.Blue;
        printf "%s" pixelForm;
        Console.ForegroundColor <- ConsoleColor.White;
    | L ->
        Console.ForegroundColor <- ConsoleColor.DarkYellow;
        printf "%s" pixelForm;
        Console.ForegroundColor <- ConsoleColor.White;
    | FigureType.J ->
        Console.ForegroundColor <- ConsoleColor.DarkBlue;
        printf "%s" pixelForm;
        Console.ForegroundColor <- ConsoleColor.White
    | O ->
        Console.ForegroundColor <- ConsoleColor.Yellow;
        printf "%s" pixelForm;
        Console.ForegroundColor <- ConsoleColor.White
    | S ->
        Console.ForegroundColor <- ConsoleColor.Green;
        printf "%s" pixelForm;
        Console.ForegroundColor <- ConsoleColor.White
    | T ->
        Console.ForegroundColor <- ConsoleColor.Magenta;
        printf "%s" pixelForm;
        Console.ForegroundColor <- ConsoleColor.White
    | Z ->
        Console.ForegroundColor <- ConsoleColor.Red;
        printf "%s" pixelForm;
        Console.ForegroundColor <- ConsoleColor.White
    

let renderMap gameContext =
    for y = 0 to Array2D.length2 gameContext.Map - 1 do
        printf "| " // wall
        for x = 0 to Array2D.length1 gameContext.Map - 1 do
            let mapPixel = gameContext.Map[x, y]
            let activeFigureCoords = Figure.getFigurePoints gameContext.ActiveFigure |> List.map (fun (x,y) -> (int x, int y))
            match mapPixel with
            | MapPixel.Empty when activeFigureCoords |> List.contains (x, y) ->
                printColor gameContext.ActiveFigure.Type "□ "
            | MapPixel.Empty -> printf "  "
            | MapPixel.GroundPixel {FigureType = figureType; IsToDelete = false}->
                printColor figureType "□ "
            | MapPixel.GroundPixel {FigureType = figureType; IsToDelete = true}->
                printColor figureType "■ "
        printf "| " // wall
        printf "\n"
    for x = 0 to Array2D.length1 gameContext.Map + 1 do
        printf "‾ " // floor