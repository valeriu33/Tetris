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


let printColor (figureType: FigureType) =
    match figureType with
    | I ->
        Console.ForegroundColor <- ConsoleColor.Blue;
        printf "□ ";
        Console.ForegroundColor <- ConsoleColor.White;
    | L ->
        Console.ForegroundColor <- ConsoleColor.DarkYellow;
        printf "□ ";
        Console.ForegroundColor <- ConsoleColor.White;
    | FigureType.J ->
        Console.ForegroundColor <- ConsoleColor.DarkBlue;
        printf "□ "
        Console.ForegroundColor <- ConsoleColor.White
    | O ->
        Console.ForegroundColor <- ConsoleColor.Yellow;
        printf "□ "
        Console.ForegroundColor <- ConsoleColor.White
    | S ->
        Console.ForegroundColor <- ConsoleColor.Green;
        printf "□ "
        Console.ForegroundColor <- ConsoleColor.White
    | T ->
        Console.ForegroundColor <- ConsoleColor.Magenta;
        printf "□ "
        Console.ForegroundColor <- ConsoleColor.White
    | Z ->
        Console.ForegroundColor <- ConsoleColor.Red;
        printf "□ "
        Console.ForegroundColor <- ConsoleColor.White
    

let renderMap gameContext =
    for y = 0 to Array2D.length2 gameContext.Map - 1 do
        printf "| " // frame
        for x = 0 to Array2D.length1 gameContext.Map - 1 do
            let mapPixel = gameContext.Map[x, y]
            let activeFigureCoords = Figure.getFigurePoints gameContext.ActiveFigure |> List.map (fun (x,y) -> (int x, int y))
            match mapPixel with
            | MapPixel.Empty when activeFigureCoords |> List.contains (x, y) -> printColor gameContext.ActiveFigure.Type
            | MapPixel.Empty -> printf "  "
            | MapPixel.Ground figureType->
                printColor figureType
        printf "| " // frame
        printf "\n"
    for x = 0 to Array2D.length1 gameContext.Map + 1 do
        printf "‾ " // frame