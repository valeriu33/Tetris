module Tetris.Renderer

open Tetris.Figure
open Tetris.GameContext
open FSharpPlus
open FSharpPlus.Lens

let drawGameOver =
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

let renderMap gameContext =
    for y = 0 to Array2D.length2 gameContext.Map - 1 do
        printf "| " // frame
        for x = 0 to Array2D.length1 gameContext.Map - 1 do
            let mapPixel = gameContext.Map[x, y]
            let activeFigureCoords = Figure.getFigurePoints gameContext.ActiveFigure |> List.map (fun (x,y) -> (int x, int y))
            match mapPixel with
            | MapPixel.Empty when activeFigureCoords |> List.contains (x, y) -> printf "□ "
            | MapPixel.Empty -> printf "  "
            | MapPixel.Ground-> printf "□ "
        printf "| " // frame
        printf "\n"
    for x = 0 to Array2D.length1 gameContext.Map + 1 do
        printf "‾ " // frame