module Tetris.GameContext

open Tetris.Figure
open FSharpPlus
open FSharpPlus.Lens
open Microsoft.FSharp.Core

type Ground =
    { FigureType: FigureType
      IsToDelete: bool }

type MapPixel =
    | Empty
    | GroundPixel of Ground
    member this.isGround =
        match this with
        | GroundPixel _ -> true
        | Empty -> false
        
type GameAction =
    | Left
    | Right
    | Rotate
    | FastDown
    | NoAction


type GameContext =
    { ExitGame: bool
      Map: MapPixel[,]
      Action: GameAction
      ActiveFigure: Figure }

let inline _changeFigure f g =
    f g.ActiveFigure <&> fun x -> { g with ActiveFigure = x }

let inline _changeAction f g =
    f g.Action <&> fun x -> { g with Action = x }

let inline _changeMap f g = f g.Map <&> fun x -> { g with Map = x }

let inline _exitGame f g =
    f g.ExitGame <&> fun x -> { g with ExitGame = x }

let inline _x_position g =
    _changeFigure << Figure._position << Position._x <| g

let inline _y_position g =
    _changeFigure << Figure._position << Position._y <| g

let inline _figureType g = _changeFigure << Figure._type <| g
let inline rotate g = _changeFigure << Figure._rotation <| g

let isValidPosition figure (map: MapPixel[,]) =
    Figure.getFigurePoints figure
    |> List.forall (fun (x, y) ->
        int y < Array2D.length2 map
        && int x < Array2D.length1 map
        && int x >= 0
        && map[int x, int y] = MapPixel.Empty)

let ceilInt = ceil<decimal> >> int<decimal>

let isGroundedPosition figure (map: MapPixel[,]) =
    Figure.getFigurePoints figure
    |> List.exists (fun (x, y) ->
        ((y |> ceilInt) + 1 >= Array2D.length2 map)
        || (map[x |> ceilInt, (y |> ceilInt) + 1].isGround))

let isGameOverPosition figure (map: MapPixel[,]) =
    Figure.getFigurePoints figure
    |> List.exists (fun (x, y) -> map[int x, int y].isGround)

let addGroundToMap (map: MapPixel[,]) (figure: Figure) =
    let points = Figure.getFigurePoints figure

    map
    |> Array2D.mapi (fun x y pixel ->
        if
            (points
             |> List.map (fun (x, y) -> (x |> ceilInt, y |> ceilInt))
             |> List.contains (x, y))
        then
            MapPixel.GroundPixel
                { FigureType = figure.Type
                  IsToDelete = false }
        else
            pixel)

let checkAndReturnLinesComplete (map: MapPixel[,]) : int list =
    [ 0 .. Array2D.length2 map - 1 ]
    |> List.filter (fun y -> [ 0 .. Array2D.length1 map - 1 ] |> List.forall (fun x -> map[x, y].isGround))

let checkAndReturnLinesToDelete (map: MapPixel[,]) =
    [ 0 .. Array2D.length2 map - 1 ]
    |> List.filter (fun y ->
        match map[0, y] with
        | GroundPixel { IsToDelete = false } -> true
        | _ -> false)


let hasLineToDelete (map: MapPixel[,]) =
    [ 0 .. Array2D.length2 map - 1 ]
    |> List.exists (fun y ->
        match map[0, y] with
        | GroundPixel { IsToDelete = false } -> true
        | _ -> false)

let markLineToDelete (map: MapPixel[,]) lineNum =
    map
    |> Array2D.mapi (fun _ y i ->
        if y = lineNum then
            match i with
            | GroundPixel { FigureType = f; IsToDelete = _ } -> GroundPixel { FigureType = f; IsToDelete = true }
            | _ -> i
        else
            i)

let removeLineFromMap (map: MapPixel[,]) lineNum =
    // Move down the ground above the line
    [| 0 .. Array2D.length1 map - 1 |]
    |> Array.map (fun x ->
        [| 0 .. Array2D.length2 map - 1 |]
        |> Array.map (fun y ->
            if y <= lineNum then
                if y <> 0 then map[x, y - 1] else map[x, y]
            else
                map[x, y]))
    |> array2D

let init () =
    let map = Array2D.init 10 16 (fun _ _ -> MapPixel.Empty)

    { ExitGame = false
      Map = map
      Action = GameAction.NoAction
      ActiveFigure = Figure.getNewFigure () }
