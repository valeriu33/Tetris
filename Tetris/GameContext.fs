module Tetris.GameContext

open Tetris.Figure
open FSharpPlus
open FSharpPlus.Lens
open Microsoft.FSharp.Core
    
type GameAction =
    | Left
    | Right
    | Rotate
    | FastDown
    | NoAction
     
type GameContext = {
    ExitGame: bool
    Map: MapPixel [,]
    Action: GameAction
    ActiveFigure: Figure
}

let inline _changeFigure f g =
    f g.ActiveFigure <&> fun x -> { g with ActiveFigure = x }
let inline _changeAction f g =
    f g.Action <&> fun x -> { g with Action = x }
let inline _changeMap f g =
    f g.Map <&> fun x -> { g with Map = x }
let inline _exitGame f g =
    f g.ExitGame <&> fun x -> { g with ExitGame = x }
let inline _x_position g =
    _changeFigure << Figure._position << Position._x <| g
let inline _y_position g =
    _changeFigure << Figure._position << Position._y <| g
let inline _figureType g =
    _changeFigure << Figure._type <| g
let inline rotate g =
    _changeFigure << Figure._rotation <| g

let addGroundToMap (map: MapPixel[,]) (points: (decimal*decimal) list) =
    map |> Array2D.mapi (fun x y pixel -> if (points
                                              |> List.map (fun (x, y) -> (int x, int y))
                                              |> List.contains (x, y)) then
                                                MapPixel.Ground
                                          else
                                                pixel)

let checkAndReturnLineComplete (map: MapPixel[,]): int list =
     [0 .. Array2D.length2 map - 1]
     |> List.filter (fun y -> 
         [0 .. Array2D.length1 map - 1]
         |> List.forall (fun x -> map[x, y] = MapPixel.Ground))

let removeLineFromMap (map: MapPixel[,]) lineNum =
    let newMap =
        map
        |> Array2D.mapi (fun _ y pixel -> if (y = lineNum) then MapPixel.Empty else pixel)

    // [|Array2D.length2 newMap - 1 .. 1|]
    // |> Array.map (fun y ->
        // [|Array2D.length1 newMap - 1 .. 0|]
        // |> Array.map (fun x -> newMap[x, y - 1]))
    // |> array2D
    for y = Array2D.length2 newMap - 1 downto 1 do // TODO: rewrite in a immutable way
        for x = Array2D.length1 newMap - 1 downto 0 do
            if (y <= lineNum) then newMap[x, y] <- newMap[x, y - 1]
    
    newMap
          
let init() =
    let map = Array2D.init 10 16 (fun _ _ -> MapPixel.Empty)
    {
        ExitGame = false
        Map = map
        Action = GameAction.NoAction
        ActiveFigure = { Position = {x = 2; y = 2}; Rotation = Rotation.Deg_0; Type = Figure.getRandomFigureType () }
    }