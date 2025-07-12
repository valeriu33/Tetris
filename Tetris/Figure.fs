module Tetris.Figure

open System
open FSharpPlus
open FSharpPlus.Lens

type Position = { x: decimal; y: decimal }
module Position =
    let inline _x f p =
        f p.x <&> fun x -> { p with x = x }
    let inline _y f p =
        (f p.y) <&> (fun x -> { p with y = x })
        
type FigureType =
    | I
    | L
    | J
    | O
    | S
    | T
    | Z

type Rotation =
    | Deg_0
    | Deg_90
    | Deg_180
    | Deg_270
    member r.Turn90Deg_counterClockwise =
        match r with
        | Deg_0 -> Deg_90
        | Deg_90 -> Deg_180
        | Deg_180 -> Deg_270
        | Deg_270 -> Deg_0
        
    member r.Turn90Deg_Clockwise =
        match r with
        | Deg_0 -> Deg_270
        | Deg_90 -> Deg_0
        | Deg_180 -> Deg_90
        | Deg_270 -> Deg_180

type Figure = {
    Position: Position
    Type: FigureType
    Rotation: Rotation
}

module Figure =
    let inline public _position f p =
        f p.Position <&> fun x -> { p with Position = x }
        
    let inline public _rotation f p =
        f p.Rotation <&> fun _ -> { p with Rotation = p.Rotation.Turn90Deg_Clockwise }
    
    let inline _type f p =
        f p.Type <&> fun x -> { p with Type = x }
    
    let inline getRandomFigureType () =
        let num = Random().Next(7)
        match num with
        | 0 -> I
        | 1 -> L
        | 2 -> J
        | 3 -> O
        | 4 -> S
        | 5 -> T
        | 6 -> Z
        | _ -> failwith "todo"

        
    let getFigurePoints (figure: Figure) =
        match figure.Type with
        | I ->
            match figure.Rotation with
            // □
            // ■
            // □
            // □
            | Deg_0
            | Deg_180 ->
                [
                 (figure.Position.x, figure.Position.y - 1m)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x, figure.Position.y + 1m)
                 (figure.Position.x, figure.Position.y + 2m)
                ]
            // □ ■ □ □
            | Deg_90
            | Deg_270 ->
                [
                 (figure.Position.x - 1m, figure.Position.y)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x + 1m, figure.Position.y)
                 (figure.Position.x + 2m, figure.Position.y)
                ]

        | L ->
            match figure.Rotation with
            // □
            // ■
            // □ □
            | Deg_0 ->
                [
                 (figure.Position.x, figure.Position.y - 1m)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x, figure.Position.y + 1m)
                 (figure.Position.x + 1m, figure.Position.y + 1m)
                ]
            //     □
            // □ ■ □
            | Deg_90 ->
                [
                 (figure.Position.x - 1m, figure.Position.y)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x + 1m, figure.Position.y)
                 (figure.Position.x + 1m, figure.Position.y - 1m)
                ]
            // □ □
            //   ■
            //   □
            | Deg_180 ->
                [
                 (figure.Position.x - 1m, figure.Position.y - 1m)
                 (figure.Position.x, figure.Position.y - 1m)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x, figure.Position.y + 1m)
                ]
            // □ ■ □
            // □
            | Deg_270 ->
                [
                 (figure.Position.x - 1m, figure.Position.y)
                 (figure.Position.x - 1m, figure.Position.y + 1m)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x + 1m, figure.Position.y)
                ]
        | J ->
            match figure.Rotation with
            //   □
            //   ■
            // □ □
            | Deg_0 ->
                [
                 (figure.Position.x, figure.Position.y - 1m)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x, figure.Position.y + 1m)
                 (figure.Position.x - 1m, figure.Position.y + 1m)
                ]
            // □ ■ □
            //     □
            | Deg_90 ->
                [
                 (figure.Position.x - 1m, figure.Position.y)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x + 1m, figure.Position.y)
                 (figure.Position.x + 1m, figure.Position.y + 1m)
                ]
            // □ □
            // ■
            // □
            | Deg_180 ->
                [
                 (figure.Position.x, figure.Position.y - 1m)
                 (figure.Position.x + 1m, figure.Position.y - 1m)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x, figure.Position.y + 1m)
                ]
            // □
            // □ ■ □
            | Deg_270 ->
                [
                 (figure.Position.x - 1m, figure.Position.y - 1m)
                 (figure.Position.x - 1m, figure.Position.y)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x + 1m, figure.Position.y)
                ]
        | O ->
            match figure.Rotation with
            // ■ □
            // □ □
            | Deg_0
            | Deg_90
            | Deg_180
            | Deg_270 ->
                [
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x, figure.Position.y + 1m)
                 (figure.Position.x + 1m, figure.Position.y)
                 (figure.Position.x + 1m, figure.Position.y + 1m)
                ]
        | S ->
            match figure.Rotation with
            // □  
            // □ ■
            //   □
            | Deg_0
            | Deg_180 ->
                [
                 (figure.Position.x - 1m, figure.Position.y - 1m)
                 (figure.Position.x - 1m, figure.Position.y)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x, figure.Position.y + 1m)
                ]
            //   □ □
            // □ ■
            | Deg_90
            | Deg_270 ->
                [
                 (figure.Position.x - 1m, figure.Position.y)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x, figure.Position.y - 1m)
                 (figure.Position.x + 1m, figure.Position.y - 1m)
                ]
        | T ->
            match figure.Rotation with
            // □ ■ □  
            //   □  
            | Deg_0 ->
                [
                 (figure.Position.x - 1m, figure.Position.y)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x + 1m, figure.Position.y)
                 (figure.Position.x, figure.Position.y + 1m)
                ]
            // □
            // ■ □
            // □
            | Deg_90 ->
                [
                 (figure.Position.x, figure.Position.y - 1m)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x + 1m, figure.Position.y)
                 (figure.Position.x, figure.Position.y + 1m)
                ]
            //   □
            // □ ■ □
            | Deg_180 ->
                [
                 (figure.Position.x - 1m, figure.Position.y)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x, figure.Position.y - 1m)
                 (figure.Position.x + 1m, figure.Position.y)
                ]
            //   □
            // □ ■
            //   □
            | Deg_270 ->
                [
                 (figure.Position.x - 1m, figure.Position.y)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x, figure.Position.y - 1m)
                 (figure.Position.x, figure.Position.y + 1m)
                ]
        | Z ->
            match figure.Rotation with
            //   □  
            // □ ■
            // □  
            | Deg_0
            | Deg_180 ->
                [
                 (figure.Position.x - 1m, figure.Position.y)
                 (figure.Position.x, figure.Position.y - 1m)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x - 1m, figure.Position.y + 1m)
                ]
            // □ □ 
            //   ■ □
            | Deg_90
            | Deg_270 ->
                [
                 (figure.Position.x - 1m, figure.Position.y - 1m)
                 (figure.Position.x, figure.Position.y)
                 (figure.Position.x, figure.Position.y - 1m)
                 (figure.Position.x + 1m, figure.Position.y)
                ]
     
    let getNewFigure () =
        { Position = { x = 4m; y = 1m }; Rotation = Deg_0; Type = getRandomFigureType () }
        