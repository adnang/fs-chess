namespace Chess
    
    type Piece = 
            | Pawn 
            | Knight
            | Rook 
            | Bishop
            | Queen
            | King

    type Color = Black | White

    type Square = (Color * Piece) option

    type Board = (Square list) list

    module ListUtil =
        let rec insert v index l =
            l
            |> List.mapi (fun i el -> if i = index then [v; el] else [el])
            |> List.concat

        let remove index l =
            l
            |> List.mapi (fun i el -> (i <> index, el)) 
            |> List.filter fst 
            |> List.map snd

        let replace i v l =
            insert v i (remove i l)

    module Game =
        open ListUtil
        
        let generateMainRow i color = 
            match i with 
            | 0 | 7 -> Some(color,Rook)
            | 1 | 6 -> Some(color,Knight)
            | 2 | 5 -> Some(color,Bishop)
            | 3 -> Some(color,Queen)
            | 4 -> Some(color,King)
            | _ -> None
                
        let generateBoard = 
            List.init 8 (fun (_) -> List.init 8 (fun(_) -> None))
            |> replace 1 (List.init 8 (fun (_) -> Some(White, Pawn)))  
            |> replace 0 (List.init 8 (fun (i) -> generateMainRow i White))
            |> replace 6 (List.init 8 (fun (_) -> Some(Black, Pawn)))  
            |> replace 7 (List.init 8 (fun (i) -> generateMainRow i Black))

        let replaceSquare square board x y =
            replace x (replace y square (List.item x board)) board 
    
        let squareAt board x y = List.item x board |> List.item y
            
        let pieceAt board x y =
            match squareAt board x y with
            | Some(_,p) -> Some(p)
            | None -> None
        
        let colorAt board x y = 
            match squareAt board x y with
            | Some(c,_) -> Some(c)
            | None -> None

        let isEnemyOrEmpty color board x y = 
            match colorAt board x y with 
            | Some(color) -> false
            | Some(_) | None -> true

        /// main game rules for legal moves
        let validateMove piece color x y x' y' =
            match true with
            | bounds when bounds = (x'>0 && x'<8 && y'>0 && y'<8) -> 
                match piece with
                | Pawn -> 
                    match color with
                    | White -> (x'=x+1) && (y'=y+1)
                    | Black -> (x'=x-1) && (y'=y-1)
                | Knight -> 
                    match true with
                    | oneRL when oneRL = (x'=x+1 || x'=x-1) -> y'=y+2 || y'=y-2
                    | twoRL when twoRL = (x'=x+2 || x'=x-2) -> y'=y+1 || y'=y-1
                    | _ -> false
                | Rook ->
                    match true with
                    | lr when lr = (x<>x') -> y=y'
                    | ud when ud = (y<>y') -> x=x'
                    | _ -> false
                | Bishop -> abs(x-x') = abs(y-y')
                | Queen -> 
                    match true with
                    | lr when lr = (y=y') -> x<>x'
                    | ud when ud = (x=x') -> y<>y'
                    | diag when diag = (abs(x-x') = abs(y-y')) -> true
                    | _ -> false
                | King ->
                    match true with
                    | oneSq when oneSq = (x<>x' || y<>y') -> (abs(x-x') < 2) && (abs(y-y') < 2)
                    | _ -> false
                | _ -> false
            | _ -> false

        let attemptTake color piece board (x,y) (x',y') =
            match piece with 
            | p -> 
                match validateMove piece color x y x' y' with
                | true -> 
                    let square = Some(color, p)
                    replaceSquare square board  x' y'
                | false -> board

        let movePiece piece board (x,y) (x',y') = 
            let color = colorAt board x y
            let canTake = 
                match color with
                | Some(c) -> isEnemyOrEmpty c board x' y'
                | None -> false
            
            match canTake, color with
            | true, Some(c) -> attemptTake c piece board (x,y) (x',y')
            | true, None | false, _ -> board

        /// attempts to move a piece from (x,y) to (x',y') on board, 
        /// returning the new state of the board (or the original board if the move is illegal)
        let movePieceAt board (x,y) (x',y') =
            let (curr, targ) = ((x,y),(x',y'))
            match pieceAt board x y with
            | Some(piece) -> movePiece piece board curr targ
            | None -> board
