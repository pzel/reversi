structure Main =
struct
  fun stringToPos s =
      case map Int.fromString (String.tokens Char.isSpace s) of
          [SOME x, SOME y] => SOME (x,y)
        | _ => NONE;

  fun readPos () =
      case TextIO.inputLine TextIO.stdIn of
          NONE => NONE
        | SOME s => stringToPos s;

  fun colorToString Game.WHITE = "🌕"
    | colorToString Game.BLACK = "🌑";

  fun rowToString board y =
      Int.toString y ^ "   "
      ^ String.concat (List.tabulate (Game.boardSize,
                                      fn x => case Game.get board (x,y) of
                                                  SOME c => colorToString c
                                                | NONE => "＿"))
      ^ "\n";

  fun boardToString board =
      "    0 1 2 3 4 5 6 7\n\n" ^
      String.concat (List.tabulate (Game.boardSize, rowToString board));

  fun movesToString positions =
      String.concat (map (fn (x,y) => "(" ^
                             Int.toString x ^
                             "," ^
                             Int.toString y ^ ") ")
                         positions)

  fun gameToString {board, next = SOME c} =
      boardToString board ^
      colorToString c ^
      "の手番です\n" ^
      movesToString (Game.possible board c) ^
      "\n"
    | gameToString {board, next = NONE} =
      boardToString board ^ "終局\n";

  fun clear () =
      let val home = implode (chr 27 :: (explode "[H"))
          val clr = implode (chr 27 :: (explode "[2J"))
      in print home; print clr
      end

  fun mainLoop game =
      let val _ = clear ()
          val _ = print (gameToString game)
          val newPos = if (#next game) = SOME Game.WHITE
                       then Game.cpuMove (#board game)
                       else readPos ()
      in
       case newPos of
           NONE => mainLoop game
         | SOME pos =>
           case Game.step game pos of
               NONE => mainLoop game
             | SOME game' => mainLoop game'
      end

end

val _ = Main.mainLoop Game.initGame;
