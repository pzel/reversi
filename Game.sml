structure Game =
struct
  type pos = int * int
  datatype color = BLACK | WHITE
  type board = (pos * color) list;
  type game = { board : board, next: color option }

  val initBoard : board = [ ((3,3), WHITE), ((4,3), BLACK),
                            ((3,4), BLACK), ((4,4), WHITE) ]
  val initGame : game = { board = initBoard, next = SOME BLACK }
  val directions = [(~1, ~1), (0, ~1), (1, ~1),
                    (~1, 0), (1, 0),
                    (~1, 1), (0, 1), (1,1)]
  val boardSize = 8
  val allPositions = List.tabulate (
        boardSize * boardSize,
        fn x => (x mod boardSize, x div boardSize))

  fun get nil pos = NONE
    | get ((p,c) :: t) pos =
      if pos = p then SOME c
      else get t pos

  fun put nil pos color = [(pos, color)]
    | put ((p,c) :: t) pos color =
      if p = pos
      then (pos, color)::t
      else (p, c) :: (put t pos color)

  fun move (board : board) (color : color) (positions : pos list) : board =
      foldl (fn (pos, board) => put board pos color)
            board
            positions

  fun flipDir' board color (x,y) (dir as (dx, dy)) =
      let val pos = (x+dx, y+dy)
      in case get board pos of
             NONE => NONE
           | SOME c =>
             if c = color
             then SOME []
             else case flipDir' board color pos dir of
                      NONE => NONE
                    | SOME t  => SOME (pos :: t)
      end

  fun flipDir board color pos dir =
      getOpt (flipDir' board color pos dir, [])

  fun flip board color pos =
      case get board pos of
          SOME _ => []
        | NONE =>
          List.concat (map (flipDir board color pos) directions)

  fun possible board color =
      List.filter (fn pos => not (null (flip board color pos))) allPositions

  fun opponent BLACK = WHITE
    | opponent WHITE = BLACK

  fun next board color =
      case possible board (opponent color) of
          _ :: _ => SOME (opponent color)
        | [] => case possible board color of
                    _ :: _ => SOME color
                  | [] => NONE

  (* cpu is always white *)
  fun cpuMove board =
      case possible board WHITE of
          p :: _ => SOME p
        | [] => NONE

  fun step { next = NONE, ...} _  = NONE : game option
    | step { board, next = SOME color} pos =
      case flip board color pos of
          nil => NONE
        | positions => let val b = move board color (pos :: positions)
                       in  SOME { board = b, next = (next b color) }
                       end
  fun play moves =
      foldl (fn (pos, NONE) => NONE
              | (pos, SOME game) => step game pos)
            (SOME initGame)
            moves
end
