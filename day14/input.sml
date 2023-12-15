
(*
this is an example from advent of code 2023 day 14

using standard ml
how represent 

list representation of a grid is astonishingly badly performing ...
lets try a rewrite using Array2d


*)

val example = map explode [
".#....#.O##O....OO#O..O##.O.O..#..O...OOO.#..................#...#O#.##.#.....###O...#OO...#...#....",
"......O........O....O##..O#..O.O#.....O.....O...#.O...O...#...#O......#O.O##....#OO...#...O....O....",
".##O.O...O......O.O............O.O.O.....O.#O.O#.#..#O.#..O..OO.....OO..#..#.O#...O..OO......#.#....",
".O.O.#...##.O.#.......#.#.......#O#..#O.....OOO.OO.........O.#.#.O......#...#...#....O..O....#..###.",
"#..#..#..#.#.O....O.O.OO....#.O...O.O.#O....#.#.O....#O.#.O#.......#..O.##O....O..OO.###O....#...OOO",
"...............OO#...####.......O...O##...........#.##O....#O#O.##O##..O.O.....O.O..O#...O...#...O.#",
"..O..OO.O...##....O#...#..OO.OO#......O.#.#O#..##...O..#O...O..#......O....O.#..#..O.O#O#.#....#....",
"....##.#..#...O......O...O..#....#.#.O......##.##OOOO#........O#..OO#OO.O.......O..O...O.O....##.#..",
"O..O.###...OO....#.#OOO..##.OO.#......OOO..#..O.O....#O.#..O#.O.....O..#...O..O#....#O.OOO..O.OO.#..",
".....#......O#OO...O.#..O.#.O.O..#O..O...O#O...O....O###..#.........O..OO#.O...O.O......O.O.......#.",
".......#....#....O......O....O.O#.#.......#.O..O..##.O..O..O...O..##.#...O...OO.##....#OOO#O.#####.O",
".O..O.....O#..O#....O.OOOOO...#...O.#O..#O..O........O.....#O..##..#........#.OO.O..O...O.OO..OO....",
"........O..OO.#..#.O...#.O.#O#.OOO...O#OO#..O.OOO.....#O.O.O.OO..........#.....O.O.....#O.O.....#O#.",
"......OOO#...O.#O#.O.O#.O.#..OOO#.#.......O.##.O.#O#...O#.O....#.O#....O.##..#.#....OO.O.OO..O.OO...",
".O.O.OO.#.#O...#...#.#...O.#.........O..#O.O........#.O...#...O.O.O#.....O#.O....O...#...O#...O.#...",
".O....O.....O.O....OOO..O#...O...#.O.....OOO..#.O.#.O.O....O.OO.O.#..#O..OOO...O.......O..#.#.....O.",
"#O###..O#.....###......#...#.#...OO...O.#.....#..#O....O.O...O#....O.....#...#...............O......",
".O.OO#O..O.OO#.#O#..#.O.##.##O.....###....O.......O#.#..#.....##...O..#...#.O.O##..OO..O.OOOO...O..#",
"..#.#.#..OO...#.....O.......#.OO.#.O.#O.O###.###.O......O...OO..##.O#.O....O..O..O..#.O..O.O...O#.#.",
"#O.O....O....O.O......#..O.O...O.O#.O....OO#.OO.O...#.....O#..O.O#..#.#..........O..O....OOO...O..O.",
".#O...........O#....O..#O.O.#...#....O..........###...#........#.O.#..O#.O...O.#.....###.....##...O#",
"..O.O..##.#O...O.#..O....O#....#...O.O...O.O........O.#.O..O....#.##...O.#OOO.#....O..............#O",
".#....O....OO.#.O....O#O#...OO##....#.#......#O.#..O..##.O..#.##.#.O..#...O...##O......#.O.#.##..O#.",
".O#..#.O.#...O.OO...#.........O.....#O..........#..O#......#.O##.#.....OO....##OO...O.#.O.OOO...O.O.",
"O..#....O.....OO.#O.....#....O.........O....O.O.O..#....#..O........#.#.O#.##..#...O#O....#...O#.#O.",
".#...OO#..O...#.#O..#.O....#.#.#.....##....O..O...#.O.O........O.#.O...O.O............#.......#.O...",
"...#.O....O.....#...#..O.OO...#O.OO....O##.OO#...O............O.....O............#O.OO...O#...#....#",
"..#O#.O.OO....O....O.....#..O...O...O..O#....OOO..O.#O.#O.......O..#.OO...#O##O..##..O...##..#...O..",
".....O##...#O..O....#...#..#O......#.#..O#.#....O..O.#....OOO.O..O.O....##..#O..#..#....#O..........",
"...#............O.O#......#.O.O...OO.....OO.....O..O..O.#O.O.#..O.O.O.O....O..O...OO..O.OO...##...O.",
"O.O......O...#....OO.#..#..#....##.#OO.....O.#OO.OO.O..#O.O#......##.#..O#.#O..O....#O....OO#...##OO",
".......O....#..OOO..#.#...O.....OO.OO.#..#...#O....O..O.....O..#...O......#....O.#.....O....#....#O.",
"..#OOO.....#.O..#....#OO.#O.....O.O...#...#O#.......#....O...##..O...O...O.O...O..#.....OO#.OO.O..O#",
"#..#.#.O#.O..O..###..#.....O.O#...#.O...#.O#..#O.OO..O.#.#..O.#.#.#....##..OO##.....#.....#.O...#...",
"#O.O.O...OO#....O....O##......#OO#.O.OO.OO#O..O..OOO.....O..#O.#..#O..O....#...OO....##OO#O...#.....",
"#.#.....O..O..O.O#.O........O........##..O.......OO...O..#...O#.O.O.O.O#.O..O#.....OOO.O.#...#O.O..O",
"O...OOOO.###.O.............#...O..........#.#........###OO.....#....O....#O...##..O...O#..O.O..#..#O",
"..O.#O.......O..OO.O...#....#O.#...#....O.O...O....OO.........O....OO.........OO#..#.##..OO.#.#..O..",
"......O.O.#O..O....OO#....O...#..O#....O###..OO.O........O.O##....##O.O.O......O.#....##O.#..O.#.O.#",
"....O....##.#...OO#.#....#....O.#.##....#..O..OO..OO....OO...OO.#O#......#O#...#O.....#.O#...OO.O...",
"..O####...........O.O.......OOOO...OO#.....#......O..O...O..OO..#..O...#O#..#.#O#O#...###..#O..#....",
".......O#..#.O.#.......#.O.#O.#..#OO#O....##..#...O..O##...#.OO.O.O.O.O....O.O...#.#.....OO...#.O...",
"O.#.....#...O...#...#.OO.#O..O.#...O.O...O#O..##......#.#.....O...#......O##...OO......O.OO....#.#O#",
"##O#.O.......#.......#..O.O..O.##...O....#......O.#....O...O....O.OO.O.O......#OOO.O#O.##..#O.O..#.#",
"O#...O.....#..O..#O..O#....##.#.O.....OOOO....O.#.................#O.O..OO.#OO.#.....#.#.#...#...O..",
"O.OO.O....#..##...O.#O......O#..#..#...O....#O#.O.O......#..O.#O#...O...#O....O..#O#O.....#.#OO...O.",
".#.O###..O#O...#.....#..O....O.O#O...O.....O.##..O#.#...#..#O...#OOOO#...#.#O#....O....#O.#..#O.#O#.",
"#....O.OO.O.......#....#..OO.O.....O..#.......OO....#......O##..O..O#..O....O..##..O..OO..#.O..#...#",
"##O#.#.O....O........OO........O#.#.#O......#...O#O...OO##.........#.##.##......#.O...O.OOO..#..O.O.",
"...........O.##.O#.O.#OO#O.#O...###...O.O.#...O...O.#....#.#....OO#.#......O..#..O.O.#.......O.#.#..",
".OO...O...........#.O....O..O..OO..O.....O.O...O#.OO....#OOOO....#.O.#.#.O#.O...#.O....O#...##O.....",
"...........O.#...#...O..#..O..#..O..O.O.........#O.OO..O.OO.....O.O........OO..O...O....O.O..#OOO...",
"O#.O##.O.OOO.O#.##........O...OOO...#...OOO....O#.....#.OO..#..O#.....O#.O...O...#.OO.O#..##...OO#..",
"....#.O....O.O.O#.O#O...O.O.O.O..OO.O#...#.OO..O#...O.#...O..#...#..#.#O..OOO.O.OO.........#.O.O..O.",
"...O....OO....##...O..O.#O...O.#.#...OO...O....O.O#.#.O...#.O#O...O.....O....O.#O...O...............",
"..#..O.#..O.#....O.O.O...O#...O....O.OO..#.OO#....O..O.O..O....OO...OO........#....OO..O......O.#...",
".#.O#.#OO##...O..O..#...##..OO....OO...O..O..#......O.O..O#...#....OO.#O.##......#...........O#O....",
".#...#OO.O..#..#.O.OO#.#...#.##...O...#O....#..#O.#.#.OO.#..O..O.#...O#OOO..O...#.O.O....#...O..OO..",
".###O.O.#O#.O....O#.#...O....#...O....OO##.#.#...O.......O.#O.....#...#O...O..O........#O..O..OO...O",
"O#.O...OO....#.##OOO.....O..OO........#.O..O..OOOOO#O..#O.#.#.....OO.O#.#..OO...O.O.OOO#O.#..#....O.",
"...#.#O..#..O.O..#.....#OO.O.OO####...##...O..O..###.O.#.#.O......O.O.#..#..##....OO#.##.O.O.O.OOOO#",
"...O#.....OOOO...O.##.O....#.#O.O#.###..#..#O..O.OO......#..#...O#.....OO.O##.O.O....#.#O.......O...",
"#...#....#.....#O....#.....OO.#..OO.##..#O..O.O.....O.O.OO.O#O..#....O....###...O........#OOO...O...",
".O...O#O.#..#O..#OO.O...OO##O.#O.#.#..#....##O..O...#.OOO..#..#.........#O.#...OO......O.#..........",
".#.OO#..O.#.O.....#...#....##O#.....###O.O....##...#...O.......OO#......#....#.OO.#OOO#.#....#O.O.OO",
"...#.O.O.O#...#....#....#...O#..#.O#...OO#...O##O..##.#O...#.OO..###.O...O##O..O..O.OO...O..###.#...",
"#O...O...#...#..#..O.O.O..#O...O.O#....O#..O................O....O..O......#.....##.#O..OO..O#..OO..",
"#..O.#.###.#.#...#..O....OO..###O...O.OO..........#..O.O..O..O.......OO#O....OO..O.O.##.#O#...OO...#",
"...O..O.#O..#OO...###OO.O....##O....O...O...........#O##.O.#..O.O.#..OOOO.#..O..O#.O........#.#OOO..",
"...O##OOO.....O.#.....O.#....#O.O...O#.#..O....#.OO..O#...OO.#.O.OOO.#..O#...O.......O#O.O#O...O....",
".#....O...O#..........O...O.O.....O..#O..#OO.#...............##....O#O#....OO.###O....#.........###.",
"...OO.OO.#.......OO#.#..O..O....OO.##O...O.#.#..O##.O.O....OO.O..O.#..O..OO.#..O##.#..##.#...##..OO.",
".......O........#...O.##..#..O#O...O..O.#.#O..#..O......OO..O.#....O...O......OOO....O.#.#....#O..#.",
"#..#....O..OO.#...#.O...O##.O#.O##..OOO.....OO..O#..#....O......OO...O.O......#.O..#..O....#....O...",
"...#.OO##.....OO...O......#.#..#..O#..O...O.O.O.........#..#.........O##..#.O..#..#....#..O......#..",
"O.#...OO..#..O.#.#OO#....##.OO#...O....#.#..#....#..OO..#O..O...O#..O..##..O#....O..O#...##...O....O",
"#.#.#.O.#O....O..#..#..O..OO.OO.#O.#..O..O....O.............#.O#...O#..#O.....O......#.......#...O..",
"....O.....##...O.#O.##.#.##OO..#....##O#.OO#.#..#..O##.#.#.#O.OO.#...O#..O.........O.##...##....#..#",
".O#.....###....#.OO#..O#....#.....O.O#O#..#O.....#....#...O...O..##.#.O...OO#.##..O.O...#..........#",
".OO..##......#O......#.O.O..O.##O.....O....#...O#.O..........O..#.O..#.......O....O.#...O.O.#.OO.O..",
"..#...O...#..#O.......O#..O..O#.#O.#O.#O#.#....O.#..O.O..#.#.O#...#.#.....#..OO.....O.#....O#.#..#..",
".O.##.O..OO........#.#..O..O.......OO.O.OO.....O.#.###O...#OO.....#O.......O.O.O...O..O.............",
"OOO...OO.......O......O#O..#.#.O..OO#.#OO.....#....#OO.#...#.....OO.O..O#...#O.....O....O.#O#.......",
"..O.....O....O.O...#..O...O#....O.O.#.....#...O#.....O.....O#.O.##...O.O............O....O##.#O..O.O",
"....O..##O..O..O....#.....#.O.#.......#O...##.O#...OO.......O....#OO.O..O.O...O#..#......O...O..#..O",
"...#.O.....#.#.O...O#.....#.O..OO.O....O..#.O#.O#.O..#....#.........O..OO.O.O.O....O..#.OO...O##..#O",
".#....#..........O.##..OO#...O...#...O..#.O#..O...#...#O#.O.......#.#..O#O#..#O..O#....#.....OO.O...",
"..O.O..O.##...O..#.OO#..O.O.....O..OO#.OO.O...O....O#...##OO..#O....##.#OO#......#.OOO.....#O..O..#O",
".#....O##..O..#.O.#....OO#O..O..#..O.O.....#O..O##...O.....O....#.#.O###.O#...O.#..#.O......O.....O.",
"O.......O.#...#....O...#....#.....O..#.#O..O#....##O.O#........OO.#......O..OO....O##O.O#.O....O..O#",
".#O.##.#O...O#...##.O....#O.#O#.#O.O.O#OO..O.#...O....#.....OO.#.......#..O.#..OO..O..OO.....O.....O",
".O#..#OO.O...O#O.O..#..O.##.....##...O.#O.O....OO.#.OO#OO.OO......#.#..#.......O..O..OO#...#.....#.#",
"O#O.##....#.OO.O......#..O..O.OOO..#.....O.O.OO#.OO....#.O#O....OO.....O.O....#..#.O#.....O...O.....",
"O.O.#....O..#O#O#O...##...OO#O....O..OO..........#...O##.#....O.O...##O.#O#.O#O.O.#OO.#..OO..O......",
"...OO####O#..##..#O.O...O..O..OO...#..O..O......O...O#O..O......#OO#.OO..O.#...#.#...#....O........#",
".....O.O...O.O#......#.#....#..O..O...OO..O.#.#..###....#.##......#....OOO.......O.O....#.#O..OO...#",
"....OO....OO...#....O.#O..........O..#..O.#O#.....#.#......#O....#..#........#O.#.....#.O..OOO#O.#O#",
"O...O...#..O....O#...#..#.O...#..#..OO.##..##O...O#..#..##.##.OO..O..#O...OO..........O..#.#O##...O.",
"#O.#...#......#OO..O.#...O.#.....O.....##.O....OO#..O..O...#.OO......O.....#OO.........#...#........",
"..##O.#..#.#.#O...OO......#O.#.O...O.#..#O..#.O....O....#.O#............####...OO.#..#O.O..#.O#.#O#."];


(*
val example = map explode [  "OOOO.#.O.." ,
		      "OO..#....#" ,
		      "OO..O##..O" ,
		      "O..#.OO..." ,
		      "........#." ,
		      "..#....#.#" ,
		      "..O..#.O.O" ,
		      "..O......." ,
		      "#....###.." ,
		      "#....#...." ];
*)

datatype rock = ball | block | empty;

(* squareType is tuple of (ock ,coord ,coord) *)
datatype squareType = square of rock * int * int ;

(* ball block and empty are all distinct elements of type square *)

exception CharToSquareException of string * char;
(* val charToSquare = fn : char -> square; *)
fun charToSquare x =
    case x of
        #"O" => ball
      | #"." => empty
      | #"#" => block
      | e => raise CharToSquareException ("unrecognised character", e);

(* how do have a lambda fn in sml  *)

(* texample is typed version of example using ball , empty and block  *)
val texample = map (fn xs => map charToSquare xs) example ;


fun get xs i j = List.nth( (List.nth (xs ,j)) ,i );

fun add_one x = x + 1 ;
                                                     
fun countBalls [] = 0
  | countBalls (h :: t) = if h = ball then 1 + countBalls t
                         else countBalls t ;

countBalls (hd texample) ;
map countBalls texample;

fun sumList [] = 0
  | sumList (h :: t) = h + sumList t;

fun countAllBalls xs = sumList (map countBalls xs);

countAllBalls texample;

val height = length texample;
val width = length (hd texample);

fun listToXYHelper xs x y lx ly =
    if y >= ly then []
    else if x >= lx then listToXYHelper xs 0 (y + 1) lx ly
    else ((get xs x y),x,y) :: listToXYHelper xs (x + 1) y lx ly ;


fun listToXY xs = listToXYHelper xs 0 0 (length (hd xs)) (length xs) ;

(* sexample combines types ball , block and empty with coordinates
 (ball,3,4) (block,5,2) (empty,6,2)
 *)
val sexample = map square (listToXY texample);

fun above x =
    if x > 3 then "above"
    else "below" ;

(* findLoad on the north wall *)
(* do not forget fun keyword *)

fun findLoad2 [] hgt = 0
  | findLoad2 (square (ball,x,y) :: t) hgt = (hgt - y) + findLoad2 t hgt
  | findLoad2 (_ :: t) hgt = findLoad2 t hgt ;

fun findXYHeight2 [] n =  n
  | findXYHeight2 (square (_,_,y) :: t) n =  if y > n then findXYHeight2 t y
                                      else findXYHeight2 t n ;
(*
height is based off zero indexed coordinates
highest coordinate y = 9 means actually 10 high
*)
fun findXYHeight xs = (findXYHeight2 xs 0) + 1  ;

findXYHeight sexample;

fun findLoad xs = findLoad2 xs (findXYHeight xs);

findLoad sexample ;


(* want to remove a square but ensure it is actually removed and is in the list
throw toys out pram if not found in list

exception WithoutException of string ;
fun without [] e = raise WithoutException "missing "
  | without (h :: t) e = if h = e then t
                         else h :: (without t e) ;
*)
fun without [] e = []
  | without (h :: t) e = if h = e then without t e
                         else h :: (without t e) ;


val ls = length sexample;
val w = (without sexample (square (ball,0,0)));
length w;


(*
tilt north 
     if ball x y > 0 and empty at x y - 1 then put empty at x y and ball at x y-1
     keep iterating until converge                                                                           *)

(* list membership *)
List.exists (fn x => x = 3) [1,2,3,4,5];

(* boolean operators  andalso orelse not *)

fun tiltNorthHelper xs x y lx ly =
    if y >= ly then xs
    else if x >= lx then tiltNorthHelper xs 0 (y + 1) lx ly
    else 
      let val b = List.exists (fn x2 => x2 = (square (ball,x,y))) xs
          val e = List.exists (fn x2 => x2 = (square (empty,x,y-1))) xs
          val xs2 = without xs (square (ball,x,y))
          val xs3 = without xs2 (square (empty,x,y-1))
          val xs4 = (square (empty,x,y)) :: xs3
          val xs5 = (square (ball,x,y-1)) :: xs4                                                    
      in
        if b andalso e then (tiltNorthHelper xs5 (x+1) y lx ly)
        else (tiltNorthHelper xs (x+1) y lx ly)
      end ;

             
(*
    let val ... in ... end
    let fun ... in ... end
*)

fun tiltNorthHelper2 xs =
    let val ys = tiltNorthHelper xs 0 1 width height in
      if xs = ys then ys
      else tiltNorthHelper2 ys
    end;

fun tiltNorth xs = tiltNorthHelper2 xs ;


(* string representation of the board
iterating over x and y
xs is constant throughtout
having to traverse xs each and every access 
 *)
exception LookupException of string;
fun lookup [] x y = raise LookupException("ouch")
  | lookup (square (s,a,b):: t) x y = if x = a andalso y = b then s
                                      else lookup t x y ;
                                                       

fun strRepHelper2 xs x y lx ly =
    if y >= ly then "\n"
    else if x >= lx then "\n" ^ strRepHelper2 xs 0 (y + 1) lx ly
    else 
      let val r = lookup xs x y 
      in
        if r = ball then "O" ^  strRepHelper2 xs (x + 1) y lx ly
        else if r = empty then "." ^  strRepHelper2 xs (x + 1) y lx ly
        else "#" ^ strRepHelper2 xs (x + 1) y lx ly
      end ;

fun strRep xs = "\n" ^ strRepHelper2 xs 0 0 width height;

print (strRep sexample) ;

val part1 = tiltNorth sexample;
val loadNorth = findLoad part1;

print (strRep part1) ;





