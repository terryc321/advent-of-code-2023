
(*

Fixed a weird bug in SML that access 2d array by Row then Column
or traversal method ... ?
RowMajor
ColumnMajor...


this is an example from advent of code 2023 day 14

using standard ml
how represent 

list representation of a grid is astonishingly badly performing ...
lets try a rewrite using Array2d

*)


(* sanity check sml array2 *)
val sixt = Array2.fromList ( map explode [ "123","456","789"]);
val one = Array2.sub(sixt,0,0);
val four = Array2.sub(sixt,1,0);
val seven = Array2.sub(sixt,2,0);

fun sub arr x y = Array2.sub(arr,y,x) ;

fun update arr x y k = Array2.update(arr,y,x,k);




(* val init = Array2.array( 100 ,100, empty); *)

val example = Array2.fromList (map explode [
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
"..##O.#..#.#.#O...OO......#O.#.O...O.#..#O..#.O....O....#.O#............####...OO.#..#O.O..#.O#.#O#."]);


val height = Array2.nRows(example);
val width = Array2.nCols(example);

(* findLoad on the north wall
iterate
Array2.sub(example,0,0) get char at 0 0 in 2d array

cheat a bit use external width height of array
 *)

fun findLoad2 arr x y tot =
    if y >= height then tot
    else if x >= width then findLoad2 arr 0 (y + 1) tot
    else if (sub arr x y) = #"O" then
      findLoad2 arr (x+1) y (tot + (height - y))
    else findLoad2 arr (x+1) y tot ;

fun findLoad arr =
    let val x = 0
        val y = 0
        val tot = 0
    in
      findLoad2 arr 0 0 0
    end ;


(* tilting is destructive inplace modification
rflag is set if ball moved , causing tiltNorth to rescan and move other balls
otherwise no balls moved , we are done
 *)
fun tiltNorth2 arr x y rflag =
    if y >= height then
      if rflag then tiltNorth2 arr 0 1 false
      else arr
    else if x >= width then tiltNorth2 arr 0 (y + 1) rflag
    else if ((sub arr x y) = #"O") andalso ((sub arr x (y-1)) = #".") then
      let val _ = (update arr x y  #".")
          val _ = (update arr x (y-1)  #"O")
      in
        tiltNorth2 arr (x+1) y true
      end
    else tiltNorth2 arr (x+1) y rflag ;

(* start search for a #"O" character on 2nd row as cant push north if already on north wall *)
fun tiltNorth arr = (tiltNorth2 arr 0 1 false)  ;



fun strRepHelper2 xs x y lx ly =
    if y >= ly then "\n"
    else if x >= lx then "\n" ^ strRepHelper2 xs 0 (y + 1) lx ly
    else 
      let val r = Array2.sub(xs,x,y) 
      in
        str(r) ^  strRepHelper2 xs (x + 1) y lx ly
      end ;

fun strRep xs = "\n" ^ strRepHelper2 xs 0 0 width height;

print (strRep example) ;

print "\n\n------------------------------------------\n\n";

findLoad example;

tiltNorth example;
print (strRep example) ;
findLoad example;


