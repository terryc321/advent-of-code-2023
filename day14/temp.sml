
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


