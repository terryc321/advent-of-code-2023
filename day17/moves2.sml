(*


number of ways to get to certain square ??
worst possible path ??



switched to using standard ml

converted file to sml data structures for ease of writing sml code

1 ) io done - parsed data to sml data structures

2 ) produce empty arrays



:: sml cons
@ sml append operator
[1,2,3]
1 :: [1,2,3] => [1,1,2,3]

(2,3) tuple

[(1,2),(3,4),(2,3)] 

example of cost 120 with associated path (2,3)->(3,4)->(1,2)
(120,[(1,2),(3,4),(2,3)]);


initial state
(0 , [(0,0)])
at 0,0 and cost is 0 as puzzle defines 1st square start from has zero cost

only two ways to move


right (0, [(0,0)])
down  (0, [(0,0)])

* -> 1 -> 2 -> 3!
! need change direction

n e s w
u r d l

At x y with cost c , path p , control right left up down
decide x y cost path right left up down

Exception noDecision

if right > 0 decideRight
if left > 0 decideLeft
if up > 0 decideUp
if down > 0 decideDown
else raise noDecision

if decide to go right I call
     decideRight (x + 1) y (cost + cost at x+1,y) 1 (n = 1)
if decide to go right again
     decideRight (x + 1) y (cost + cost at x+1,y) n + 1 (n = 2)
if decide to go left - cannot be going back on myself
     decideDown x (y + 1) (cost + cost at x y+1) 1
     decideUp x (y - 1) (cost + cost at x y-1) 1


val z = let val a = 4
          val b = 5
	   val c = 8
	   in
	    a + b + c
	    end ;
	    

fun twice ( x : int ): int= x + 2 ;

val add = fn (x : int) => fn (y : int) : int => x + y ;

fun add2 x y = x + y;

(* we can type assert each variable ourselves and provide return value also *)
fun add3 (x:int) (y:int) : int = x + y;



(* question then is what is the return type of decideRight ??
  lets just say unit () 
*)
(x,y,c,p,n)

*)


(*
=======================================================

- see how cost x2 y2 should be written does it need brackets?

(x:int) (y:int) (c:int) (p:((int * int) list)) (n : int) : ()

fun cost x y = x + y ;

fun solve =  let val f = fn x => g (x + 1)
		and		   
		 val g = fn y => y + 2 
	     in
		 (f 3) + (g 4)
	     end;

(* mutually recursive functions *)
fun f x = g (x + 1)
and
    g y = y + 2;

val solve = (f 3) + (g 4);

let rec fun f x = x + 2 ;

fun cost x y = x + y ;

x y could still be off board 

decide dir x y c p n
 if atEnd(x,y) then  ... () done
 else if dir = left then 
      let val x2 = x - 1
          val y2 = y
          in
           if onboard (x2,y2) then let val c2 = c + cost(x2,y2)
                                       val p2 = ((x2,y2) :: p)
                                       



       x y cost path n  prev1 prev2 prev3
decide 0 0 0 [(0,0)] 0 none none none


fun decide x y c p pr1 pr2 pr3 
                   left left left  - cant go left 




(* Left *)
fun decide1 x y c p pr1 pr2 pr3 =
    if pr1 = left then
        if pr2 = left then
	    if pr3 = left then
????? no and /or
		


fun decide x y c p pr1 pr2 pr3 =
    let val _ = decide1 x y c p pr1 pr2 pr3
	val _ = decide2 x y c p pr1 pr2 pr3
    in () end ;





=======================================================
*)
	
	   

exception unknownDirection ;
datatype direction = left | right | up | down | none ;

fun cost x y = x + y;

val width = 13;
val height = 13;


fun self x y pr1 pr2 pr3 =
    if pr1 = left then
	left
    else
	right;

fun self x y pr1 pr2 pr3 =
    if pr1 = left then
	if pr2 = left then
	    right
	else
	    left
    else
	right;

l l l
  l l e
  

fun left x y pr1 pr2 pr3 =
    if (pr1,pr2,pr3) = (left,left,left) then
	    right
	else
	    left

(*
onboard will check x 0 .. width-1  , y 0 ... height-1
but for


for left x > 0
for up y >= 1
for down y <= (height - 2)
for right x <= (width - 2)


 *)		
fun left x y c p n =
    if n >= 3 then
	let val _ = up (x + 1) y 1
	    val _ = down x y 1
        in ()
	end		       
    else
	let val _ = left x y (n + 1)
	    val _ = up x y 1
	    val _ = down x y 1
        in ()
	end ;

fun left x y c p n =
    if n >= 3 then
	let val _ = up (x + 1) y 1
	    val _ = down x y 1
        in ()
	end		       
    else
	let val _ = left x y (n + 1)
	    val _ = up x y 1
	    val _ = down x y 1
        in ()
	end ;

fun left x y c p n =
    if n >= 3 then
	let val _ = up (x + 1) y 1
	    val _ = down x y 1
        in ()
	end		       
    else
	let val _ = left x y (n + 1)
	    val _ = up x y 1
	    val _ = down x y 1
        in ()
	end ;

fun left x y c p n =
    if n >= 3 then
	let val _ = up (x + 1) y 1
	    val _ = down x y 1
        in ()
	end		       
    else
	let val _ = left x y (n + 1)
	    val _ = up x y 1
	    val _ = down x y 1
        in ()
	end ;












fun right2 (x:int) (y:int) (c:int)  (p:((int * int)list)) (n:int) (f: int -> int -> int -> ((int * int)list) -> int -> ()) = 3;

    if n < 3 then let val x2 = x + 1
                  val y2 = y
	      in
		  let val c2 = c + (cost x2 y2)
		      val p2 = ((x2,y2) :: p)
		      val n2 = n + 1
		  in
		      f x2 y2 c2 p2 n2  (* check x2 y2 *)
 		  end
	      end
    else () ;



fun onboard x y = 
    if x >= 0 then
	if x < width then
	    if y >= 0 then
		if y < height then true
		else false	                      
	    else false
	else false
    else false;


fun  decideRight x y c p n =
     let val _ =  decideRight2 x y c p n
	 val _ =  decideRight3 x y c p n
	 val _ =  decideRight4 x y c p n
     in
	 ()
     end
and	
decideRight2 x y c p n = right2 (x2,y2,c2,p2,n2, decideRight)
and

and
decideRight3 x y c p n =
let val x2 = x
    val y2 = y + 1
    val c2 = c + (cost x2 y2)
    val p2 = (x2,y2) :: p
    val n2 = 1
in
    decideDown x2 y2 c2 p2 n2  (* check x2 y2 *)
end 
and
decideRight4 x y c p n =
let val x2 = x
    val y2 = y - 1
    val c2 = c + cost x2 y2
    val p2 = (x2,y2) :: p
    val n2 = 1
in
    decideUp x2 y2 c2 p2 n2  (* check x2 y2 *)
end 
and

(* =================================================== *)
decideLeft x y c p n =
let val _ =  decideLeft2 x y c p n
    val _ =  decideLeft3 x y c p n
    val _ =  decideLeft4 x y c p n
in
    ()
end
and
decideLeft2 x y c p n =
if n < 3 then let val x2 = x - 1
                  val y2 = y
		  val c2 = c + (cost x2 y2)
		  val p2 = ((x2,y2) :: p)
		  val n2 = n + 1
	      in
		  decideLeft x2 y2 c2 p2 n2 (* check x2 y2 *)
 	      end
else
    () 
and
decideLeft3 x y c p n =
let val x2 = x
    val y2 = y + 1
    val c2 = c + cost x2 y2
    val p2 = (x2,y2) :: p
    val n2 = 1
in
    decideDown x2 y2 c2 p2 n2  (* check x2 y2 *)
end
and
decideLeft4 x y c p n =
let val x2 = x
    val y2 = y - 1
    val c2 = c + (cost x2 y2)
    val p2 = (x2,y2) :: p
    val n2 = 1
in
    decideUp x2 y2 c2 p2 n2  (* check x2 y2 *)
end 
and

(* =================================================== *)
decideUp x y c p n =
let val _ =  decideUp2 x y c p n
    val _ =  decideUp3 x y c p n
    val _ =  decideUp4 x y c p n
in
    ()
end
and
decideUp2 x y c p n =
if n < 3 then let val x2 = x 
                  val y2 = y - 1
	      in
		  if onboard(x2,y2) then
		      let 
			  val c2 = c + (cost x2 y2)
			  val p2 = ((x2,y2) :: p)
			  val n2 = (n + 1)
		      in     
			  decideUp x2 y2 c2 p2 n2      (* check x2 y2 *)
 		      end
		  else
		      ()
	      end 
else
    () 
and
decideUp3 x y c p n =
let val x2 = x - 1
    val y2 = y 
    val c2 = c + cost x2 y2
    val p2 = (x2,y2) :: p
    val n2 = 1
in
    decideLeft x2 y2 c2 p2 n2 (* check x2 y2 *)
end 
and
decideUp4 x y c p n =
let val x2 = x + 1
    val y2 = y 
    val c2 = c + cost x2 y2
    val p2 = (x2,y2) :: p
    val n2 = 1
in
    decideRight x2 y2 c2 p2 n2 (* check x2 y2 *)
end 
and

(* =================================================== *)
decideDown x y c p n  =
let val _ =  decideDown2 x y c p n
    val _ =  decideDown3 x y c p n
    val _ =  decideDown4 x y c p n
in
    ()
end
and
decideDown2 x y c p n =
if n < 3 then let val x2 = x 
                  val y2 = y + 1
		  val c2 = c + (cost x2 y2)
		  val p2 = ((x2,y2) :: p)
		  val n2 = (n + 1)
	      in
		  decideDown x2 y2 c2 p2 n2 (* check x2 y2 *)
 	      end
else
    () 
and
decideDown3 x y c p n =
let val x2 = x - 1
    val y2 = y 
    val c2 = c + cost x2 y2
    val p2 = (x2,y2) :: p
    val n2 = 1
in
    decideLeft x2 y2 c2 p2 n2 (* check x2 y2 *)
end 
and
decideDown4 x y c p n =
let val x2 = x + 1
    val y2 = y 
    val c2 = c + cost x2 y2
    val p2 = (x2,y2) :: p
    val n2 = 1
in
    decideRight x2 y2 c2 p2 n2 (* check x2 y2 *)
end
;

  




