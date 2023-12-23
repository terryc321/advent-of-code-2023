
#|

val width = 13;
val height = 13;

fun cost x y = x + y;

fun left x y c p n =
    let val _ = let val x2 = x     (* up = neg Y *)
		    val y2 = y - 1
		in if y2 >= 0 then   (* keeps y2 on board ? *)
		       let val c2 = c + (cost x2 y2)
		       in up x2 y2 c2 ((x2,y2)::p) (n+1)  (* go up *)
		       end
		   else
		       ()
		end		    
	val _ = let val x2 = x     (* down = pos Y *)
		    val y2 = y + 1
		in if y2 < height then   (* keeps y2 on board ? *)
		       let val c2 = c + (cost x2 y2)
		       in down x2 y2 c2 ((x2,y2)::p) (n+1)  (* go down *)
		       end
		   else
		       ()
		end		    
        val _ = if n < 3 then
		    let val x2 = x - 1   (* left = neg X *)
			val y2 = y 
		    in if x2 >= 0 then   (* keeps x2 on board ? *)
			   let val c2 = c + (cost x2 y2)
			   in left x2 y2 c2 ((x2,y2)::p) (n+1)  (* go left *)
			   end
		       else
			   ()
		    end		    
		else ()
    in ()
    end		       
and
right x y c p n =
if n >= 3 then
    let val _ = up (x + 1) y 1
	val _ = down x y 1
    in ()
    end		       
else
    let val _ = right x y (n + 1)
	val _ = up x y 1
	val _ = down x y 1
    in ()
    end 
and
up x y c p n =
if n >= 3 then
    let val _ = left (x + 1) y 1
	val _ = right x y 1
    in ()
    end		       
else
    let val _ = up x y (n + 1)
	val _ = left x y 1
	val _ = right x y 1
    in ()
    end 
and
down x y c p n =
if n >= 3 then
    let val _ = left (x + 1) y 1
	val _ = right x y 1
    in ()
    end		       
else
    let val _ = down x y (n + 1)
	val _ = left x y 1
	val _ = right x y 1
    in ()
    end ;

|#

