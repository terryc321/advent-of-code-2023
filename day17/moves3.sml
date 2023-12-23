
val width = 13;
val height = 13;

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

