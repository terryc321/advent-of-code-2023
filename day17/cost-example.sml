


val example = Array2.fromList ([[2,4,1,3,4,3,2,3,1,1,3,2,3],
				[3,2,1,5,4,5,3,5,3,5,6,2,3],
				[3,2,5,5,2,4,5,6,5,4,2,5,4],
				[3,4,4,6,5,8,5,8,4,5,4,5,2],
				[4,5,4,6,6,5,7,8,6,7,5,3,6],
				[1,4,3,8,5,9,8,7,9,8,4,5,4],
				[4,4,5,7,8,7,6,9,8,7,7,6,6],
				[3,6,3,7,8,7,7,9,7,9,6,5,3],
				[4,6,5,4,9,6,7,9,8,6,8,8,7],
				[4,5,6,4,6,7,9,9,8,6,4,5,3],
				[1,2,2,4,6,8,6,8,6,5,5,6,3],
				[2,5,4,6,5,4,8,8,8,7,7,3,5],
				[4,3,2,2,6,7,4,6,5,5,5,3,3]]);

val costExample = Array2.fromList([[0,0,0,0,0,0,0,0,0,0,0,0,0],
				   [0,0,0,0,0,0,0,0,0,0,0,0,0],
				   [0,0,0,0,0,0,0,0,0,0,0,0,0],
				   [0,0,0,0,0,0,0,0,0,0,0,0,0],
				   [0,0,0,0,0,0,0,0,0,0,0,0,0],
				   [0,0,0,0,0,0,0,0,0,0,0,0,0],
				   [0,0,0,0,0,0,0,0,0,0,0,0,0],
				   [0,0,0,0,0,0,0,0,0,0,0,0,0],
				   [0,0,0,0,0,0,0,0,0,0,0,0,0],
				   [0,0,0,0,0,0,0,0,0,0,0,0,0],
				   [0,0,0,0,0,0,0,0,0,0,0,0,0],
				   [0,0,0,0,0,0,0,0,0,0,0,0,0],
				   [0,0,0,0,0,0,0,0,0,0,0,0,0]]);

val pathExample = Array2.fromList([[[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)]],
				   [[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)]],
				   [[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)]],
				   [[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)]],
				   [[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)]],
				   [[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)]],
				   [[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)]],
				   [[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)]],
				   [[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)]],
				   [[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)]],
				   [[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)]],
				   [[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)]],
				   [[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)],[(0,0)]]]);

(* validated *)

(* individual costs *)
val width = 13;
val height = 13;
fun cost x y = Array2.sub (example, y,x) ;

(* sum costs *)
fun sumCost x y = Array2.sub (costExample, y,x) ;
fun updateSumCost x y c = Array2.update( costExample,y,x,c) ;

(* path record how got to best square *)
fun updatePath x y p = Array2.update( pathExample,y,x,p) ;
fun path x y = Array2.sub (pathExample, y,x) ;

(* reports true if keep search path alive *)
fun reportCost x y c p = let val s = sumCost x y
		       in
			   if s = 0 then
			       let val _ = updateSumCost x y c
				   (* val _ = updatePath *)
				   val _ = print ("new cost for " ^ (Int.toString x) ^ "," ^ (Int.toString y) ^ " => " ^
						  (Int.toString c) ^ "\n")
			       in false
			       end
			   else if s < c then  true
			        else false 				   
		       end ;

(* this should terminate this search branch *)
fun finish x y c = if x = width then 
                     if y = height then let val _ = print ("solution at " ^ (Int.toString x) ^ "," ^ (Int.toString y) ^ " : cost => " ^
							   (Int.toString c) ^ "\n")
					in
					    true
					end					    
                     else false  
                  else false;  




fun up x y c p n =  (* ============= up procedure ================ *) 
if finish x y c then () else if reportCost x y c p then () else  let val _ = let val x2 = x + ~1 (* x + ~1  *)  (* left *) 
            val y2 = y + 0 (*  left y unchanged  *)
        in if x2 > 0 then (*  x2 > 0 still onboard  *) 
              let val c2 = c + (cost x2 y2)
                in left x2 y2 c2 ((x2,y2)::p) (n+1)
              end
           else ()
         end
val _ = let val x2 = x + 1 (* x + 1  *)  (* right *) 
            val y2 = y + 0 (*  right y unchanged  *)
        in if x2 < width then (*  x2 < width still onboard  *) 
              let val c2 = c + (cost x2 y2)
                in right x2 y2 c2 ((x2,y2)::p) (n+1)
              end
           else ()
         end
val _ = if n < 3 then  (* up-up *) 
                 let val x2 = x + 0 (* x2 no change *) 
                     val y2 = y + ~1 (* y + ~1 *) 
                 in if y2 > 0 then (*  y2 > 0  ok  *)  
                    let val c2 = c + (cost x2 y2)              
                        in up x2 y2 c2 ((x2,y2)::p) (n + 1) (* go up *) 
                        end          
                    else ()             
                 end                 
         else ()                  
in () end
and   down x y c p n =  (* ============ down procedure =============== *)
if finish x y c then () else 
if reportCost x y c p then () else 
 let val _ = let val x2 = x + ~1 (* x + ~1  *)  (* left *) 
            val y2 = y + 0 (*  left y unchanged  *)
        in if x2 > 0 then (*  x2 > 0 still onboard  *) 
              let val c2 = c + (cost x2 y2)
                in left x2 y2 c2 ((x2,y2)::p) (n+1)
              end
           else ()
         end
val _ = let val x2 = x + 1 (* x + 1  *)  (* right *) 
            val y2 = y + 0 (*  right y unchanged  *)
        in if x2 < width then (*  x2 < width still onboard  *) 
              let val c2 = c + (cost x2 y2)
                in right x2 y2 c2 ((x2,y2)::p) (n+1)
              end
           else ()
         end
val _ = if n < 3 then  (* down-down *) 
                 let val x2 = x + 0 (* x2 no change *) 
                     val y2 = y + 1 (* y + 1 *) 
                 in if y2 < height then (*  y2 < height  ok  *)  
                    let val c2 = c + (cost x2 y2)              
                        in down x2 y2 c2 ((x2,y2)::p) (n + 1) (* go down *) 
                        end          
                    else ()             
                 end                 
         else ()                  
in () end
and    left x y c p n = (* ========== left procedure ============= *) 
if finish x y c then () else 
if reportCost x y c p then () else 
 let val _ = let val x2 = x + 0 (* x unchanged  *)  (* up *) 
            val y2 = y + ~1 (*  up y2 = y + ~1  *)
        in if y2 > 0 then (*  y2 > 0  *) 
              let val c2 = c + (cost x2 y2)
                in up x2 y2 c2 ((x2,y2)::p) (n+1)
              end
           else ()
         end
val _ = let val x2 = x + 0 (* x unchanged  *)  (* down *) 
            val y2 = y + 1 (*  down y2 = y + 1  *)
        in if y2 < height then (*  y2 < height  *) 
              let val c2 = c + (cost x2 y2)
                in down x2 y2 c2 ((x2,y2)::p) (n+1)
              end
           else ()
         end
val _ = if n < 3 then  (* left-left *) 
                 let val x2 = x + ~1 (* x + ~1 go left *) 
                     val y2 = y + 0 (* no change y *) 
                 in if x2 > 0 then (*  x2 > 0  ok  *)  
                    let val c2 = c + (cost x2 y2)              
                        in left x2 y2 c2 ((x2,y2)::p) (n + 1) (* go left *) 
                        end          
                    else ()             
                 end                 
         else ()                  
in () end
and   right x y c p n =  (* ========= right procedure ========= *)
if finish x y c then () else 
if reportCost x y c p then () else 
 let val _ = let val x2 = x + 0 (* x unchanged  *)  (* up *) 
            val y2 = y + ~1 (*  up y2 = y + ~1  *)
        in if y2 > 0 then (*  y2 > 0  *) 
              let val c2 = c + (cost x2 y2)
                in up x2 y2 c2 ((x2,y2)::p) (n+1)
              end
           else ()
         end
val _ = let val x2 = x + 0 (* x unchanged  *)  (* down *) 
            val y2 = y + 1 (*  down y2 = y + 1  *)
        in if y2 < height then (*  y2 < height  *) 
              let val c2 = c + (cost x2 y2)
                in down x2 y2 c2 ((x2,y2)::p) (n+1)
              end
           else ()
         end
val _ = if n < 3 then  (* right-right *) 
                 let val x2 = x + 1 (* x + 1 go right *) 
                     val y2 = y + 0 (* no change y *) 
                 in if x2 < width then (*  x2 < width  ok  *)  
                    let val c2 = c + (cost x2 y2)              
                        in right x2 y2 c2 ((x2,y2)::p) (n + 1) (* go right *) 
                        end          
                    else ()             
                 end                 
         else ()                  
in () end ;



fun run () =
    let val _ = right 1 0 (cost 1 0) [(1,0),(0,0)] 1
	val _ = down 0 1 (cost 0 1) [(0,1),(0,0)] 1
    in
	()
    end;


