


fun up x y c p n =  (* ============= up procedure ================ *) 
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
in () end
;

  
