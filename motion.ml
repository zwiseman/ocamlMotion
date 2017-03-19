
(**
CPSC/ECE 3520 SSII 2016\\ Software Design Exercise \#2
Zack Wiseman
07/25/2016
motion.caml
THis program is to take an two images, find the defferences and track the motion that occurred. 
*)

(*diffImRow(image1Row,image2Row)*)
let rec diffImRow(image1Row,image2Row) = 
	match image1Row,image2Row with
	| [],[] -> []
	| hd::tl,hd1::tl1 -> hd1-hd :: diffImRow(tl,tl1)
	| _,_ -> [];;

(*diffIm(image1,image2)*)
let rec diffIm(image1,image2) =
	match image1,image2 with
	| [],[] -> []
	| hd::tl,hd1::tl1 -> diffImRow(hd,hd1) :: diffIm(tl,tl1)
	| _,_ -> [];;

let rec noDiffR(row) = 
	match row with
	| [] -> true
	| 0::tl -> noDiffR(tl)
	| hd::tl -> false;;

(*noDiff(diffimage)*)
let rec noDiff(diffimage) = 
	match diffimage with 
	| [] -> true 
	| hd::tl -> if noDiffR(hd) = false then false else noDiff(tl);;

let rec printR r =
	match r with 
	| [] -> print_string"\n"
	| hd::tl -> print_string " "; print_int hd ; printR tl;;

(*pp_my_image s *)
let rec pp_my_image s =
	match s with
	| [] -> ()
	| hd::tl -> printR hd ; pp_my_image tl;;

(*rowmaskpos(image2row,diffrow)*)
let rec rowmaskpos(image2row,diffrow) = 
	match image2row,diffrow with
	| [],[] -> []
	| hd::tl, hd1::tl1-> if hd1 > 0 then hd :: rowmaskpos(tl,tl1) else 0 :: rowmaskpos(tl,tl1)
	| _,_ -> [];;

(*maskpos(image2,diffimage)*)
let rec maskpos(image2,diffimage) =
	match image2,diffimage with 
	| [],[] -> []
	| hd::tl, hd1::tl1 -> rowmaskpos(hd,hd1) :: maskpos(tl,tl1)
	| _,_ -> [];;

(*rowmaskneg(image1row,diffrow)*)
let rec rowmaskneg(image1row,diffrow) =
	match image1row,diffrow with 
	| [],[] -> []
	| hd::tl,hd1::tl1 -> if hd1 < 0 then hd :: rowmaskneg(tl,tl1) else 0 :: rowmaskneg(tl,tl1)
	| _,_ -> [];;

(*maskneg(image,diffimage)*)
let rec maskneg(image1,diffimage) = 
	match image1,diffimage with 
	| [],[] -> []
	| hd::tl,hd1::tl1 -> rowmaskneg(hd,hd1) :: maskneg(tl,tl1)
	| _,_ -> [];;

(*tuplediffint((i1,j1),(i2,j2))*)
let rec tuplediffint((i1,j1),(i2,j2)) =
	(i2 - i1 , j2 - j1);;

(*fnzrow(maskedrow)*)
let rec fnzrow(maskedrow) =  
	match maskedrow with
	| [] -> 0
	| hd::tl -> if hd <> 0 then hd  else fnzrow(tl);;

(*firstnonzero(maskedimage)*)
let rec firstnonzero(maskedimage) = 
	match maskedimage with
	| [] -> 0,0;
	| hd::tl -> firstnonzero(tl);;

(*motion(image1,image2)*) 
let motion(image1,image2) = tuplediffint(firstnonzero(maskneg(image1,(diffIm (image1,image2)))),firstnonzero(maskpos(image2,(diffIm (image1,image2)))));;
