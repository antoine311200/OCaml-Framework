type 'a bilist = {
	before: 'a list;
	after: 'a list;
	length: int
};;

let print_array arr = 
	let n = Array.length arr in
	print_string "[| ";print_int arr.(0);print_string "; ";
	for i=1 to (n-2) do
		print_int arr.(i);print_string "; ";
	done;
	print_int arr.(n-1);print_string " |]";print_string "\n";;
   
let print_list lst = 
	let rec print = function
    	| [] -> print_string " |]";
        | [a] -> print_int a; print_string " |]";
        | a::q -> print_int a; print_string"; "; print q;
    in
    print_string "[| ";
    print lst;
    print_string "\n";
;;

let print_bilist blst =
	let rec print l1 l2 =
		match l1, l2 with
		| [], [] -> ()
		| a :: q, [] -> print q []; print_int a; print_string " ";
		| [], a :: q -> print_int a; print_string " "; print [] q;
		| la, lb -> print [] lb; print la [];
	in
	print_string "[ ";
	print blst.after blst.before;
    print_string "]\n";;

let debug_bilist blst =
	let rec print = function
    	| [] -> ()
        | [a] -> print_int a;
        | a::q -> print_int a; print_string" "; print q;
    in
    print_string "[ "; print blst.before; print_string " || "; print blst.after; print_string "]\n"
;;

let rec split l =
	let len = List.length l in
	let rec aux acc = function
      | [] -> [],[],acc
      | a::q ->
      	let (l1,l2,g) = aux (acc+1) q in
      	if acc < len/2 then a::l1,l2,acc+1
      	else l1,a::l2,acc+1
    in
    let (l1,l2,m) = aux 0 l in
    (l1, l2);
;;

let reverse blst = {after=blst.before; before=blst.after; length=blst.length};;

let init_from_array tab =
	let n = Array.length tab in
	let before = ref [] and after = ref [] in
	for i = n/2-1 downto 0 do
		before := tab.(i) :: !before
	done;
	for i = n/2 to n-1 do
		after := tab.(i) :: !after
	done;
	{after = !after; before = !before; length = n};;

let init_from_list lst = 
	let (bfr,aft) = split lst in
    { before = bfr; after = List.rev aft; length = List.length lst}
;;

let list_of_bilist blst =
    let rec aux l_aft = function
        | [], [] -> l_aft
        | a :: q, [] -> a :: aux l_aft (q, [])
        | bfr, a::q -> aux (a::l_aft) (bfr, q)
    in
    aux [] (blst.before, blst.after)
;;

let array_of_bilist blst =
	let tab = if blst.length = 0 then [||] else let h = if blst.before = [] then (List.hd blst.after) else (List.hd blst.before) in
	Array.make blst.length h in
	let rec convert i bf af =
		match bf, af with
		| [], [] -> ()
		| [], a :: q -> tab.(i) <- a; convert (i+1) [] q
		| a :: q, l -> tab.(i) <- a; convert (i+1) q l
	in
	convert 0 blst.before (List.rev blst.after);
	tab;;

let add_queue blst a = {before = (a :: blst.before); after = blst.after; length = blst.length+1};;
let add_head blst a = {before = blst.before; after = (a :: blst.after); length = blst.length+1};;

let shift blst =
	let rec split l =
      let len = List.length l in
      let rec aux acc = function
        | [] -> [],[],acc
        | a::q ->
          let (l1,l2,g) = aux (acc+1) q in
          if acc < len/2 then a::l1,l2,acc+1
          else l1,a::l2,acc+1
      in
      let (l1,l2,m) = aux 0 l in
      (l1, l2);
    in
    match (blst.before, blst.after) with
    	| ([],aft) when aft <> [] -> let (splt1, splt2) = split blst.after in
    		{before = (List.tl (List.rev splt1)); after = splt2; length = (blst.length-1)}
        | (_,_) -> {before = (List.tl blst.before); after = blst.after; length = blst.length}
;;

let pop blst =
	let rec split l =
      let len = List.length l in
      let rec aux acc = function
        | [] -> [],[],acc
        | a::q ->
          let (l1,l2,g) = aux (acc+1) q in
          if acc < len/2 then a::l1,l2,acc+1
          else l1,a::l2,acc+1
      in
      let (l1,l2,m) = aux 0 l in
      (l1, l2);
    in
    match (blst.before, blst.after) with
    	| ([],[]) -> raise (Failure "bilist is empty")
    	| (bfr,[]) when bfr <> [] -> let (splt1, splt2) = split blst.before in
        	{before = splt1; after = List.tl splt2; length = blst.length-1}
        | (_,_) -> {before = blst.before; after = List.tl blst.after; length = blst.length-1}
;;

let blst2 = ref (init_from_list [1;4;5;6;4;5;45;6;4;5]);;
print_bilist !blst2;;
debug_bilist !blst2;;
print_list (list_of_bilist !blst2);;
print_list (List.rev (list_of_bilist !blst2));;
print_bilist (reverse !blst2);;

let blst = ref (init_from_array [|1;4;6;2;8|]);;
blst := add_head !blst 9;;
print_bilist !blst;;
blst := add_queue !blst 5;;
blst := add_queue !blst 41;;
blst := add_queue !blst 7;;
blst := add_head !blst 8;;
print_bilist !blst;;
blst := pop !blst;;
print_bilist !blst;;
blst := pop !blst;;
print_bilist !blst;;
blst := pop !blst;;
print_bilist !blst;;
blst := pop !blst;;
print_bilist !blst;;
blst := pop !blst;;
print_bilist !blst;;
blst := pop !blst;;
print_bilist !blst;;
blst := pop !blst;;
print_bilist !blst;;
blst := pop !blst;;
print_bilist !blst;;
blst := pop !blst;;
print_bilist !blst;;
blst := pop !blst;;
print_bilist !blst;;
let blst = ref (init_from_array [|8;4;6;1;5;7;5;2;3|]);;
print_bilist !blst;;
print_bilist (reverse !blst);;
blst := shift !blst;;
print_bilist !blst;;
blst := shift !blst;;
print_bilist !blst;;
blst := shift !blst;;
print_bilist !blst;;
blst := shift !blst;;
print_bilist !blst;;
blst := shift !blst;;
print_bilist !blst;;
blst := shift !blst;;
print_bilist !blst;;
blst := shift !blst;;
print_bilist !blst;;
print_string "\n";;
(*shift blst;;
shift blst;;
shift blst;;
shift blst;;
print_bilist blst;; 
shift blst;;
print_bilist blst;; 
shift blst;;
print_bilist blst;; 
shift blst;;
print_bilist blst;; 
shift blst;;
print_bilist blst;; 
shift blst;;
print_bilist blst;; 
shift blst;;
print_bilist blst;; 
shift blst;;
print_bilist blst;;*)



(*let t = [1;2;3;4;5;6;7;8];;
let (t1,t2) = split t;;
print_list t1;;
print_list t2;;*)
