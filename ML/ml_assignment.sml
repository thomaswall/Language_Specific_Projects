Control.Print.printDepth := 100;
Control.Print.printLength := 100;


fun partition pivot (op <) ([],left,right) = (left,right)
       | partition pivot (op <) (x::xs, left, right) =
       	    if x < pivot then
       	    	partition pivot (op <) (xs,x::left,right)
       	    else
       	    	partition pivot (op <) (xs,left,x::right);


fun partitionSort operation [] = []
|  partitionSort operation (x::xs) = 
   let val (left,right) =
   	   partition x operation (xs,[],[])
   	in
   	  (partitionSort operation left) @ [x] @ (partitionSort operation right)
   	end;



fun intPartitionSort [] = []
|  intPartitionSort (x::xs) = 
   let val (left,right) =
   	   partition x (op <) (xs,[],[])
   	in
   	  (intPartitionSort left) @ [x] @ (intPartitionSort right)
   	end;


datatype 'a tree = leaf of 'a | node of ('a tree list);


fun sortTree operation (leaf x) = leaf (partitionSort operation x)
|   sortTree operation (node x) =
	node (map (sortTree operation) x);



fun helper_merge (op <) L1 [] L3 = L3 @ L1
| helper_merge (op <) [] L2 L3 = L3 @ L2
| helper_merge (op <) (x::xs) (y::ys) L3 =
    if x < y then
    	helper_merge (op <) xs (y::ys) (L3 @ [x])
    else
    	helper_merge (op <) (x::xs) ys (L3 @ [y]);


fun merge operation [] [] = []
|   merge operation L1 L2 =
	  helper_merge operation L1 L2 [];



fun helper_mergeTree operation (leaf x) = x
|  helper_mergeTree operation (node x) =
     foldr (fn (L1,L2) => merge operation L1 L2) [] (map (helper_mergeTree operation) x);


fun mergeTree operation T = helper_mergeTree operation (sortTree operation T);


	



