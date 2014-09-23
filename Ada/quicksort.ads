package Quicksort is
	type The_Array is array (Integer range 1..30) of Integer;
	procedure Sorting(X: in out The_Array;First,Second: in Integer);
end Quicksort;