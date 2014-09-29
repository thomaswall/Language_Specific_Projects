with Quicksort; use Quicksort;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

	X: The_Array;
	
	procedure Start(X: in out The_Array) is
		task Printer is
			entry sorted;
			entry added(sum: in Integer);
		end Printer;

		task adder is
			entry sorted;
		end adder;

		task sorter is
			entry unsorted;
		end sorter;

		task body Printer is
		begin
			New_line;
			Put("Unsorted: ");
			for i in 1..30 loop
				Put(X(i)); -- print unsorted array
			end loop;

			sorter.unsorted; -- call to sort array

			accept sorted; -- wait for sorted array
			New_line;
			Put("Sorted: ");
			for i in 1..30 loop
				Put(X(i)); -- print unsorted array
			end loop;

			accept added(sum: in integer) do -- wait for sum then print
			New_line;
			Put("Sum:");
			Put(sum);
			end added;
		end;


		task body sorter is
		begin
			accept unsorted; -- start sorting
			Quicksort.Sorting(X,1,30); -- begin quicksorting with array
			adder.sorted; -- call to adder to add numbers in array
			Printer.sorted; -- call to printer to print sorted array
		end;

		task body adder is
		sum: Integer:= 0;
		begin
			accept sorted; -- wait for sorting to finish then add
			for i in 1..30 loop
				sum:=sum+X(i);
			end loop;
			Printer.added(sum); -- give printer sum value
		end;
	begin
		null;
	end Start;

begin
	for i in 1..30 loop
		Get(X(i));
	end loop;
	Start(X); -- put all procedures in function to avoid rendezvous
end Main;
