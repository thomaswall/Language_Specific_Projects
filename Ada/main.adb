with Quicksort; use Quicksort;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

	X: The_Array;
	
	task Printer is
		entry print;
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
		accept print;
		New_line;
		Put("Unsorted: ");
		for i in 1..30 loop
			Put(X(i));
		end loop;

		sorter.unsorted;

		accept sorted;
		New_line;
		Put("Sorted: ");
		for i in 1..30 loop
			Put(X(i));
		end loop;

		accept added(sum: in integer) do
		New_line;
		Put("Sum:");
		Put(sum);
		end added;
	end;


	task body sorter is
	begin
		accept unsorted;
		Quicksort.Sorting(X,1,30);
		adder.sorted;
		Printer.sorted;
	end;

	task body adder is
	sum: Integer:= 0;
	begin
		accept sorted;
		for i in 1..30 loop
			sum:=sum+X(i);
		end loop;
		Printer.added(sum);
	end;

begin
	for i in 1..30 loop
		Get(X(i));
	end loop;
	Printer.print;


end Main;
