with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Quicksort is

procedure Sorting(X: in out The_Array;First,Second: in Integer) is
	
	Pivot:Integer;

	procedure Fork(X: in out The_Array; Pivot,L,U: in Integer) is -- runs sorting procedure on left side of pivot and right side concurrently

		task LeftSide;
		task RightSide;

		task body LeftSide is
		begin
			Sorting(X,L,Pivot-1); -- left side of pivot
		end LeftSide;

		task body RightSide is
		begin
			Sorting(X,Pivot+1,U); -- right side of pivot
		end RightSide;
	begin
		null;
	end Fork;

	procedure Split(X: in out The_Array;L,U: in Integer) is

		M: Integer;
		i: Integer;
		j: Integer;

		function Median (A,B,C: Integer) return Integer is --median function
		begin
			if A < B then
				if A >= C then
					return A;
				elsif B < C then
					return B;
				end if;
			else
				if A < C then
					return A;
				elsif B >= C then
					return B;
				end if;
			end if;
			return C;
		end Median;

		procedure Swap(X: in out The_Array; A,B: in out Integer) is -- swap function
		Temp:Integer;
		begin
			Temp:= A;
			A:=B;
			B:=Temp;
		end Swap;

	begin
			i:=L;
			j:=U;

			if i-j=1 then -- find median, if no median, set to X(i)
					M := X(i);
				else
					M := Median(X(i),X(j),X((i+j)/2));
				end if;

			while i < j loop --until i,j same index
				while X(i) < M loop --while less than median
					i:= i+1;
				end loop;

				while X(j) > M loop -- while greater than median
					j:=j-1;
				end loop;

				if X(i) > X(j) then
					Swap(X,X(i),X(j)); -- swap if X(i)>X(j)
				end if;
			end loop;
			Pivot:=i; -- pivot is where i leaves off
	end Split;

begin
	if First<=Second then -- while array portion has some size
		Split(X,First,Second); -- split and find pivot
		Fork(X,Pivot,First,Second); -- fork right and left side of pivot to run same process concurrently
	end if;
end Sorting;

end QuickSort;
