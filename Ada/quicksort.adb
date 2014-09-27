with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Quicksort is

procedure Sorting(X: in out The_Array;First,Second: in Integer) is
	
	Pivot:Integer;

	procedure Fork(X: in out The_Array; Pivot,L,U: in Integer) is

		task LeftSide;
		task RightSide;

		task body LeftSide is
		begin
			Sorting(X,L,Pivot-1);
		end LeftSide;

		task body RightSide is
		begin
			Sorting(X,Pivot+1,U);
		end RightSide;
	begin
		null;
	end Fork;

	procedure Split(X: in out The_Array;L,U: in Integer) is

		M: Integer;
		i: Integer;
		j: Integer;

		function Median (A,B,C: Integer) return Integer is
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

		procedure Swap(X: in out The_Array; A,B: in out Integer) is
		Temp:Integer;
		begin
			Temp:= A;
			A:=B;
			B:=Temp;
		end Swap;

	begin
			i:=L;
			j:=U;

			if i-j=1 then
					M := X(i);
				else
					M := Median(X(i),X(j),X((i+j)/2));
				end if;

			while i < j loop
				while X(i) < M loop
					i:= i+1;
				end loop;

				while X(j) > M loop
					j:=j-1;
				end loop;

				if X(i) > X(j) then
					Swap(X,X(i),X(j));
				end if;
			end loop;
			Pivot:=i;
	end Split;

begin
	if First<=Second then
		Split(X,First,Second);
		Fork(X,Pivot,First,Second);
	end if;
end Sorting;

end QuickSort;
