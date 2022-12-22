with AI302.Containers.Ordered_Sets;

package Set_Algorithms_Shootout is

   type Integer is 1 .. 1_000_000;

   package Integer_Sets is new Ada.Containers.Ordered_Sets (Integer);

   procedure Generate_Random_Set
      (Set : in out Integer_Sets.Set;
       Size : Natural;
       Max_Integer : Integer);

   type Set_Array is array (Positive range <>) of Integer_Sets.Set;

   generic
      with procedure Process (Integer);
   procedure Standard_Intersect (S : Set_Array);

end;

with Charles.Algorithms.Generic_Set_Intersection;

package body Set_Algorithms_Shootout is

   use Integer_Sets;

   procedure Standard_Intersect (S : Set_Array) is
   begin
      if S'Length > 2 then
         declare
            S_1_2 : Set_Type;
            procedure Ins (Iter : Cursor_Type) is
            begin Insert (S_1_2, Element (Iter)); end;
            procedure I_1_2 is new Charles.Algorithms.Generic_Set_Intersect
              (Iterator_Type => Iter, Process => Ins)
         begin
            I_1_2 (S (S'First), S (S'First + 1));
            Standard_Intersect
              (Set_Array'(1 => S_1_2) & S (S'First + 2 .. S'Last));
         end;
      elsif S'Length = 2 then
         declare
            procedure P (Iter : Cursor_Type) is
            begin Process (Element (Iter)); end;
            procedure Do_It is new Charles.Algorithms.Generic_Set_Intersect
              (Iterator_Type => Cursor_Type, Process => P)
         begin
            Do_It (S (S'First), S (S'First + 1));
         end;
      elsif S'Length = 1 then
         declare
            procedure P (Iter : Cursor_Type) is
            begin Process (Element (Iter)); end;
            procedure Traverse is new Integer_Sets.Generic_Iteration (P);
         begin
            Traverse (S (S'First));
         end;
      end if;
   end;

end;