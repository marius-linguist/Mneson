with Charles.Algorithms.Generic_Binary_Search;
with Ada.Numerics.Discrete_Random;
with Integer_Sets;  use Integer_Sets;
with Integer_Vectors;  use Integer_Vectors;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;
with AI302.Containers;  use AI302.Containers;

procedure Test_Sets is

--     procedure Print (S : in Set_Type) is
--        procedure Process (C : Cursor_Type) is
--        begin
--           Put (Element (C), Width => 0);
--           Put (' ');
--        end;

--        procedure Iterate is
--           new Integer_Sets.Generic_Iteration;
--     begin
--        Iterate (S);
--        New_Line;
--     end Print;


   package Size_IO is
      new Ada.Text_IO.Integer_IO (AI302.Containers.Size_Type);

   use Size_IO;


   procedure Vet (S : in Set_Type) is
      E : Integer := -1;

      I : Cursor_Type := First (S);
      J : constant Cursor_Type := Back (S);
   begin
      while I /= J loop
         pragma Assert (Element (I) > E);
         E := Element (I);
         Increment (I);
      end loop;
   end Vet;

   function "=" (S : Set_Type; V : Vector_Type) return Boolean is
      I : Cursor_Type := First (S);
   begin
      if Length (S) /= Length (V) then
         return False;
      end if;

      for Index in First (V) .. Last (V) loop
         if Element (V, Index) /= Element (I) then
            return False;
         end if;

         Increment (I);
      end loop;

      pragma Assert (I = Back (S));
      return True;
   end "=";

   function Find (V : Vector_Type; E : Integer) return Integer is

      function Is_Less
        (Index : Integer;
         Item  : Integer) return Boolean is
      begin
         return Element (V, Index) < Item;
      end;

      function Is_Greater
        (Index : Integer;
         Item  : Integer) return Boolean is
      begin
         return Element (V, Index) > Item;
      end;

      function Find is
         new Charles.Algorithms.Generic_Binary_Search
              (Iterator_Type => Integer,
               Element_Type  => Integer);
   begin
      return Find (First (V), Back (V), Item => E);
   end;

   function Is_In (E : Integer; V : Vector_Type) return Boolean is
   begin
      return Find (V, E) /= Back (V);
   end;

   procedure Sort is
      new Integer_Vectors.Generic_Sort;

   package Random_Integers is
      new Ada.Numerics.Discrete_Random (Integer);

   G : Random_Integers.Generator;
   use Random_Integers;

   S, S2 : Set_Type;
   E : Natural;
   C : Cursor_Type;
   B : Boolean;
   N : Natural;
   V : Vector_Type;

   Seed : Integer := 0;

begin

   if Argument_Count > 0 then
      begin
         Seed := Integer'Value (Argument (1));
      exception
         when others =>
            null;
      end;
   end if;

   Put ("seed=");
   Put (Seed, Width => 0);
   New_Line (2);

   Reset (G, Seed);

   for I in 0 .. 1000 loop

      if I mod 100 = 0 then
         Put ("I=");
         Put (I, Width => 0);
         Put (" S.Length=");
         Put (Length (S), Width => 0);
         New_Line;
      end if;

--        if Length (S) mod 100 = 0 then   --spurious GNAT warning??
--            Put (Length (S), Width => 0);
--            New_Line;
--        end if;

--        Print (S);
--        New_Line;
      S2 := S;
      pragma Assert (S2 = S);

      E := Random (G) mod 750;  --?

      declare
         C2 : constant Cursor_Type := Find (S, E);
      begin
         Insert (S, E, C, B);
         pragma Assert (Element (C) = E);

         if B then
            pragma Assert (C2 = Back (S));
            Append (V, New_Item => E);
            Sort (V);
         else
            pragma Assert (C2 = C);
            null;
         end if;

         pragma Assert (Is_In (E, S));

         declare
            LB : constant Cursor_Type := Lower_Bound (S, E);
         begin
            pragma Assert (LB /= Back (S));
            pragma Assert (LB /= Upper_Bound (S, E));
            pragma Assert (Element (LB) = E);
            null;
         end;

         pragma Assert (Is_In (E, V));
      end;

      Vet (S);
      pragma Assert (S = V);
      pragma Assert (Succ (Last (S)) = Back (S));
      pragma Assert (Pred (Back (S)) = Last (S));
      pragma Assert (Succ (Back (S)) = First (S));
      pragma Assert (Pred (First (S)) = Back (S));

   end loop;

   pragma Assert (Length (S) = Length (V));

   New_Line;

   while not Is_Empty (S) loop

      if Length (S) mod 50 = 0 then
         Put ("S.Length=");
         Put (Length (S), Width => 0);
         New_Line;
      end if;

      S2 := S;
      pragma Assert (S2 = S);

      N := Random (G) mod Size_Type'Pos (Length (S));
      C := First (S);

      for I in 1 .. N loop
         Increment (C);
      end loop;

      pragma Assert (C /= Back (S));

      E := Element (C);

      N := Find (V, E);
      pragma Assert (N /= Back (V));

      Delete (S, Cursor => C);
      pragma Assert (not Is_In (E, S));
      pragma Assert (Lower_Bound (S, E) = Upper_Bound (S, E));

      Delete (V, Index => N);
      pragma Assert (not Is_In (E, V));

      pragma Assert (S = V);
      pragma Assert (Succ (Last (S)) = Back (S));
      pragma Assert (Pred (Back (S)) = Last (S));
      pragma Assert (Succ (Back (S)) = First (S));
      pragma Assert (Pred (First (S)) = Back (S));

--        if Length (S) mod 100 = 0 then
--           Put (Length (S), Width => 0);
--           New_Line;
--        end if;

--        Print (S);
--        New_Line;
      Vet (S);

   end loop;


end Test_Sets;



