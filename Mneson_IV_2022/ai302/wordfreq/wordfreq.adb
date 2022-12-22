with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

--with AI302.Containers.Generic_Sort;
with AI302.Containers.Generic_Sort_Constrained_Array;
with AI302.Containers.Size_IO;  use AI302.Containers, AI302.Containers.Size_IO;

with String_Integer_Maps;  use String_Integer_Maps;

procedure Wordfreq is

   Line  : String (1 .. 133);
   Last  : Natural;

   Word : Unbounded_String;
   Map : Map_Type;

   type Integer_Access is access all Integer;
   for Integer_Access'Storage_Size use 0;

   function To_Access is
      new Generic_Element (Integer_Access);

   procedure Insert is
      C : Cursor_Type;
      B : Boolean;
   begin
      if Length (Word) = 0 then
         return;
      end if;

      Insert (Map, To_String (Word), 0, C, B);

      declare
         N : Integer renames To_Access (C).all;
      begin
         N := N + 1;

         Put (Key (C));
         Put (':');
         Put (N, Width => 0);
         New_Line;
      end;

      Word := To_Unbounded_String ("");
   end Insert;


begin

   --use a pipe to execute this:
   -- cat file.txt | wordfreq
   --from a win shell use type instead of cat

   while not End_Of_File loop

      Get_Line (Line, Last);

      for I in Line'First .. Last loop
         if Is_Alphanumeric (Line (I)) then
            Append (Word, To_Lower (Line (I)));
         else
            Insert;
         end if;
      end loop;

      if Last < Line'Last then
         Insert;
      end if;

   end loop;

   declare
      I : Cursor_Type := First (Map);
      J : constant Cursor_Type := Back (Map);
   begin
      while I /= J loop
         Put (Key (I));
         Put (':');
         Put (Element (I), Width => 0);
         New_Line;

         I := Succ (Map, I);
      end loop;
   end;

   New_Line;

   Put ("map.length=");
   Put (Length (Map), Width => 0);
   New_Line;

   declare
--one way:
--      A : array (1 .. Length (Map)) of Cursor_Type;
--        function Is_Less (R, L : Positive) return Boolean is
--           RI : constant Cursor_Type := A (R);
--           LI : constant Cursor_Type := A (L);
--        begin
--           if Element (RI) = Element (LI) then
--              return Key (RI) > Key (LI);
--           else
--              return Element (RI) > Element (LI);
--           end if;
--        end Is_Less;

--        procedure Swap (I, J : Positive) is
--           E : constant Cursor_Type := A (I);
--        begin
--           A (I) := A (J);
--           A (J) := E;
--        end;

--        procedure Sort is
--           new AI302.Containers.Generic_Sort (Positive);

--here's another way:
      subtype Index_Subtype is Size_Type range 1 .. Length (Map);

      type Cursor_Array is array (Index_Subtype) of Cursor_Type;

      function "<" (R, L : Cursor_Type) return Boolean is
      begin
         if Element (R) = Element (L) then
            return Key (R) > Key (L);
         else
            return Element (R) > Element (L);
         end if;
      end;

      procedure Sort is
         new AI302.Containers.Generic_Sort_Constrained_Array
        (Index_Subtype,
         Cursor_Type,
         Cursor_Array);

      A : Cursor_Array;
      I : Cursor_Type := First (Map);
   begin
      for Index in A'Range loop
         A (Index) := I;
         I := Succ (Map, I);
      end loop;

      --Sort (First => A'First, Back => A'First + A'Length);
      Sort (A);

      for Index in A'Range loop
         Put (Element (A (Index)), Width => 0);
         Put (':');
         Put (Key (A (Index)));
         New_Line;
      end loop;
   end;

end Wordfreq;
