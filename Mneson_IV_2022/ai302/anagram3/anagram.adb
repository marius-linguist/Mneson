--This program was adapted from the example in Chapter 14 of
--   STL Tutorial and Reference Guide, 2nd ed.
--   Musser, Derge, and Saini
--
--The example in the book uses a map whose element type is a list.  The
--argument is that if you used a vector of lists, and you had to expand
--the vector, then you'd have to the copy the vector elements during the
--expansion.  The vector elements are potentially large lists, so the
--expansion is expensive.
--
--If you use a map, then there is no cost for expansion, since the list
--elements are stored on internal storage nodes, which just get rehashed
--onto the new hash table; there is no copying of list elements.
--
--However, the copying of elements in only done in the case of a definite
--vector.  When an indefinite vector is expanded, the internal element
--pointers simply slide up.  The vector elements (here, integer-element
--vectors instead of lists) are not copied.  In this sense an indefinite
--vector is very similar to a map.
--
--The dictionary file for this program can be found at Musser's web site:
--
--  <http://www.cs.rpi.edu/~musser/stl-book/source/>
--
--The file name is "diction" or "diction.txt".
--

with Anagram_Types;  use Anagram_Types;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with AI302.Containers.Size_IO;  use AI302.Containers, AI302.Containers.Size_IO;
with AI302.Strings.Case_Insensitive;
with AI302.Containers.Generic_Sort_Unconstrained_Array;
with Charles.Algorithms.Generic_Adjacent_Find;

procedure Anagram is

   V : Anagram_Types.Pair_Vectors.Vector_Type;
   use Pair_Vectors;

   M : Map_Types.Vector_Type;
   use Map_Types;

   function Less_Case_Insensitive (I, J : Character) return Boolean is
   begin
      return To_Lower (I) < To_Lower (J);
   end;

   procedure Sort is
      new AI302.Containers.Generic_Sort_Unconstrained_Array
        (Positive,
         Character,
         String,
         "<" => Less_Case_Insensitive);

   use Charles.Algorithms;
   use AI302.Strings;

begin

   if Argument_Count = 0 then
      Put_Line ("no dictionary specified");
      return;
   end if;

   declare
      File : File_Type;

      Line : String (1 .. 81);
      Last : Natural;
   begin
      Open (File, In_File, Argument (1));

      while not End_Of_File (File) loop

         Get_Line (File, Line, Last);
         pragma Assert (Last > 0);
         pragma Assert (Last < Line'Last);

         declare
            N    : constant Positive := Last - Line'First + 1;
            Pair : Pair_Type (Length => N);
         begin
            Pair.Word := Line (Line'First .. Last);
            Pair.Sorted := Pair.Word;
            Sort (Pair.Sorted);
            Append (V, New_Item => Pair);
         end;

      end loop;

      Close (File);
   end;

   Put ("dictionary has ");
   Put (Length (V), Width => 0);
   Put_Line (" entries");
   New_Line;

   declare
      function "<" (L, R : Pair_Type) return Boolean is
         LS : String renames L.Sorted;
         RS : String renames R.Sorted;
      begin
         return Case_Insensitive."<" (LS, RS);
      end;

      procedure Sort is
         new Pair_Vectors.Generic_Sort;
   begin
      Sort (V);
   end;

   declare

      function Is_Equal (I, J : Positive) return Boolean is
         IE : Pair_Type renames To_Access (V, I).all;
         JE : Pair_Type renames To_Access (V, J).all;
      begin
         return Case_Insensitive."=" (IE.Sorted, JE.Sorted);
      end;

      function Find is
         new Charles.Algorithms.Generic_Adjacent_Find
          (Positive,
           Integer'Succ);

      I : Positive := First (V);
      J : constant Positive := Back (V);
      K : Positive;

      N : Map_Types.Index_Subtype;

   begin

      loop

         I := Find (I, J);

         exit when I = J;

         K := I + 1;

         while K /= J
           and then Is_Equal (I, K)
         loop
            K := K + 1;
         end loop;

         N := K - I;

         if Last (M) < N then

            --NOTE:
            --Expand the vector.  This does copy the existing
            --elements, because the internal pointers just
            --slide up.  See my comments at the top of this
            --module.
            --END NOTE.

            declare
               Empty : Map_Types.Element_Subtype;
               Count : constant Positive := N - Last (M);
            begin
               Insert (M, Back (M), Empty, Size_Type (Count));
            end;

         end if;

         declare
            ME : Map_Types.Element_Subtype renames To_Access (M, N).all;
            use Integer_Vectors;
         begin
            Append (ME, New_Item => I);
         end;

         I := K;

      end loop;

   end;

   for I in First (M) .. Last (M) loop

      declare

         procedure Process (J : in Positive) is
            K : constant Positive := J + I - 1;
         begin
            Put ("   ");

            for II in J .. K loop
               Put (To_Access (V, II).Word);
               Put (' ');
            end loop;

            New_Line;
         end;

         procedure Iterate is
            new Integer_Vectors.Generic_Constant_Iteration;

         ME : Map_Types.Element_Subtype renames To_Access (M, I).all;
         use Integer_Vectors;

      begin

         Put ("There are ");
         Put (Length (ME), Width => 0);
         Put (" anagram group(s) of size ");
         Put (I, Width => 0);
         Put_Line (".");
         New_Line;

         if not Is_Empty (ME) then
            Iterate (ME);
            New_Line;
         end if;

      end;

   end loop;

end Anagram;

