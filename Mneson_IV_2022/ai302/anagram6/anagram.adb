--This program was adapted from the example in Chapter 14 of
--   STL Tutorial and Reference Guide, 2nd ed.
--   Musser, Derge, and Saini
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

   M : Map_Types.Map_Type;
   use Map_Types;

   use Charles.Algorithms;
   use AI302.Strings;

begin

   if Argument_Count = 0 then
      Put_Line ("no dictionary specified");
      return;
   end if;

   declare

      function "<" (I, J : Character) return Boolean is
      begin
         return Standard."<" (To_Upper (I), To_Upper (J));
      end;

      procedure Sort is
         new AI302.Containers.Generic_Sort_Unconstrained_Array
           (Positive,
            Character,
            String);

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

      C : Cursor_Type;
      B : Boolean;

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

         Insert (M, K - I, C, B);

         declare
            ME : Integer_Vectors.Vector_Type renames To_Access (C).all;
            use Integer_Vectors;
         begin
            Append (ME, New_Item => I);
         end;

         I := K;

      end loop;

   end;


   declare

      type Cursor_Array is array (Size_Type range <>) of Cursor_Type;

      Cursors : Cursor_Array (1 .. Length (M));

   begin

      declare
         I : Size_Type := 0;

         procedure Process (C : Cursor_Type) is
         begin
            I := I + 1;
            Cursors (I) := C;
         end;

         procedure Iterate is
            new Map_Types.Generic_Iteration;
      begin
         Iterate (M);
      end;

      declare
         function "<" (L, R : Cursor_Type) return Boolean is
         begin
            return Key (L) < Key (R);
         end;

         procedure Sort is
            new AI302.Containers.Generic_Sort_Unconstrained_Array
              (Size_Type,
               Cursor_Type,
               Cursor_Array);
      begin
         Sort (Cursors);
      end;


      for Index in Cursors'Range loop

         declare

            C : constant Cursor_Type := Cursors (Index);

            I : constant Positive := Key (C);

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

            ME : Integer_Vectors.Vector_Type renames To_Access (C).all;
            use Integer_Vectors;
            pragma Assert (not Is_Empty (ME));

         begin

            Put ("There are ");
            Put (Length (ME), Width => 0);
            Put (" anagram group(s) of size ");
            Put (I, Width => 0);
            Put_Line (".");
            New_Line;
            Iterate (ME);
            New_Line;

         end;

      end loop;

   end;


end Anagram;

