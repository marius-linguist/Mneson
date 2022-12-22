--This program was adapted from the example in Chapter 15 of
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
with AI302.Containers.Generic_Sort_Unconstrained_Array;

procedure Anagram is

   M : Map_Types.Map_Type;
   V : Cursor_Vectors.Vector_Type;

   use Map_Types;
   use String_Vectors;

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

   --use Charles.Algorithms;
   --use AI302.Strings;

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
            E : String renames Line (Line'First .. Last);
            K : String := E;
            C : Cursor_Type;
            B : Boolean;
         begin
            Sort (K);
            Insert (M, K, C, B);

            declare
               ME : String_Vectors.Vector_Type renames To_Access (C).all;
               use Cursor_Vectors;
            begin
               if Length (ME) = 1 then
                  Append (V, New_Item => C);
               end if;

               Append (ME, New_Item => E);
            end;
         end;

      end loop;

      Close (File);
   end;

   declare
      function "<" (L, R : Cursor_Type) return Boolean is
         use String_Vectors;
         LV : Vector_Type renames To_Access (L).all;
         RV : Vector_Type renames To_Access (R).all;
      begin
         return Length (LV) < Length (RV);
      end;

      procedure Sort is
         new Cursor_Vectors.Generic_Sort;
   begin
      Sort (V);
   end;


   declare

      procedure Print (C : Cursor_Type) is
         procedure Process (S : String) is
         begin
            Put (S);
            Put (' ');
         end;

         procedure Iterate is
            new String_Vectors.Generic_Constant_Iteration;

         use String_Vectors;

         E : Vector_Type renames To_Access (C).all;
      begin
         Put ("   ");
         Iterate (E);
         New_Line;
      end;

      use Cursor_Vectors;

      I : Positive := First (V);
      J : constant Positive := Back (V);
      K : Positive;

      N : Size_Type;

   begin

      New_Line;

      while I /= J loop

         declare
            C : constant Cursor_Type := Element (V, I);
            E : String_Vectors.Vector_Type renames To_Access (C).all;
         begin
            N := Length (E);
         end;

         K := I + 1;

         while K /= J loop

            declare
               C : constant Cursor_Type := Element (V, K);
               E : String_Vectors.Vector_Type renames To_Access (C).all;
            begin
               exit when Length (E) /= N;
            end;

            K := K + 1;

         end loop;

         Put ("There are ");
         Put (Positive'(K - I), Width => 0);
         Put (" anagram group(s) of size ");
         Put (N, Width => 0);
         Put_Line (".");
         New_Line;

         while I /= K loop
            Print (C => Element (V, I));
            I := I + 1;
         end loop;

         New_Line;

      end loop;

   end;


end Anagram;

