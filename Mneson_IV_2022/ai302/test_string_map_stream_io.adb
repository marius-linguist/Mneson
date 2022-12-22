with String_Integer_Maps;     use String_Integer_Maps;

with Ada.Numerics.Discrete_Random;
with Ada.Streams.Stream_IO;   use Ada.Streams;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;

procedure Test_String_Map_Stream_IO is

   procedure Print (M : in Map_Type) is

      procedure Process (C : Cursor_Type) is
      begin
         Put (Key (C));
         Put (Element (C), Width => 0);
         Put (' ');
      end;

      procedure Iterate is
         new Generic_Iteration;
   begin
      Iterate (M);
      New_Line;
   end;

   function Compare (L, R : Map_Type) return Boolean is

      LC : Cursor_Type := First (L);
      RC : Cursor_Type := First (R);

   begin

      loop

         if LC = Null_Cursor then
            return RC = Null_Cursor;
         end if;

         if RC = Null_Cursor then
            return False;
         end if;

         if Key (LC) /= Key (RC) then
            return False;
         end if;

         if Element (LC) /= Element (RC) then
            return False;
         end if;

         Increment (L, LC);
         Increment (R, RC);

      end loop;

   end Compare;

   package Random_Integers is
      new Ada.Numerics.Discrete_Random (Integer);

   subtype Character_Subtype is Character range Character'Succ (' ') .. '~';

   package Random_Characters is
      new Ada.Numerics.Discrete_Random (Character_Subtype);

   GI : Random_Integers.Generator;
   GC : Random_Characters.Generator;

   use Random_Integers, Random_Characters;

   M, M2 : Map_Type;

   F : Stream_IO.File_Type;
   use Stream_IO;

   C : Cursor_Type;
   B : Boolean;

   K : String (1 .. 8);
   E : Integer;

begin

   Reset (GI);
   Reset (GC);

   pragma Assert (M2 = M);
   pragma Assert (Compare (M2, M));

   Create (F);

   Map_Type'Write (Stream (F), M);

   Reset (F, In_File);

   Map_Type'Read (Stream (F), M2);
   pragma Assert (M2 = M);

   for I in 1 .. 100 loop

      Put (I, Width => 0);
      New_Line;

      for J in K'Range loop
         K (J) := Random (GC);
      end loop;

      E := Random (GI) mod 100;

      Insert (M, K, E, C, B);
      Put ("M1: ");
      Print (M);

      Reset (F, Out_File);

      Map_Type'Write (Stream (F), M);

      Reset (F, In_File);

      Map_Type'Read (Stream (F), M2);

      Put ("M2: ");
      Print (M2);
      New_Line;

      pragma Assert (M2 = M);
      pragma Assert (Compare (M2, M));

   end loop;

end Test_String_Map_Stream_IO;

