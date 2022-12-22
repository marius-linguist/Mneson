with Integer_String_Maps;  use Integer_String_Maps;
with Ada.Numerics.Discrete_Random;
with Ada.Streams.Stream_IO;   use Ada.Streams;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;

procedure Test_Map_Stream_IO is

   procedure Print (M : in Map_Type) is

      procedure Process (C : Cursor_Type) is
      begin
         Put (Key (C), Width => 0);
         Put (Element (C));
         Put (' ');
      end;

      procedure Iterate is
         new Integer_String_Maps.Generic_Iteration;
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

   subtype Key_Subtype is Integer range 0 .. 99;

   package Random_Keys is
      new Ada.Numerics.Discrete_Random (Key_Subtype);

   subtype Element_Subtype is Character range Character'Succ (' ') .. '~';

   package Random_Elements is
      new Ada.Numerics.Discrete_Random (Element_Subtype);

   GK : Random_Keys.Generator;
   GE : Random_Elements.Generator;

   use Random_Keys, Random_Elements;

   M, M2 : Map_Type;

   F : Stream_IO.File_Type;
   use Stream_IO;

   C : Cursor_Type;
   B : Boolean;

begin

   Reset (GK);
   Reset (GE);

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

      Insert (M, Random (GK), String'(1 .. 1 => Random (GE)), C, B);
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

end Test_Map_Stream_IO;

