with Integer_Sets;  use Integer_Sets;

with Ada.Numerics.Discrete_Random;
with Ada.Streams.Stream_IO;   use Ada.Streams;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;

procedure Test_Set_Stream_IO is

   procedure Print (S : Set_Type) is

      procedure Process (C : Cursor_Type) is
      begin
         Put (Element (C), Width => 0);
         Put (' ');
      end;

      procedure Iterate is
         new Generic_Iteration;
   begin
      Iterate (S);
      New_Line;
   end;


   package Random_Integers is
      new Ada.Numerics.Discrete_Random (Integer);

   GI : Random_Integers.Generator;
   use Random_Integers;

   S, S2 : Set_Type;

   F : Stream_IO.File_Type;
   use Stream_IO;

   C : Cursor_Type;
   B : Boolean;

   E : Integer;

begin

   Reset (GI);

   pragma Assert (S2 = S);
   pragma Assert (Succ (Last (S)) = Back (S));
   pragma Assert (Pred (Back (S)) = Last (S));
   pragma Assert (Succ (Back (S)) = First (S));
   pragma Assert (Pred (First (S)) = Back (S));

   Create (F);

   Set_Type'Write (Stream (F), S);

   Reset (F, In_File);

   Set_Type'Read (Stream (F), S2);
   pragma Assert (S2 = S);

   for I in 1 .. 100 loop

      Put (I, Width => 0);
      New_Line;

      E := Random (GI) mod 100;

      Insert (S, E, C, B);
      Put ("S1: ");
      Print (S);

      Reset (F, Out_File);

      Set_Type'Write (Stream (F), S);

      Reset (F, In_File);

      Set_Type'Read (Stream (F), S2);

      Put ("S2: ");
      Print (S2);
      New_Line;

      pragma Assert (S2 = S);
      pragma Assert (Succ (Last (S)) = Back (S));
      pragma Assert (Pred (Back (S)) = Last (S));
      pragma Assert (Succ (Back (S)) = First (S));
      pragma Assert (Pred (First (S)) = Back (S));
      --E := Element (Back (S));

   end loop;

end Test_Set_Stream_IO;

