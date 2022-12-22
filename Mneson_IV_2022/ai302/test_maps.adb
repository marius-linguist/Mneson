with Integer_Character_Maps;  use Integer_Character_Maps;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with AI302.Containers.Size_IO;  use AI302.Containers, AI302.Containers.Size_IO;

procedure Test_Maps is

   procedure Print (M : in Map_Type) is
      I : Cursor_Type := First (M);
   begin
      Put (Length (M), Width => 0);
      Put ('/');
      Put (Size (M), Width => 0);
      Put (": ");

      while I /= Null_Cursor loop -- or Back (M)
         Put (Key (I), Width => 0);
         Put (' ');
         Increment (M, I);
      end loop;

      New_Line;
   end Print;


   subtype Key_Subtype is Integer; --range 0 .. 75;

   package Random_Keys is
      new Ada.Numerics.Discrete_Random (Key_Subtype);

   subtype Element_Subtype is Character range ' ' .. '~';

   package Random_Elements is
      new Ada.Numerics.Discrete_Random (Element_Subtype);

   GK : Random_Keys.Generator;
   GE : Random_Elements.Generator;

   use Random_Keys, Random_Elements;

   M, M2 : Map_Type;

   K : Integer;
   E : Character;

   C, C2 : Cursor_Type;
   B : Boolean;

   N : Integer;

begin

   Reset (GK);
   Reset (GE);

   Print (M);

   M2 := M;
   pragma Assert (M2 = M);

   for I in 1 .. 100 loop

      K := Random (GK) mod 75;
      E := Random (GE);

      C := Find (M, K);

      Insert (M, K, E, C2, B);
      pragma Assert (Key (C2) = K);

      if B then
         pragma Assert (C = Null_Cursor);
         pragma Assert (Element (C2) = E);
         null;
      else
         pragma Assert (C = C2);
         null;
      end if;

      pragma Assert (Is_In (K, M));

      M2 := M;
      pragma Assert (M2 = M);

      Print (M);

   end loop;

   New_Line;

   while not Is_Empty (M) loop

      C := First (M);

      N := Random (GK) mod Size_Type'Pos (Length (M));

      for I in 1 .. N loop
         Increment (M, C);
      end loop;

      pragma Assert (C /= Null_Cursor);

      K := Key (C);

      Delete (M, C);

      pragma Assert (not Is_In (K, M));

      Delete (M, K);  -- should have no effect

      M2 := M;
      pragma Assert (M2 = M);

      Print (M);

   end loop;


end Test_Maps;


