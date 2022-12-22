with Ada.Numerics.Discrete_Random;
with Integer_Vectors;  use Integer_Vectors;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with AI302.Containers.Size_IO;  use AI302.Containers, AI302.Containers.Size_IO;

procedure Test_Sort is

   procedure Print (V : in Vector_Type) is
   begin
      for I in First (V) .. Last (V) loop
         Put (Element (V, I), Width => 0);
         Put (' ');
      end loop;

      New_Line;
   end Print;

   procedure Vet (V : in Vector_Type) is
      E : Integer := -1;
   begin
      for I in First (V) .. Last (V) loop
         pragma Assert (Element (V, I) >= E);
         E := Element (V, I);
      end loop;
   end Vet;

   package Random_Integers is
      new Ada.Numerics.Discrete_Random (Integer);

   G : Random_Integers.Generator;
   use Random_Integers;


   V, VS : Vector_Type;

   N : Natural;
   I : Integer_Vectors.Index_Subtype;
   E : Natural;

   procedure Sort is
      new Integer_Vectors.Generic_Sort;

--     procedure Reverse_Sort is
--        new Integer_Vectors.Generic_Sort2 (">");

begin

   loop

      VS := V;
      pragma Assert (VS = V);

      if Length (VS) mod 100 = 0 then
         Put (Length (VS), Width => 0);
         New_Line;
      end if;

      --Put_Line ("unsorted:");
      --Print (VS);

      Sort (VS);

      --Put_Line ("sorted:");
      --Print (VS);

      Vet (VS);

      --New_Line;

      exit when Length (V) >= 2000;

      E := Random (G) mod 1000;

      N := Random (G) mod Size_Type'Pos ((Length (V) + 1));

      I := Integer_Vectors.Index_Subtype'First + N;

      Insert (V, Before => I, New_Item => E);

      --Put ("length=");  Put (Length (V), Width => 0);
      --Put (" index="); Put (I, Width => 0);
      --Put (" item="); Put (E, Width => 0);
      --New_Line;

   end loop;


   while not Is_Empty (V) loop

      N := Random (G) mod Size_Type'Pos (Length (V));
      I := Integer_Vectors.Index_Subtype'First + N;

      Delete (V, Index => I);

      VS := V;
      pragma Assert (VS = V);

      if Length (VS) mod 100 = 0 then
         Put (Length (VS), Width => 0);
         New_Line;
      end if;

      --Put_Line ("unsorted:");
      --Print (VS);

      Sort (VS);

      --Put_Line ("sorted:");
      --Print (VS);

      Vet (VS);

      --New_Line;

   end loop;


end Test_Sort;

