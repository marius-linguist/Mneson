with Ada.Streams.Stream_IO;  use Ada.Streams.Stream_IO;
with Integer_Vectors;        use Integer_Vectors;
with Ada.Numerics.Discrete_Random;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Vector_Stream_IO is

   procedure Print (V : in Vector_Type) is

      procedure Process (I : Integer) is
      begin
         Put (I, Width => 0);
         Put (' ');
      end;

      procedure Iterate is
         new Integer_Vectors.Generic_Constant_Iteration;

   begin -- Print

      Iterate (V);
      New_Line;

   end Print;


   subtype Integer_Subtype is Integer range 0 .. 99;

   package Random_Integers is
      new Ada.Numerics.Discrete_Random (Integer_Subtype);

   G : Random_Integers.Generator;
   use Random_Integers;

   V, V2 : Vector_Type;

   F : Ada.Streams.Stream_IO.File_Type;

--     function Sort is
--        new Integer_Vectors.Generic_Sort;

begin

   Create (F);

   Vector_Type'Write (Stream (F), V);

   Reset (F, In_File);

   Vector_Type'Read (Stream (F), V2);

   pragma Assert (V2 = V);

   for I in 1 .. 50 loop

      Put (I, Width => 0);
      New_Line;

      Append (V, New_Item => Random (G));

      Reset (F, Out_File);

      Vector_Type'Write (Stream (F), V);

      Reset (F, In_File);

      Vector_Type'Read (Stream (F), V2);

      Put (" V: ");
      Print (V);

      Put ("V2: ");
      Print (V2);

      New_Line;

      pragma Assert (V2 = V);

   end loop;


end Test_Vector_Stream_IO;



