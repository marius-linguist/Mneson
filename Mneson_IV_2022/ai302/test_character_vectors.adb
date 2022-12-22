with Ada.Text_IO;  use Ada.Text_IO;
with Character_Vectors;  use Character_Vectors;
with AI302.Containers.Size_IO;  use AI302.Containers, AI302.Containers.Size_IO;

procedure Test_Character_Vectors is

   V : Vector_Type;
   N : Size_Type := Length (V);

begin

   for C in Character range ' ' .. '~' loop

      Append (V, C);

      Put ("length=");
      Put (Length (V), Width => 0);
      Put (" size=");
      Put (Size (V), Width => 0);
      New_Line;

      for I in First (V) .. Last (V) loop
         Put (Element (V, I));
      end loop;

      New_Line;

      --Put_Line (To_Array (V));

      New_Line;

   end loop;

end Test_Character_Vectors;

