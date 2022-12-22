with String_Integer_Maps;  use String_Integer_Maps;
with Ada.Command_Line;     use Ada.Command_Line;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with To_Key;
with AI302.Containers.Size_IO;  use AI302.Containers, AI302.Containers.Size_IO;

procedure Test_Hash is

   Map : Map_Type;

   N : Natural;

   --Last : Natural;

   C : Cursor_Type;
   B : Boolean;

begin

   if Argument_Count = 0 then

      N := 2**16;

   else

      declare
         Image : constant String := Argument (1);
      begin
         N := Integer'Value (Image);
      end;

   end if;

   Resize (Map, Size_Type (N));

   for I in 1 .. N loop

      Insert (Map, To_Key (I, Base => 16), I, C, B);

      if I rem 10000 = 0 then
         Put ("i=");
         Put (I, Width => 0);
         New_Line;
      end if;

   end loop;

--     declare
--        I : Cursor_Type := First (Map);
--        J : constant Cursor_Type := Back (Map);
--     begin
--        while I /= J loop
--           Put (Key (I));
--           Put (":");
--           Put (Element (I), Width => 0);
--           New_Line;
--           I := Succ (Map, I);
--        end loop;

--        New_Line;
--     end;

   Put ("map.length=");
   Put (Length (Map), Width => 0);
   New_Line;

   declare
      Count : Integer'Base := 0;
   begin
      for I in reverse Integer range 1 .. N loop
         if Is_In (To_Key (I, Base => 10), Map) then
            Count := Count + 1;
         end if;
      end loop;

      Put ("map.count=");
      Put (Count, Width => 0);
      New_Line;
   end;

end Test_Hash;
