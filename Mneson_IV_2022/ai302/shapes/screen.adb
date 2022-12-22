with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

package body Screen is

   Buffer : array (X_Subtype, Y_Subtype) of Character :=
     (others => (others => ' '));


   procedure Put_Point (X, Y : Integer) is
   begin
      if X in Buffer'Range (1)
        and then Y in Buffer'Range (2)
      then
         Buffer (X, Y) := '*';
      end if;
   end;


   procedure Clear is
   begin
      Buffer := (others => (others => ' '));
   end;


   procedure Refresh is
   begin

      New_Line;

      Put ("   ");

      for X in X_Subtype loop
         Put (X rem 10, Width => 1);
      end loop;

      New_Line;

      for Y in reverse Buffer'Range (2) loop

         Put (Y, Width => 2);
         Put (':');

         for X in Buffer'Range (1) loop

            Put (Buffer (X, Y));

         end loop;

         Put (':');
         Put (Y, Width => 0);

         New_Line;

      end loop;

      Put ("   ");

      for X in X_Subtype loop
         Put (X rem 10, Width => 1);
      end loop;

      New_Line;

   end Refresh;



   procedure Put_Line
     (X0, Y0 : Integer;
      X1, Y1 : Integer) is

      X : Integer := X0;
      Y : Integer := Y0;

      DX, DY : Integer;

      A : Integer := X1 - X0;
      B : Integer := Y1 - Y0;

      Two_A, Two_B : Integer;

      Xcrit : Integer;
      Eps : Integer;

   begin

      if A < 0 then
         DX := -1;
         A := -A;
      else
         DX := 1;
      end if;

      if B < 0 then
         DY := -1;
         B := -B;
      else
         DY := 1;
      end if;

      Two_A := 2 * A;
      Two_B := 2 * B;

      Xcrit := -B + Two_A;
      Eps := 0;

      loop

         Put_Point (X, Y);

         exit when X = X1 and Y = Y1;

         if Eps <= Xcrit then
            X := X + DX;
            Eps := Eps + Two_B;
         end if;

         if Eps >= A or A <= B then
            Y := Y + DY;
            Eps := Eps - Two_A;
         end if;

      end loop;

   end Put_Line;


   procedure Put_Line (P0, P1 : Point_Type) is
   begin
      Put_Line (P0.X, P0.Y, P1.X, P1.Y);
   end;


   procedure Put_Point (P : Point_Type) is
   begin
      Put_Point (P.X, P.Y);
   end;


end Screen;
