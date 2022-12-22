with Screen_Types;  use Screen_Types;

package Screen is

   pragma Elaborate_Body;

   procedure Put_Point (X, Y : Integer);

   procedure Put_Point (P : Point_Type);

   procedure Put_Line
     (X0, Y0 : Integer;
      X1, Y1 : Integer);

   procedure Put_Line (P0, P1 : Point_Type);

   procedure Refresh;

   procedure Clear;

end Screen;
