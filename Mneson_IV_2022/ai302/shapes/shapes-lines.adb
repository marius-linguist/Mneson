with Screen;

package body Shapes.Lines is

   procedure Initialize
     (Line   : in out Line_Type;
      P0, P1 : in     Point_Type) is
   begin
      Line.W := P0;
      Line.E := P1;
   end;


   procedure Initialize
     (Line   : in out Line_Type;
      P0     : in     Point_Type;
      Length : in     Natural) is
   begin
      Line.W := (X => P0.X + Length - 1, Y => P0.Y);  --?
      Line.E := P0;
   end;


   procedure Draw (Line : in Line_Type) is
   begin
      Screen.Put_Line (Line.W, Line.E);
   end;


   procedure Move
     (Line : in out Line_Type;
      DX   : in     Integer;
      DY   : in     Integer) is

      W : Point_Type renames Line.W;
      E : Point_Type renames Line.E;

   begin

      W.X := W.X + DX;
      W.Y := W.Y + DY;

      E.X := E.X + DX;
      E.Y := E.Y + DY;

   end Move;



   function North (Line : Line_Type) return Point_Type is

      W : Point_Type renames Line.W;
      E : Point_Type renames Line.E;

      X : constant Integer := (W.X + E.X) / 2;
      Y : constant Integer := Integer'Max (W.Y, E.Y);
   begin
      return Point_Type'(X, Y);
   end;


   function South (Line : Line_Type) return Point_Type is

      W : Point_Type renames Line.W;
      E : Point_Type renames Line.E;

      X : constant Integer := (W.X + E.X) / 2;
      Y : constant Integer := Integer'Min (W.Y, E.Y);
   begin
      return Point_Type'(X, Y);
   end;


   function West (Line : Line_Type) return Point_Type is

      W : Point_Type renames Line.W;
      E : Point_Type renames Line.E;

      X : constant Integer := Integer'Min (W.X, E.X);
      Y : constant Integer := (W.Y + E.Y) / 2;
   begin
      return (X, Y);
   end;


   function SWest (Line : Line_Type) return Point_Type is

      W : Point_Type renames Line.W;
      E : Point_Type renames Line.E;

      X : constant Integer := Integer'Min (W.X, E.X);
      Y : constant Integer := Integer'Min (W.Y, E.Y);
   begin
      return (X, Y);
   end;


   function NEast (Line : Line_Type) return Point_Type is

      W : Point_Type renames Line.W;
      E : Point_Type renames Line.E;

      X : constant Integer := Integer'Max (W.X, E.X);
      Y : constant Integer := Integer'Max (W.Y, E.Y);
   begin
      return (X, Y);
   end;


end Shapes.Lines;
