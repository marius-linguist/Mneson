package Shapes.Lines is

   pragma Elaborate_Body;

   type Line_Type is new Shape_Type with private;

   procedure Initialize
     (Line   : in out Line_Type;
      P0, P1 : in     Point_Type);

   procedure Initialize
     (Line   : in out Line_Type;
      P0     : in     Point_Type;
      Length : in     Natural);

   procedure Draw (Line : in Line_Type);

   procedure Move
     (Line : in out Line_Type;
      DX   : in     Integer;
      DY   : in     Integer);

   function North (Line : Line_Type) return Point_Type;

   function South (Line : Line_Type) return Point_Type;

   function West (Line : Line_Type) return Point_Type;

   function SWest (Line : Line_Type) return Point_Type;

   function NEast (Line : Line_Type) return Point_Type;

private

   type Line_Type is new Shape_Type with record
      W, E : Point_Type;
   end record;

end Shapes.Lines;



