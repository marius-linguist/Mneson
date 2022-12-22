package Shapes.Rectangles is

   pragma Elaborate_Body;

   type Rectangle_Type is new Shape_Type with private;


   procedure Initialize
     (Rectangle : in out Rectangle_Type;
      P0, P1    : in     Point_Type);


   procedure Draw (Rectangle : in Rectangle_Type);

   procedure Move
     (Rectangle : in out Rectangle_Type;
      DX        : in     Integer;
      DY        : in     Integer);

   function North (Rectangle : Rectangle_Type) return Point_Type;

   function South (Rectangle : Rectangle_Type) return Point_Type;

   function West (Rectangle : Rectangle_Type) return Point_Type;

   function SWest (Rectangle : Rectangle_Type) return Point_Type;

   function NEast (Rectangle : Rectangle_Type) return Point_Type;

private

   type Rectangle_Type is new Shape_Type with record
      SW, NE : Point_Type;
   end record;


end Shapes.Rectangles;



