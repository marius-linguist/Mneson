with Screen_Types;       use Screen_Types;
with Shapes;             use Shapes;
with Shapes.Rectangles;
with Shapes.Lines;

package Faces is

   pragma Elaborate_Body;

   type Face_Type is new Shape_Type with private;

   procedure Initialize
     (Face   : in out Face_Type;
      P0, P1 : in     Point_Type);

   procedure Draw (Face : in Face_Type);

   procedure Move
     (Face : in out Face_Type;
      DX   : in     Integer;
      DY   : in     Integer);

   function North (Face : Face_Type) return Point_Type;

   function South (Face : Face_Type) return Point_Type;

   function West (Face : Face_Type) return Point_Type;

   function SWest (Face : Face_Type) return Point_Type;

   function NEast (Face : Face_Type) return Point_Type;

private

   use Shapes.Rectangles, Shapes.Lines;

   type Face_Type is new Rectangle_Type with record
      L_Eye : Line_Type;
      R_Eye : Line_Type;
      Mouth : Line_Type;
   end record;

end Faces;





