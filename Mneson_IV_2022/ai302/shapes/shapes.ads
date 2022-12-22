with Screen_Types;  use Screen_Types;
with Ada.Finalization;

package Shapes is

   pragma Elaborate_Body;

   type Shape_Type is abstract tagged limited private;

   type Shape_Class_Access is access all Shape_Type'Class;
   for Shape_Class_Access'Storage_Size use 0;

   procedure Draw (Shape : in Shape_Type) is abstract;

   procedure Move
     (Shape : in out Shape_Type;
      DX    : in     Integer;
      DY    : in     Integer) is abstract;

   function North (Shape : Shape_Type) return Point_Type is abstract;

   function South (Shape : Shape_Type) return Point_Type is abstract;

   function West (Shape : Shape_Type) return Point_Type is abstract;

   function SWest (Shape : Shape_Type) return Point_Type is abstract;

   function NEast (Shape : Shape_Type) return Point_Type is abstract;

   procedure Stack
     (Shape     : in out Shape_Type'Class;
      On_Top_Of : in     Shape_Type'Class);

   procedure Refresh;

private

   use Ada.Finalization;

   type Control_Type (Shape : access Shape_Type) is
     new Limited_Controlled with null record;

   procedure Initialize (Control : in out Control_Type);

   procedure Finalize (Control : in out Control_Type);

   type Shape_Type is abstract tagged limited record
      Control : Control_Type (Shape_Type'Access);
      --Next    : Shape_Class_Access;
   end record;

   --List : Shape_Class_Access;

end Shapes;


