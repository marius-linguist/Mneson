with Screen_Types;       use Screen_Types;
with Shapes;             use Shapes;
with Shapes.Rectangles;  use Shapes.Rectangles;
with Shapes.Lines;       use Shapes.Lines;
--with Shapes.Refresh;
with Faces;              use Faces;
with Shape_Vectors;      use Shape_Vectors;

with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

procedure Test_Shapes is

   procedure Output_West_X (V : in Vector_Type) is
   begin

      for I in First (V) .. Last (V) loop

         Put ("The x-coordinate of the west point of shape ");
         Put (I, Width => 0);
         Put (" is ");

         declare
            S : Shape_Type renames Element (V, I).all;
            W : constant Point_Type := West (S);
         begin
            Put (W.X, Width => 0);
         end;

         Put_Line (".");

      end loop;

   end Output_West_X;

   Rect : aliased Rectangle_Type;
   Line : aliased Line_Type;
   Face : aliased Face_Type;

   V : Vector_Type;

begin

   Initialize (Rect, P0 => (0, 0), P1 => (10, 10));
   Initialize (Line, P0 => (0, 15), Length => 17);
   Initialize (Face, P0 => (15, 10), P1 => (27, 18));

   Shapes.Refresh;

   Move (Face, DX => -10, DY => -10);

   Shapes.Stack (Line, On_Top_Of => Face);
   Shapes.Stack (Rect, On_Top_Of => Line);

   Shapes.Refresh;

   Append (V, Rect'Unchecked_Access);
   Append (V, Line'Unchecked_Access);
   Append (V, Face'Unchecked_Access);

   declare
      procedure Process
        (Shape : in Shape_Vectors.Element_Subtype) is
      begin
         Move (Shape.all, DX => 20, DY => 0);
      end;

      procedure Iterate is
         new Shape_Vectors.Generic_Constant_Iteration;
   begin
      Iterate (V);
   end;

   Shapes.Refresh;

   New_Line;

   Output_West_X (V);

   Put_Line ("Sorting the shapes according to the " &
             "x-coordinate of their west points.");

   declare
      function "<" (L, R : Shape_Vectors.Element_Subtype)
          return Boolean is

         LW : constant Point_Type := West (L.all);
         RW : constant Point_Type := West (R.all);
      begin
         return LW.X < RW.X;
      end;

      procedure Sort is
         new Shape_Vectors.Generic_Sort;
   begin
      Sort (V);
   end;

   Put_Line ("After sorting:");

   Output_West_X (V);

   New_Line;


end Test_Shapes;



