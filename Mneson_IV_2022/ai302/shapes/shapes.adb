with AI302.Containers.Size_IO;  use AI302.Containers.Size_IO;

with AI302.Containers.Sorted_Sets;
pragma Elaborate_All (AI302.Containers.Sorted_Sets);

with System; use type System.Address;

with Screen;

with Ada.Text_IO;  use Ada.Text_IO;

package body Shapes is

   function "<" (L, R : Shape_Class_Access) return Boolean is
      pragma Inline ("<");
   begin
      return L.all'Address < R.all'Address;
   end;

   package Shape_Sets is
      new AI302.Containers.Sorted_Sets (Shape_Class_Access);

   Set : Shape_Sets.Set_Type;

   use Shape_Sets;


   procedure Initialize (Control : in out Control_Type) is

      E : constant Shape_Class_Access :=
        Control.Shape.all'Unchecked_Access;

      C : Cursor_Type;
      B : Boolean;
   begin
      --Control.Shape.Next := List;
      --List := Control.Shape.all'Unchecked_Access;

      Put ("shapes.set.length=");
      Put (Length (Set), Width => 0);
      Put_Line (" (before)");

      Insert (Set, E, C, B);
      pragma Assert (B);
      pragma Assert (Element (C) = E);
      pragma Assert (Is_In (E, Set));

      Put ("shapes.set.length=");
      Put (Length (Set), Width => 0);
      Put_Line (" (after)");

      New_Line;
   end;


   procedure Finalize (Control : in out Control_Type) is

      E : constant Shape_Class_Access :=
        Control.Shape.all'Unchecked_Access;
   begin
      Put ("shapes.set.length=");
      Put (Length (Set), Width => 0);
      Put_Line (" (before)");

      Delete (Set, E);
      pragma Assert (not Is_In (E, Set));

      Put ("shapes.set.length=");
      Put (Length (Set), Width => 0);
      Put_Line (" (after)");

      New_Line;
   end;


   procedure Stack
     (Shape     : in out Shape_Type'Class;
      On_Top_Of : in     Shape_Type'Class) is

      N : constant Point_Type := North (On_Top_Of);
      S : constant Point_Type := South (Shape);
   begin
      Move (Shape, DX => N.X - S.X, DY => N.Y - S.Y + 1);
   end;


   procedure Refresh is

      procedure Process (C : Cursor_Type) is
         Shape : Shape_Type'Class renames Element (C).all;
      begin
         Draw (Shape);
      end;

      procedure Iterate is
         new Shape_Sets.Generic_Iteration;
   begin
      Screen.Clear;
      Iterate (Set);
      Screen.Refresh;
   end;


end Shapes;

