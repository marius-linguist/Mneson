with Screen;

package body Shapes.Rectangles is


   procedure Initialize
     (Rectangle : in out Rectangle_Type;
      P0, P1    : in     Point_Type) is

      SW : Point_Type renames Rectangle.SW;
      NE : Point_Type renames Rectangle.NE;

   begin
      
      if P0.X <= P1.X then
         
         if P0.Y <= P1.Y then
            SW := P0;
            NE := P1;
         else
            SW := (P0.X, P1.Y);
            NE := (P1.X, P0.Y);
         end if;
         
      else
         
         if P0.Y <= P1.Y then
            SW := (P1.X, P0.Y);
            NE := (P0.X, P1.Y);
         else
            SW := P1;
            NE := P0;
         end if;
         
      end if;
      
   end Initialize;
   

   procedure Draw (Rectangle : in Rectangle_Type) is
      
      SW : Point_Type renames Rectangle.SW;
      NE : Point_Type renames Rectangle.NE;

      NW : constant Point_Type := (SW.X, NE.Y);
      SE : constant Point_Type := (NE.X, SW.Y);

   begin
      
      Screen.Put_Line (NW, NE);
      Screen.Put_Line (NE, SE);
      Screen.Put_Line (SE, SW);
      Screen.Put_Line (SW, NW);
      
   end Draw;
   

   procedure Move
     (Rectangle : in out Rectangle_Type;
      DX        : in     Integer;
      DY        : in     Integer) is
      
      SW : Point_Type renames Rectangle.SW;
      NE : Point_Type renames Rectangle.NE;
      
   begin
      
      SW.X := SW.X + DX;
      SW.Y := SW.Y + DY;
      
      NE.X := NE.X + DX;
      NE.Y := NE.Y + DY;
      
   end Move;
      

   function North (Rectangle : Rectangle_Type) return Point_Type is
      
      SW : Point_Type renames Rectangle.SW;
      NE : Point_Type renames Rectangle.NE;
   begin
      return ((SW.X + NE.X)/2, NE.Y);
   end;
   

   function South (Rectangle : Rectangle_Type) return Point_Type is

      SW : Point_Type renames Rectangle.SW;
      NE : Point_Type renames Rectangle.NE;
   begin
      return ((SW.X + NE.X)/2, SW.Y);
   end;
   

   function West (Rectangle : Rectangle_Type) return Point_Type is

      SW : Point_Type renames Rectangle.SW;
      NE : Point_Type renames Rectangle.NE;
      
      X : constant Integer := SW.X;
      Y : constant Integer := (NE.Y + SW.Y) / 2;
   begin
      return (X, Y);
   end;
   

   function SWest (Rectangle : Rectangle_Type) return Point_Type is
   begin
      return Rectangle.SW;
   end;
   

   function NEast (Rectangle : Rectangle_Type) return Point_Type is
   begin
      return Rectangle.NE;
   end;
   

end Shapes.Rectangles;



