with Screen;

package body Faces is


   procedure Initialize
     (Face   : in out Face_Type;
      P0, P1 : in     Point_Type) is
      
      NE, SW : Point_Type;

      LL, HH : Integer;

      Eye_Y : Integer;

   begin
      
      Initialize (Rectangle_Type (Face), P0, P1);

      NE := NEast (Face);
      SW := SWest (Face);
      
      LL := NE.X - SW.X + 1;
      HH := NE.Y - SW.Y + 1;

      Eye_Y := SW.Y + (HH * 3) / 4;

      declare
         X : constant Integer := SW.X + 2;
      begin
         Initialize (Face.L_Eye, P0 => (X, Eye_Y), Length => 2);
      end;
      
      declare
         X : constant Integer := SW.X + LL - 4;
      begin
         Initialize (Face.R_Eye, P0 => (X, Eye_Y), Length => 2);
      end;
      
      declare
         X : constant Integer := SW.X + 2;
         Y : constant Integer := SW.Y + HH / 4;
      begin
         Initialize (Face.Mouth, P0 => (X, Y), Length => LL - 4);
      end;
      
   end Initialize;
   

   procedure Draw (Face : in Face_Type) is
      
      SW : constant Point_Type := SWest (Face);
      NE : constant Point_Type := NEast (Face);

      X : constant Integer := (SW.X + NE.X) / 2;
      Y : constant Integer := (SW.Y + NE.Y) / 2;
   begin
      Draw (Rectangle_Type (Face));
      Screen.Put_Point (X, Y);
   end;
   

   procedure Move
     (Face : in out Face_Type;
      DX   : in     Integer;
      DY   : in     Integer) is
   begin
      Move (Rectangle_Type (Face), DX, DY);
      Move (Face.L_Eye, DX, DY);
      Move (Face.R_Eye, DX, DY);
      Move (Face.Mouth, DX, DY);
   end;
   

   function North (Face : Face_Type) return Point_Type is
   begin
      return North (Rectangle_Type (Face));
   end;
   

   function South (Face : Face_Type) return Point_Type is
   begin
      return South (Rectangle_Type (Face));
   end;


   function West (Face : Face_Type) return Point_Type is
   begin
      return West (Rectangle_Type (Face));
   end;
   

   function SWest (Face : Face_Type) return Point_Type is
   begin
      return SWest (Rectangle_Type (Face));
   end;
   

   function NEast (Face : Face_Type) return Point_Type is
   begin
      return NEast (Rectangle_Type (Face));
   end;
   

end Faces;





