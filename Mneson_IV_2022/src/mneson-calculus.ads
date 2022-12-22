-- GENERIC PACKAGE MNESON.CALCULUS (SPEC)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

generic

   with package Work is new Mneson.Work (<>);

package Mneson.Calculus is

   Marker : Vertex := Transient;

   subtype Selection is Vertex;

   function To_Selection (Set : Vertex) return Selection;
   function Singleton (X : Vertex) return Selection;

   function Targets (Set : Selection) return Selection;
   function Sources (Set : Selection) return Selection;
   function Intersect (Set_1, Set_2 : Selection) return Selection;
   function Subtract (Set_1, Set_2 : Selection) return Selection;
   --function To_List
     --(First_Instance : Vertex;
      --Order_Type : Vertex := Next) return Selection;
     
   function Is_In (X : Vertex; Set : Selection) return Boolean;

   procedure For_Each (Set : Selection; Process : Process_Vertex);
   
   function Extract
     (Set : Selection; Index : Positive := 1) return Vertex;

   function "+" (X : Vertex) return Selection
      renames Singleton;
   function "**" (Left, Right : Selection) return Selection
      renames Intersect;
   function "-" (Left, Right : Selection) return Selection
      renames Subtract;

   procedure Clear;
   
   --procedure Reorder_Elements
     --(Set : in out Selection;
      --Less_Than : Less_Than_Function;
      --Order_Type : Vertex := Next);
     
end;

-- NOTES

-- Extract (Singleton (X)) = X always holds.
