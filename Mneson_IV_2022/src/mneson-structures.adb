-- PACKAGE MNESON.STRUCTURES (GENERIC BODY)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with ada.text_io;use ada.text_io;

package body Mneson.Structures is

   use Work;

   --Transient_Set : Vertex := New_Vertex;

   function New_Set (Elements : Vertex_Array) return Vertex is
      Set : Vertex := New_Vertex;
   begin
      for I in Elements'Range loop
         Connect (Set, Elements (I));
      end loop;
      return Set;
   end;

   --function New_Transient_Record
   --  (Instances : Vertex_Array) return Vertex
   --is
   --   Rec : Vertex := New_Record (Instances);
   --begin
   --   Connect (Transient_Set, Rec);
   --   return Rec;
   --end;

   function New_Instance
     (Attribute_Type : Vertex; Value : Vertex) return Vertex
   is
      Instance : Vertex := New_Vertex;
   begin
--put_line("NEW_INSTANCE enter");
      Connect (Attribute_Type, Instance);
      Connect (Instance, Value);
--put_line("NEW_INSTANCE exit");
      return Instance;
   end;

   function New_Instance
     (Attribute_Type : Vertex; Value : String) return Vertex is
   begin
      return New_Instance (Attribute_Type, To_Vertex (Value));
   end;

   function New_Instance
     (Attribute_Type : Vertex; Value : Integer) return Vertex is
   begin
      return New_Instance (Attribute_Type, To_Vertex (Value));
   end;

   function New_Instance
     (Attribute_Type : String; Value : Vertex) return Vertex is
   begin
      return New_Instance (To_Vertex (Attribute_Type), Value);
   end;

   function New_Instance
     (Attribute_Type : String; Value : String) return Vertex is
   begin
      return New_Instance
        (To_Vertex (Attribute_Type),
         To_Vertex (Value));
   end;

   function New_Instance
     (Attribute_Type : String; Value : Integer) return Vertex is
   begin
      return New_Instance
        (To_Vertex (Attribute_Type),
         To_Vertex (Value));
   end;

   function New_Instance
     (Attribute_Type : Integer; Value : Vertex) return Vertex is
   begin
      return New_Instance (To_Vertex (Attribute_Type), Value);
   end;

   function New_Instance
     (Attribute_Type : Integer; Value : String) return Vertex is
   begin
      return New_Instance
        (To_Vertex (Attribute_Type),
         To_Vertex (Value));
   end;

   function New_Instance
     (Attribute_Type : Integer; Value : Integer) return Vertex is
   begin
      return New_Instance
        (To_Vertex (Attribute_Type),
         To_Vertex (Value));
   end;

   function Get_Element
     (Set : Vertex; Index : Positive := 1) return Vertex
   is
      N : Natural := 0;
      Element : Vertex;
      Done : exception;
      procedure Return_Indexed_Element (Target : Vertex) is
      begin
         N := N + 1;
         if N = Index then
            Element := Target;
            raise Done;
         end if;
      end;
   begin
      For_Each_Target (Set, Return_Indexed_Element'Unrestricted_Access);
      raise Constraint_Error;
   exception
      when Done => return Element;
   end;

   function Get_Instance
     (Subject, Attribute_Type : Vertex) return Vertex
   is
      Instance : Vertex;
      Done : exception;
      procedure Return_First (X : Vertex) is
      begin
         Instance := X;
         raise Done;
      end;
   begin
      For_Each_Common_Target
        (Subject, Attribute_Type, Return_First'Unrestricted_Access);
      raise Type_Error;
   exception
      when Done => return Instance;
   end;

   function Get_Value (Subject, Attribute_Type : Vertex) return Vertex is
   begin
      return Get_Element (Get_Instance (Subject, Attribute_Type));
   end;


   -- export this?
   procedure Delete_Record (Rec : Vertex) is
      procedure Disconnect_If_Valueless (X : Vertex) is
      begin
         if Is_Valueless (X) then Disconnect (X); end if;
      end;
   begin
      For_Each_Target (Rec, Disconnect_If_Valueless'Unrestricted_Access);
      Disconnect (Rec);
   end;


   --procedure Delete_All_Transient_Records is
   --begin
   --   For_Each_Target (Transient_Set, Delete_Record'Unrestricted_Access);
   --end;

end;

-- NOTES

-- Instance = attribute instance
-- Value = attribute value

-- Function Get_Instance is the only *retrieval* facility defined here.
-- Additional retrieval facilities are build upon the Mneson.Calculus,
-- which depends on Structures.
