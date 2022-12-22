-- GENERIC PACKAGE MNESON.STRUCTURES (SPEC)
-- Facilities for managing complex structures in the untyped graph.
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

generic

   with package Work is new Mneson.Work (<>);

package Mneson.Structures is

   type Vertex_Array is array (Positive range <>) of Vertex;

   function New_Set (Elements : Vertex_Array) return Vertex;

   function New_Record (Instances : Vertex_Array) return Vertex
      renames New_Set;

   --function New_Transient_Record (Instances : Vertex_Array) return Vertex;

   function New_Instance
     (Attribute_Type : Vertex; Value : Vertex) return Vertex;

   function New_Instance
     (Attribute_Type : Vertex; Value : String) return Vertex;

   function New_Instance
     (Attribute_Type : Vertex; Value : Integer) return Vertex;

   function New_Instance
     (Attribute_Type : String; Value : Vertex) return Vertex;

   function New_Instance
     (Attribute_Type : String; Value : String) return Vertex;

   function New_Instance
     (Attribute_Type : String; Value : Integer) return Vertex;

   function New_Instance
     (Attribute_Type : Integer; Value : Vertex) return Vertex;

   function New_Instance
     (Attribute_Type : Integer; Value : String) return Vertex;

   function New_Instance
     (Attribute_Type : Integer; Value : Integer) return Vertex;

   function Get_Element
     (Set : Vertex; Index : Positive := 1) return Vertex;

   function Get_Instance
     (Subject, Attribute_Type : Vertex) return Vertex;

   function Get_Value
     (Subject, Attribute_Type : Vertex) return Vertex;

   --procedure Delete_All_Transient_Records;

end;

-- NOTES

-- Instance = attribute instance
-- Value = attribute value

-- The Get_ functions are the only *retrieval* facilities defined
-- here. Their implementation depend exclusively on Work.
-- Additional retrieval facilities are build upon the Mneson.Calculus,
-- which depends on Structures.
