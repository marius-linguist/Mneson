with Data_Maps;

with AI302.Containers.Sorted_Sets;
pragma Elaborate_All (AI302.Containers.Sorted_Sets);

--with AI302.Containers.Sorted_Indefinite_Sets;
--pragma Elaborate_All (AI302.Containers.Sorted_Indefinite_Sets);

with AI302.Containers.String_Hashed_Maps;
pragma Elaborate_All (AI302.Containers.String_Hashed_Maps);

with AI302.Strings.Case_Insensitive;  use AI302.Strings;

with String_Vectors;  use String_Vectors;

package Relation_Maps is

   pragma Elaborate_Body;

   Places : Data_Maps.Map_Type;
   Dates  : Data_Maps.Map_Type;

   type String_Access is access all String;
   for String_Access'Storage_Size use 0;

   function To_Access is
      new Data_Maps.Generic_Element (String_Access);


   use AI302.Containers;

   function To_Access is
      new String_Vectors.Generic_Element (String_Access);


   function "<" (L, R : Vector_Type) return Boolean;

   package Date_Ordered_Sets is
      new Sorted_Sets (Vector_Type);
      --new Sorted_Indefinite_Sets (Vector_Type);

   procedure Insert
     (Set  : in out Date_Ordered_Sets.Set_Type;
      Name : in     String);

   type Vector_Access is access all Vector_Type;
   for Vector_Access'Storage_Size use 0;

   function To_Access is
      new Date_Ordered_Sets.Generic_Element (Vector_Access);

   function Find
     (Set  : Date_Ordered_Sets.Set_Type;
      Date : String) return Date_Ordered_Sets.Cursor_Type;

   function Is_In
     (Set  : Date_Ordered_Sets.Set_Type;
      Name : String) return Boolean;

--I added this to vectors directly.
--     function Is_In
--       (Vector : String_Vectors.Vector_Type;
--        Name   : String) return Boolean;

   package Date_Ordered_Set_Maps is
      new AI302.Containers.String_Hashed_Maps
       (Date_Ordered_Sets.Set_Type,
        Case_Insensitive.Hash,
        Case_Insensitive."=",
        Date_Ordered_Sets."=");


   type Set_Access is access all Date_Ordered_Sets.Set_Type;
   for Set_Access'Storage_Size use 0;

   function To_Access is
      new Date_Ordered_Set_Maps.Generic_Element (Set_Access);


   package String_Vector_Maps is
      new AI302.Containers.String_Hashed_Maps
       (String_Vectors.Vector_Type,
        Case_Insensitive.Hash,
        Case_Insensitive."=",
        String_Vectors."=");

   function To_Access is
      new String_Vector_Maps.Generic_Element (Vector_Access);


end Relation_Maps;

