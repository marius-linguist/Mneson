with AI302.Containers.Indefinite_Vectors;
pragma Elaborate_All (AI302.Containers.Indefinite_Vectors);

with AI302.Containers.Vectors;
pragma Elaborate_All (AI302.Containers.Vectors);

with AI302.Containers.Hashed_Maps;
pragma Elaborate_All (AI302.Containers.Hashed_Maps);

with Hash_Integer;

package Anagram_Types is
   pragma Preelaborate;

   type Pair_Type (Length : Positive) is
      record
         Sorted : String (1 .. Length);
         Word   : String (1 .. Length);
      end record;

   package Pair_Vectors is
      new AI302.Containers.Indefinite_Vectors (Positive, Pair_Type);

   type Pair_Access is access all Pair_Type;
   for Pair_Access'Storage_Size use 0;

   function To_Access is
      new Pair_Vectors.Generic_Element (Pair_Access);

   package Integer_Vectors is
      new AI302.Containers.Vectors (Positive, Positive);

   use Integer_Vectors;

   subtype Map_Index_Subtype is Positive range 2 .. Positive'Last;

   package Map_Types is
      new AI302.Containers.Hashed_Maps
        (Map_Index_Subtype,
         Integer_Vectors.Vector_Type,
         Hash_Integer);

   type Integer_Vector_Access is access all Integer_Vectors.Vector_Type;
   for Integer_Vector_Access'Storage_Size use 0;

   function To_Access is
      new Map_Types.Generic_Element (Integer_Vector_Access);

end Anagram_Types;




