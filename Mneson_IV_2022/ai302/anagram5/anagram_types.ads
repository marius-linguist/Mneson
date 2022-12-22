with AI302.Containers.Indefinite_Vectors;
pragma Elaborate_All (AI302.Containers.Indefinite_Vectors);

with AI302.Containers.Vectors;
pragma Elaborate_All (AI302.Containers.Vectors);

with AI302.Containers.Sorted_Sets;
pragma Elaborate_All (AI302.Containers.Sorted_Sets);

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

   subtype Map_Index_Subtype is Positive range 2 .. Positive'Last;

   type Entry_Type is
      record
         Index : Map_Index_Subtype;
         Data  : Integer_Vectors.Vector_Type;
      end record;

   function "<" (L, R : Entry_Type) return Boolean;

   package Set_Types is
      new AI302.Containers.Sorted_Sets (Entry_Type);

   type Entry_Access is access all Entry_Type;
   for Entry_Access'Storage_Size use 0;

   function To_Access is
      new Set_Types.Generic_Element (Entry_Access);

   function "<" (L : Map_Index_Subtype;
                 R : Entry_Type) return Boolean;

   function ">" (L : Map_Index_Subtype;
                 R : Entry_Type) return Boolean;

   package Map_Types is
      new Set_Types.Generic_Keys (Map_Index_Subtype);

   procedure Set_Key (E : in out Entry_Type;
                      K : in     Map_Index_Subtype);

   package Key_Insertion is
      new Map_Types.Generic_Insertion;

end Anagram_Types;




