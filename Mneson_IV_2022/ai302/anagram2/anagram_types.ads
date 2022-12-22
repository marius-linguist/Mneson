with AI302.Containers.Indefinite_Vectors;
pragma Elaborate_All (AI302.Containers.Indefinite_Vectors);

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

end Anagram_Types;




