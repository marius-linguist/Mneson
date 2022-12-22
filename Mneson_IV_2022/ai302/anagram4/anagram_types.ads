with AI302.Containers.String_Hashed_Maps;
pragma Elaborate_All (AI302.Containers.String_Hashed_Maps);

with AI302.Containers.Indefinite_Vectors;
pragma Elaborate_All (AI302.Containers.Indefinite_Vectors);

with AI302.Containers.Vectors;
pragma Elaborate_All (AI302.Containers.Vectors);

with AI302.Strings.Case_Insensitive;  use AI302.Strings;

package Anagram_Types is
   pragma Preelaborate;

   package String_Vectors is
      new AI302.Containers.Indefinite_Vectors
       (Positive,
        String,
        Case_Insensitive."=");

   use String_Vectors;

   package Map_Types is
      new AI302.Containers.String_Hashed_Maps
        (Vector_Type,
         Case_Insensitive.Hash,
         Case_Insensitive."=");

   type String_Vector_Access is access all Vector_Type;
   for String_Vector_Access'Storage_Size use 0;

   function To_Access is
      new Map_Types.Generic_Element (String_Vector_Access);

   use Map_Types;

   package Cursor_Vectors is
      new AI302.Containers.Vectors
       (Positive,
        Map_Types.Cursor_Type);

end Anagram_Types;




