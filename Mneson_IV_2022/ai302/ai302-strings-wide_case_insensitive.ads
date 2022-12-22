with AI302.Containers;

package AI302.Strings.Wide_Case_Insensitive is

   --pragma Pure;
   --
   --NOTE
   --Pure cat. is what the report specifies, but I need Ada.Characters.Handling
   --to get To_Upper.  So for now I'll just relax the cat.
   --END NOTE.
   --
   pragma Preelaborate;


   function "=" (Left, Right : Wide_String) return Boolean;

   function ">" (Left, Right : Wide_String) return Boolean;

   function ">=" (Left, Right : Wide_String) return Boolean;

   function "<" (Left, Right : Wide_String) return Boolean;

   function "<=" (Left, Right : Wide_String) return Boolean;

   function Hash (Key : Wide_String) return Containers.Hash_Type;

end AI302.Strings.Wide_Case_Insensitive;

