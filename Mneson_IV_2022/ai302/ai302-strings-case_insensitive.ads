with AI302.Containers;

package AI302.Strings.Case_Insensitive is

   --pragma Pure;
   --
   --NOTE
   --Pure cat. is what the report specifies, but I need Ada.Characters.Handling
   --to get To_Upper.  So for now I'll just relax the cat.
   --END NOTE.
   --
   pragma Preelaborate;


   function "=" (Left, Right : String) return Boolean;

   --NOTE:
   --The following declaration is in the committee report:
   --
   --  function "/=" (Left, Right : String) return Boolean;
   --
   --but GNAT is telling me that:
   --"explicit definition of inequality not allowed"
   --so it looks like a bug in the report.
   --END NOTE.

   function ">" (Left, Right : String) return Boolean;

   function ">=" (Left, Right : String) return Boolean;

   function "<" (Left, Right : String) return Boolean;

   function "<=" (Left, Right : String) return Boolean;

   function Hash (Key : String) return Containers.Hash_Type;

end AI302.Strings.Case_Insensitive;





