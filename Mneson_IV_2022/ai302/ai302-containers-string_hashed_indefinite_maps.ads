with AI302.Containers.Hash_Tables;
pragma Elaborate_All (AI302.Containers.Hash_Tables);

with Ada.Streams;
with Ada.Finalization;

with AI302.Strings.Hash;

generic

   type Element_Type (<>) is private;

   with function Hash (Key : String)
     return Hash_Type is AI302.Strings.Hash;

   with function Is_Equal_Key (Left, Right : String)
      return Boolean is "=";

   with function "=" (Left, Right : Element_Type)
      return Boolean is <>;

package AI302.Containers.String_Hashed_Indefinite_Maps is
   pragma Preelaborate (String_Hashed_Indefinite_Maps);

   subtype Key_Subtype is String;
   subtype Element_Subtype is Element_Type;

   type Map_Type is private;

   type Cursor_Type is private;

   Null_Cursor : constant Cursor_Type;

   function "=" (Left, Right : Map_Type) return Boolean;

   function Length (Map : Map_Type) return Size_Type;

   function Is_Empty (Map : Map_Type) return Boolean;

   procedure Clear (Map : in out Map_Type);

   procedure Swap (Left, Right : in out Map_Type);

   procedure Insert (Map      : in out Map_Type;
                     Key      : in     String;
                     New_Item : in     Element_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean);

   --NOTE:
   --The following operation might be useful:
   --
   --procedure Insert (Map      : in out Map_Type;
   --                  Key      : in     String;
   --                  New_Item : in     Element_Type);
   --The only problem is that if you were to change from
   --a map to a mulitmap, then the meaning of the program
   --would change (since for a multimap insert always
   --succeeds.)
   --END NOTE.


   procedure Replace (Map      : in out Map_Type;
                      Key      : in     String;
                      New_Item : in     Element_Type);

   procedure Insert (Map      : in out Map_Type;
                     Key      : in     String;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean);

   procedure Delete (Map : in out Map_Type;
                     Key : in     String);

   procedure Delete (Map    : in out Map_Type;
                     Cursor : in out Cursor_Type);

   function Is_In (Key : String;
                   Map : Map_Type)
      return Boolean;

   function Find (Map : Map_Type;
                  Key : String)
      return Cursor_Type;

   function Element (Map : Map_Type;
                     Key : String)
     return Element_Type;

   function Size (Map : Map_Type)
     return Size_Type;

   procedure Resize (Map  : in out Map_Type;
                     Size : in     Size_Type);

   function First (Map : Map_Type) return Cursor_Type;

   function Back (Map : Map_Type) return Cursor_Type;

   function Succ
     (Map    : Map_Type;
      Cursor : Cursor_Type) return Cursor_Type;

   procedure Increment
     (Map    : in     Map_Type;
      Cursor : in out Cursor_Type);

   function Key (Cursor : Cursor_Type) return String;

   function Is_Equal_Key (Left, Right : Cursor_Type)
     return Boolean;

   function Is_Equal_Key (Left  : Cursor_Type;
                          Right : String)
     return Boolean;

   function Is_Equal_Key (Left  : String;
                          Right : Cursor_Type)
     return Boolean;


   --NOTE:
   --The following predicates might be useful too:
   --
   --function Is_Equal_Element (Left, Right : Cursor_Type)
   --  return Boolean;
   --
   --function Is_Equal_Element (Left  : Cursor_Type;
   --                           Right : Element_Type)
   --  return Boolean;
   --
   --function Is_Equal_Element (Left  : Element;
   --                           Right : Cursor_Type)
   --  return Boolean;
   --END NOTE.


   function Element (Cursor : Cursor_Type)
     return Element_Type;

   generic
      type Element_Access is access all Element_Type;
   function Generic_Element (Cursor : Cursor_Type)
      return Element_Access;

   procedure Replace_Element (Cursor : in Cursor_Type;
                              By     : in Element_Type);

   generic
      with procedure Process (Cursor : in Cursor_Type) is <>;
   procedure Generic_Iteration (Map : in Map_Type);

private

   type Node_Type;
   type Node_Access is access Node_Type;

   function Hash_Node
     (Node : Node_Access) return Hash_Type;
   pragma Inline (Hash_Node);

   function Next
     (Node : Node_Access) return Node_Access;
   pragma Inline (Next);

   procedure Set_Next
     (Node : Node_Access;
      Next : Node_Access);
   pragma Inline (Set_Next);

   function Is_Equal_Key_Node
     (Key  : String;
      Node : Node_Access) return Boolean;
   pragma Inline (Is_Equal_Key_Node);

   function New_Node
     (Node : Node_Access) return Node_Access;
   pragma Inline (New_Node);

   procedure Free (X : in out Node_Access);

   package Hash_Table_Types is
      new AI302.Containers.Hash_Tables
        (String,
         Node_Access,
         null,
         Hash_Key => Hash,
         Hash_Node => Hash_Node,
         Next => Next,
         Set_Next => Set_Next,
         Is_Equal_Key_Node => Is_Equal_Key_Node,
         New_Node => New_Node,
         Free => Free);

   use Hash_Table_Types;

   type Map_Type is new Ada.Finalization.Controlled with record
      HT : Hash_Table_Type;
   end record;

   procedure Adjust (Map : in out Map_Type);

   procedure Finalize (Map : in out Map_Type);

   type Cursor_Type is
      record
         Node : Node_Access;
      end record;

   Null_Cursor : constant Cursor_Type := (Node => null);

   
   use Ada.Streams;

   procedure Write
     (Stream : access Root_Stream_Type'Class;
      Map    : in     Map_Type);

   for Map_Type'Write use Write;


   procedure Read
     (Stream : access Root_Stream_Type'Class;
      Map    :    out Map_Type);

   for Map_Type'Read use Read;


end AI302.Containers.String_Hashed_Indefinite_Maps;

