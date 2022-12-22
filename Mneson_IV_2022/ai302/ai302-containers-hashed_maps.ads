with AI302.Containers.Hash_Tables;
pragma Elaborate_All (AI302.Containers.Hash_Tables);

with Ada.Streams;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;  --need elab pragma?

generic

   type Key_Type is private;

   type Element_Type is private;

   with function Hash (Key : Key_Type)
      return Hash_Type is <>;

   with function Is_Equal_Key (Left, Right : Key_Type)
      return Boolean is "=";

   with function "=" (Left, Right : Element_Type)
      return Boolean is <>;

package AI302.Containers.Hashed_Maps is
   pragma Preelaborate (Hashed_Maps);

   subtype Key_Subtype is Key_Type;
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
                     Key      : in     Key_Type;
                     New_Item : in     Element_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean);

   procedure Replace (Map      : in out Map_Type;
                      Key      : in     Key_Type;
                      New_Item : in     Element_Type);

   procedure Insert (Map      : in out Map_Type;
                     Key      : in     Key_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean);

   procedure Delete (Map : in out Map_Type;
                     Key : in     Key_Type);

   procedure Delete (Map    : in out Map_Type;
                     Cursor : in out Cursor_Type);

   function Is_In (Key : Key_Type;
                   Map : Map_Type)
      return Boolean;

   function Find (Map : Map_Type;
                  Key : Key_Type)
      return Cursor_Type;

   function Element (Map : Map_Type;
                     Key : Key_Type)
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

   function Key (Cursor : Cursor_Type) return Key_Type;

   generic
      type Key_Access is access constant Key_Type;
   function Generic_Key (Cursor : Cursor_Type)
      return Key_Access;

   function Is_Equal_Key (Left, Right : Cursor_Type)
     return Boolean;

   function Is_Equal_Key (Left  : Cursor_Type;
                          Right : Key_Type)
     return Boolean;

   function Is_Equal_Key (Left  : Key_Type;
                          Right : Cursor_Type)
     return Boolean;

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

   type Node_Type is
      record  -- make this limited in Ada 0Y
         Key     : aliased Key_Type;
         Element : aliased Element_Type;
         Next    : Node_Access;
      end record;

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
     (Key  : Key_Type;
      Node : Node_Access) return Boolean;
   pragma Inline (Is_Equal_Key_Node);

   function New_Node
     (Node : Node_Access) return Node_Access;
   pragma Inline (New_Node);

   procedure Free is
      new Ada.Unchecked_Deallocation (Node_Type, Node_Access);

   package Hash_Table_Types is
      new AI302.Containers.Hash_Tables
        (Key_Type,
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


   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Map    : in     Map_Type);

   for Map_Type'Write use Write;


   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Map    :    out Map_Type);

   for Map_Type'Read use Read;


end AI302.Containers.Hashed_Maps;




