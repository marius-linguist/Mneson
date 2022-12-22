with AI302.Containers.Hash_Tables;
pragma Elaborate_All (AI302.Containers.Hash_Tables);

with Ada.Finalization;
with Ada.Streams;

generic

   type Element_Type (<>) is private;

   with function Hash (Element : Element_Type)
      return AI302.Containers.Hash_Type is <>;

   with function Is_Equal_Key (Left, Right : Element_Type)
      return Boolean is "=";

   with function "=" (Left, Right : Element_Type)
      return Boolean is <>;

package Indefinite_Hashed_Sets is

   pragma Preelaborate;

   subtype Element_Subtype is Element_Type;


   type Set_Type is private;

   type Cursor_Type is private;

   Null_Cursor : constant Cursor_Type;

   function "=" (Left, Right : Set_Type) return Boolean;

   function Length (Set : Set_Type) return AI302.Containers.Size_Type;

   function Is_Empty (Set : Set_Type) return Boolean;

   procedure Clear (Set : in out Set_Type);

   procedure Swap (Left, Right : in out Set_Type);

   procedure Insert (Set      : in out Set_Type;
                     New_Item : in     Element_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean);

   procedure Delete (Set  : in out Set_Type;
                     Item : in     Element_Type);

   procedure Delete (Set    : in out Set_Type;
                     Cursor : in out Cursor_Type);

   function Is_In (Item : Element_Type;
                   Set  : Set_Type) return Boolean;

   function Find (Set  : Set_Type;
                  Item : Element_Type) return Cursor_Type;

   function Size (Set : Set_Type) return Natural;

   procedure Resize (Set  : in out Set_Type;
                     Size : in     AI302.Containers.Size_Type);

   function First (Set : Set_Type) return Cursor_Type;

   function Back (Set : Set_Type) return Cursor_Type;

   function Succ
     (Set    : Set_Type;
      Cursor : Cursor_Type) return Cursor_Type;

   procedure Increment
     (Set    : in     Set_Type;
      Cursor : in out Cursor_Type);

   function Is_Equal_Key (Left, Right : Cursor_Type)
     return Boolean;

   function Is_Equal_Key (Left  : Cursor_Type;
                          Right : Element_Type)
     return Boolean;

   function Is_Equal_Key (Left  : Element_Type;
                          Right : Cursor_Type)
     return Boolean;

   function Element (Cursor : Cursor_Type) return Element_Type;

   generic
      type Element_Access is access constant Element_Type;
   function Generic_Element
     (Cursor : Cursor_Type) return Element_Access;

   generic
      with procedure Process (Cursor : in Cursor_Type) is <>;
   procedure Generic_Iteration (Set : in Set_Type);

   --TODO: package Generic_Keys is ...;

private

   use Ada.Finalization;

   type Node_Type;
   type Node_Access is access Node_Type;

   type Element_Access is access Element_Type;

   type Node_Type is
      record
         Element : Element_Access;
         Next    : Node_Access;
      end record;

   function Hash_Node
     (Node : Node_Access) return AI302.Containers.Hash_Type;
   pragma Inline (Hash_Node);

   function Next
     (Node : Node_Access) return Node_Access;
   pragma Inline (Next);

   procedure Set_Next
     (Node : Node_Access;
      Next : Node_Access);
   pragma Inline (Set_Next);

   function Is_Equal_Key_Element_Node
     (Element : Element_Type;
      Node    : Node_Access) return Boolean;
   pragma Inline (Is_Equal_Key_Element_Node);

   function New_Node
     (Node : Node_Access) return Node_Access;
   pragma Inline (New_Node);

   procedure Free_Node (X : in out Node_Access);

   package Hash_Table_Types is
      new AI302.Containers.Hash_Tables
        (Element_Type,
         Node_Access,
         null,
         Hash_Key => Hash,
         Hash_Node => Hash_Node,
         Next => Next,
         Set_Next => Set_Next,
         Is_Equal_Key_Node => Is_Equal_Key_Element_Node,
         New_Node => New_Node,
         Free => Free_Node);

   use Hash_Table_Types;


   type Set_Type is new Controlled with record
      HT : Hash_Table_Type;
   end record;


   procedure Adjust (Set : in out Set_Type);

   procedure Finalize (Set : in out Set_Type);


   type Cursor_Type is
      record
         Node : Node_Access;
      end record;

   Null_Cursor : constant Cursor_Type := (Node => null);


   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Set    : in     Set_Type);

   for Set_Type'Write use Write;


   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Set    :    out Set_Type);

   for Set_Type'Read use Read;


end Indefinite_Hashed_Sets;


