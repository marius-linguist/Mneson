with Ada.Streams;
with Ada.Finalization;
with AI302.Containers.Hashed_Maps;

generic

   type Key_Type is private;

   type Element_Type (<>) is private;

   with function Hash (Key : Key_Type)
      return AI302.Containers.Hash_Type is <>;

   with function Is_Equal_Key (Left, Right : Key_Type)
      return Boolean is "=";

   with function "=" (Left, Right : Element_Type)
      return Boolean is <>;

package Indefinite_Maps is

   pragma Preelaborate;

   subtype Key_Subtype is Key_Type;
   subtype Element_Subtype is Element_Type;

   type Map_Type is private;

   type Cursor_Type is private;

   function Null_Cursor return Cursor_Type;

   function "=" (Left, Right : Map_Type) return Boolean;

   function Length (Map : Map_Type) return Natural;

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

   function Size (Map : Map_Type) return Natural;

   procedure Resize (Map  : in out Map_Type;
                     Size : in     Natural);

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

   use Ada.Finalization;

   type Element_Access is access Element_Type;

   type Control_Type is new Controlled with record
      X : Element_Access;
   end record;

   use Ada.Streams;

   procedure Write
     (Stream  : access Root_Stream_Type'Class;
      Control : in     Control_Type);

   for Control_Type'Write use Write;

   procedure Read
     (Stream  : access Root_Stream_Type'Class;
      Control :    out Control_Type);

   for Control_Type'Read use Read;

   procedure Adjust (Control : in out Control_Type);

   procedure Finalize (Control : in out Control_Type);

   function Is_Equal_Element (L, R : Control_Type)
     return Boolean;

   package Rep_Types is
      new AI302.Containers.Hashed_Maps
        (Key_Type,
         Control_Type,
         Hash,
         Is_Equal_Key,
         Is_Equal_Element);

   type MT is new Rep_Types.Map_Type;

   type Map_Type is new MT;

   type CT is new Rep_Types.Cursor_Type;

   type Cursor_Type is new CT;

end Indefinite_Maps;





