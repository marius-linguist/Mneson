with System;  use type System.Address;

package body AI302.Containers.Wide_String_Hashed_Maps is

   function Hash_Node
     (Node : Node_Access) return Hash_Type is
   begin
      return Hash (Node.Key);
   end;

   function Next
     (Node : Node_Access) return Node_Access is
   begin
      return Node.Next;
   end;

   procedure Set_Next
     (Node : Node_Access;
      Next : Node_Access) is
   begin
      Node.Next := Next;
   end;

   function Is_Equal_Key_Node
     (Key  : Wide_String;
      Node : Node_Access) return Boolean is
   begin
      return Is_Equal_Key (Key, Node.Key);
   end;


   function New_Node
     (Node : Node_Access) return Node_Access is

      Result : constant Node_Access :=
        new Node_Type'(Key_Length => Node.Key_Length,
                       Key        => Node.Key,
                       Element    => Node.Element,
                       Next       => null);
   begin
      return Result;
   end;


   procedure Adjust (Map : in out Map_Type) is
   begin
      Adjust (Map.HT);
   end;


   procedure Finalize (Map : in out Map_Type) is
   begin
      Finalize (Map.HT);
   end;


   function Is_Equal_Element_Node_Node
     (L, R : Node_Access) return Boolean is

      pragma Inline (Is_Equal_Element_Node_Node);
   begin
      return L.Element = R.Element;
   end;

   function Is_Equal is
      new Hash_Table_Types.Generic_Equal (Is_Equal_Element_Node_Node);

   function "=" (Left, Right : Map_Type) return Boolean is
   begin

      if Left'Address = Right'Address then
         return True;
      end if;

      return Is_Equal (Left.HT, Right.HT);

   end "=";


   function Length (Map : Map_Type) return Size_Type is
   begin
      return Map.HT.Length;
   end;


   function Is_Empty (Map : Map_Type) return Boolean is
   begin
      return Length (Map) = 0;
   end;


   procedure Clear (Map : in out Map_Type) is
   begin
      Clear (Map.HT);
   end;


   procedure Swap (Left, Right : in out Map_Type) is
   begin
      Swap (Left.HT, Right.HT);
   end;


   function Size (Map : Map_Type) return Size_Type is
   begin
      if Map.HT.Buckets = null then
         return 0;
      end if;

      return Map.HT.Buckets'Length;
   end;


   procedure Resize (Map  : in out Map_Type;
                     Size : in     Size_Type) is
   begin
      Resize (Map.HT, Size);
   end;


   procedure Insert (Map      : in out Map_Type;
                     Key      : in     Wide_String;
                     New_Item : in     Element_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean) is

      function New_Node
        (Next : Node_Access) return Node_Access is

         Node : constant Node_Access :=
            new Node_Type'(Key'Length, Key, New_Item, Next);
      begin
         return Node;
      end;

      procedure Insert is
        new Hash_Table_Types.Generic_Conditional_Insert (New_Node);

      HT : Hash_Table_Type renames Map.HT;

   begin -- Insert

      Resize (HT, HT.Length + 1);

      Insert (HT, Key, Cursor.Node, Success);

   end Insert;


   procedure Replace (Map      : in out Map_Type;
                      Key      : in     Wide_String;
                      New_Item : in     Element_Type) is

      Cursor  : Cursor_Type;
      Success : Boolean;

   begin

      Insert (Map, Key, New_Item, Cursor, Success);

      if not Success then
         Cursor.Node.Element := New_Item;
      end if;

   end Replace;


   procedure Insert (Map      : in out Map_Type;
                     Key      : in     Wide_String;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean) is

      function New_Node
        (Next : Node_Access) return Node_Access is

         Node : Node_Access := new Node_Type (Key'Length);  -- fix in Ada 0Y
      begin
         Node.Key := Key;
         Node.Next := Next;

         return Node;
      exception
         when others =>
            Free (Node);
            raise;
      end;

      procedure Insert is
        new Hash_Table_Types.Generic_Conditional_Insert (New_Node);

      HT : Hash_Table_Type renames Map.HT;

   begin -- Insert

      Resize (HT, HT.Length + 1);

      Insert (HT, Key, Cursor.Node, Success);

   end Insert;


   procedure Delete (Map : in out Map_Type;
                     Key : in     Wide_String) is

      HT : Hash_Table_Type renames Map.HT;

   begin

      if HT.Length = 0 then
         return;
      end if;

      Delete (HT, Key);

   end Delete;


   procedure Delete (Map    : in out Map_Type;
                     Cursor : in out Cursor_Type) is
   begin

      if Cursor = Null_Cursor then
         return;
      end if;

      Delete (Map.HT, Cursor.Node);

   end Delete;


   function Find (Map : Map_Type;
                  Key : Wide_String) return Cursor_Type is

      HT : Hash_Table_Type renames Map.HT;

   begin

      if HT.Length = 0 then
         return Null_Cursor;
      end if;

      return (Node => Find (HT, Key));

   end Find;


   function Is_In (Key : Wide_String;
                   Map : Map_Type) return Boolean is
   begin
      return Find (Map, Key) /= Null_Cursor;
   end;


   function Element (Map : Map_Type;
                     Key : Wide_String)
     return Element_Type is

      C : constant Cursor_Type := Find (Map, Key);
   begin
      return C.Node.Element;
   end;


   function First (Map : Map_Type) return Cursor_Type is
   begin
      return (Node => First (Map.HT));
   end;


   function Back (Map : Map_Type) return Cursor_Type is
      pragma Warnings (Off, Map);
   begin
      return Null_Cursor;
   end;


   function Succ
     (Map    : Map_Type;
      Cursor : Cursor_Type) return Cursor_Type is
   begin
      return (Node => Succ (Map.HT, Cursor.Node));
   end;


   procedure Increment
     (Map    : in     Map_Type;
      Cursor : in out Cursor_Type) is
   begin
      Cursor := Succ (Map, Cursor);
   end;


   function Key (Cursor : Cursor_Type) return Wide_String is
   begin
      return Cursor.Node.Key;
   end;


   function Is_Equal_Key (Left, Right : Cursor_Type)
     return Boolean is
   begin
      return Is_Equal_Key (Left.Node.Key, Right.Node.Key);
   end;


   function Is_Equal_Key (Left  : Cursor_Type;
                          Right : Wide_String)
     return Boolean is
   begin
      return Is_Equal_Key (Left.Node.Key, Right);
   end;


   function Is_Equal_Key (Left  : Wide_String;
                          Right : Cursor_Type)
      return Boolean is
   begin
      return Is_Equal_Key (Left, Right.Node.Key);
   end;



   function Element (Cursor : Cursor_Type)
      return Element_Type is
   begin
      return Cursor.Node.Element;
   end;


   function Generic_Element (Cursor : Cursor_Type)
      return Element_Access is
   begin
      return Cursor.Node.Element'Access;
   end;


   procedure Replace_Element (Cursor : in Cursor_Type;
                              By     : in Element_Type) is
   begin
      Cursor.Node.Element := By;
   end;


   procedure Generic_Iteration (Map : in Map_Type) is

      procedure Process (Node : in Node_Access) is
         pragma Inline (Process);
      begin
         Process (Cursor => Cursor_Type'(Node => Node));
      end;

      procedure Iterate is
         new Hash_Table_Types.Generic_Iteration (Process);

   begin -- Generic_Iteration

      Iterate (Map.HT);

   end Generic_Iteration;


   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Node   : in     Node_Access) is

   begin

      Integer'Write (Stream, Node.Key_Length);

      --TODO: better way to write string?
      --
      for I in 1 .. Node.Key_Length loop
         Wide_Character'Write (Stream, Node.Key (I));
      end loop;

      Element_Type'Write (Stream, Node.Element);

   end Write;

   procedure Write is
      new Hash_Table_Types.Generic_Write (Write);

   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Map    : in     Map_Type) is
   begin
      Write (Stream, Map.HT);
   end;


   function New_Node (Stream : access Ada.Streams.Root_Stream_Type'Class)
     return Node_Access is

      N : Integer;

   begin

      Integer'Read (Stream, N);
      pragma Assert (N >= 0);

      declare
         Node : Node_Access := new Node_Type (Key_Length => N);
      begin
         for I in 1 .. N loop
            Wide_Character'Read (Stream, Node.Key (I));  -- TODO: hack!
         end loop;

         Element_Type'Read (Stream, Node.Element);

         return Node;
      exception
         when others =>
            Free (Node);
            raise;
      end;

   end New_Node;

   procedure Read is
      new Hash_Table_Types.Generic_Read (New_Node);

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Map    :    out Map_Type) is
   begin
      Read (Stream, Map.HT);
   end;


end AI302.Containers.Wide_String_Hashed_Maps;




