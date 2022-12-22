with Ada.Streams;

generic

   type Key_Type (<>) is limited private;

   type Node_Access is private;

   Null_Node : in Node_Access;

   with function Hash_Key
     (Key : Key_Type) return Hash_Type is <>;

   with function Hash_Node
     (Node : Node_Access) return Hash_Type is <>;

   with function Next
     (Node : Node_Access) return Node_Access is <>;

   with procedure Set_Next
     (Node : Node_Access;
      Next : Node_Access) is <>;

   with function Is_Equal_Key_Node
     (Key  : Key_Type;
      Node : Node_Access) return Boolean is <>;

    with function New_Node
       (Node : Node_Access) return Node_Access is <>;

   with procedure Free
     (X : in out Node_Access) is <>;

package AI302.Containers.Hash_Tables is

   pragma Preelaborate;

   type Buckets_Type is array (Hash_Type range <>) of Node_Access;

   type Buckets_Access is access Buckets_Type;

   type Hash_Table_Type is
      record
         Buckets : Buckets_Access;
         Length  : Size_Type := 0;
      end record;

   pragma Volatile (Hash_Table_Type);  -- want by-ref: pragma ok?

   function "=" (L, R : Hash_Table_Type) return Boolean is abstract;

   procedure Adjust (HT : in out Hash_Table_Type);

   procedure Finalize (HT : in out Hash_Table_Type);


   generic

      with function Is_Equal
        (L, R : Node_Access) return Boolean is <>;

   function Generic_Equal
     (L, R : Hash_Table_Type) return Boolean;


   procedure Clear (HT : in out Hash_Table_Type);

   procedure Swap (L, R : in out Hash_Table_Type);

   procedure Resize
     (HT : in out Hash_Table_Type;
      N  : in     Size_Type);


   generic

      with function New_Node
        (Next : Node_Access) return Node_Access is <>;

   procedure Generic_Conditional_Insert
     (HT      : in out Hash_Table_Type;
      Key     : in     Key_Type;
      Node    :    out Node_Access;
      Success :    out Boolean);


   procedure Delete
     (HT  : in out Hash_Table_Type;
      Key : in     Key_Type);


   procedure Delete
     (HT : in out Hash_Table_Type;
      X  : in out Node_Access);


   function Find (HT  : Hash_Table_Type;
                  Key : Key_Type) return Node_Access;

   function First (HT : Hash_Table_Type)
     return Node_Access;


   function Succ (HT   : Hash_Table_Type;
                  Node : Node_Access)
     return Node_Access;


   generic

      with procedure Process (Node : in Node_Access) is <>;

   procedure Generic_Iteration (HT : in Hash_Table_Type);


   generic

      with procedure Write
        (Stream : access Ada.Streams.Root_Stream_Type'Class;
         Node   : in     Node_Access) is <>;

   procedure Generic_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      HT     : in     Hash_Table_Type);



   generic

      use Ada.Streams;

      with function New_Node (Stream : access Root_Stream_Type'Class)
         return Node_Access is <>;

   procedure Generic_Read
     (Stream : access Root_Stream_Type'Class;
      HT     :    out Hash_Table_Type);



end AI302.Containers.Hash_Tables;

