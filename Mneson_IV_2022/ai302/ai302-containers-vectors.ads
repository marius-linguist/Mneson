with Ada.Finalization;
with Ada.Streams;

generic

   type Index_Type is range <>;  --NOTE: "range <>" or "(<>)"?

   type Element_Type is private;

   with function "=" (Left, Right : Element_Type)
     return Boolean is <>;

package AI302.Containers.Vectors is
   pragma Preelaborate (Vectors);

   pragma Assert (Index_Type'Base'First < Index_Type'First);

   subtype Index_Subtype is Index_Type;
   subtype Element_Subtype is Element_Type;

--NOTE: our tentative plan is to use Containers.Size_Type
--     subtype Size_Type is  --NOTE: "Size_Subtype" ?
--       Index_Type'Base range 0 .. Index_Type'Base'Last;

   type Vector_Type is private;

--   function New_Vector return Vector_Type;

   function To_Vector
     (New_Item : Element_Type;
      Count    : Size_Type) return Vector_Type;

   function "=" (Left, Right : Vector_Type) return Boolean;

   function Length (Vector : Vector_Type) return Size_Type;

   function Is_Empty (Vector : Vector_Type) return Boolean;

   procedure Clear (Vector : in out Vector_Type);

   procedure Swap (Left, Right : in out Vector_Type);

   procedure Append (Vector   : in out Vector_Type;
                     New_Item : in     Element_Type);

   procedure Insert (Vector   : in out Vector_Type;
                     Before   : in     Index_Type'Base;
                     New_Item : in     Element_Type);

   --NOTE: get rid of?
   procedure Insert (Vector   : in out Vector_Type;
                     Before   : in     Index_Type'Base;
                     New_Item : in     Element_Type;
                     Count    : in     Size_Type);

   --NOTE: get rid of?
--     procedure Insert (Vector   : in out Vector_Type;
--                       Before   : in     Index_Type'Base);

   procedure Insert_N (Vector : in out Vector_Type;
                       Before : in     Index_Type'Base;
                       Count  : in     Size_Type);

   procedure Delete (Vector : in out Vector_Type;
                     Index  : in     Index_Type'Base);

   procedure Delete_Last (Vector : in out Vector_Type);

   --NOTE:
   --the name and signature of the deletion operations is
   --still in flux.
   --END NOTE.
   --
   procedure Delete (Vector  : in out Vector_Type;
                     From    : in     Index_Type'Base;
                     Through : in     Index_Type'Base);

   procedure Delete_N (Vector : in out Vector_Type;
                       First  : in     Index_Type'Base;
                       Count  : in     Size_Type);

   function Size (Vector : Vector_Type) return Size_Type;

   procedure Resize (Vector : in out Vector_Type;
                     Size   : in     Size_Type);

   function Front (Vector : Vector_Type) return Index_Type'Base;

   function First (Vector : Vector_Type) return Index_Type;

   function First_Element (Vector : Vector_Type) return Element_Type;

   function Last (Vector : Vector_Type) return Index_Type'Base;

   function Last_Element (Vector : Vector_Type) return Element_Type;

   function Back (Vector : Vector_Type) return Index_Type'Base;

   function Element (Vector : Vector_Type;
                     Index  : Index_Type'Base)
      return Element_Type;

   generic
      type Element_Access is access all Element_Type;
   function Generic_Element (Vector : Vector_Type;
                             Index  : Index_Type'Base)
     return Element_Access;

   procedure Replace_Element (Vector : in Vector_Type;
                              Index  : in Index_Type'Base;
                              By     : in Element_Type);

   --NOTE:
   --The following passive iterators aren't in the committee report,
   --but there seems to be interest in having some kind of passive
   --iterator for vector containers, too.
   --END NOTE.

   generic
      with procedure Process (Element : in Element_Type) is <>;
   procedure Generic_Constant_Iteration (Vector : in Vector_Type);

   generic
      with procedure Process (Element : in out Element_Type) is <>;
   procedure Generic_Iteration (Vector : in Vector_Type);

   generic
      with procedure Process (Element : in Element_Type) is <>;
   procedure Generic_Constant_Reverse_Iteration (Vector : in Vector_Type);

   generic
      with procedure Process (Element : in out Element_Type) is <>;
   procedure Generic_Reverse_Iteration (Vector : in Vector_Type);


   generic

      with function "<" (Left, Right : Element_Type)
         return Boolean is <>;

   procedure Generic_Sort (Vector : in Vector_Type);


   --NOTE:
   --Not in committee report, but appears to be useful,
   --especially when using vectors as storage for
   --multi-map and multi-set complex containers.
   --END NOTE.

   function Find (Vector : Vector_Type;
                  Item   : Element_Type;
                  Index  : Index_Type'Base := Index_Type'First)
      return Index_Type'Base;

   function Reverse_Find (Vector : Vector_Type;
                          Item   : Element_Type;
                          Index  : Index_Type'Base :=
                            Index_Type'Pred (Index_Type'First))
      return Index_Type'Base;

   function Is_In (Item   : Element_Type;
                   Vector : Vector_Type)
      return Boolean;

private

   type Elements_Type is
     array (Index_Type range <>) of aliased Element_Type;

   function "=" (L, R : Elements_Type) return Boolean is abstract;

   type Elements_Access is access Elements_Type;

   use Ada.Finalization;

   type Vector_Type is new Controlled with record
      Elements : Elements_Access;
      Last     : Index_Type'Base := Index_Type'Pred (Index_Type'First);
   end record;

   procedure Adjust (Vector : in out Vector_Type);

   procedure Finalize (Vector : in out Vector_Type);


   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Vector : in     Vector_Type);

   for Vector_Type'Write use Write;


   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Vector :    out Vector_Type);

   for Vector_Type'Read use Read;


end AI302.Containers.Vectors;
