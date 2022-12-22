with AI302.Containers.Vectors;
pragma Elaborate_All (AI302.Containers.Vectors);

with Ada.Streams;
with Ada.Finalization;

generic

   type Index_Type is range <>;

   type Element_Type (<>) is private;

   with function "=" (Left, Right : Element_Type)
     return Boolean is <>;

package Indefinite_Vectors is

   pragma Preelaborate;

   subtype Index_Subtype is Index_Type;
   subtype Element_Subtype is Element_Type;

   type Vector_Type is private;

   function "=" (Left, Right : Vector_Type) return Boolean;

   function Length (Vector : Vector_Type) return Natural;

   function Is_Empty (Vector : Vector_Type) return Boolean;

   procedure Clear (Vector : in out Vector_Type);

   procedure Swap (Left, Right : in out Vector_Type);

   procedure Append (Vector   : in out Vector_Type;
                     New_Item : in     Element_Type);

   procedure Insert (Vector   : in out Vector_Type;
                     Before   : in     Index_Type'Base;
                     New_Item : in     Element_Type);

   --NOTE:
   --This will leave us with null elements.
   --END NOTE.
   --
   procedure Insert (Vector   : in out Vector_Type;
                     Before   : in     Index_Type'Base);

   procedure Insert_N (Vector   : in out Vector_Type;
                       Before   : in     Index_Type'Base;
                       Count    : in     Natural;
                       New_Item : in     Element_Type);

   --NOTE: see note above
   procedure Insert_N (Vector : in out Vector_Type;
                       Before : in     Index_Type'Base;
                       Count  : in     Natural);

   procedure Delete (Vector : in out Vector_Type;
                     Index  : in     Index_Type'Base);

   procedure Delete_Last (Vector : in out Vector_Type);

   procedure Delete_N (Vector : in out Vector_Type;
                       First  : in     Index_Type'Base;
                       Count  : in     Natural);

   function Size (Vector : Vector_Type) return Natural;

   procedure Resize (Vector : in out Vector_Type;
                     Size   : in     Natural);

   function Front (Vector : Vector_Type) return Index_Type'Base;

   function First (Vector : Vector_Type) return Index_Type;

   function First_Element (Vector : Vector_Type) return Element_Type;

   function Last (Vector : Vector_Type) return Index_Type'Base;

   function Last_Element (Vector : Vector_Type) return Element_Type;

   function Back (Vector : Vector_Type) return Index_Type'Base;

   function Element (Vector : Vector_Type;
                     Index  : Index_Type'Base)
      return Element_Type;


   --NOTE:
   --We should say that we return null here for null elements.
   --END NOTE.
   --
   generic
      type Element_Access is access all Element_Type;
   function Generic_Element (Vector : Vector_Type;
                             Index  : Index_Type'Base)
     return Element_Access;

   --NOTE:
   --If an element can be null (because we did an insert sans new_item),
   --then it might be nice to have a form of replace that means "replace
   --this element with the value null."
   --END NOTE.
   --
   procedure Replace_Element (Vector : in Vector_Type;
                              Index  : in Index_Type'Base;
                              By     : in Element_Type);



   --NOTE:
   --There is an anomoly here in that if you do an item-less insert,
   --then those elements are null.  What should the passive iterator
   --do?  If it tries to process an element, you'll get CE because
   --there's no element there (the elem ptr is null).  We could either
   --let the client get the exception, or just test for null internally
   --and then skip over the null elements.  Or we could just disallow
   --item-less forms of insert.
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


   --NOTE:
   --How should be handle null elements?  I suppose we could
   --make a rule that a null element is always less than a
   --non-null element.
   --END NOTE.

   procedure Swap
     (Vector : in Vector_Type;
      I, J   : in Index_Type'Base);

   generic
      with function "<" (Left, Right : Element_Type)
         return Boolean is <>;
   procedure Generic_Sort (Vector : in Vector_Type);

--     generic
--        with function "<" (Left, Right : Element_Type)
--           return Boolean is <>;
--     procedure Generic_Sort2 (Vector : in Vector_Type);

   generic
      with function "<" (Left, Right : Element_Type)
         return Boolean is <>;
   procedure Generic_Sort3 (Vector : in Vector_Type);

private

   type Element_Access is access Element_Type;

   use Ada.Finalization;

   type Control_Type is
     new Controlled with record
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

   function Is_Equal (L, R : Control_Type) return Boolean;

   package Rep_Types is
      new AI302.Containers.Vectors
       (Index_Type,
        Control_Type,
        "=" => Is_Equal);

   type VT is new Rep_Types.Vector_Type;

   type Vector_Type is new VT;

end Indefinite_Vectors;


