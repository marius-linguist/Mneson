with Ada.Unchecked_Deallocation;

with AI302.Containers.Generic_Sort;

package body Indefinite_Vectors is

   procedure Free is
      new Ada.Unchecked_Deallocation (Element_Type, Element_Access);



   procedure Write
     (Stream  : access Root_Stream_Type'Class;
      Control : in     Control_Type) is
   begin

      --TODO: check for null.

      Element_Type'Output (Stream, Control.X.all);
   end;


   procedure Read
     (Stream  : access Root_Stream_Type'Class;
      Control :    out Control_Type) is

      X : Element_Access := Control.X;
   begin
      --TODO: check for null.

      Control.X := new Element_Type'(Element_Type'Input (Stream));
      Free (X);
   end;



   procedure Adjust (Control : in out Control_Type) is
      X : constant Element_Access := Control.X;
   begin

      --TODO: check for null.

      Control.X := new Element_Type'(X.all);
   exception
      when others =>
         Control.X := null;
         raise;
   end;


   procedure Finalize (Control : in out Control_Type) is
   begin
      Free (Control.X);
   exception
      when others =>
         Control.X := null;
         raise;
   end;


   function Is_Equal (L, R : Control_Type) return Boolean is
   begin
      --TODO: check for null.

      return L.X.all = R.X.all;
   end;


   function "=" (Left, Right : Vector_Type) return Boolean is
   begin
      return VT (Left) = VT (Right);
   end;


   function Length (Vector : Vector_Type) return Natural is
   begin
      return Natural (Length (VT (Vector)));
   end;


   function Is_Empty (Vector : Vector_Type) return Boolean is
   begin
      return Is_Empty (VT (Vector));
   end;


   procedure Clear (Vector : in out Vector_Type) is
   begin
      Clear (VT (Vector));
   end;


   procedure Swap (Left, Right : in out Vector_Type) is
   begin
      Swap (VT (Left), VT (Right));
   end;


   type Control_Access is access all Control_Type;

   function To_Access is
      new Rep_Types.Generic_Element (Control_Access);


   procedure Insert (Vector   : in out Vector_Type;
                     Before   : in     Index_Type'Base) is
   begin

      --TODO: define the semantics here.  Should this copy the elem
      --ptr at Before to Before + 1, and insert a null elem at Before;
      --or, should we copy the elem at Before to Before + 1, and leave
      --what was already there?  It depends how you define what an
      --item-less insert means.
      --END TODO.

      Insert_N (VT (Vector), Before, Count => 1);
   end;


   procedure Insert (Vector   : in out Vector_Type;
                     Before   : in     Index_Type'Base;
                     New_Item : in     Element_Type) is
   begin
      Insert_N (Vector, Before, Integer'(1), New_Item);
   end;


   procedure Append (Vector   : in out Vector_Type;
                     New_Item : in     Element_Type) is
   begin
      Insert (Vector, Back (Vector), New_Item);
   end;


   procedure Insert_N (Vector : in out Vector_Type;
                       Before : in     Index_Type'Base;
                       Count  : in     Natural) is
   begin
      Insert_N (VT (Vector), Before, AI302.Containers.Size_Type (Count));
   end;


   procedure Insert_N (Vector   : in out Vector_Type;
                       Before   : in     Index_Type'Base;
                       Count    : in     Natural;
                       New_Item : in     Element_Type) is
   begin

      --NOTE:
      --We could do this:
      --   Insert_N
      --     (Vector => VT (Vector),
      --      Before => Before,
      --      Count  => Count,
      --      New_Item => (Controlled with X => new Element_Type'(New_Item)));
      --
      --This should work fine except that New_Item is effectively a
      --temporary: it gets created, assigned Count times, and then
      --destroyed.  The manual loop below doesn't create any temporaries.
      --END NOTE.

      Insert_N (Vector, Before, Count);

      for I in Before .. Before + Integer'Pos (Count) - 1 loop

         declare
            C : Control_Type renames
              To_Access (Rep_Types.Vector_Type (Vector), I).all;

            X : Element_Access := C.X;
         begin
            C.X := new Element_Type'(New_Item);
            Free (X);
         end;

      end loop;

   end Insert_N;


   procedure Delete (Vector : in out Vector_Type;
                     Index  : in     Index_Type'Base) is
   begin
      Delete (VT (Vector), Index);
   end;


   procedure Delete_Last (Vector : in out Vector_Type) is
   begin
      Delete_Last (VT (Vector));
   end;


   procedure Delete_N (Vector : in out Vector_Type;
                       First  : in     Index_Type'Base;
                       Count  : in     Natural) is
   begin
      Delete_N (VT (Vector), First, AI302.Containers.Size_Type (Count));
   end;


   function Size (Vector : Vector_Type) return Natural is
   begin
      return Natural (Size (VT (Vector)));
   end;


   procedure Resize (Vector : in out Vector_Type;
                     Size   : in     Natural) is
   begin
      Resize (VT (Vector), AI302.Containers.Size_Type (Size));
   end;


   function Front (Vector : Vector_Type) return Index_Type'Base is
   begin
      return Front (VT (Vector));
   end;


   function First (Vector : Vector_Type) return Index_Type is
   begin
      return First (VT (Vector));
   end;


   function First_Element (Vector : Vector_Type) return Element_Type is

      I : constant Index_Type := Index_Type'First;

      C : Control_Type renames
        To_Access (Rep_Types.Vector_Type (Vector), I).all;
   begin
      return C.X.all;
   end;


   function Last (Vector : Vector_Type) return Index_Type'Base is
   begin
      return Last (VT (Vector));
   end;


   function Last_Element (Vector : Vector_Type) return Element_Type is

      I : constant Index_Type := Last (Vector);

      C : Control_Type renames
        To_Access (Rep_Types.Vector_Type (Vector), I).all;
   begin
      return C.X.all;
   end;


   function Back (Vector : Vector_Type) return Index_Type'Base is
   begin
      return Back (VT (Vector));
   end;



   function Element (Vector : Vector_Type;
                     Index  : Index_Type'Base)
      return Element_Type is

      C : Control_Type renames
        To_Access (Rep_Types.Vector_Type (Vector), Index).all;
   begin
      return C.X.all;
   end;


   function Generic_Element (Vector : Vector_Type;
                             Index  : Index_Type'Base)
     return Element_Access is

      C : Control_Type renames
        To_Access (Rep_Types.Vector_Type (Vector), Index).all;
   begin

      --TODO: return null here is elem ptr is null?

      return C.X.all'Access;
   end;


   procedure Replace_Element (Vector : in Vector_Type;
                              Index  : in Index_Type'Base;
                              By     : in Element_Type) is

      C : Control_Type renames
        To_Access (Rep_Types.Vector_Type (Vector), Index).all;

      X : Element_Access := C.X;
   begin
      C.X := new Element_Type'(By);
      Free (X);

      --NOTE:
      --Of course we could have done this:
      --  Replace_Element
      --    (Vector => VT (Vector),
      --     Index  => Index,
      --     By     => (Controlled with X => new Element_Type'(By)));
      --
      --But this creates a temporary, which is assigned and then
      --immediately destroyed.  The manual assignment above avoids
      --creating a temporary.
      --END NOTE.
   end;


   procedure Generic_Constant_Iteration (Vector : in Vector_Type) is

      procedure Process (C : in Control_Type) is
         pragma Inline (Process);
      begin
         Process (C.X.all);
      end;

      procedure Iterate is
         new Rep_Types.Generic_Constant_Iteration (Process);
   begin
      Iterate (Rep_Types.Vector_Type (Vector));
   end;


   procedure Generic_Iteration (Vector : in Vector_Type) is

      procedure Process (C : in Control_Type) is
         pragma Inline (Process);
      begin
         Process (C.X.all);
      end;

      procedure Iterate is
         new Rep_Types.Generic_Constant_Iteration (Process);
   begin
      Iterate (Rep_Types.Vector_Type (Vector));
   end;


   procedure Generic_Constant_Reverse_Iteration (Vector : in Vector_Type) is

      procedure Process (C : in Control_Type) is
         pragma Inline (Process);
      begin
         Process (C.X.all);
      end;

      procedure Iterate is
         new Rep_Types.Generic_Constant_Reverse_Iteration (Process);
   begin
      Iterate (Rep_Types.Vector_Type (Vector));
   end;


   procedure Generic_Reverse_Iteration (Vector : in Vector_Type) is

      procedure Process (C : in Control_Type) is
         pragma Inline (Process);
      begin
         Process (C.X.all);
      end;

      procedure Iterate is
         new Rep_Types.Generic_Constant_Reverse_Iteration (Process);
   begin
      Iterate (Rep_Types.Vector_Type (Vector));
   end;


   procedure Generic_Sort (Vector : in Vector_Type) is

      function Is_Less (L, R : Control_Type) return Boolean is
         pragma Inline (Is_Less);
      begin
         return L.X.all < R.X.all;
      end;

      --NOTE:
      --This is a case where a user-supplied
      --swap makes sense.  See ex. below:
      --END NOTE.

      procedure Sort is
         new Rep_Types.Generic_Sort ("<" => Is_Less);
   begin
      Sort (Rep_Types.Vector_Type (Vector));
   end;


   procedure Swap
     (Vector : in Vector_Type;
      I, J   : in Index_Type'Base) is

      CI : Control_Type renames
        To_Access (Rep_Types.Vector_Type (Vector), I).all;

      CJ : Control_Type renames
        To_Access (Rep_Types.Vector_Type (Vector), J).all;

      CIX : constant Element_Access := CI.X;

   begin

      CI.X := CJ.X;
      CJ.X := CIX;

   end Swap;


--     procedure Generic_Sort2 (Vector : in Vector_Type) is

--        function Is_Less (L, R : Control_Type) return Boolean is
--        begin
--           return L.X.all < R.X.all;
--        end;

--        procedure Swap
--          (V    : in Rep_Types.Vector_Type;
--           I, J : in Index_Type'Base) is
--        begin
--           Swap (Vector_Type (V), I, J);
--        end;

--        procedure Sort is
--           new Rep_Types.Generic_Sort2 (Is_Less, Swap);

--     begin

--        Sort (Rep_Types.Vector_Type (Vector));

--     end Generic_Sort2;


   procedure Generic_Sort3 (Vector : in Vector_Type) is

      function Is_Less
        (Left, Right : Index_Type) return Boolean is

         CL : Control_Type renames
           To_Access (Rep_Types.Vector_Type (Vector), Left).all;

         CR : Control_Type renames
           To_Access (Rep_Types.Vector_Type (Vector), Right).all;
      begin
         return CL.X.all < CR.X.all;
      end;

      procedure Swap
        (Left, Right : Index_Type) is

         CL : Control_Type renames
           To_Access (Rep_Types.Vector_Type (Vector), Left).all;

         CR : Control_Type renames
           To_Access (Rep_Types.Vector_Type (Vector), Right).all;

         CLX : constant Element_Access := CL.X;

      begin

         --NOTE:
         --There's no copying of vector elements anymore: all we're
         --doing is swapping pointers.  There's a good change this
         --version (Sort2) will run faster than Sort(1) above.
         --END NOTE.

         CL.X := CR.X;
         CR.X := CLX;

      end Swap;

      function Add_Index_Integer
        (Left  : Index_Type'Base;
         Right : Integer'Base) return Index_Type'Base is

         pragma Inline (Add_Index_Integer);
      begin
         return Left + Integer'Pos (Right);
      end;

      function Sub_Index_Integer
        (Left  : Index_Type'Base;
         Right : Integer'Base) return Index_Type'Base is

         pragma Inline (Sub_Index_Integer);
      begin
         return Left - Integer'Pos (Right);
      end;

      function Sub_Index_Index
        (Left, Right : Index_Type'Base) return Integer'Base is

         pragma Inline (Sub_Index_Index);
      begin
         return Index_Type'Pos (Left) - Index_Type'Pos (Right);
      end;

      procedure Sort is
         new AI302.Containers.Generic_Sort
            (Index_Type'Base,
             Is_Less,
             Swap,
             Add_Index_Integer,
             Sub_Index_Integer,
             Sub_Index_Index);
   begin
      Sort (First (Vector), Back (Vector));
   end;


end Indefinite_Vectors;


