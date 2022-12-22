-----------------------------------------------------
-- PACKAGE MNESON (SPEC ONLY)                      --
-- (C) Marius Amado Alves (amado.alves@netcabo.pt) --
-- License: SDC Conditions (www.softdevelcoop.org) --
-----------------------------------------------------

with Ada.Direct_IO;

package Mneson is

   ---------------------
   -- NUMERICAL TYPES --
   ---------------------

   type Float is digits 15;
   for Float'Size use 64;

   type Integer is range - 2 ** 63 .. 2 ** 63 - 1;
   for Integer'Size use 64;

   subtype Natural is Integer range 0 .. Integer'Last;
   subtype Positive is Natural range 1 .. Natural'Last;

   subtype Vertex_Count is Natural;
   subtype Serial_Number is Vertex_Count range 1 .. Vertex_Count'Last;

   type Natural_16 is range 0 .. 2 ** 16 - 1;
   for Natural_16'Size use 16;

   type Modular_64 is mod 2 ** 64;
   for Modular_64'Size use 64;

   -----------------
   -- VERTEX TYPE --
   -----------------

   type Vertex is record
      Tip : Natural_16;
      Cue : Modular_64;
   end record;
   for Vertex'Size use 80;
   pragma Pack (Vertex);

   pragma Export
     (Convention => C,
      Entity => Vertex,
      External_Name => "mneson_vertex");

   -------------
   -- STRINGS --
   ------------- 

   subtype String_8 is String (1 .. 8);
   package Short_String_IO is new Ada.Direct_IO (Character);
   subtype Img_String_Length is Standard.Natural;
   Default_Max_Length : Img_String_Length := 65;
   
   -------------------------------
   -- ACCESS TO PROCEDURE TYPES --
   -------------------------------

   type Process_Vertex is access procedure (X : Vertex);
   Process_Vertex_Size : constant := 64;
   type Process_Link   is access procedure (Source, Target : Vertex);
   type Process_String is access procedure (S : String);

   ----------------
   -- TIP VALUES --
   ----------------

   subtype Tiny_String_Tip  is  Natural_16 range 0 ..      8;
   subtype Short_String_Tip is  Natural_16 range 9 .. 65_000;
   Long_String_Tip   : constant Natural_16 :=         65_010; -- (*)
   Huge_String_Tip   : constant Natural_16 :=         65_020; -- (*)
   Float_Tip         : constant Natural_16 :=         65_110;
   Integer_Tip       : constant Natural_16 :=         65_120;
   Process_Vertex_Tip: constant Natural_16 :=         65_130;
   Serial_Number_Tip : constant Natural_16 :=         65_140;
   Special_Tip       : constant Natural_16 :=         65_200;
   --Front_Vertex_Tip  : constant Natural_16 :=         65_210;
   --Back_Vertex_Tip   : constant Natural_16 :=         65_220;
   --Special_Tip       : constant Natural_16 :=         65_230;
   -- (*) not implemented yet

   ---------------------------
   -- SPECIAL VERTEX VALUES --
   ---------------------------

   Front_Vertex  : constant Vertex := (Special_Tip, 10);
   Back_Vertex   : constant Vertex := (Special_Tip, 20);
   Ada_Proc      : constant Vertex := (Special_Tip, 30);
   Args          : constant Vertex := (Special_Tip, 40);
   Exec_Now      : constant Vertex := (Special_Tip, 50);
   Procaddr      : constant Vertex := (Special_Tip, 60);
   Markers       : constant Vertex := (Special_Tip, 70);
   Transient     : constant Vertex := (Special_Tip, 80);
   
   ----------------
   -- EXCEPTIONS --
   ----------------

   Not_Implemented_Yet : exception;
   State_Error         : exception;
   Syntax_Error        : exception;
   Type_Error          : exception;

   ------------------------
   -- PACKAGE SIGNATURES --
   ------------------------

   generic
      with procedure Create (Name : String) is <>;
      with procedure Open (Name : String) is <>;
      with procedure Save is <>;
      with procedure Save_As (Name : String) is <>;
      with procedure Close is <>;
      with procedure Set_Monitor (Monitor : Process_String) is <>;
      with procedure Set_Logging (On : Boolean) is <>;
   package File is end;

   generic
      with function To_Vertex (Value : Float) return Vertex is <>;
      with function To_Vertex (Value : Integer) return Vertex is <>;
      with function To_Vertex (Value : Process_Vertex) return Vertex is <>;
      with function To_Vertex (Value : String) return Vertex is <>;
      with function Valueless_Vertex
        (Number : Serial_Number) return Vertex is <>;
      with function New_Serial_Number return Serial_Number is <>;
      with function New_Vertex return Vertex is <>;
      with procedure Mark (Marker : Vertex) is <>;
      with procedure Unmark (Marker : Vertex) is <>;      
      with function Value (X : Vertex) return Float is <>;
      with function Value (X : Vertex) return Integer is <>;
      with function Value (X : Vertex) return Process_Vertex is <>;
      with function Value (X : Vertex) return String is <>;
      with function Length (X : Vertex) return Standard.Natural is <>;
      with function Slice
        (X : Vertex; Low : Standard.Positive; High : Standard.Natural)
         return String is <>;
      with procedure Connect (Source, Target : Vertex) is <>;
      with procedure Disconnect (Source, Target : Vertex) is <>;
      with procedure Disconnect_From_Targets (Source : Vertex) is <>;
      with procedure Disconnect_From_Sources (Target : Vertex) is <>;
      with procedure Disconnect (X : Vertex) is <>;
      with procedure Reconnect
        (Source, Target, New_Target : Vertex) is <>;
      with procedure Inv_Reconnect
        (Target, Source, New_Source : Vertex) is <>;
      with function Connected
        (Source, Target : Vertex) return Boolean is <>;
      with function Connected (Source : Vertex) return Boolean is <>;
      with function Inv_Connected (Target : Vertex) return Boolean is <>;
      with procedure For_Each_Target
        (Source : Vertex; Process : Process_Vertex) is <>;
      with procedure For_Each_Source
        (Target : Vertex; Process : Process_Vertex) is <>;
      with procedure For_Each_Link (Process : Process_Link) is <>;
      with procedure For_Each_Common_Target
        (Source_1, Source_2 : Vertex; Process : Process_Vertex) is <>;
      with function Is_Integer (X : Vertex) return Boolean is <>;
      with function Is_Float (X : Vertex) return Boolean is <>;
      with function Is_String (X : Vertex) return Boolean is <>;
      with function Is_Valueless (X : Vertex) return Boolean is <>;
   package Work is end;

   generic
      with function Img (X : Natural_16) return String is <>;
      with function Img (X : Modular_64) return String is <>;
      with function Img (X : Vertex; Max_Length : Img_String_Length := Default_Max_Length ) return String is <>;
      with function Val (S : String) return Vertex is <>;
      with function Escape (S : String) return String is <>;
      with function Unescape (S : String) return String is <>;
   package Text is end;

end;

-- NOTES

-- The term "Modular" is used instead of "Unsigned". 
-- The latter is misleading, as the type can represent signed values.

-- Type Vertex is exposed (and not private) for several reasons,
-- including the full view being required for container package
-- instantiations.

-- Mneson.Float has range
--   [-16#0.FFFF_FFFF_FFFF_F8#E+256, 16#0.FFFF_FFFF_FFFF_F8#E+256] =
-- = ]   -1.797_693_134_862_32E+308,    1.797_693_134_862_32E+308[
-- (note the open bounds of the decimal range).

-- A type Vertex_Array is defined in Mneson.Base (and not here)
-- for reasons explained there.

-- Children of this package include:

-- BASE
-- An instantiation of this package represents an untyped graph.

-- STRUCTURES
-- Facilities for the creation of complex structures in the untyped graph.

-- CALCULUS
-- Implements the Mneson Calculus.
