-- GENERIC PACKAGE MNESON.BASE (SPEC)
-- An instantiation of this package represents an untyped graph.
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with AI302.Containers;
with AI302.Containers.Sorted_Sets;
with AI302.Containers.String_Hashed_Maps;
with AI302.Containers.Vectors;
with Mneson.Order;

generic package Mneson.Base is

   procedure Create (Name : String);
   procedure Open (Name : String);
   procedure Save;
   procedure Save_As (Name : String);
   procedure Close;

   procedure Set_Monitor (Monitor : Process_String);
   procedure Set_Logging (On : Boolean);

   function To_Vertex (Value : Float) return Vertex;
   function To_Vertex (Value : Integer) return Vertex;
   function To_Vertex (Value : Process_Vertex) return Vertex;
   function To_Vertex (Value : String) return Vertex;

   function Valueless_Vertex (Number : Serial_Number) return Vertex;
   function New_Serial_Number return Serial_Number;
   function New_Vertex return Vertex;
   procedure Mark (Marker : Vertex);
   procedure Unmark (Marker : Vertex);

   function Value (X : Vertex) return Float;
   function Value (X : Vertex) return Integer;
   function Value (X : Vertex) return Process_Vertex;
   function Value (X : Vertex) return String;

   function Length (X : Vertex) return Standard.Natural;
   function Slice
     (X : Vertex; Low : Standard.Positive; High : Standard.Natural)
      return String;

   procedure Connect (Source, Target : Vertex);
   procedure Disconnect (Source, Target : Vertex);
   procedure Disconnect_From_Targets (Source : Vertex);
   procedure Disconnect_From_Sources (Target : Vertex);
   procedure Disconnect (X : Vertex);
   procedure Reconnect (Source, Target, New_Target : Vertex);
   procedure Inv_Reconnect (Target, Source, New_Source : Vertex);
   function Connected (Source, Target : Vertex) return Boolean;
   function Connected (Source : Vertex) return Boolean;
   function Inv_Connected (Target : Vertex) return Boolean;

   procedure For_Each_Target (Source : Vertex; Process : Process_Vertex);
   procedure For_Each_Source (Target : Vertex; Process : Process_Vertex);
   procedure For_Each_Link (Process : Process_Link);

   procedure For_Each_Common_Target
     (Source_1, Source_2 : Vertex; Process : Process_Vertex);
   procedure Disconnect_Targets (Source : Vertex);

   function Is_Float (X : Vertex) return Boolean;
   function Is_Integer (X : Vertex) return Boolean;
   function Is_Process_Vertex (X : Vertex) return Boolean;
   function Is_String (X : Vertex) return Boolean;
   function Is_Valueless (X : Vertex) return Boolean;

   function Img (X : Natural_16) return String;
   function Img (X : Modular_64) return String;
   function Img (X : Vertex; Max_Length : Img_String_Length := Default_Max_Length) return String;
   function Val (S : String) return Vertex;
   function Escape (S : String) return String;
   function Unescape (S : String) return String;

   package File is new Mneson.File;
   package Work is new Mneson.Work;
   package Text is new Mneson.Text;

private

   function "<" (Left, Right : Vertex) return Boolean;
   package Vertex_Order is new Mneson.Order (Vertex);

   use Vertex_Order;
   use AI302.Containers;

   subtype Link_Type is Vertex_Order.One_Dimensional_Array (1 .. 2);
   package Link_Sets is new Sorted_Sets (Link_Type);
   package String_Maps is new String_Hashed_Maps (Vertex);
   package Vertex_Vectors is new Vectors (Positive, Vertex); -- zombie line?

end;

-- NOTES

-- Inv = inverse, inversely. Same as converse in certain literature.

-- Instantiations of AI302 container packages must be done here
-- (and not in the body), for a number of reasons.
-- Of necessity, some required parameter entities are also declared here.

-- Some entities do not depend on the state of the instantiation,
-- e.g. To_Vertex (Value : Integer), Is_Integer, Is_String,
-- but they are put here not to complicate the package system,
-- and also because separating the dependent from the independent entities
-- would separate To_Vertex (Value : Integer) (independent) from
-- To_Vertex (Value : String) (dependent for non-tiny strings), which
-- would be unnatural.

-- To_Vertex for tiny strings is independent of the graph state,
-- and that is why predefined names are tiny strings e.g. Proc_Now.
