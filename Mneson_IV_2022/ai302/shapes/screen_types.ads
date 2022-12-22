package Screen_Types is

   pragma Pure;

   subtype X_Subtype is Integer range 0 .. 40;
   subtype Y_Subtype is Integer range 0 .. 24;

   type Point_Type is record
      X : Integer;
      Y : Integer;
   end record;

end Screen_Types;

