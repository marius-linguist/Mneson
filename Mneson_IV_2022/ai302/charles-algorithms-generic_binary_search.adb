------------------------------------------------------------------------------
--                                                                          --
--                      CHARLES CONTAINER LIBRARY                           --
--                                                                          --
--           Charles.Algorithms.Generic_Binary_Search (body)                --
--                                                                          --
--                                                                          --
--              Copyright (C) 2001-2004 Matthew J Heaney                    --
--                                                                          --
-- The Charles Container Library ("Charles") is free software; you can      --
-- redistribute it and/or modify it under terms of the GNU General Public   --
-- License as published by the Free Software Foundation; either version 2,  --
-- or (at your option) any later version.  Charles is distributed in the    --
-- hope that it will be useful, but WITHOUT ANY WARRANTY; without even the  --
-- implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. --
-- See the GNU General Public License for more details.  You should have    --
-- received a copy of the GNU General Public License distributed with       --
-- Charles;  see file COPYING.TXT.  If not, write to the Free Software      --
-- Foundation,  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                          --
-- As a special exception, if other files instantiate generics from this    --
-- unit, or you link this unit with other files to produce an executable,   --
-- this unit does not by itself cause the resulting executable to be        --
-- covered by the GNU General Public License.  This exception does not      --
-- however invalidate any other reasons why the executable file might be    --
-- covered by the GNU Public License.                                       --
--                                                                          --
-- Charles is maintained by Matthew J Heaney.                               --
--                                                                          --
-- http://home.earthlink.net/~matthewjheaney/index.html                     --
-- mailto:matthewjheaney@earthlink.net                                      --
--                                                                          --
------------------------------------------------------------------------------
with Charles.Algorithms.Generic_Lower_Bound;

function Charles.Algorithms.Generic_Binary_Search
  (First, Back : Iterator_Type;
   Item        : Element_Type) return Iterator_Type is

   function Lower_Bound is
      new Generic_Lower_Bound
        (Iterator_Type   => Iterator_Type,
         Difference_Type => Integer,
         Element_Type    => Element_Type,
         Is_Less         => Is_Less,
         "+"             => "+",
         "-"             => "-");

   I : constant Iterator_Type := Lower_Bound (First, Back, Item);

begin

   if I = Back then
      return Back;
   end if;

   if Is_Greater (I, Item) then
      return Back;
   end if;

   return I;

end Charles.Algorithms.Generic_Binary_Search;
