procedure AI302.Containers.Generic_Sort (First, Back : Cursor_Type) is

   Length : constant Integer := Back - First;

   Pivot, Lo, Mid, Hi : Cursor_Type;

begin

   if Length <= 1 then
      return;
   end if;

   Lo := First;

   Hi := Back - 1;

   if Length = 2 then

      if not Is_Less (Lo, Hi) then
         Swap (Lo, Hi);
      end if;

      return;

   end if;

   Mid := Lo + (Hi - Lo) / 2;

-- median of 3:
-- x < y < z
-- x < z < y
-- z < x < y
-- y < x < z
-- y < z < x
-- z < y < x

   --Debug (Lo, "low iter");
   --Debug (Mid, "middle iter");
   --Debug (Hi, "high iter");

   if Is_Less (Lo, Mid) then

      if Is_Less (Lo, Hi) then

         if Is_Less (Mid, Hi) then

            --Debug ("swapping lo and mid (mid is median)");

            Swap (Lo, Mid);

         else

            --Debug ("swapping lo and hi (hi is median)");

            Swap (Lo, Hi);

         end if;

      else

         --Debug ("lo is median");

         null;  --lo is median

      end if;

   elsif Is_Less (Lo, Hi) then

      --Debug ("lo is median");

      null; --lo is median

   elsif Is_Less (Mid, Hi) then

      --Debug ("swapping lo and hi (hi is median)");

      Swap (Lo, Hi);

   else

      --Debug ("swapping lo and mid (mid is median)");

      Swap (Lo, Mid);

   end if;

   Pivot := Lo;
   --Debug (Pivot, "median of three is done");

   Outer :
   loop

      loop
         exit Outer when not (Pivot < Hi);

         if Is_Less (Hi, Pivot) then
            --Debug (Hi, "hi < pivot");
            Swap (Hi, Pivot);
            Pivot := Hi;
            Lo := Lo + 1;
            exit;
         else
            --Debug (Hi, "hi >= pivot");
            Hi := Hi - 1;
         end if;
      end loop;

      loop
         exit Outer when not (Lo < Pivot);

         if Is_Less (Lo, Pivot) then
            --Debug (Lo, "lo < pivot");
            Lo := Lo + 1;
         else
            --Debug (Lo, "lo >= pivot");
            Swap (Lo, Pivot);
            Pivot := Lo;
            Hi := Hi - 1;
            exit;
         end if;
      end loop;

   end loop Outer;

   --Debug (pivot, "partition done");

   Generic_Sort (First, Pivot);
   Generic_Sort (Pivot + 1, Back);

end AI302.Containers.Generic_Sort;


