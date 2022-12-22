with Ada.Text_IO;
pragma Elaborate_All (Ada.Text_IO);

package AI302.Containers.Size_IO is
   new Ada.Text_IO.Integer_IO (Size_Type);

