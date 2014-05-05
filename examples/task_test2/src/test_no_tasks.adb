with GNAT.IO;
package body Test_No_Tasks is

   procedure Update is
   begin
      GNAT.IO.Put_Line("Update!");
      inter.Update(bufIn, bufOut);
   end Update;

end Test_No_Tasks;
