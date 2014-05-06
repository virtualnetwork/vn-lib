with VN.Message;
with System;
with Ada.Text_IO;

package Subnet_Manager_Local is

   task type SM_L(Pri : System.Priority;
                     Cycle_Time : Positive;
                     Task_ID : Positive;
                     Increment_By : Positive) is
      pragma Priority(Pri);
   end SM_L;

end Subnet_Manager_Local;
