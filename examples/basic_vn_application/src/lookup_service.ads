with VN.Message;
with System;
with Ada.Text_IO;

package Lookup_Service is

   task type LS(Pri : System.Priority;
                     Cycle_Time : Positive;
                     Task_ID : Positive;
                     Increment_By : Positive) is
      pragma Priority(Pri);
   end LS;

end Lookup_Service;
