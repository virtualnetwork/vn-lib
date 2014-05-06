with VN.Message;
with System;
with Ada.Text_IO;

package Central_Addressing_Service is

   task type CAS(Pri : System.Priority;
                     Cycle_Time : Positive;
                     Task_ID : Positive;
                     Increment_By : Positive) is
      pragma Priority(Pri);
   end CAS;

end Central_Addressing_Service;
