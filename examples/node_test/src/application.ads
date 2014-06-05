with VN.Message;
with System;

package Application is

   task type VN_Application(Pri : System.Priority;
                     Cycle_Time : Positive;
                     Task_ID : Positive;
                     Increment_By : Positive) is
      pragma Priority(Pri);
   end VN_Application;

end Application ;
