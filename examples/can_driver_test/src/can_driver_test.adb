
with GNAT.IO;

package body CAN_Driver_Test is

   procedure Init is
   begin
      System.BB.Interrupts.Attach_Handler(Handler'Access, System.BB.Interrupts.Interrupt_ID(8));
   end Init;

   procedure Handler(ID : System.BB.Interrupts.Interrupt_ID) is
   begin
      GNAT.IO.Put_Line("Handler called");
   end Handler;

end CAN_Driver_Test;
