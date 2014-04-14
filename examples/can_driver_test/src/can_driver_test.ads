

with System.BB.Interrupts;

package CAN_Driver_Test is


  -- Pragma Import(C, CAN_IRQHandler, "CAN_IRQHandler");

   procedure Init;

private

   procedure Handler(ID : System.BB.Interrupts.Interrupt_ID);

end CAN_Driver_Test;
