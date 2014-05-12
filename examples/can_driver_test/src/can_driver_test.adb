
with GNAT.IO;

package body CAN_Driver_Test is

   procedure Init is
   begin
      System.BB.Interrupts.Attach_Handler(Handler'Access, System.BB.Interrupts.Interrupt_ID(16));


--        System.BB.Interrupts.Attach_Handler(Handler'Access, System.BB.Interrupts.Interrupt_ID(32));


--        System.BB.Interrupts.Attach_Handler(TestHandler'Access, System.BB.Interrupts.Interrupt_ID(30));
--        System.BB.Interrupts.Attach_Handler(TestHandler'Access, System.BB.Interrupts.Interrupt_ID(31));
--        System.BB.Interrupts.Attach_Handler(TestHandler'Access, System.BB.Interrupts.Interrupt_ID(32));
--        System.BB.Interrupts.Attach_Handler(TestHandler'Access, System.BB.Interrupts.Interrupt_ID(23));
--        System.BB.Interrupts.Attach_Handler(TestHandler'Access, System.BB.Interrupts.Interrupt_ID(24));

      null;

   end Init;


   procedure Handler(ID : System.BB.Interrupts.Interrupt_ID) is
      intID : Integer := Integer(ID);
   begin
      GNAT.IO.Put_Line("Handler called, ID= " & intID'Img);
   end Handler;

   procedure TestHandler(ID : System.BB.Interrupts.Interrupt_ID) is
   begin
      GNAT.IO.Put_Line("Test-Handler called");
   end TestHandler;

end CAN_Driver_Test;
