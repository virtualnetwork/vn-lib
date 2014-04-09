with Interfaces;
with Interfaces.C;

package CAN_Driver is

   type Data_Array is array(0..7) of Interfaces.C.signed_char;

   type CAN_Message_Physical is
      record
         ID		: Interfaces.C.unsigned; --??
         Length   	: Interfaces.C.unsigned; --??
         Data     	: Data_Array;
      end record;
   pragma Convention (C, CAN_Message_Physical);

   procedure Send(msg : CAN_Message_Physical);
   pragma Import(C, Send, "Send_CAN_Message");

   function Receive(msg : CAN_Message_Physical) return Interfaces.C.int;
   pragma Import(C, Receive, "Receive_CAN_Message");

   function Test return Interfaces.C.int;
   pragma Import(C, Test, "test");

   procedure Temp(msg : CAN_Message_Physical);

end CAN_Driver;
