with Ada.Text_IO;
with VN.Message;
use VN.Message;
with VN.Message.Local_Hello;

package body Logger.Print_Out is

   procedure Log(This: in out Print_Out_Logger;
                 Message: out VN.Message.VN_Message_Basic) is
      use Ada.Text_IO;
      Local_Hello_Msg: VN.Message.Local_Hello.VN_Message_Local_Hello;
   begin
      if Message.Header.Opcode = OPCODE_LOCAL_HELLO then
         VN.Message.Local_Hello.To_Local_Hello(Message, Local_Hello_Msg);
         Put("Local Hello from: " &
             VN.VN_Logical_Address'Image(Local_Hello_Msg.Header.Source) &
            " to " &
            VN.VN_Logical_Address'Image(Local_Hello_Msg.Header.Destination) &
            " Comp_Type is " &
            VN.Message.VN_Component_Type'Image(Local_Hello_Msg.Component_Type));
         Put_Line("");
      elsif Message.Header.Opcode = OPCODE_LOCAL_ACK then
         Put("Local_Ack");
      end if;
   end Log;

end Logger.Print_Out;
