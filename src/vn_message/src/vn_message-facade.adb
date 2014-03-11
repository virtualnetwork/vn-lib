with VN_Message.Local_Hello;
use VN_Message.Local_Hello;

package body VN_Message.Facade is

   procedure Cast_Message_To(Message: in out VN_Message'Class; Msg_Type: Message_Type) is
   begin
      null;

--      case Msg_Type is
--         when Local_Hello     => Message := VN_Message.Local_Hello.VN_Message_Local_Hello (Message);
--         when Others          => null;
--      end case;
--
   end Cast_Message_To;

   -- VN_Message_Empty
   -- Get_Payload
   function Get_Payload(Message: VN_Message_Empty) return VN_Payload is
   begin
      return Message.Payload;
   end Get_Payload;

   -- Set_Payload
   procedure Set_Payload(Message: in out VN_Message_Empty; Payload: VN_Payload) is
   begin
      Message.Payload := Payload;
   end Set_Payload;

end VN_Message.Facade;
