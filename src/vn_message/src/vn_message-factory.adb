with VN_Message.Empty;
use VN_Message.Empty;
with VN_Message.Local_Hello;
use VN_Message.Local_Hello;

package body VN_Message.Factory is

   procedure Make_VN_Message(Message: in out VN_Message'Class; Msg_Type: Message_Type) is
      One: VN_Message_Local_Hello := VN_Message_Local_Hello (Message);
   begin
      null;

--      case Msg_Type is
--         when Local_Hello     => Message := VN_Message.Local_Hello.VN_Message_Local_Hello (Message);
--         when Others          => null;
--      end case;
--
   end Make_VN_Message;

end VN_Message.Factory;
