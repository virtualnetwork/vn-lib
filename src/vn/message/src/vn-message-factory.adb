with VN.Message.Local_Hello;
use VN.Message.Local_Hello;
with VN.Message.Local_Ack;
use VN.Message.Local_Ack;
with VN.Message.Assign_Address;
use VN.Message.Assign_Address;
with VN.Message.Assign_Address_Block;
use VN.Message.Assign_Address_Block;
with VN.Message.Request_Address_Block;
use VN.Message.Request_Address_Block;

package body VN.Message.Factory is

   function Create(VN_Msg_Type: in VN_Message_Type) return VN_Message_Basic is
      VN_Msg: VN_Message_Basic;
   begin
      case VN_Msg_Type is
         when Type_Basic => null;
         when Type_Local_Hello =>
            declare
               Temp_Msg: VN_Message_Local_Hello;
            begin
               To_Local_Hello(VN_Msg, Temp_Msg);
               To_Basic(Temp_Msg, VN_Msg);
            end;
         when Type_Local_Ack => null;
            declare
               Temp_Msg: VN_Message_Local_Ack;
            begin
               To_Local_Ack(VN_Msg, Temp_Msg);
               To_Basic(Temp_Msg, VN_Msg);
            end;
         when Type_Assign_Address => null;
            declare
               Temp_Msg: VN_Message_Assign_Address;
            begin
               To_Assign_Address(VN_Msg, Temp_Msg);
               To_Basic(Temp_Msg, VN_Msg);
            end;
         when Type_Request_Address_Block => null;
            declare
               Temp_Msg: VN_Message_Request_Address_Block;
            begin
               To_Request_Address_Block(VN_Msg, Temp_Msg);
               To_Basic(Temp_Msg, VN_Msg);
            end;
         when Type_Assign_Address_Block => null;
            declare
               Temp_Msg: VN_Message_Assign_Address_Block;
            begin
               To_Assign_Address_Block(VN_Msg, Temp_Msg);
               To_Basic(Temp_Msg, VN_Msg);
            end;
      end case;

      return VN_Msg;
   end Create;

end VN.Message.Factory;
