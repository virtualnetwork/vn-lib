with VN.Message.Factory;
with VN.Message.Local_Hello;
use VN.Message.Local_Hello;
with VN.Message.Local_Ack;
use VN.Message.Local_Ack;
with Text_IO;

procedure Main is
   use VN.Message;

   package VN_Checksum_Print is new Text_IO.Modular_IO(VN.Message.VN_Checksum);
   package VN_Opcode_Print is new Text_IO.Modular_IO(VN.Message.VN_Opcode);

   VN_Msg_Basic         : VN.Message.VN_Message_Basic;
   VN_Msg_Local_Hello   : VN_Message_Local_Hello;
   VN_Msg_Local_Ack     : VN_Message_Local_Ack;
begin
   -- Set starting values
   VN_Msg_Basic.Header.Opcode := 16#0F#;
   VN_Msg_Basic.Checksum      := 16#FF#;

   -- Print starting values
   VN_Opcode_Print.Put(VN_Msg_Basic.Header.Opcode);
   VN_Checksum_Print.Put(VN_Msg_Basic.Checksum);
   Text_IO.Put_Line("");

   VN_Msg_Basic := VN.Message.Factory.Create(Type_Local_Hello);

   -- Print values after a Local_Hello message was created
   VN_Opcode_Print.Put(VN_Msg_Basic.Header.Opcode);
   VN_Checksum_Print.Put(VN_Msg_Basic.Checksum);
   Text_IO.Put_Line("");
end Main;
