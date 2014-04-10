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

   -- Convert to Local Hello Message Type
   To_Local_Hello(VN_Msg_Basic, VN_Msg_Local_Hello);

   -- Change checksum value
   VN_Msg_Local_Hello.Checksum := VN_Msg_Local_Hello.Checksum + 1;

   -- Print values in Local Hello Message
   VN_Opcode_Print.Put(VN_Msg_Local_Hello.Header.Opcode);
   VN_Checksum_Print.Put(VN_Msg_Local_Hello.Checksum);
   Text_IO.Put_Line("");

   -- Convert Back to Basic VN Message
   To_Basic(VN_Msg_Local_Hello, VN_Msg_Basic);
   -- Convert to Local Ack Message
   To_Local_Ack(VN_Msg_Basic, VN_Msg_Local_Ack);

   -- Change checksum value
   VN_Msg_Local_Ack.Checksum := VN_Msg_Local_Ack.Checksum + 1;

   -- Print values in Local Ack Message
   VN_Opcode_Print.Put(VN_Msg_Local_Ack.Header.Opcode);
   VN_Checksum_Print.Put(VN_Msg_Local_Ack.Checksum);
   Text_IO.Put_Line("");
end Main;
