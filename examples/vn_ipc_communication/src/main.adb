with VN.Message.Factory;
with VN.Communication.PO;
with VN.Communication.IPC;
with Text_IO;

procedure Main is
   use VN.Message;

   PO_To_Application : VN.Communication.PO.VN_PO_Access
                                             := new VN.Communication.PO.VN_PO;

   IPC_Com_SM_L         : VN.Communication.IPC.IPC_Wrapper(PO_To_Application,
                                                           True);
   IPC_Com_Application  : VN.Communication.IPC.IPC_Wrapper(PO_To_Application,
                                                           False);

   Send_Status : VN.Send_Status;
   Recv_Status : VN.Receive_Status;

   package VN_Checksum_Print is new Text_IO.Modular_IO(VN.Message.VN_Checksum);
   package VN_Opcode_Print is new Text_IO.Modular_IO(VN.Message.VN_Opcode);

   VN_Msg_Basic           : VN.Message.VN_Message_Basic;
   VN_Msg_Basic_Received  : VN.Message.VN_Message_Basic;
begin
   -- Set starting values
   VN_Msg_Basic.Header.Opcode := 16#0F#;
   VN_Msg_Basic.Checksum      := 16#FF#;

   VN_Msg_Basic_Received.Header.Opcode := 16#BE#;
   VN_Msg_Basic_Received.Checksum      := 16#EF#;

   -- Print starting values
   VN_Opcode_Print.Put(VN_Msg_Basic.Header.Opcode);
   VN_Checksum_Print.Put(VN_Msg_Basic.Checksum);
   Text_IO.Put_Line("");

   VN_Opcode_Print.Put(VN_Msg_Basic_Received.Header.Opcode);
   VN_Checksum_Print.Put(VN_Msg_Basic_Received.Checksum);
   Text_IO.Put_Line("");
   Text_IO.Put_Line("");

   IPC_Com_SM_L.Send(VN_Msg_Basic, Send_Status);
   IPC_Com_Application.Receive(VN_Msg_Basic_Received, Recv_Status);

   -- Print values after a Local_Hello message was created
   VN_Opcode_Print.Put(VN_Msg_Basic.Header.Opcode);
   VN_Checksum_Print.Put(VN_Msg_Basic.Checksum);
   Text_IO.Put_Line("");

   VN_Opcode_Print.Put(VN_Msg_Basic_Received.Header.Opcode);
   VN_Checksum_Print.Put(VN_Msg_Basic_Received.Checksum);
   Text_IO.Put_Line("");

end Main;
