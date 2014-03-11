with VN_Message.Facade;
with Text_IO;

procedure Main is
   VN_Msg   : VN_Message.Facade.VN_Message_Empty;
   To_Type  : VN_Message.Facade.Message_Type := VN_Message.Facade.Local_Hello;
   package VN_Print is new Text_IO.Modular_IO(VN_Message.VN_Version);
begin

   VN_Msg.Cast_Message_To(To_Type);

   VN_Print.Put(VN_Msg.Get_Version);

end Main;
