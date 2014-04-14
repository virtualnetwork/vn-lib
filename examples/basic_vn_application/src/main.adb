with VN.Message.Local_Hello;
with VN.Message.Handler_Local_Hello;
with Text_IO;

procedure Main is
   VN_Msg   : VN_Message.Local_Hello.VN_Message_Local_Hello;
   package VN_Version_Print is new Text_IO.Modular_IO(VN_Message.VN_Version);
   package VN_CUUID_Print is new Text_IO.Modular_IO(VN_Message.VN_CUUID);
begin
   VN_Version_Print.Put(VN_Msg.Get_Version);
   VN_Msg.Set_Version(1);
   VN_Version_Print.Put(VN_Msg.Get_Version);
   VN_Msg.Set_Version(2);
   VN_Version_Print.Put(VN_Msg.Get_Version);

   VN_Msg.Set_CUUID(16#FFFF_FFFF_FFFF_FFFF#);
   VN_CUUID_Print.Put(VN_Msg.Get_CUUID);
   VN_Message.Handler_Local_Hello.Parse(VN_Msg);
   VN_Msg.Set_CUUID(16#FFFF_FFFF_DEAD_BEAF#);
   VN_CUUID_Print.Put(VN_Msg.Get_CUUID);
   VN_Message.Handler_Local_Hello.Parse(VN_Msg);
end Main;
