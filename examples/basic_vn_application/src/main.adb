with VN_Message.Local_Hello;

with VN_Message.Factory;
with Text_IO;

procedure Main is
   VN_Msg : VN_Message.Local_Hello.VN_Message_Local_Hello;
   package VN_Print is new Text_IO.Modular_IO(VN_Message.VN_Version);

begin

   VN_Print.Put(VN_Msg.Get_Version);

end Main;
