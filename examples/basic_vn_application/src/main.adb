with VN_Message;
with Text_IO;

procedure Main is
   VN_Msg : VN_Message.VN_Message;
   package Print_Int is new Text_IO.Integer_IO (Natural);
begin

   VN_Message.Get_VN_Message(VN_Msg);
   VN_Msg.Set_Version(6);
   Print_Int.Put(VN_Msg.Version);

end Main;
