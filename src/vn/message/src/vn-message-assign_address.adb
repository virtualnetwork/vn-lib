package body VN.Message.Assign_Address is

   procedure To_Basic(Assign_Address_VN_Msg: in VN_Message_Assign_Address;
                      Basic_VN_Msg: out VN_Message_Basic) is
      tempMsg : VN_Message_Assign_Address := Assign_Address_VN_Msg;
      for tempMsg'Address use Basic_VN_Msg'Address;
   begin
      null;
   end To_Basic;

   procedure To_Assign_Address(
                           Basic_VN_Msg: in VN_Message_Basic;
                           Assign_Address_VN_Msg: out VN_Message_Assign_Address) is
      tempMsg : VN_Message_Basic := Basic_VN_Msg;
      for tempMsg'Address use Assign_Address_VN_Msg'Address;
   begin
      null;
   end To_Assign_Address;

end VN.Message.Assign_Address;
