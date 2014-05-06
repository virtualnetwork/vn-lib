with VN.Message.Assign_Address;
with VN.Message.Local_Hello;

package body VN.Application_Information is

   function Has_Logical_Address(App_Info: in VN_Application_Information)
                                    return Boolean is
   begin
      if App_Info.Logical_Address /= 0 then
         return true; -- TODO: Fix correct response.
      else
         return false;
      end if;
   end Has_Logical_Address;

   procedure Set_Logical_Address(
            App_Info: out VN_Application_Information;
            Message: in VN.Message.Assign_Address.VN_Message_Assign_Address) is
   begin
      App_Info.Logical_Address := Message.Assigned_Address;
   end Set_Logical_Address;

   procedure Get_Application_Information(
            App_Info: in VN_Application_Information;
            Message: out VN.Message.Local_Hello.VN_Message_Local_Hello) is
   begin
      Message.CUUID := App_Info.CUUID;
      Message.Component_Type := App_Info.Component_Type;
      Message.Header.Source := App_Info.Logical_Address;
   end Get_Application_Information;

end VN.Application_Information;
