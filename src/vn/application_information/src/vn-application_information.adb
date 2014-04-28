with VN.Message.Assign_Address;
with VN.Message.Local_Hello;

package body VN.Application_Information is

   function Has_Logical_Address return Boolean is
   begin
      return true;
   end Has_Logical_Address;

   procedure Set_Logical_Address(
            Message: in VN.Message.Assign_Address.VN_Message_Assign_Address) is
   begin
      This_Application.Logical_Address := Message.Assigned_Address;
   end Set_Logical_Address;

   procedure Get_Application_Information(
            Message: out VN.Message.Local_Hello.VN_Message_Local_Hello) is
   begin
      Message.CUUID := This_Application.CUUID;
      Message.Component_Type := This_Application.Component_Type;
   end Get_Application_Information;

   procedure Init(CUUID:            in VN.VN_CUUID;
                  Component_Type:   in VN.Message.VN_Component_Type) is
   begin
      This_Application.CUUID := CUUID;
      This_Application.Component_Type := Component_Type;
   end Init;

end VN.Application_Information;
