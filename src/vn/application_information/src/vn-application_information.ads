with VN.Message;
with VN.Message.Assign_Address;
with VN.Message.Local_Hello;

package VN.Application_Information is

   function Has_Logical_Address return Boolean;

   procedure Set_Logical_Address(
            Message: in VN.Message.Assign_Address.VN_Message_Assign_Address);

   procedure Get_Application_Information(
            Message: out VN.Message.Local_Hello.VN_Message_Local_Hello);

   procedure Init(CUUID:            in VN.VN_CUUID;
                  Component_Type:   in VN.Message.VN_Component_Type);

   type VN_Application_Information is private;

private

   type VN_Application_Information is
      record
         Logical_Address   : VN.VN_Logical_Address := 16#0000_0000#;
         CUUID             : VN.VN_CUUID; -- TODO: Assign default value.
         Component_Type    : VN.Message.VN_Component_Type := VN.Message.Other;
      end record;

   This_Application : VN_Application_Information;

end VN.Application_Information;
