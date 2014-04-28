with VN.Message;
with VN.Message.Assign_Address;
with VN.Message.Local_Hello;

package VN.Application_Information is

   type VN_Application_Information is
      record
         Logical_Address   : VN.VN_Logical_Address := 16#0000_0000#;
         CUUID             : VN.VN_CUUID; -- TODO: Assign default value.
         Component_Type    : VN.Message.VN_Component_Type := VN.Message.Other;
      end record;

   function Has_Logical_Address(App_Info: in VN_Application_Information)
                                     return Boolean;

   procedure Set_Logical_Address(
            App_Info: out VN_Application_Information;
            Message: in VN.Message.Assign_Address.VN_Message_Assign_Address);

   procedure Get_Application_Information(
            App_Info: in VN_Application_Information;
            Message: out VN.Message.Local_Hello.VN_Message_Local_Hello);

end VN.Application_Information;
