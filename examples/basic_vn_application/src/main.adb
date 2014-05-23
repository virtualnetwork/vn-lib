with Global_Settings;
with Subnet_Manager_Local;
with Central_Addressing_Service;
with Lookup_Service;
with Application;

procedure Main is
begin
   null;
   Global_Settings.Com_Application.Init;
   Global_Settings.Com_CAS.Init;
   Global_Settings.Com_LS.Init;
   Global_Settings.Com_SM_L.Add_Interface(Global_Settings.PO_Router'Access);
   Global_Settings.PO_Router.Add_Interface(Global_Settings.PO_Wrapper_To_Application'Access);
   Global_Settings.PO_Router.Add_Interface(Global_Settings.PO_Wrapper_To_CAS'Access);
   Global_Settings.PO_Router.Add_Interface(Global_Settings.PO_Wrapper_To_LS'Access);
end Main;
