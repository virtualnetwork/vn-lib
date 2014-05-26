with Global_Settings;
with Subnet_Manager_Local;
with SM_X;
with Central_Addressing_Service;
with Lookup_Service;
with Application;
with App2;

procedure Main is
begin
   null;
   Global_Settings.Com_Application.Init;
   Global_Settings.Com_CAS.Init;
   Global_Settings.Com_LS.Init;
   Global_Settings.Com_App2.Init;
   Global_Settings.PO_Wrapper_To_SM_L.Init;

   Global_Settings.Com_SM_L.Add_Interface(Global_Settings.PO_Router'Access);
   Global_Settings.PO_Router.Add_Interface(Global_Settings.PO_Wrapper_To_Application'Access);
   Global_Settings.PO_Router.Add_Interface(Global_Settings.PO_Wrapper_To_CAS'Access);
   Global_Settings.PO_Router.Add_Interface(Global_Settings.PO_Wrapper_To_LS'Access);
   Global_Settings.PO_Router.Add_Interface(Global_Settings.PO_Wrapper_To_SM_x'Access);

   Global_Settings.Com_SM_x.Add_Interface(Global_Settings.PO_Router_SM_x'Access);
   Global_Settings.PO_Router_SM_x.Add_Interface(Global_Settings.PO_Wrapper_To_App2'Access);
   Global_Settings.PO_Router_SM_x.Add_Interface(Global_Settings.PO_Wrapper_To_SM_L'Access);

end Main;
