------------------------------------------------------------------------------
--  This file is part of VN-Lib.
--
--  VN-Lib is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  VN-Lib is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with VN-Lib.  If not, see <http://www.gnu.org/licenses/>.
--
--  Copyright 2014 Christoffer Holmstedt (christoffer.holmstedt@gmail.com)
------------------------------------------------------------------------------

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
