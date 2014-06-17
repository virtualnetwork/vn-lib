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

with VN.Message;
with VN.Message.Assign_Address;
with VN.Message.Local_Hello;

package VN.Application_Information is

   type VN_Application_Information is tagged limited
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
