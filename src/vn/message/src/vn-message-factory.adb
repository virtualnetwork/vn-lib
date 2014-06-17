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

with VN.Message.Local_Hello;
use VN.Message.Local_Hello;
with VN.Message.Local_Ack;
use VN.Message.Local_Ack;
with VN.Message.Assign_Address;
use VN.Message.Assign_Address;
with VN.Message.Assign_Address_Block;
use VN.Message.Assign_Address_Block;
with VN.Message.Request_Address_Block;
use VN.Message.Request_Address_Block;
with VN.Message.Distribute_Route;
use VN.Message.Distribute_Route;

with VN.Message.Request_LS_Probe;
use VN.Message.Request_LS_Probe;
with VN.Message.Probe_Request;
use VN.Message.Probe_Request;
with VN.Message.Probe_Reply;
use VN.Message.Probe_Reply;

package body VN.Message.Factory is

   function Create(VN_Msg_Type: in VN_Message_Type) return VN_Message_Basic is
      VN_Msg: VN_Message_Basic;
   begin
      case VN_Msg_Type is
         when Type_Basic => null;
         when Type_Local_Hello =>
            declare
               Temp_Msg: VN_Message_Local_Hello;
            begin
               To_Local_Hello(VN_Msg, Temp_Msg);
               To_Basic(Temp_Msg, VN_Msg);
            end;
         when Type_Local_Ack => null;
            declare
               Temp_Msg: VN_Message_Local_Ack;
            begin
               To_Local_Ack(VN_Msg, Temp_Msg);
               To_Basic(Temp_Msg, VN_Msg);
            end;
         when Type_Assign_Address => null;
            declare
               Temp_Msg: VN_Message_Assign_Address;
            begin
               To_Assign_Address(VN_Msg, Temp_Msg);
               To_Basic(Temp_Msg, VN_Msg);
            end;
         when Type_Request_Address_Block => null;
            declare
               Temp_Msg: VN_Message_Request_Address_Block;
            begin
               To_Request_Address_Block(VN_Msg, Temp_Msg);
               To_Basic(Temp_Msg, VN_Msg);
            end;
         when Type_Assign_Address_Block => null;
            declare
               Temp_Msg: VN_Message_Assign_Address_Block;
            begin
               To_Assign_Address_Block(VN_Msg, Temp_Msg);
               To_Basic(Temp_Msg, VN_Msg);
            end;
         when Type_Distribute_Route => null;
            declare
               Temp_Msg: VN_Message_Distribute_Route;
            begin
               To_Distribute_Route(VN_Msg, Temp_Msg);
               To_Basic(Temp_Msg, VN_Msg);
            end;
         when Type_Request_LS_Probe => null;
            declare
               Temp_Msg: VN_Message_Request_LS_Probe;
            begin
               To_Request_LS_Probe(VN_Msg, Temp_Msg);
               To_Basic(Temp_Msg, VN_Msg);
            end;
         when Type_Probe_Request => null;
            declare
               Temp_Msg: VN_Message_Probe_Request;
            begin
               To_Probe_Request(VN_Msg, Temp_Msg);
               To_Basic(Temp_Msg, VN_Msg);
            end;
         when Type_Probe_Reply => null;
            declare
               Temp_Msg: VN_Message_Probe_Reply;
            begin
               To_Probe_Reply(VN_Msg, Temp_Msg);
               To_Basic(Temp_Msg, VN_Msg);
            end;
      end case;

      return VN_Msg;
   end Create;

end VN.Message.Factory;
