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
with System;
with VN.Application_Information;
with VN.Message.Factory;
with VN.Message.Local_Hello;
with VN.Message.Assign_Address;
with VN.Message.Request_LS_Probe;
with VN.Message.Probe_Request;
with VN.Message.Probe_Reply;
with Buffers;

package Lookup_Service is

   task type LS(Pri : System.Priority;
                     Cycle_Time : Positive;
                     Task_ID : Positive;
                     Increment_By : Positive) is
      pragma Priority(Pri);
   end LS;

   private

      package VN_Logical_Address_Buffer is
         new Buffers(VN.VN_Logical_Address);

      App_Info: VN.Application_Information.VN_Application_Information;

      Basic_Msg: VN.Message.VN_Message_Basic;
      Local_Hello_Msg: VN.Message.Local_Hello.VN_Message_Local_Hello;
      Assign_Address_Msg: VN.Message.Assign_Address.VN_Message_Assign_Address;
      Request_LS_Probe_Msg: VN.Message.Request_LS_Probe.VN_Message_Request_LS_Probe;
      Probe_Request_Msg: VN.Message.Probe_Request.VN_Message_Probe_Request;
      Probe_Reply_Msg: VN.Message.Probe_Reply.VN_Message_Probe_Reply;

      Recv_Status: VN.Receive_Status;
      Send_Status: VN.Send_Status;

      Temp_Logical_Address: VN.VN_Logical_Address := VN.LOGICAL_ADDRES_UNKNOWN;

      -- TODO: Change this buffer to some kind of data store.
      Probe_Request_Buffer: VN_Logical_Address_Buffer.Buffer(10);

end Lookup_Service;
