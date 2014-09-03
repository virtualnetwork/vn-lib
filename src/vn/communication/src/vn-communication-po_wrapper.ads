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
with VN.Communication.PO;

package VN.Communication.PO_Wrapper is

   -- This PO_Wrapper makes it possible to wrap two different Protected
   -- Objects in one "wrapper". The two different Protected Objects are
   -- buffers for in and outgoing traffic respectively.
   type VN_PO_Wrapper(VN_PO_Access: VN.Communication.PO.VN_PO_Access;
                      CUUID_Access: access VN.VN_CUUID;
                      Component_Type: VN.Message.VN_Component_Type;
                    Is_SM_L: Boolean)
                     is new Com with Private;

   procedure Send(This: in out VN_PO_Wrapper;
                  Message: in VN.Message.VN_Message_Basic;
                  Status: out VN.Send_Status);

   procedure Receive(This: in out VN_PO_Wrapper;
                     Message: out VN.Message.VN_Message_Basic;
                     Status: out VN.Receive_Status);

   procedure Init(This: in out VN_PO_Wrapper);
   procedure Send_Local_Ack(This: in out VN_PO_Wrapper);
   procedure Send_Request_Address_Block(This: in out VN_PO_Wrapper);

   type PO_Wrapper_Access is access all VN_PO_Wrapper'Class;

private

   type VN_Buffer is array (1 .. 10) of VN.Message.VN_Message_Basic;

   type VN_PO_Wrapper(VN_PO_Access: VN.Communication.PO.VN_PO_Access;
                      CUUID_Access: access VN.VN_CUUID;
                      Component_Type: VN.Message.VN_Component_Type;
                    Is_SM_L: Boolean)
                     is new Com with
      record
         PO_Access: VN.Communication.PO.VN_PO_Access := VN_PO_Access;
         Is_From_SM_L: Boolean := Is_SM_L;
         CUUID:  VN.VN_CUUID := CUUID_Access.all;
         This_Component_Type:  VN.Message.VN_Component_Type := Component_Type;
      end record;

end VN.Communication.PO_Wrapper;
