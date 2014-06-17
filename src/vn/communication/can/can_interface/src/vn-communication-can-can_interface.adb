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
--  Copyright 2014, Nils Brynedal Ignell (nils.brynedal@gmail.com)
------------------------------------------------------------------------------

-- Summary:
-- CAN_Interface is a protected object that holds a VN.Communication.CAN.Logic.SM.Main_Duty.
-- CAN_Interface is to be used by two tasks: One higher level task and
-- CAN_task, a lower level task that handles CAN communication.

-- Each task that accesses an instance of CAN_Interface will do so using an
-- access variable (pointer).

-- Please note: If the Ravenscar profile had not been used, the CAN_Task
-- would have been put inside CAN_Interface which would have simplyfied the
-- interface of CAN_SM_Type. This would however violate the NO_TASK_HIERARCY
-- restriction that the Ravenscar profile imposes.

package body VN.Communication.CAN.CAN_Interface is

   protected body CAN_Interface_Type is

      overriding procedure Send(Message: in VN.Message.VN_Message_Basic;
                                Status: out VN.Send_Status) is
      begin

         case data.unitType is
            when SM_CAN =>
               VN.Communication.CAN.Logic.SM.Send(data.SMDuty, Message, Status);

            when Node =>
               VN.Communication.CAN.Logic.Node.Send(data.nodeDuty, Message, Status);
         end case;
      end Send;

      overriding procedure Receive(Message : out VN.Message.VN_Message_Basic;
                                   Status: out VN.Receive_Status) is
      begin
         case data.unitType is
            when SM_CAN =>
               VN.Communication.CAN.Logic.SM.Receive(data.SMDuty, Message, Status);

            when Node =>
               VN.Communication.CAN.Logic.Node.Receive(data.nodeDuty, Message, Status);
         end case;
      end Receive;


      ----------- FUNCTIONS FOR CAN_task ---------------------
      procedure Update(msgsBuffer : in out VN.Communication.CAN.CAN_Message_Buffers.Buffer;
                       ret : out VN.Communication.CAN.CAN_Message_Buffers.Buffer) is

      begin
         case data.unitType is
            when SM_CAN =>
               VN.Communication.CAN.Logic.SM.Update(data.SMDuty, msgsBuffer, ret);

            when Node =>
               VN.Communication.CAN.Logic.Node.Update(data.nodeDuty, msgsBuffer, ret);
         end case;
   end Update;

   end CAN_Interface_Type;

end VN.Communication.CAN.CAN_Interface;
