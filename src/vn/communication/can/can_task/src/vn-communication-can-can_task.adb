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
-- CAN_Task is the lowlevel task that accesses the CAN_Interface object.
-- It reads CAN messages from the lowlevel read buffer, runs the Update
-- function of CAN_Interface and writes CAN messages to the lowlevel send buffer.
-- Each task that accesses an instance of CAN_Interface will do so using an
-- access variable (pointer).

with Ada.Real_Time;

with VN;
with VN.Communication.CAN;
with VN.Communication.CAN.Logic.SM;
with VN.Communication.CAN.CAN_Driver;

with Ada.Exceptions;

package body VN.Communication.CAN.CAN_Task is

   package buf renames VN.Communication.CAN.CAN_Message_Buffers;

   task body CAN_Task_Type is
      use Ada.Real_Time;

      myPeriod : Ada.Real_Time.Time_Span;

      Next_Period : Ada.Real_Time.Time;

      BUFFER_SIZE : constant integer := 100; --ToDO: Put this in a config file of some sort
      msgsIn, msgsOut : buf.Buffer(BUFFER_SIZE);

      procedure Input is
         status  : VN.Receive_Status;
         msgLog  : VN.Communication.CAN.CAN_Message_Logical;
      begin

       CAN_Driver.Receive(msgLog, status);

         while not buf.Full(msgsIn) and
           (status = VN.MSG_RECEIVED_NO_MORE_AVAILABLE or status = VN.MSG_RECEIVED_MORE_AVAILABLE) loop -- ToDo: Update if more options of VN.Receive_Status are added

            buf.Insert(msgLog, msgsIn);

            CAN_Driver.Receive(msgLog, status);
         end loop;
      end Input;

      procedure Output is
         msgLog  : VN.Communication.CAN.CAN_Message_Logical;
         status : VN.Send_Status;
      begin

         while not buf.Empty(msgsOut) and not CAN_Driver.Send_Buffer_Full loop
            buf.Remove(msgLog, msgsOut);
            CAN_Driver.Send(msgLog, status);
         end loop;
      end Output;

   begin

      VN.Text_IO.Put_Line("CAN_Task started");

      myPeriod := thePeriod.all;
      Next_Period := Ada.Real_Time.Clock;

      loop
         Next_Period := Next_Period + myPeriod;
         delay until Next_Period;

         Input;

         myAccess.Update(msgsIn, msgsOut);

        -- CAN_Driver.Update_Filters(CANFilter);

--           VN.Text_IO.Put_Line("CAN_Task update ended");

         Output;

        -- VN.Text_IO.Put_Line("CAN_Task loop ended");
      end loop;

   end CAN_Task_Type;
end VN.Communication.CAN.CAN_Task;
