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

with System;
with Ada.Real_Time;
with VN.Communication.CAN.CAN_Interface;

with VN.Communication.CAN.CAN_Filtering;

--with BBB_CAN;

package VN.Communication.CAN.Can_Task is

   task type CAN_Task_Type(myAccess : VN.Communication.CAN.CAN_Interface.CAN_Interface_Access;
                           Pri : System.Priority;
                           thePeriod : access Ada.Real_Time.Time_Span;
                           CANFilter : VN.Communication.CAN.CAN_Filtering.CAN_Filter_Access) is 
      pragma Priority(Pri);
   end CAN_Task_Type;
end VN.Communication.CAN.Can_Task;
