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

-- Summary: This is an example that shows how to include the VN-CAN subnet code.


With VN.Communication.CAN.Can_Task;
with VN.Communication.CAN.CAN_Interface;
with VN.Communication.CAN.CAN_Filtering;

with Ada.Real_Time;
with System;

package Task_Test is

   theFilter : aliased VN.Communication.CAN.CAN_Filtering.CAN_Filter_Type;

   thePeriod : aliased Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(100);
   U1 : aliased VN.Communication.CAN.UCID := 1;
   C1 : aliased VN.VN_CUUID := (1, others => 5);

   myInterface : aliased VN.Communication.CAN.CAN_Interface.CAN_Interface_Type
     (U1'Unchecked_Access, C1'Unchecked_Access, theFilter'Unchecked_Access, 
      VN.Communication.CAN.CAN_Interface.SM_CAN);

   myTask : aliased VN.Communication.CAN.Can_Task.CAN_Task_Type
     (myInterface'Access, 7, thePeriod'Access, theFilter'Unchecked_Access);

end Task_Test;
