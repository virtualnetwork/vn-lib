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

-- Summary:
-- This is an old test of the CAN drivers, might not work.
------------------------------------------------------------------------------

with System.BB.Interrupts;

package CAN_Driver_Test is


  -- Pragma Import(C, CAN_IRQHandler, "CAN_IRQHandler");

   procedure Init;

private

   procedure Handler(ID : System.BB.Interrupts.Interrupt_ID);

   procedure TestHandler(ID : System.BB.Interrupts.Interrupt_ID);

end CAN_Driver_Test;
