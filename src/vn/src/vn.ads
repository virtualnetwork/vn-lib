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
--  Copyright 2014, Nils Brynedal Ignell (nils.brynedal@gmail.com) and
--  Christoffer Holmstedt (christoffer.holmstedt@gmail.com).
------------------------------------------------------------------------------

-- Summary:
-- This package implements the VN protocol.

with Interfaces;
with GNAT.IO;

package VN is

   package Text_IO renames GNAT.IO;

   type VN_CUUID is Array(1..16) of Interfaces.Unsigned_8;
   for VN_CUUID'Alignment use 4;

   type VN_Logical_Address is mod 2 ** 32;
   for VN_Logical_Address'Size use 32;

   LOGICAL_ADDRES_UNKNOWN : VN_Logical_Address := 2;
   CAS_LOGICAL_ADDRESS : constant VN.VN_Logical_Address := 1;

   type Send_Status is (OK,
                        ERROR_UNKNOWN,
                        ERROR_BUFFER_FULL,
                        ERROR_NO_ADDRESS_RECEIVED);

   type Receive_Status is (NO_MSG_RECEIVED,
                           MSG_RECEIVED_NO_MORE_AVAILABLE,
                           MSG_RECEIVED_MORE_AVAILABLE,
                           ERROR_UNKNOWN);
end VN;
