------------------------------------------------------------------------------
--                                                                          --
--                           GNAT RAVENSCAR for NXT                         --
--                                                                          --
--                     Copyright (C) 2010-2011, AdaCore                     --
--                                                                          --
-- This is free software; you can  redistribute it  and/or modify it under  --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. This is distributed in the hope that it will be useful, but WITH-  --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

--  Non-blocking circular bounded buffers

generic
   type Element is private;
package Buffers is
   pragma Elaborate_Body;

   -- type Buffer (Capacity : Positive) is limited private;
   type Buffer (Capacity : Positive) is private;

   procedure Insert (Item : Element;  Into : in out Buffer);
   --  Insert Item into the buffer, overwriting if necessary.
   --  Does not block caller if no space is available.

   procedure Remove (Item : out Element; From : in out Buffer);
   --  Remove next available Element from buffer.
   --  Does not block caller is none available.

   function Empty (This : Buffer) return Boolean;
   --  Returns whether the instance contains any Elements.

   function Full (This : Buffer) return Boolean;
   --  Returns whether any space remains within the instance.

   function Extent (This : Buffer) return Natural;
   --  Returns the number of Element values currently held
   --  within the instance.

   procedure Clear (This : in out Buffer);
   --  Makes This buffer become empty

   function Find (Item : Element;  buff : Buffer) return Boolean;
   --  Returns whether the buffer contains the item.

   generic
      with procedure Visit (This : Element);
   procedure Iteration (Over : Buffer);

private

   type Content is array (Positive range <>) of Element;

   type Buffer (Capacity : Positive) is
      record
         Values   : Content (1 .. Capacity);
         Next_In  : Positive := 1;
         Next_Out : Positive := 1;
         Count    : Natural  := 0;
      end record;

end Buffers;
