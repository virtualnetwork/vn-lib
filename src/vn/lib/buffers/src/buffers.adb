------------------------------------------------------------------------------
--                                                                          --
--                           GNAT RAVENSCAR for NXT                         --
--                                                                          --
--                    Copyright (C) 2010-2011, AdaCore                      --
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

package body Buffers is

   ------------
   -- Insert --
   ------------

   procedure Insert (Item : Element;  Into : in out Buffer) is
   begin
      Into.Values (Into.Next_In) := Item;
      Into.Next_In := (Into.Next_In mod Into.Capacity) + 1;
      Into.Count := Into.Count + 1;
   end Insert;

   ------------
   -- Remove --
   ------------

   procedure Remove (Item : out Element; From : in out Buffer) is
   begin
      Item := From.Values (From.Next_Out);
      From.Next_Out := (From.Next_Out mod From.Capacity) + 1;
      From.Count := From.Count - 1;
   end Remove;

   -----------
   -- Empty --
   -----------

   function Empty (This : Buffer) return Boolean is
   begin
      return This.Count = 0;
   end Empty;

   ----------
   -- Full --
   ----------

   function Full (This : Buffer) return Boolean is
   begin
      return This.Count = This.Capacity;
   end Full;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Buffer) is
   begin
      This.Count := 0;
      This.Next_In := 1;
      This.Next_Out := 1;
   end Clear;

   ------------
   -- Extent --
   ------------

   function Extent (This : Buffer) return Natural is
   begin
      return This.Count;
   end Extent;

   ---------------
   -- Iteration --
   ---------------

   procedure Iteration (Over : Buffer) is
      Next : Positive := Over.Next_Out;
   begin
      for K in 1 .. Over.Count loop
         Visit (Over.Values (Next));
         Next := (Next mod Over.Capacity) + 1;
      end loop;
   end Iteration;

   ------------
   -- Find --
   ------------

   function Find (Item : Element;  buff : Buffer) return Boolean is
      Next : Positive := buff.Next_Out;
   begin
      for i in 1 .. buff.Count loop
         if (buff.Values (Next) = Item) then
            return true;
         end if;
         Next := (Next mod buff.Capacity) + 1;
      end loop;
      return false;
   end Find;

end Buffers;
