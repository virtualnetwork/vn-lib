
-- This file is a modification of the s-htable.adb file from GNAT.
-- No rights reserved.


pragma Compiler_Unit;

with Ada.Unchecked_Deallocation;
with System.String_Hash;

package body HTable is

   -------------------
   -- Static_HTable --
   -------------------

   package body Static_HTable is

      ---------
      -- Get --
      ---------

      function Get (this : in Table; K : Key) return Elmt_Ptr is
         Elmt : Elmt_Ptr;

      begin
         Elmt := this.Table (Hash (K));
         loop
            if Elmt = Null_Ptr then
               return Null_Ptr;

            elsif Equal (Get_Key (Elmt), K) then
               return Elmt;

            else
               Elmt := Next (Elmt);
            end if;
         end loop;
      end Get;

      ---------------
      -- Get_First --
      ---------------

      function Get_First (this : in out Table) return Elmt_Ptr is
         ret : Elmt_Ptr;
      begin
         this.Iterator_Started := True;
         this.Iterator_Index := this.Table'First;
         this.Iterator_Ptr := this.Table (this.Iterator_Index);

         Get_Non_Null(this, ret);
         return ret;
      end Get_First;

      --------------
      -- Get_Next --
      --------------

      function Get_Next (this : in out Table) return Elmt_Ptr is
         ret : Elmt_Ptr;
      begin
         if not this.Iterator_Started then
            return Null_Ptr;
         else
            this.Iterator_Ptr := Next (this.Iterator_Ptr);
            Get_Non_Null(this, ret);
            return ret;
         end if;
      end Get_Next;

      ------------------
      -- Get_Non_Null --
      ------------------

      procedure Get_Non_Null(this : in out Table; ptr : out Elmt_Ptr) is
      begin
         while this.Iterator_Ptr = Null_Ptr loop
            if this.Iterator_Index = this.Table'Last then
               this.Iterator_Started := False;
               ptr := Null_Ptr;
               return;
            end if;

            this.Iterator_Index := this.Iterator_Index + 1;
            this.Iterator_Ptr   := this.Table (this.Iterator_Index);
         end loop;

         ptr := Null_Ptr;
         return;
      end Get_Non_Null;

      -------------
      -- Present --
      -------------

      function Present (this : in Table; K : Key) return Boolean is
      begin
         return Get (this, K) /= Null_Ptr;
      end Present;

      ------------
      -- Remove --
      ------------

      procedure Remove (this : in out Table; K : Key) is
         Index     : constant Header_Num := Hash (K);
         Elmt      : Elmt_Ptr;
         Next_Elmt : Elmt_Ptr;

      begin
         Elmt := this.Table (Index);

         if Elmt = Null_Ptr then
            return;

         elsif Equal (Get_Key (Elmt), K) then
            this.Table (Index) := Next (Elmt);

         else
            loop
               Next_Elmt :=  Next (Elmt);

               if Next_Elmt = Null_Ptr then
                  return;

               elsif Equal (Get_Key (Next_Elmt), K) then
                  Set_Next (Elmt, Next (Next_Elmt));
                  return;

               else
                  Elmt := Next_Elmt;
               end if;
            end loop;
         end if;
      end Remove;

      -----------
      -- Reset --
      -----------

      procedure Reset (this : in out Table) is
      begin
         for J in this.Table'Range loop
            this.Table (J) := Null_Ptr;
         end loop;
      end Reset;

      ---------
      -- Set --
      ---------

      procedure Set (this : in out Table; E : Elmt_Ptr) is
         Index : Header_Num;
      begin
         Index := Hash (Get_Key (E));
         Set_Next (E, this.Table (Index));
         this.Table (Index) := E;
      end Set;

      ------------------------
      -- Set_If_Not_Present --
      ------------------------

--        function Set_If_Not_Present (E : Elmt_Ptr) return Boolean is
--           K : Key renames Get_Key (E);
--           --  Note that it is important to use a renaming here rather than
--           --  define a constant initialized by the call, because the latter
--           --  construct runs into bootstrap problems with earlier versions
--           --  of the GNAT compiler.
--
--           Index : constant Header_Num := Hash (K);
--           Elmt  : Elmt_Ptr;
--
--        begin
--           Elmt := Table (Index);
--           loop
--              if Elmt = Null_Ptr then
--                 Set_Next (E, Table (Index));
--                 Table (Index) := E;
--                 return True;
--
--              elsif Equal (Get_Key (Elmt), K) then
--                 return False;
--
--              else
--                 Elmt := Next (Elmt);
--              end if;
--           end loop;
--        end Set_If_Not_Present;

   end Static_HTable;

   ----------
   -- Hash --
   ----------

   function Hash (Key : String) return Header_Num is
      type Uns is mod 2 ** 32;

      function Hash_Fun is
         new System.String_Hash.Hash (Character, String, Uns);

   begin
      return Header_Num'First +
        Header_Num'Base (Hash_Fun (Key) mod Header_Num'Range_Length);
   end Hash;

end HTable;
