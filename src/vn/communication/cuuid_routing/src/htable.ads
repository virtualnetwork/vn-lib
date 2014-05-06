
-- This file is a modification of the s-htable.ads file from GNAT.
-- No rights reserved.

--  Hash table searching routines

--  The Static_HTable package provides a more complex interface that allows
--  complete control over allocation.

pragma Compiler_Unit;

package HTable is
   pragma Preelaborate;

   -------------------
   -- Static_HTable --
   -------------------

   --  A low-level Hash-Table abstraction, not as easy to instantiate as
   --  Simple_HTable but designed to allow complete control over the
   --  allocation of necessary data structures. Particularly useful when
   --  dynamic allocation is not desired. The model is that each Element
   --  contains its own Key that can be retrieved by Get_Key. Furthermore,
   --  Element provides a link that can be used by the HTable for linking
   --  elements with same hash codes:

   --       Element

   --         +-------------------+
   --         |       Key         |
   --         +-------------------+
   --         :    other data     :
   --         +-------------------+
   --         |     Next Elmt     |
   --         +-------------------+

   generic
      type Header_Num is range <>;
      --  An integer type indicating the number and range of hash headers

      type Element (<>) is limited private;
      --  The type of element to be stored. This is historically part of the
      --  interface, even though it is not used at all in the operations of
      --  the package.

      pragma Warnings (Off, Element);
      --  We have to kill warnings here, because Element is and always
      --  has been unreferenced, but we cannot remove it at this stage,
      --  since this unit is in wide use, and it certainly seems harmless.

      type Elmt_Ptr is private;
      --  The type used to reference an element (will usually be an access
      --  type, but could be some other form of type such as an integer type).

      Null_Ptr : Elmt_Ptr;
      --  The null value of the Elmt_Ptr type

      with procedure Set_Next (E : Elmt_Ptr; Next : Elmt_Ptr);
      with function  Next     (E : Elmt_Ptr) return Elmt_Ptr;
      --  The type must provide an internal link for the sake of the
      --  staticness of the HTable.

      type Key is limited private;
      with function Get_Key (E : Elmt_Ptr) return Key;
      with function Hash    (F : Key)      return Header_Num;
      with function Equal   (F1, F2 : Key) return Boolean;

   package Static_HTable is

      type Table is private;

      procedure Reset (this : in out Table);
      --  Resets the hash table by setting all its elements to Null_Ptr. The
      --  effect is to clear the hash table so that it can be reused. For the
      --  most common case where Elmt_Ptr is an access type, and Null_Ptr is
      --  null, this is only needed if the same table is reused in a new
      --  context. If Elmt_Ptr is other than an access type, or Null_Ptr is
      --  other than null, then Reset must be called before the first use
      --  of the hash table.

      procedure Set (this : in out Table; E : Elmt_Ptr);
      --  Insert the element pointer in the HTable

      function Get (this : in Table; K : Key) return Elmt_Ptr;
      --  Returns the latest inserted element pointer with the given Key
      --  or null if none.

      function Present (this : in Table; K : Key) return Boolean;
      --  True if an element whose Get_Key is K is in the table

   --   function Set_If_Not_Present (this : in Table; E : Elmt_Ptr) return Boolean;
      --  If Present (Get_Key (E)), returns False. Otherwise, does Set (E), and
      --  then returns True. Present (Get_Key (E)) is always True afterward,
      --  and the result True indicates E is newly Set.

      procedure Remove (this : in out Table; K : Key);
      --  Removes the latest inserted element pointer associated with the
      --  given key if any, does nothing if none.

      function Get_First (this : in out Table) return Elmt_Ptr;
      --  Returns Null_Ptr if the HTable is empty, otherwise returns one
      --  non specified element. There is no guarantee that two calls to this
      --  function will return the same element.

      function Get_Next (this : in out Table) return Elmt_Ptr;
      --  Returns a non-specified element that has not been returned by the
      --  same function since the last call to Get_First or Null_Ptr if
      --  there is no such element or Get_First has never been called. If
      --  there is no call to 'Set' in between Get_Next calls, all the
      --  elements of the HTable will be traversed.

   private

      procedure Get_Non_Null(this : in out Table; ptr : out Elmt_Ptr);
      --  Returns Null_Ptr if Iterator_Started is false or the Table is empty.
      --  Returns Iterator_Ptr if non null, or the next non null element in
      --  table if any.

      type Element_Array_Type is array (Header_Num) of Elmt_Ptr;

      type Table is
         record
            Table : Element_Array_Type;

            Iterator_Index   : Header_Num;
            Iterator_Ptr     : Elmt_Ptr;
            Iterator_Started : Boolean := False;
         end record;

   end Static_HTable;

   ----------
   -- Hash --
   ----------

   --  A generic hashing function working on String keys

   generic
      type Header_Num is range <>;
   function Hash (Key : String) return Header_Num;

end HTable;
