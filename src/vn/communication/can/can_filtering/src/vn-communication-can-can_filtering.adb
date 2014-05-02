-- Copyright (c) 2014 All Rights Reserved
-- Author: Nils Brynedal Ignell
-- Date: 2014-XX-XX
-- Summary:
-- CAN_Filtering keeps track of what the hardware filters of the CAN controller should be.
-- The purpose of this is to filter out all CAN messages that are not needed.

package body VN.Communication.CAN.CAN_Filtering is

   procedure Create_Filter(this : in out CAN_Filter_Type;
                           filterID : out Filter_ID_Type;
                           template : CAN_message_ID;
                           mask     : CAN_message_ID) is
      CREATE_FILTER_ERROR : exception;
   begin
      for i in this.myFilters'Range loop  -- ToDo: This search could be optimized
         if not this.myFilters(i).isUsed then
            this.myFilters(i).isUsed := true;
            this.myFilters(i).template := template;
            this.myFilters(i).mask := mask;
            filterID := i;
            return;
         end if;
      end loop;
      raise CREATE_FILTER_ERROR; --ToDo, we should have a better way of handling when we run out of space...
   end Create_Filter;

   procedure Change_Filter(this : in out CAN_Filter_Type;
                           filterID : Filter_ID_Type;
                           template : CAN_message_ID;
                           mask     : CAN_message_ID) is
   begin
      -- This if statement prevents error if the filterID was not initialized.
      -- I.e. read before written to.
      if filterID >= Filter_ID_Type'First and filterID <= Filter_ID_Type'Last then
         this.myFilters(filterID).template := template;
         this.myFilters(filterID).mask := mask;
      end if;
   end Change_Filter;

   procedure Remove_Filter(this : in out CAN_Filter_Type;
                           filterID : Filter_ID_Type) is
   begin
      this.myFilters(filterID).isUsed := false;
   end Remove_Filter;

   procedure Get_Filter(this : in CAN_Filter_Type;
                        filterID : Filter_ID_Type;
                        template : out CAN_message_ID;
                        mask 	 : out CAN_message_ID;
                        isUsed 	 : out Boolean) is
   begin
      template 	:= this.myFilters(filterID).template;
      mask 	:= this.myFilters(filterID).mask;
      isUsed 	:= this.myFilters(filterID).isUsed;
   end Get_Filter;

end VN.Communication.CAN.CAN_Filtering;
