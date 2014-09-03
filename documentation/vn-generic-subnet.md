VN Generic Subnet Protocol
==========================
This protocol defines the specifications that lie on each subnet protocol. A
subnet protocol is specific for the underlying communication protocol. Examples
include the VN-CAN protocol for communication over the Controller Area Network
(CAN).

Copyright (c) 2014 All Rights Reserved  <br/>
Author: Nils Brynedal Ignell

### Addresses
**There exists two forms of addresses in this protocol:**
  * **The logical addresses.**  Defined in the VN protocol.
  * **Low level addresses.** The address specific for the underlying protocol.
    These are defined in the underlying protocol.  An example is the CAN
    addresses defined in the VN-CAN low level protocol.

### Stored data
**The following data is stored in this protocol:**
  * Send buffer, for VN-messages.
  * Receive buffer, for VN-messages.
  * A primary routing table, connecting a given logical address to a low level
    address.
  * A CUUID routing table, connecting a given CUUID to a subnet address. This
    table is used in some special cases of VN messages (e.g. **AssignAddr**)
    that cannot rely on logical addresses for routing.

### Interface
**There exists two public functions stored in this protocol:**
  * Send. Will take a VN message as argument, an out-variable will tell the
    outcome of the call, i.e. “Transmission OK” or “Buffer full”. The message
    is written to the send buffer if it is not full.
  * Receive. Will give a VN message as an out-variable, another out-variable
    will tell the outcome of the call, i.e. “No message received”, “Message
    received, buffer empty” and “Message received, more available”.

### Unit discovery process
This protocol does not specify how the discovery process is to be done on the
subnet, this is up to the specific subnet protocol to define.

Each unit (node or Subnet Manager) on the subnet shall send a **LocalHello**
message containing the unit's CUUID and type to each Subnet Manager it
discovers. If no **LocalAck message** is received within a certain time the
**LocalHello message** shall be resent. <br/>
_The responsibility to send the
**LocalHello messages** lies on the subnet layer._ <br/>
The sender and receiver (logical) addresses of any **LocalHello** or **LocalAck** message shall
be set to *VN.LOGICAL_ADDRES_UNKNOWN*.

The **LocalHello** messages enable the higher level protocols on the Subnet Managers to discover which units
(their type and CUUID) that exist on the subnet.

When a **LocalHello message** is received over the subnet the following
shall be done:
  * Actions according to Route discovery process.
  * A **LocalAck message** shall be sent in response.
  * The **LocalHello message** shall be passed on to the overlying protocol.

### Route discovery process
##### For routes to overlying units
The VN generic subnet protocol assigns no responsibilities to the subnet protocol regarding routing to overlying layers, all
received messages are passed on to the overlying layer.

##### For routes to underlying units
Whenever a VN message is received from another unit on the subnet (i.e. is to
be added to the Receive buffer), it can be concluded that this unit can route
VN messages to the source address of the VN message. Consequently, the source
address (a logical address) of the VN message and the local address of the unit
from which the VN message was received from shall be entered into the Primary routing
table. <br/>
_The above does not apply to **LocalHello** and **LocalAck** messages.
No routing information regarding logical addresses shall be retrieved from
**LocalHello** and **LocalAck** messages._

Whenever a **DistributeRoute** message is received from another unit on the
subnet, it can be concluded that this unit can route VN messages to the logical
address contained in the **DistributeRoute** message. <br/>
Consequently, the logical address contained in the **DistributeRoute** message
and the local address of the unit from which the message was received from
shall be entered into the Primary routing table. <br/>
_Since **DistributeRoute** messages are the least reliable source of routing information, 
routing information regarding a particular logical address shall only be retrieved from **DistributeRoute** messages if no previous routing information regarding this logical address. _


This section is only relevant for subnet managers, not nodes: <br/>
If the unit sends a **AssignAddr** message 
to another unit on the subnet, this unit's low level address and the logical address it gets
assigned shall be entered to the Primary routing table.  <br/>
If the unit sends a **AssignAddrBlock** message with the Receiver address set equal to
*VN.LOGICAL_ADDRES_UNKNOWN* to another on the subnet, this unit's low level address and the
base address for the assigned address block shall be into the Primary routing table.


### Transmission of VN messages
If a VN message is addressed to logical address 0 it shall be discarded. 

Whenever a VN message is
sent by the higher level protocol the receiver address of the message is to be
looked up in the Primary routing table. The Primary routing table will give the local address
that is to be used to send the message over the subnet.   <br/>
There exist the following exceptions from the above rule:

1. If the message is an **AssignAddrBlock** message or an **AssignAddr** message whose *Destination* address equals
*VN.LOGICAL_ADDRES_UNKNOWN*, the CUUID in its payload shall be used for a lookup in the CUUID routing table as opposed to the primary routing table.
2. Whenever a **LocalHello** or **LocalAck** message is sent by the higher level
protocol the receiver address of the message is to be looked up in the CUUID
routing table. The CUUID routing table will give the local address that is to
be used to send the message over the subnet.

If the logical address is not found this shall be reported to the overlying
protocol.

### Reception of VN messages
When a VN message is received on the subnet it shall be pushed to the receive
buffer. Actions according to section _Route discovery process_ shall also be
performed.

### Logical addresses
Assignment of logical addresses is handled by higher level protocols and is not
described in this protocol.
