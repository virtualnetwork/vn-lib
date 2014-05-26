VN Inter Subnet Protocol
==========================
This protocol defines the communication to and from the application layer and between different subnet protocols.
A subnet protocol is specific for the underlying communication protocol. An example of subnet protocols is the VN-CAN protocol for communication over the Controller Area Network (CAN).

### Addresses
**There exists two forms of addresses in this protocol:**
  * **The logical addresses.**  Defined in the VN protocol.
  * **Subnet addresses.** This address specifies which underlying protocol that a certain VN message shall be sent to, or if it shall be sent up to the application layer. <br/>
Example: A Subnet Manager can communicate over UDP and CAN, a subnet address can then have any of the values  UDP, CAN or APPLICATION.

### Stored data
**The following data is stored in this protocol:**
  * *Send buffer*, for VN-messages.
  * *Receive buffer*, for VN-messages.
  * A *primary routing* table, connecting a given logical address to a low level
    address.
  * A *CUUID routing* table, connecting a given CUUID to a subnet address. This
    table is used in some special cases of VN messages (e.g. **AssignAddr**)
    that cannot rely on logical addresses for routing.


### Interface
**There exists two public functions stored in this protocol:**
  * *Send*. Will take a VN message as argument, an out-variable will tell the
    outcome of the call, i.e. “Transmission OK” or “Buffer full”. The message
    is written to the send buffer if it is not full.
  * *Receive*. Will give a VN message as an out-variable, another out-variable
    will tell the outcome of the call, i.e. “No message received”, “Message
    received, buffer empty” and “Message received, more available”.

### Unit discovery process

Discovery Units is not the responsibility of this protocol.

### Route discovery process

#### For routes to overlying units
Whenever a VN message is sent by the higher level protocol the sender address (a logical address) of the message shall be added to the routing table, setting the corresponding subnet address equal to “APPLICATION”. This way the information that VN messages sent to this logical address is to be delivered to the application layer is stored. <br/>
The above does not apply to **Local Hello** or **Local Ack** messages, which should not be sent by the application layer anyway.

#### For routes to underlying units
Whenever a VN message is received from an underlying subnet, it can be concluded that VN messages addressed to the sender address of this VN message can be routed via that particular subnet.  <br/>
Consequently, the logical address of the VN message shall be entered into the routing table with corresponding subnet address set according to the subnet from which the message was received. <br/>
*The above does not apply to LocalHello and LocalAck messages. No routing information regarding logical addresses shall be retrieved from LocalHello and LocalAck messages.* <br/>
Whenever a **Distribute Route** message is received from an underlying subnet, it can be concluded that VN messages addressed to the logical address contained in the **Distribute Route** message can be routed via this subnet. <br/>
Consequently, the logical address contained in the **Distribute Route** message shall be entered into the routing table with corresponding subnet address set according to the subnet from which the message was received. <br/>
Whenever a **Local Hello** message is received from a particular subnet, the CUUID contained in its payload shall be entered into the CUUID routing table. This saves the information that **AssignAddr** and **AssignAddrBlock** messages sent to this CUUID shall be sent via this subnet.

#### Transmission of VN messages
Whenever a VN message is received from a subnet, or sent by the application layer, actions according to section *Route discovery process* shall be performed. <br/>
Whenever a VN message is received from a subnet, or sent by the application layer, the receiver address of the message shall be looked up in the primary routing table. The routing table will tell which subnet that is to be used to send the message, or if it should be delivered to the application layer.  <br/>
There exist the following exceptions from the above rule:

1. If the message is an **AssignAddrBlock** message or an **AssignAddr** message whose *Destination* address equals
*VN.LOGICAL_ADDRES_UNKNOWN*, the CUUID in its payload shall be used for a lookup in the CUUID routing table as opposed to the primary routing table.
2. Whenever a **LocalHello** or **LocalAck** message is sent by the higher level
protocol the receiver address of the message is to be looked up in the CUUID
routing table. The CUUID routing table will give the local address that is to
be used to send the message over the subnet.

If no match is found in the lookup in the routing table this shall be reported to the overlying protocol.

