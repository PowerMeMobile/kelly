db.outgoing_messages.ensureIndex({customer_id: 1, client_type: 1, in_msg_id: 1}, {unique: true});
db.outgoing_messages.ensureIndex({gateway_id: 1, out_msg_id: 1});
db.outgoing_messages.ensureIndex({req_time: 1});
db.incoming_messages.ensureIndex({req_time: 1});
