// create kellydb indexes
db = db.getSiblingDB("kellydb");
db.mt_messages.ensureIndex({customer_id: 1, client_type: 1, in_msg_id: 1}, {unique: true});
db.mt_messages.ensureIndex({gateway_id: 1, out_msg_id: 1});
db.mt_messages.ensureIndex({req_time: 1});
db.mo_messages.ensureIndex({req_time: 1});
// create billydb indexes
db = db.getSiblingDB("billydb");
db.transactions.ensureIndex({session_id: 1, transaction_idx: 1}, {unique: true});
db.transactions.ensureIndex({commit_time: 1});
