// create kelly's indexes
// moved index creating to k_storage_manager.
// create billy's indexes
db = db.getSiblingDB("billy");
db.transactions.ensureIndex({si: 1, ti: 1}, {unique: true});
db.transactions.ensureIndex({cmt: 1});
