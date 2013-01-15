// create kelly's indexes
db = db.getSiblingDB("kelly");
db.mt_messages.ensureIndex({ci: 1, ct: 1, imi: 1}, {unique: true});
db.mt_messages.ensureIndex({gi: 1, omi: 1});
db.mt_messages.ensureIndex({rqt: 1});
db.mo_messages.ensureIndex({rqt: 1});
// create billy's indexes
db = db.getSiblingDB("billy");
db.transactions.ensureIndex({si: 1, ti: 1}, {unique: true});
db.transactions.ensureIndex({cmt: 1});
