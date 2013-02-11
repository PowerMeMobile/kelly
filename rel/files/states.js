function states() {
	all = db.mt_messages.count();
	req = db.mt_messages.count({rqt:{$exists:true}});
	resp = db.mt_messages.count({rpt:{$exists:true}});
	dlr = db.mt_messages.count({dt:{$exists:true}});
	print("all: " + all + " req: " + req + " resp: " + resp + " dlr: " + dlr);
};

states();
