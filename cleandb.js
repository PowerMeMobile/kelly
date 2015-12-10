var m = db.getMongo();
dbs = m.getDBNames();
dbs = dbs.filter(function (element, index, array) {
	if (element.indexOf("kelly") > -1) {
	    return true
	}
});
dbs.forEach(function (element) {
	tmpDb = db.getSiblingDB(element);
	tmpDb.dropDatabase()
});