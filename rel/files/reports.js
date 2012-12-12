var fromDate = ISODate("2012-12-11T14:20:00Z");
//var toDate = ISODate("2012-12-11T14:40:00Z");
var toDate = ISODate("2012-12-12T23:00:00Z")

function time(fun) {
	var start = new Date();
	var res = fun();
	var stop = new Date();

	printjson(res);
	print("runtime: " + (stop-start)/1000 + " secs");
};

function dlr_statuses() {
	db.outgoing_messages.distinct("dlr_status").forEach(function(status) {
		var count = db.outgoing_messages.count( { dlr_status : status} );
		print(status + ": " + count);
	});
};

function rep1_mr() {
	var mapf = function() {
		if (this.dlr_status) {
			emit(this.dlr_status, 1);
		} else if (this.resp_status) {
			emit(this.resp_status, 1);
		} else {
			emit("submitted", 1);
		}
	};
	var reducef = function(key, values) {
		return Array.sum(values);
	};
	return db.runCommand({
		mapreduce : "outgoing_messages",
		query : { req_time : { $gte : fromDate, $lt : toDate } },
		map : mapf,
		reduce : reducef,
		out : { inline : 1 }
	});
};

function rep2_mr() {
	var mapf = function() {
		this.charges.forEach(function(charge) {
			emit(charge.package_id, {
				sent : charge.svc_quantity,
				revenue : charge.svc_quantity * charge.svc_price
			});
		});
	};
	var reducef = function(key, values) {
		var result = { sent : 0, revenue : 0 };

		values.forEach(function(value) {
			result.sent += value.sent;
			result.revenue += value.revenue;
		});

		return result;
	};
	return db.runCommand({
		mapreduce : "transaction_registry",
		query : { commit_time : { $gte : fromDate, $lt : toDate } },
		map : mapf,
		reduce : reducef,
		out : { inline : 1 }
	});
};

function rep3_aggr() {
	return db.runCommand({
		aggregate : "transaction_registry",
		pipeline : [
			{ $match : { commit_time : { $gte : fromDate, $lt : toDate } } },
			{ $project : {
				_id : 0,
				client_type : 1,
				charges : 1
			}},
			{ $unwind : "$charges" },
			{ $project : {
				client_type : 1,
				quantity : "$charges.svc_quantity",
				revenue : { $multiply : ["$charges.svc_price", "$charges.svc_quantity"] }
			}},
			//{ $limit : 100 },
			{ $group : {
				_id : "$client_type",
				sent : { $sum : "$quantity" },
				revenue : { $sum : "$revenue" }
			}},
			{ $project : {
				_id : 0,
				client_type : "$_id",
				sent : "$sent",
				revenue : "$revenue"
			}}
		]});
};

function rep3_mr() {
	var mapf = function() {
		var client_type = this.client_type;

		this.charges.forEach(function(charge) {
			emit(client_type, {
				sent : charge.svc_quantity,
				revenue : charge.svc_quantity * charge.svc_price
			});
		});
	};
	var reducef = function(key, values) {
		var result = { sent : 0, revenue : 0 };

		values.forEach(function(value) {
			result.sent += value.sent;
			result.revenue += value.revenue;
		});

		return result;
	};
	return db.runCommand({
		mapreduce : "transaction_registry",
		query : { commit_time : { $gte : fromDate, $lt : toDate } },
		map : mapf,
		reduce : reducef,
		out : { inline : 1 }
	});
};

function rep4_aggr() {
	return db.runCommand({
		aggregate : "transaction_registry",
		pipeline : [
			{ $match : { commit_time : { $gte : fromDate, $lt : toDate } } },
			{ $project : {
				_id : 0,
				customer_id : 1,
				charges : 1
			}},
			{ $unwind : "$charges" },
			{ $project : {
				customer_id : 1,
				type : "$charges.svc_type",
				quantity : "$charges.svc_quantity",
				revenue : { $multiply : ["$charges.svc_price", "$charges.svc_quantity"] }
			}},
			{ $group : {
				_id : { customer_id : "$customer_id", type : "$type" },
				sent : { $sum : "$quantity" },
				revenue : { $sum : "$revenue" }
			}},
			{ $group : {
				_id : "$_id.customer_id",
				details : { $addToSet : {
					type : "$_id.type",
					sent : "$sent",
					revenue : "$revenue"
				}}}},
			{ $project : {
				_id : 0,
				customer_id : "$_id",
				details : "$details"
			}}
		]});
};

function rep4_mr() {
	var mapf = function() {
		var customer_id = this.customer_id;

		this.charges.forEach(function(charge) {
			emit({ customer_id : customer_id, type : charge.svc_type }, {
				sent : charge.svc_quantity,
				revenue : charge.svc_quantity * charge.svc_price
			});
		});
	};
	var reducef = function(key, values) {
		var result = { sent : 0, revenue : 0 };

		values.forEach(function(value) {
			result.sent += value.sent;
			result.revenue += value.revenue;
		});

		return result;
	};
	return db.runCommand({
		mapreduce : "transaction_registry",
		query : { commit_time : { $gte : fromDate, $lt : toDate } },
		map : mapf,
		reduce : reducef,
		out : { inline : 1 }
	});
};
