conf = {
	"_id" : "alley",
	"members" : [
		{"_id" : 0, "host" : "127.0.0.1:40001"},
		{"_id" : 1, "host" : "127.0.0.1:40002"},
		{"_id" : 2, "host" : "127.0.0.1:40003", "arbiterOnly" : true}
	]
};

rs.initiate(conf);
