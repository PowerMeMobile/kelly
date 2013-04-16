conf = {
	"_id" : "kelly",
	"members" : [
		{"_id" : 0, "host" : "10.10.0.222:27017"},
		{"_id" : 1, "host" : "10.10.0.223:27017"},
		{"_id" : 2, "host" : "10.10.0.86:27017"}
	],
	"settings" : {
		"getLastErrorDefaults" : {"w" : 2, "wtimeout" : 5000}
	}
};

rs.initiate(conf);
