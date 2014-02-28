// !!! Do not use 127.0.0.1 for remote replicaset
// !!! The remote app will fail to connect to the replicaset
conf = {
    "_id" : "alley",
    "members" : [
        {"_id" : 0, "host" : "127.0.0.1:40001"},
        {"_id" : 1, "host" : "127.0.0.1:40002"},
        {"_id" : 2, "host" : "127.0.0.1:40003", "arbiterOnly" : true}
    ]
};

rs.initiate(conf);
