#!/bin/bash

Today=`date +%Y-%m-%d`
From="${Today}T00:00"
To="${Today}T23:59"

function make_request() {
	Url=$1
	echo Request:
	echo $Url
	echo Result:
	curl $Url
	echo; echo
}

#
# Statistical interface for statuses
#

# All
make_request "http://localhost:8080/report/statuses?from=${From}&to=${To}"

# By status
make_request "http://localhost:8080/report/statuses?from=${From}&to=${To}&status=delivered"

#
# Statistical interface for uplink and downlink
#

# Uplink
make_request "http://localhost:8080/report/uplink"

# Downlink
make_request "http://localhost:8080/report/downlink"

#
# Statistical interface for Messages
#
make_request "http://localhost:8080/report/messages/details?from=${From}&to=${To}&slice_length=M60"

#
# Statistical interface for Customers, Networks
#

# Customers
make_request "http://localhost:8080/report/messages/customers?from=${From}&to=${To}"

# Networks
make_request "http://localhost:8080/report/messages/networks?from=${From}&to=${To}"
