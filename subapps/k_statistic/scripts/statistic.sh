#!/bin/bash

#
# Statistical interface for statuses
#

# All
curl "http://localhost:8080/report/status?from=2012-07-13T00:00&to=2012-07-13T23:59"
echo; echo

# By status
curl "http://localhost:8080/report/status?from=2012-07-13T00:00&to=2012-07-13T23:59&status=delivered"
echo; echo

#
# Statistical interface for uplink and downlink
#

# Uplink
curl "http://localhost:8080/report/uplink"
echo; echo

# Downlink
curl "http://localhost:8080/report/downlink"
echo; echo

#
# Statistical interface for Customers, Networks, Gateways
#

# Customers
curl "http://localhost:8080/report/1?from=2012-07-13T00:00&to=2012-07-13T23:59"
echo; echo

# Networks
curl "http://localhost:8080/report/2?from=2012-07-13T00:00&to=2012-07-13T23:59"
echo; echo

# Gateways

curl "http://localhost:8080/report/gateways?from=2012-07-13T00:00&to=2012-07-13T23:59"
echo; echo

#
# Message status request.
#
curl "http://localhost:8080/message_status/83774/customer/feda5822-5271-11e1-bd27-001d0947ec73"
echo; echo
