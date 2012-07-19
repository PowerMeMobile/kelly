#!/bin/bash

Today=`date +%Y-%m-%d`
From="${Today}T00:00"
To="${Today}T23:59"

#
# Statistical interface for statuses
#

# All
curl "http://localhost:8080/report/status?from=${From}&to=${To}"
echo; echo

# By status
curl "http://localhost:8080/report/status?from=${From}&to=${To}&status=delivered"
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
# Statistical interface for Messages
#
curl "http://localhost:8080/report/messages/details?from=${From}&to=${To}&slice_length=M60"
echo; echo

#
# Statistical interface for Customers, Networks, Gateways
#

# Customers
curl "http://localhost:8080/report/messages/customers?from=${From}&to=${To}"
echo; echo

# Networks
curl "http://localhost:8080/report/messages/networks?from=${From}&to=${To}"
echo; echo

# Gateways

curl "http://localhost:8080/report/gateways?from=${From}&to=${To}"
echo; echo

#
# Message status request.
#
curl "http://localhost:8080/message_status/83986/customer/feda5822-5271-11e1-bd27-001d0947ec73"
echo; echo
