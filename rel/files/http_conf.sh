#!/bin/bash

PORT="8080"
HOST="127.0.0.1"

# SMPPSim
OUTPUT_SMPP_HOST=127.0.0.1
OUTPUT_SMPP_PORT=8001
# SMS Collider
#OUTPUT_SMPP_HOST=127.0.1.1
#OUTPUT_SMPP_PORT=2775

###
# Be aware:
# 1. Customer's `priority' and `rps' settings are disabled, due to http://extranet.powermemobile.com/issues/17465
# 2. Customer's `default_validity' in time format, described in SMPP specification v3.4
###

post(){
    TARGET=$1
    BODY=$2
    echo -n "Creating $TARGET..."
    CODE=`curl -s -D - -X "POST" http://$HOST:$PORT/$TARGET -d $BODY -l -o /dev/null | grep -i http | awk '{ print $2 }'`
    if [ "$CODE" != "201" ]; then
        echo -e "\nUnexpected return code [$TARGET:$BODY:$CODE]"
        exit 1
    fi
    echo "OK"
}

#
# Gateways
#

post gateways "id=7dc235d0-c938-4b66-8f8c-c9037c7eace7&name=SMPPSim&rps=10000"

# set gateway's settings
#curl -s -D - -X "POST" http://localhost:8080/gateways/7dc235d0-c938-4b66-8f8c-c9037c7eace7/settings -d 'id=log_smpp_pdus&value=true'


#
# Connections (SMPPSim)
#
post "gateways/7dc235d0-c938-4b66-8f8c-c9037c7eace7/connections" "id=0&host=$OUTPUT_SMPP_HOST&port=$OUTPUT_SMPP_PORT&bind_type=transmitter&system_id=smppclient1&password=password&system_type=SMPP&addr_ton=1&addr_npi=1&addr_range="

post "gateways/7dc235d0-c938-4b66-8f8c-c9037c7eace7/connections" "id=1&host=$OUTPUT_SMPP_HOST&port=$OUTPUT_SMPP_PORT&bind_type=receiver&system_id=smppclient2&password=password&system_type=SMPP&addr_ton=1&addr_npi=1&addr_range="


#
# Providers
#
post "providers" 'id=0a89542c-5270-11e1-bf27-001d0947ec73&name=Provider1&gateway_id=7dc235d0-c938-4b66-8f8c-c9037c7eace7&bulk_gateway_id=7dc235d0-c938-4b66-8f8c-c9037c7eace7&receipts_supported=true&sms_add_credits=0'

#
# Networks
#
post "networks" 'id=920a009a-5270-11e1-b961-001d0947ec73&name=Network1&country_code=375&numbers_len=12&prefixes=29;33;44;25&provider_id=0a89542c-5270-11e1-bf27-001d0947ec73'

#
# Customers
#

#
# Funnel/Kannel

# postpaid customer
post "customers" 'customer_uuid=feda5822-5271-11e1-bd27-001d0947ec73&customer_id=fun-postpaid&name=funnel-postpaid&&originators=375296660001,1,1&networks=920a009a-5270-11e1-b961-001d0947ec73&receipts_allowed=true&default_validity=000003000000000R&max_validity=259200&default_provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&default_originator=375296660001,1,1&pay_type=postpaid&state=1'

# Funnel/Kannel users
FKTARGET="customers/feda5822-5271-11e1-bd27-001d0947ec73/users"
post $FKTARGET 'id=user&password=password&connection_types=transmitter;receiver;transceiver'
post $FKTARGET 'id=user2&password=password&connection_types=transmitter;receiver;transceiver'
post $FKTARGET 'id=user3&password=password&connection_types=transmitter;receiver;transceiver'
post $FKTARGET 'id=user4&password=password&connection_types=transmitter;receiver;transceiver'
post $FKTARGET 'id=user5&password=password&connection_types=transmitter;receiver;transceiver'

# prepaid customer
post "customers" 'customer_uuid=6bd667ae-1793-11e2-95fe-00269e42f7a5&customer_id=fun-prepaid&name=funnel-prepaid&originators=375296660002,1,1&networks=920a009a-5270-11e1-b961-001d0947ec73&receipts_allowed=true&default_validity=000003000000000R&max_validity=259200&default_provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&default_originator=375296660002,1,1&pay_type=prepaid&state=1'

# Funnel/Kannel's users
FKTARGET="customers/6bd667ae-1793-11e2-95fe-00269e42f7a5/users"
post $FKTARGET 'id=user&password=password&connection_types=transmitter;receiver;transceiver'
post $FKTARGET 'id=user2&password=password&connection_types=transmitter;receiver;transceiver'
post $FKTARGET 'id=user3&password=password&connection_types=transmitter;receiver;transceiver'
post $FKTARGET 'id=user4&password=password&connection_types=transmitter;receiver;transceiver'
post $FKTARGET 'id=user5&password=password&connection_types=transmitter;receiver;transceiver'

#
# OneAPI
#

# postpaid customer
post "customers" 'customer_uuid=a3ddc34a-1793-11e2-9602-00269e42f7a5&customer_id=oneapi-postpaid&name=oneapi-postpaid&originators=375296660003,1,1&networks=920a009a-5270-11e1-b961-001d0947ec73&receipts_allowed=true&default_validity=000003000000000R&max_validity=259200&default_provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&default_originator=375296660003,1,1&pay_type=postpaid&state=1'

post "customers/a3ddc34a-1793-11e2-9602-00269e42f7a5/users" 'id=user&password=password&connection_types=transmitter;receiver;transceiver;oneapi'

# prepaid customer
post "customers" 'customer_uuid=16f87550-1794-11e2-ade6-00269e42f7a5&customer_id=oneapi-prepaid&name=oneapi-prepaid&originators=375296660004,1,1&networks=920a009a-5270-11e1-b961-001d0947ec73&receipts_allowed=true&default_validity=000003000000000R&max_validity=259200&default_provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&default_originator=375296660004,1,1&pay_type=prepaid&state=1'

post "customers/16f87550-1794-11e2-ade6-00269e42f7a5/users" 'id=user&password=password&connection_types=transmitter;receiver;transceiver;oneapi'

#
# Soap
#

# postpaid customer
post "customers" 'customer_uuid=c173786e-63ce-11e2-8740-001d0947ec73&customer_id=soap-postpaid&name=soap-postpaid&originators=999,6,0&networks=920a009a-5270-11e1-b961-001d0947ec73&receipts_allowed=true&default_validity=000003000000000R&max_validity=259200&default_provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&default_originator=999,6,0&pay_type=postpaid&state=1'

post "customers/c173786e-63ce-11e2-8740-001d0947ec73/users" 'id=user&password=password&connection_types=transmitter;receiver;transceiver;oneapi'

#
# Addr2Cust
#

post "addr2cust" 'msisdn=375296660001,1,1&customer=feda5822-5271-11e1-bd27-001d0947ec73&user=user'
post "addr2cust" 'msisdn=375296660002,1,1&customer=6bd667ae-1793-11e2-95fe-00269e42f7a5&user=undefined'
# k1api postpaid customer
post "addr2cust" 'msisdn=375296660003,1,1&customer=a3ddc34a-1793-11e2-9602-00269e42f7a5&user=undefined'
# k1api prepaid customer
post "addr2cust" 'msisdn=375296660004,1,1&customer=16f87550-1794-11e2-ade6-00269e42f7a5&user=undefined'

exit 0
