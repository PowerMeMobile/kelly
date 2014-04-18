#!/bin/bash

PORT="8080"
HOST="127.0.0.1"

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
#post "gateways/7dc235d0-c938-4b66-8f8c-c9037c7eace7/settings" "id=log_smpp_pdus&value=true"

#
# Connections (SMPPSim)
#
post "gateways/7dc235d0-c938-4b66-8f8c-c9037c7eace7/connections" "id=0&host=127.0.0.1&port=8001&bind_type=transmitter&system_id=smppclient1&password=password&system_type=SMPP&addr_ton=1&addr_npi=1&addr_range="

post "gateways/7dc235d0-c938-4b66-8f8c-c9037c7eace7/connections" "id=1&host=127.0.0.1&port=8001&bind_type=receiver&system_id=smppclient2&password=password&system_type=SMPP&addr_ton=1&addr_npi=1&addr_range="

#
# Providers
#
post "providers" 'id=0a89542c-5270-11e1-bf27-001d0947ec73&name=Provider1&gateway_id=7dc235d0-c938-4b66-8f8c-c9037c7eace7&bulk_gateway_id=7dc235d0-c938-4b66-8f8c-c9037c7eace7&receipts_supported=true&sms_add_points=0.0'

#
# Networks
#
post "networks" 'id=0456837f-e874-4b05-8e89-95ae20b897d2&name=Mobile%20Telesystems&country=Belarus&hex_code=&country_code=375&number_len=9&prefixes=292;295;296;297;298;33&gmt_diff=%2B2&dst=7,5,3;7,5,10&provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&is_home=true&sms_points=1.0&sms_mult_points=1.0'

post "networks" 'id=6dd0af45-7cdf-41aa-954b-cc368fe1968e&name=Velcom%20Mobile%20Digital%20Comm.&country=Belarus&hex_code=&country_code=375&number_len=9&prefixes=291;293;296;299;44&gmt_diff=%2B2&dst=7,5,3;7,5,10&provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&is_home=true&sms_points=1.0&sms_mult_points=1.0'

post "networks" 'id=793e7b47-b248-4c86-a26f-eadfc44f84e2&name=Life&country=Belarus&hex_code=&country_code=375&number_len=9&prefixes=251;252;253;254;255;256;257;258;259&gmt_diff=%2B2&dst=7,5,3;7,5,10&provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&is_home=true&sms_points=1.0&sms_mult_points=1.0'

#
# Network maps
#
post "network_maps" 'id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&name=Belarus&network_ids=0456837f-e874-4b05-8e89-95ae20b897d2;6dd0af45-7cdf-41aa-954b-cc368fe1968e;793e7b47-b248-4c86-a26f-eadfc44f84e2'

#
# Customers
#

#
# Kannel

# postpaid customer
post "customers" 'customer_uuid=feda5822-5271-11e1-bd27-001d0947ec73&customer_id=fun-postpaid&name=funnel-postpaid&&allowed_sources=375296660001,1,1&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&default_validity=000003000000000R&max_validity=259200&default_provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&default_source=375296660001,1,1&pay_type=postpaid&credit=10000.0&credit_limit=10000.0&state=1'

# Funnel/Kannel users
FKTARGET="customers/feda5822-5271-11e1-bd27-001d0947ec73/users"
post $FKTARGET 'id=user&password=password&connection_types=transmitter;receiver;transceiver'
post $FKTARGET 'id=user2&password=password&connection_types=transmitter;receiver;transceiver'
post $FKTARGET 'id=user3&password=password&connection_types=transmitter;receiver;transceiver'
post $FKTARGET 'id=user4&password=password&connection_types=transmitter;receiver;transceiver'
post $FKTARGET 'id=user5&password=password&connection_types=transmitter;receiver;transceiver'

# prepaid customer
post "customers" 'customer_uuid=6bd667ae-1793-11e2-95fe-00269e42f7a5&customer_id=fun-prepaid&name=funnel-prepaid&allowed_sources=375296660002,1,1&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&default_validity=000003000000000R&max_validity=259200&default_provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&default_source=375296660002,1,1&pay_type=prepaid&credit=10000.0&credit_limit=10000.0&state=1'

# Funnel/Kannel's users
FKTARGET="customers/6bd667ae-1793-11e2-95fe-00269e42f7a5/users"
post $FKTARGET 'id=user&password=password&connection_types=transmitter;receiver;transceiver'
post $FKTARGET 'id=user2&password=password&connection_types=transmitter;receiver;transceiver'
post $FKTARGET 'id=user3&password=password&connection_types=transmitter;receiver;transceiver'
post $FKTARGET 'id=user4&password=password&connection_types=transmitter;receiver;transceiver'
post $FKTARGET 'id=user5&password=password&connection_types=transmitter;receiver;transceiver'

#
# OneAPI

# postpaid customer
post "customers" 'customer_uuid=a3ddc34a-1793-11e2-9602-00269e42f7a5&customer_id=oneapi-postpaid&name=oneapi-postpaid&allowed_sources=375296660003,1,1&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&default_validity=000003000000000R&max_validity=259200&default_provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&default_source=375296660003,1,1&pay_type=postpaid&credit=10000.0&credit_limit=10000.0&state=1'

post "customers/a3ddc34a-1793-11e2-9602-00269e42f7a5/users" 'id=user&password=password&connection_types=oneapi'

# prepaid customer
post "customers" 'customer_uuid=16f87550-1794-11e2-ade6-00269e42f7a5&customer_id=oneapi-prepaid&name=oneapi-prepaid&allowed_sources=375296660004,1,1&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&default_validity=000003000000000R&max_validity=259200&default_provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&default_source=375296660004,1,1&pay_type=prepaid&credit=10000.0&credit_limit=10000.0&state=1'

post "customers/16f87550-1794-11e2-ade6-00269e42f7a5/users" 'id=user&password=password&connection_types=oneapi'

#
# Soap

# postpaid customer
post "customers" 'customer_uuid=c173786e-63ce-11e2-8740-001d0947ec73&customer_id=soap-postpaid&name=soap-postpaid&allowed_sources=999,6,0&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&default_validity=000003000000000R&max_validity=259200&default_provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&default_source=999,6,0&pay_type=postpaid&credit=10000.0&credit_limit=10000.0&state=1'

post "customers/c173786e-63ce-11e2-8740-001d0947ec73/users" 'id=user&password=password&connection_types=soap'

#
# MM-client

# postpaid customer
post "customers" 'customer_uuid=8032706a-b4ec-11e3-b3d7-00269e42f7a5&customer_id=1&name=mm-postpaid&allowed_sources=375296660003,1,1&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&default_validity=000003000000000R&max_validity=259200&default_provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&default_source=375296660004,1,1&pay_type=postpaid&credit=10000.0&credit_limit=10000.0&state=1'

post "customers/8032706a-b4ec-11e3-b3d7-00269e42f7a5/users" "id=user&password=password&connection_types=mm"

#
# smppload

# postpaid customer
post "customers" 'customer_uuid=493b3678-9dc8-11e2-8cce-00269e42f7a5&customer_id=&name=smppload-postpaid&allowed_sources=375296660002,1,1&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&default_validity=000003000000000R&max_validity=259200&default_provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&default_source=375296660002,1,1&pay_type=postpaid&credit=10000.0&credit_limit=10000.0&state=1'

post "customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/users" "id=user&password=password&connection_types=transmitter;receiver;transceiver"
post "customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/users" "id=user2&password=password&connection_types=transmitter;receiver;transceiver"
post "customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/users" "id=user3&password=password&connection_types=transmitter;receiver;transceiver"
post "customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/users" "id=user4&password=password&connection_types=transmitter;receiver;transceiver"
post "customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/users" "id=user5&password=password&connection_types=transmitter;receiver;transceiver"

# prepaid customer
post "customers" "customer_uuid=50cec0fa-ea33-11e2-8cb1-00269e42f7a5&customer_id=prepaid&name=smppload-prepaid&allowed_sources=375296660002,1,1&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&default_validity=000003000000000R&max_validity=259200&default_provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&default_source=375296660002,1,1&pay_type=prepaid&credit=10000.0&credit_limit=10000.0&state=1"

post "customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/users" "id=user&password=password&connection_types=transmitter;receiver;transceiver"
post "customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/users" "id=user2&password=password&connection_types=transmitter;receiver;transceiver"
post "customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/users" "id=user3&password=password&connection_types=transmitter;receiver;transceiver"
post "customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/users" "id=user4&password=password&connection_types=transmitter;receiver;transceiver"
post "customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/users" "id=user5&password=password&connection_types=transmitter;receiver;transceiver"


#
# Addr2Cust
#

post "addr2cust" 'msisdn=375296660001,1,1&customer=feda5822-5271-11e1-bd27-001d0947ec73&user=user'
post "addr2cust" 'msisdn=375296660002,1,1&customer=6bd667ae-1793-11e2-95fe-00269e42f7a5&user=undefined'
# oneapi postpaid customer
post "addr2cust" 'msisdn=375296660003,1,1&customer=a3ddc34a-1793-11e2-9602-00269e42f7a5&user=undefined'
# oneapi prepaid customer
post "addr2cust" 'msisdn=375296660004,1,1&customer=16f87550-1794-11e2-ade6-00269e42f7a5&user=undefined'
# soap postpaid customer
post "addr2cust" 'msisdn=375296660005,1,1&customer=c173786e-63ce-11e2-8740-001d0947ec73&user=undefined'

exit 0
