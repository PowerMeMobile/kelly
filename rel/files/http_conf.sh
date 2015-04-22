#!/bin/bash

KELLY_HOST=127.0.0.1
KELLY_PORT=8080

SMPPSIM_HOST=127.0.0.1
SMPPSIM_PORT=8001

SMPPSINK_HOST=127.0.0.1
SMPPSINK_PORT=8002

###
# Be aware:
# Customer's `default_validity' in time format, described in SMPP specification v3.4
###

post() {
    TARGET=$1
    BODY=$2
    echo -n "Creating $TARGET..."
    CODE=`curl -s -D - -X "POST" http://$KELLY_HOST:$KELLY_PORT/$TARGET -d $BODY -l -o /dev/null | grep -i http | awk '{ print $2 }'`
    if [ "$CODE" != "201" ]; then
        echo -e "\nUnexpected return code [$TARGET:$BODY:$CODE]"
        exit 1
    fi
    echo "OK"
}

#
# Gateways
#

post gateways "id=7dc235d0-c938-4b66-8f8c-c9037c7eace7&name=smppsim&rps=1000"

# set gateway's settings
#post "gateways/7dc235d0-c938-4b66-8f8c-c9037c7eace7/settings" "name=log_smpp_pdus&value=true"

post gateways "id=b4040248-abca-4dca-a9d4-987894753975&name=smppsink&rps=1000"

#
# Connections
#

# smppsim
post "gateways/7dc235d0-c938-4b66-8f8c-c9037c7eace7/connections" "id=1&host=$SMPPSIM_HOST&port=$SMPPSIM_PORT&bind_type=transmitter&system_id=smppclient1&password=password&system_type=smpp&addr_ton=1&addr_npi=1&addr_range="

post "gateways/7dc235d0-c938-4b66-8f8c-c9037c7eace7/connections" "id=2&host=$SMPPSIM_HOST&port=$SMPPSIM_PORT&bind_type=receiver&system_id=smppclient1&password=password&system_type=smpp&addr_ton=1&addr_npi=1&addr_range="

# smppsink
post "gateways/b4040248-abca-4dca-a9d4-987894753975/connections" "id=1&host=$SMPPSINK_HOST&port=$SMPPSINK_PORT&bind_type=transceiver&system_id=smppclient1&password=password&system_type=smpp&addr_ton=1&addr_npi=1&addr_range="

#
# Providers
#

# smppsim
post "providers" "id=0a89542c-5270-11e1-bf27-001d0947ec73&name=smppsim&description=&gateway_id=7dc235d0-c938-4b66-8f8c-c9037c7eace7&bulk_gateway_id=7dc235d0-c938-4b66-8f8c-c9037c7eace7&receipts_supported=true&sms_add_points=0.0"

# smppsink
post "providers" "id=25081bd8-15c7-4956-ad96-d52334ea028a&name=smppsink&description=&gateway_id=b4040248-abca-4dca-a9d4-987894753975&bulk_gateway_id=b4040248-abca-4dca-a9d4-987894753975&receipts_supported=true&sms_add_points=0.0"

#
# Networks
#

# smppsim
post "networks" "id=6dd0af45-7cdf-41aa-954b-cc368fe1968e&name=Velcom%20Mobile%20Digital%20Comm.&country=Belarus&hex_code=&country_code=375&number_len=9&prefixes=291;293;296;299;44&gmt_diff=%2B3&dst=&provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&is_home=true&sms_points=1.0&sms_mult_points=1.0"

post "networks" "id=0456837f-e874-4b05-8e89-95ae20b897d2&name=Mobile%20Telesystems&country=Belarus&hex_code=&country_code=375&number_len=9&prefixes=292;295;297;298;33&gmt_diff=%2B3&dst=&provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&is_home=false&sms_points=2.0&sms_mult_points=1.0"

post "networks" "id=793e7b47-b248-4c86-a26f-eadfc44f84e2&name=Life&country=Belarus&hex_code=&country_code=375&number_len=9&prefixes=251;252;253;254;255;256;257;258;259&gmt_diff=%2B3&dst=&provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&is_home=false&sms_points=3.0&sms_mult_points=1.0"

# smppsink
post "networks" "id=3ef3529f-7d37-4285-9259-8d78101c8f14&name=Sink&country=Sink&hex_code=&country_code=999&number_len=9&prefixes=296&gmt_diff=&dst=&provider_id=25081bd8-15c7-4956-ad96-d52334ea028a&is_home=false&sms_points=1.0&sms_mult_points=1.0"

#
# Network maps
#
post "network_maps" "id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&name=Belarus&network_ids=0456837f-e874-4b05-8e89-95ae20b897d2;6dd0af45-7cdf-41aa-954b-cc368fe1968e;793e7b47-b248-4c86-a26f-eadfc44f84e2;3ef3529f-7d37-4285-9259-8d78101c8f14"

#
# Customers
#

#
# OneAPI

# postpaid customer
post "customers" 'customer_uuid=a3ddc34a-1793-11e2-9602-00269e42f7a5&customer_id=oneapi-postpaid&name=oneapi-postpaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&pay_type=postpaid&credit=10000.0&credit_limit=10000.0&language=en&state=active'

# postpaid originators
post "customers/a3ddc34a-1793-11e2-9602-00269e42f7a5/originators" 'id=ede7b0a0-f2d3-11e3-aec1-00269e42f7a5&address=375296660003,1,1&description=&is_default=true&state=approved'

# postpaid users
post "customers/a3ddc34a-1793-11e2-9602-00269e42f7a5/users" 'id=user&password=password&connection_types=oneapi&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active'

# prepaid customer
post "customers" 'customer_uuid=16f87550-1794-11e2-ade6-00269e42f7a5&customer_id=oneapi-prepaid&name=oneapi-prepaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&pay_type=prepaid&credit=10000.0&credit_limit=0.0&language=en&state=active'

# prepaid originators
post "customers/16f87550-1794-11e2-ade6-00269e42f7a5/originators" 'id=5f3a9330-f2d4-11e3-950a-00269e42f7a5&address=375296660004,1,1&description=&is_default=true&state=approved'

# prepaid users
post "customers/16f87550-1794-11e2-ade6-00269e42f7a5/users" 'id=user&password=password&connection_types=oneapi&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active'

#
# Soap

# postpaid customer
post "customers" 'customer_uuid=c173786e-63ce-11e2-8740-001d0947ec73&customer_id=10003&name=soap-postpaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&pay_type=postpaid&credit=10000.0&credit_limit=10000.0&language=en&state=active'

# postpaid originators
post "customers/c173786e-63ce-11e2-8740-001d0947ec73/originators" 'id=8cb7aa6e-f2d4-11e3-9a73-00269e42f7a5&address=999,6,0&description=&is_default=true&state=approved'
post "customers/c173786e-63ce-11e2-8740-001d0947ec73/originators" 'id=e1783f42-11b9-11e4-9166-0015588e6deb&address=SMS,5,0&description=&is_default=false&state=approved'

# postpaid users
post "customers/c173786e-63ce-11e2-8740-001d0947ec73/users" 'id=user&password=password&connection_types=soap&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active'

# prepaid customer
post "customers" 'customer_uuid=f9251298-381e-49c4-a60d-ff51e66c4f1c&customer_id=10004&name=soap-prepaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&pay_type=prepaid&credit=10000.0&credit_limit=0.0&language=en&state=active'

# prepaid originators
post "customers/f9251298-381e-49c4-a60d-ff51e66c4f1c/originators" 'id=5220e2fa-c794-4387-ad92-29d4243a6e29&address=999,6,0&description=&is_default=true&state=approved'
post "customers/f9251298-381e-49c4-a60d-ff51e66c4f1c/originators" 'id=f67fe174-11b9-11e4-a395-0015588e6deb&address=SMS,5,0&description=&is_default=false&state=approved'

# prepaid users
post "customers/f9251298-381e-49c4-a60d-ff51e66c4f1c/users" 'id=user&password=password&connection_types=soap&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active'

#
# MM-client

# postpaid customer
post "customers" 'customer_uuid=8032706a-b4ec-11e3-b3d7-00269e42f7a5&customer_id=10001&name=mm-postpaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&pay_type=postpaid&credit=10000.0&credit_limit=10000.0&language=en&state=active'

# postpaid originators
post "customers/8032706a-b4ec-11e3-b3d7-00269e42f7a5/originators" 'id=d8372096-f2d4-11e3-950a-00269e42f7a5&address=375296660004,1,1&description=&is_default=true&state=approved'
post "customers/8032706a-b4ec-11e3-b3d7-00269e42f7a5/originators" 'id=1165be86-f2d5-11e3-950a-00269e42f7a5&address=AlfaOrig,5,0&description=&is_default=false&state=approved'

# postpaid users
post "customers/8032706a-b4ec-11e3-b3d7-00269e42f7a5/users" "id=user&password=password&connection_types=mm&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active"

# prepaid customer
post "customers" 'customer_uuid=77005b1e-d84b-4053-8917-fe91a19eb35d&customer_id=10002&name=mm-prepaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&pay_type=prepaid&credit=10000.0&credit_limit=0.0&language=en&state=active'

# prepaid originators
post "customers/77005b1e-d84b-4053-8917-fe91a19eb35d/originators" 'id=d0787e41-0bbd-4ec3-a2b9-318c7fd92373&address=375296660004,1,1&description=&is_default=true&state=approved'
post "customers/77005b1e-d84b-4053-8917-fe91a19eb35d/originators" 'id=64f5e633-c94f-4aef-a57b-153622d2bc6e&address=AlfaOrig,5,0&description=&is_default=false&state=approved'

# prepaid users
post "customers/77005b1e-d84b-4053-8917-fe91a19eb35d/users" 'id=user&password=password&connection_types=mm&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active'

#
# smppload

# postpaid customer
post "customers" 'customer_uuid=493b3678-9dc8-11e2-8cce-00269e42f7a5&customer_id=&name=smppload-postpaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&pay_type=postpaid&credit=10000.0&credit_limit=10000.0&language=en&state=active'

# postpaid originators
post "customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/originators" 'id=c71175cc-f091-11e3-8ee4-00269e42f7a5&address=375296660001,1,1&description=&is_default=true&state=approved'
post "customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/originators" 'id=cfefe958-f091-11e3-8ee4-00269e42f7a5&address=375296660002,1,1&description=&is_default=false&state=approved'

# postpaid users
post "customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/users" "id=user&password=password&connection_types=transmitter;receiver;transceiver&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active"
post "customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/users" "id=user2&password=password&connection_types=transmitter;receiver;transceiver&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active"
post "customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/users" "id=user3&password=password&connection_types=transmitter;receiver;transceiver&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active"
post "customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/users" "id=user4&password=password&connection_types=transmitter;receiver;transceiver&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active"
post "customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/users" "id=user5&password=password&connection_types=transmitter;receiver;transceiver&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active"

# prepaid customer
post "customers" "customer_uuid=50cec0fa-ea33-11e2-8cb1-00269e42f7a5&customer_id=prepaid&name=smppload-prepaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&pay_type=prepaid&credit=10000.0&credit_limit=0.0&language=en&state=active"

# prepaid originators
post "customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/originators" 'id=e3bcab38-51ac-499b-a7a6-79b86300cf0c&address=375296660001,1,1&description=&is_default=true&state=approved'
post "customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/originators" 'id=a1f1b0e0-f2d5-11e3-950a-00269e42f7a5&address=375296660002,1,1&description=&is_default=true&state=approved'

# prepaid users
post "customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/users" "id=user&password=password&connection_types=transmitter;receiver;transceiver&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active"
post "customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/users" "id=user2&password=password&connection_types=transmitter;receiver;transceiver&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active"
post "customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/users" "id=user3&password=password&connection_types=transmitter;receiver;transceiver&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active"
post "customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/users" "id=user4&password=password&connection_types=transmitter;receiver;transceiver&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active"
post "customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/users" "id=user5&password=password&connection_types=transmitter;receiver;transceiver&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active"

#
# Addr2Cust
#

# smppload postpaid
post "addr2cust" 'msisdn=375296660001,1,1&customer=493b3678-9dc8-11e2-8cce-00269e42f7a5&user=user'
# smppload prepaid
post "addr2cust" 'msisdn=375296660002,1,1&customer=50cec0fa-ea33-11e2-8cb1-00269e42f7a&user=user'
# oneapi postpaid
post "addr2cust" 'msisdn=375296660003,1,1&customer=a3ddc34a-1793-11e2-9602-00269e42f7a5&user=user'
# oneapi prepaid
post "addr2cust" 'msisdn=375296660004,1,1&customer=16f87550-1794-11e2-ade6-00269e42f7a5&user=user'
# soap postpaid
post "addr2cust" 'msisdn=375296660005,1,1&customer=c173786e-63ce-11e2-8740-001d0947ec73&user=user'
# mm postpaid
post "addr2cust" 'msisdn=375296660007,1,1&customer=8032706a-b4ec-11e3-b3d7-00269e42f7a5&user=user'

exit 0
