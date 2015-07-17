#!/bin/bash

set -o verbose

check() {
    read status
    echo ${status}

    while read header && [[ ${#header} != 1 ]]; do
        echo ${header}
    done
    echo

    read body
    echo ${body}

    code=`echo ${status} | awk '{ print $2 }'`
    if [[ "${code}" != "201" ]] && [[ "${code}" != "200" ]]; then
        echo -e "\e[31mFAIL\e[0m"
        echo
        return 1
    fi
    echo
    return 0
}

###
# Be aware:
# Customer's `default_validity' in time format, described in SMPP specification v3.4
###

#
# Gateways
#
curl -s -D - -X POST 127.0.0.1:8080/gateways -d "id=7dc235d0-c938-4b66-8f8c-c9037c7eace7&name=smppsim&rps=100" | check || exit 1

# set gateway's settings
#curl -s -D - -X POST 127.0.0.1:8080/gateways/7dc235d0-c938-4b66-8f8c-c9037c7eace7/settings -d "name=log_smpp_pdus&value=true"
#curl -s -D - -X POST 127.0.0.1:8080/gateways/7dc235d0-c938-4b66-8f8c-c9037c7eace7/settings -d "name=default_encoding&value=latin1"
#curl -s -D - -X POST 127.0.0.1:8080/gateways/7dc235d0-c938-4b66-8f8c-c9037c7eace7/settings -d "name=default_bitness&value=7"
#curl -s -D - -X DELETE 127.0.0.1:8080/gateways/7dc235d0-c938-4b66-8f8c-c9037c7eace7/settings/default_encoding

curl -s -D - -X POST 127.0.0.1:8080/gateways -d "id=b4040248-abca-4dca-a9d4-987894753975&name=smppsink&rps=100" | check || exit 1

#
# Connections
#

# smppsim
curl -s -D - -X POST 127.0.0.1:8080/gateways/7dc235d0-c938-4b66-8f8c-c9037c7eace7/connections -d "id=1&host=127.0.0.1&port=8001&bind_type=transmitter&system_id=smppclient1&password=password&system_type=smpp&addr_ton=1&addr_npi=1&addr_range=" | check || exit 1

curl -s -D - -X POST 127.0.0.1:8080/gateways/7dc235d0-c938-4b66-8f8c-c9037c7eace7/connections -d "id=2&host=127.0.0.1&port=8001&bind_type=receiver&system_id=smppclient1&password=password&system_type=smpp&addr_ton=1&addr_npi=1&addr_range=" | check || exit 1

# smppsink
curl -s -D - -X POST 127.0.0.1:8080/gateways/b4040248-abca-4dca-a9d4-987894753975/connections -d "id=1&host=localhost&port=8002&bind_type=transceiver&system_id=smppclient1&password=password&system_type=smpp&addr_ton=1&addr_npi=1&addr_range=" | check || exit 1

#
# Providers
#

# smppsim
curl -s -D - -X POST 127.0.0.1:8080/providers -d "id=0a89542c-5270-11e1-bf27-001d0947ec73&name=smppsim&description=&gateway_id=7dc235d0-c938-4b66-8f8c-c9037c7eace7&bulk_gateway_id=7dc235d0-c938-4b66-8f8c-c9037c7eace7&receipts_supported=true&sms_add_points=0.0" | check || exit 1

# smppsink
curl -s -D - -X POST 127.0.0.1:8080/providers -d "id=25081bd8-15c7-4956-ad96-d52334ea028a&name=smppsink&description=&gateway_id=b4040248-abca-4dca-a9d4-987894753975&bulk_gateway_id=b4040248-abca-4dca-a9d4-987894753975&receipts_supported=true&sms_add_points=0.0" | check || exit 1

#
# Networks
#

# smppsim
curl -s -D - -X POST 127.0.0.1:8080/networks -d "id=6dd0af45-7cdf-41aa-954b-cc368fe1968e&name=Velcom%20Mobile%20Digital%20Comm.&country=Belarus&hex_code=&country_code=375&number_len=9&prefixes=291;293;296;299;44&gmt_diff=%2B3&dst=&provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&is_home=true&sms_points=1.0&sms_mult_points=1.0" | check || exit 1

curl -s -D - -X POST 127.0.0.1:8080/networks -d "id=0456837f-e874-4b05-8e89-95ae20b897d2&name=Mobile%20Telesystems&country=Belarus&hex_code=&country_code=375&number_len=9&prefixes=292;295;297;298;33&gmt_diff=%2B3&dst=&provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&is_home=false&sms_points=2.0&sms_mult_points=1.0" | check || exit 1

curl -s -D - -X POST 127.0.0.1:8080/networks -d "id=793e7b47-b248-4c86-a26f-eadfc44f84e2&name=Life&country=Belarus&hex_code=&country_code=375&number_len=9&prefixes=251;252;253;254;255;256;257;258;259&gmt_diff=%2B3&dst=&provider_id=0a89542c-5270-11e1-bf27-001d0947ec73&is_home=false&sms_points=3.0&sms_mult_points=1.0" | check || exit 1

# smppsink
curl -s -D - -X POST 127.0.0.1:8080/networks -d "id=3ef3529f-7d37-4285-9259-8d78101c8f14&name=Sink&country=Sink&hex_code=&country_code=999&number_len=9&prefixes=296&gmt_diff=&dst=&provider_id=25081bd8-15c7-4956-ad96-d52334ea028a&is_home=false&sms_points=1.0&sms_mult_points=1.0" | check || exit 1

#
# Network maps
#
curl -s -D - -X POST 127.0.0.1:8080/network_maps -d "id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&name=Belarus&network_ids=0456837f-e874-4b05-8e89-95ae20b897d2;6dd0af45-7cdf-41aa-954b-cc368fe1968e;793e7b47-b248-4c86-a26f-eadfc44f84e2;3ef3529f-7d37-4285-9259-8d78101c8f14" | check || exit 1


#
# Customers
#

#
# Funnel

# postpaid customer
curl -s -D - -X POST 127.0.0.1:8080/v1/customers -d "customer_uuid=493b3678-9dc8-11e2-8cce-00269e42f7a5&customer_id=10001&name=funnel-postpaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&interfaces=transmitter;receiver;transceiver&features=inbox,true&pay_type=postpaid&credit=1000000000.0&credit_limit=10000.0&language=en&state=active" | check || exit 1

# postpaid originators
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/originators -d "id=c71175cc-f091-11e3-8ee4-00269e42f7a5&msisdn=375296660001,1,1&description=&is_default=true&state=approved" | check || exit 1

# postpaid users
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/users -d "user_id=user&password=password&interfaces=transmitter;receiver;transceiver&features=inbox,true&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/users -d "user_id=user2&password=password&interfaces=transmitter;receiver;transceiver&features=&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/users -d "user_id=user3&password=password&interfaces=transmitter;receiver;transceiver&features=&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/users -d "user_id=user4&password=password&interfaces=transmitter;receiver;transceiver&features=&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/users -d "user_id=user5&password=password&interfaces=transmitter;receiver;transceiver&features=&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1

# prepaid customer
curl -s -D - -X POST 127.0.0.1:8080/v1/customers -d "customer_uuid=50cec0fa-ea33-11e2-8cb1-00269e42f7a5&customer_id=10002&name=funnel-prepaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&interfaces=transmitter;receiver;transceiver&features=inbox,true&pay_type=prepaid&credit=10000.0&credit_limit=0.0&language=en&state=active" | check || exit 1

# prepaid originators
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/originators -d "id=a1f1b0e0-f2d5-11e3-950a-00269e42f7a5&msisdn=375296660002,1,1&description=&is_default=true&state=approved" | check || exit 1

# prepaid users
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/users -d "user_id=user&password=password&interfaces=transmitter;receiver;transceiver&features=inbox,true&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/users -d "user_id=user2&password=password&interfaces=transmitter;receiver;transceiver&features=&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/users -d "user_id=user3&password=password&interfaces=transmitter;receiver;transceiver&features=&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/users -d "user_id=user4&password=password&interfaces=transmitter;receiver;transceiver&features=&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/users -d "user_id=user5&password=password&interfaces=transmitter;receiver;transceiver&features=&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1

#
# SOAP

# postpaid customer
curl -s -D - -X POST 127.0.0.1:8080/v1/customers -d "customer_uuid=c173786e-63ce-11e2-8740-001d0947ec73&customer_id=10003&name=soap-postpaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&interfaces=soap&features=inbox,true&pay_type=postpaid&credit=10000.0&credit_limit=10000.0&language=en&state=active" | check || exit 1

# postpaid originators
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/c173786e-63ce-11e2-8740-001d0947ec73/originators -d "id=8cb7aa6e-f2d4-11e3-9a73-00269e42f7a5&msisdn=375296660003,1,1&description=&is_default=true&state=approved" | check || exit 1

# postpaid users
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/c173786e-63ce-11e2-8740-001d0947ec73/users -d "user_id=user&password=password&interfaces=soap&features=inbox,true&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/c173786e-63ce-11e2-8740-001d0947ec73/users -d "user_id=user_no_inbox&password=password&interfaces=soap&features=&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1

# prepaid customer
curl -s -D - -X POST 127.0.0.1:8080/v1/customers -d "customer_uuid=f9251298-381e-49c4-a60d-ff51e66c4f1c&customer_id=10004&name=soap-prepaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&interfaces=soap&features=inbox,true&pay_type=prepaid&credit=10000.0&credit_limit=0.0&language=en&state=active" | check || exit 1

# prepaid originators
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/f9251298-381e-49c4-a60d-ff51e66c4f1c/originators -d "id=5220e2fa-c794-4387-ad92-29d4243a6e29&msisdn=375296660004,1,1&description=&is_default=true&state=approved" | check || exit 1

# prepaid users
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/f9251298-381e-49c4-a60d-ff51e66c4f1c/users -d "user_id=user&password=password&interfaces=soap&features=&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1


#
# MM

# postpaid customer
curl -s -D - -X POST 127.0.0.1:8080/v1/customers -d "customer_uuid=8032706a-b4ec-11e3-b3d7-00269e42f7a5&customer_id=10005&name=mm-postpaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&interfaces=mm&features=inbox,true&pay_type=postpaid&credit=10000.0&credit_limit=10000.0&language=en&state=active" | check || exit 1

# postpaid originators
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/8032706a-b4ec-11e3-b3d7-00269e42f7a5/originators -d "id=d8372096-f2d4-11e3-950a-00269e42f7a5&msisdn=375296660005,1,1&description=&is_default=true&state=approved" | check || exit 1

# postpaid users
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/8032706a-b4ec-11e3-b3d7-00269e42f7a5/users -d "user_id=user&password=password&interfaces=mm&features=inbox,true&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/8032706a-b4ec-11e3-b3d7-00269e42f7a5/users -d "user_id=user_no_inbox&password=password&interfaces=mm&features=&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1

# prepaid customer
curl -s -D - -X POST 127.0.0.1:8080/v1/customers -d "customer_uuid=77005b1e-d84b-4053-8917-fe91a19eb35d&customer_id=10006&name=mm-prepaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&interfaces=mm&features=inbox,true&pay_type=prepaid&credit=10000.0&credit_limit=0.0&language=en&state=active" | check || exit 1

# prepaid originators
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/77005b1e-d84b-4053-8917-fe91a19eb35d/originators -d "id=d0787e41-0bbd-4ec3-a2b9-318c7fd92373&msisdn=375296660006,1,1&description=&is_default=true&state=approved" | check || exit 1

# prepaid users
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/77005b1e-d84b-4053-8917-fe91a19eb35d/users -d "user_id=user&password=password&interfaces=mm&features=&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1

#
# OneAPI

# postpaid customer
curl -s -D - -X POST 127.0.0.1:8080/v1/customers -d "customer_uuid=a3ddc34a-1793-11e2-9602-00269e42f7a5&customer_id=10007&name=oneapi-postpaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&interfaces=oneapi&features=inbox,true&pay_type=postpaid&credit=10000.0&credit_limit=10000.0&language=en&state=active" | check || exit 1

# postpaid originators
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/a3ddc34a-1793-11e2-9602-00269e42f7a5/originators -d "id=ede7b0a0-f2d3-11e3-aec1-00269e42f7a5&msisdn=375296660007,1,1&description=&is_default=true&state=approved" | check || exit 1

# postpaid users
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/a3ddc34a-1793-11e2-9602-00269e42f7a5/users -d "user_id=user&password=password&interfaces=oneapi&features=inbox,true&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1

# prepaid customer
curl -s -D - -X POST 127.0.0.1:8080/v1/customers -d "customer_uuid=16f87550-1794-11e2-ade6-00269e42f7a5&customer_id=10008&name=oneapi-prepaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&interfaces=oneapi&features=inbox,true&pay_type=prepaid&credit=10000.0&credit_limit=0.0&language=en&state=active" | check || exit 1

# prepaid originators
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/16f87550-1794-11e2-ade6-00269e42f7a5/originators -d "id=5f3a9330-f2d4-11e3-950a-00269e42f7a5&msisdn=375296660008,1,1&description=&is_default=true&state=approved" | check || exit 1

# prepaid users
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/16f87550-1794-11e2-ade6-00269e42f7a5/users -d "user_id=user&password=password&interfaces=oneapi&features=inbox,true&mobile_phone=&first_name=&last_name=&company=&occupation=&email=&country=&language=en&state=active" | check || exit 1

#
# Email

# postpaid customer
curl -s -D - -X POST 127.0.0.1:8080/v1/customers -d "customer_uuid=b5801eb1-d5ed-4a6e-80b4-a9bc35bdb3e9&customer_id=10009&name=email-postpaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&interfaces=email&features=inbox,true&pay_type=postpaid&credit=10000.0&credit_limit=10000.0&language=en&state=active"

# postpaid originators
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/b5801eb1-d5ed-4a6e-80b4-a9bc35bdb3e9/originators -d "id=eca649a3-fb77-4800-ba5a-0c7c1c14c9fb&msisdn=FromEmail,5,0&description=&is_default=true&state=approved"

# postpaid users
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/b5801eb1-d5ed-4a6e-80b4-a9bc35bdb3e9/users -d "user_id=user&password=password&interfaces=email&features=inbox,true;sms_from_email,true&mobile_phone=375296660009&first_name=&last_name=&company=&occupation=&email=email-postpaid@mail.com&country=&language=en&state=active"
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/b5801eb1-d5ed-4a6e-80b4-a9bc35bdb3e9/users -d "user_id=user_no_sms_from_email&password=password&interfaces=email&features=sms_from_email,false&mobile_phone=375296660019&first_name=&last_name=&company=&occupation=&email=user_no_sms_from_email-postpaid@mail.com&country=&language=en&state=active"

# prepaid customer
curl -s -D - -X POST 127.0.0.1:8080/v1/customers -d "customer_uuid=01a2d05d-fd2d-4532-847c-16681302101e&customer_id=10010&name=email-prepaid&priority=1&rps=1000&network_map_id=c51a94bf-618a-48a4-90bf-7508e3d93b5d&receipts_allowed=true&no_retry=false&default_validity=000003000000000R&max_validity=259200&default_provider_id=&interfaces=email&features=inbox,true&pay_type=prepaid&credit=10000.0&credit_limit=0.0&language=en&state=active"

# prepaid originators
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/01a2d05d-fd2d-4532-847c-16681302101e/originators -d "id=192ebc79-0beb-4e51-be90-548bb1340b66&msisdn=FromEmail,5,0&description=&is_default=true&state=approved"

# prepaid users
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/01a2d05d-fd2d-4532-847c-16681302101e/users -d "user_id=user&password=password&interfaces=email&features=inbox,true;sms_from_email,true&mobile_phone=375296660010&first_name=&last_name=&company=&occupation=&email=email-prepaid@mail.com&country=&language=en&state=active"

#
# MSISDNs Pool
#

curl -s -D - -X POST 127.0.0.1:8080/v1/msisdns -d "msisdn=0011,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/msisdns -d "msisdn=0021,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/msisdns -d "msisdn=0031,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/msisdns -d "msisdn=0041,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/msisdns -d "msisdn=0051,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/msisdns -d "msisdn=0061,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/msisdns -d "msisdn=0071,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/msisdns -d "msisdn=0081,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/msisdns -d "msisdn=0091,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/msisdns -d "msisdn=0101,6,0" | check || exit 1

# funnel postpaid
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/msisdns -d "msisdn=0011,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/493b3678-9dc8-11e2-8cce-00269e42f7a5/users/user/msisdns -d "msisdn=0011,6,0" | check || exit 1
# funnel prepaid
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/msisdns -d "msisdn=0021,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/50cec0fa-ea33-11e2-8cb1-00269e42f7a5/users/user/msisdns -d "msisdn=0021,6,0" | check || exit 1
# soap postpaid
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/c173786e-63ce-11e2-8740-001d0947ec73/msisdns -d "msisdn=0031,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/c173786e-63ce-11e2-8740-001d0947ec73/users/user/msisdns -d "msisdn=0031,6,0" | check || exit 1
# soap prepaid
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/f9251298-381e-49c4-a60d-ff51e66c4f1c/msisdns -d "msisdn=0041,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/f9251298-381e-49c4-a60d-ff51e66c4f1c/users/user/msisdns -d "msisdn=0041,6,0" | check || exit 1
# mm postpaid
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/8032706a-b4ec-11e3-b3d7-00269e42f7a5/msisdns -d "msisdn=0051,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/8032706a-b4ec-11e3-b3d7-00269e42f7a5/users/user/msisdns -d "msisdn=0051,6,0" | check || exit 1
# mm prepaid
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/77005b1e-d84b-4053-8917-fe91a19eb35d/msisdns -d "msisdn=0061,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/77005b1e-d84b-4053-8917-fe91a19eb35d/users/user/msisdns -d "msisdn=0061,6,0" | check || exit 1
# oneapi postpaid
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/a3ddc34a-1793-11e2-9602-00269e42f7a5/msisdns -d "msisdn=0071,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/a3ddc34a-1793-11e2-9602-00269e42f7a5/users/user/msisdns -d "msisdn=0071,6,0" | check || exit 1
# oneapi prepaid
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/16f87550-1794-11e2-ade6-00269e42f7a5/msisdns -d "msisdn=0081,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/16f87550-1794-11e2-ade6-00269e42f7a5/users/user/msisdns -d "msisdn=0081,6,0" | check || exit 1
# email postpaid
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/b5801eb1-d5ed-4a6e-80b4-a9bc35bdb3e9/msisdns -d "msisdn=0091,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/b5801eb1-d5ed-4a6e-80b4-a9bc35bdb3e9/users/user/msisdns -d "msisdn=0091,6,0" | check || exit 1
# email prepaid
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/01a2d05d-fd2d-4532-847c-16681302101e/msisdns -d "msisdn=0101,6,0" | check || exit 1
curl -s -D - -X POST 127.0.0.1:8080/v1/customers/01a2d05d-fd2d-4532-847c-16681302101e/users/user/msisdns -d "msisdn=0101,6,0" | check || exit 1
