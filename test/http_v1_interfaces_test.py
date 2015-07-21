# -*- coding: utf-8 -*-

import pytest

import os
import requests

KELLY_HOST = os.getenv('KELLY_HOST')
if KELLY_HOST == None or KELLY_HOST == '':
    KELLY_HOST = '127.0.0.1'

KELLY_PORT = os.getenv('KELLY_PORT')
if KELLY_PORT == None or KELLY_PORT == '':
    KELLY_PORT = '8080'

CUSTOMER_UUID = '2dca065f-c328-4a19-bc92-b222f48764e1'
CUSTOMER_ID = '0'
CUSTOMER_NAME = 'name'

USER_ID = 'user'

BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/v1'
CUSTOMERS_URL = BASE_URL+'/customers'
USERS_URL = CUSTOMERS_URL+'/'+CUSTOMER_UUID+'/users'

@pytest.fixture(scope="function")
def http(request):
    http = requests

    cust_data = {'customer_uuid':CUSTOMER_UUID,
            'customer_id':CUSTOMER_ID,
            'name':CUSTOMER_NAME,
            'priority':1,
            'rps':1000,
            'receipts_allowed':True,
            'no_retry':False,
            'default_validity':'000003000000000R',
            'max_validity':259200,
            'default_provider_id':'0a89542c-5270-11e1-bf27-001d0947ec73',
            'network_map_id':'befa8b7c-c4a3-11e3-b670-00269e42f7a5',
            'interfaces':'soap;mm;email',
            'features':'',
            'pay_type':'postpaid',
            'credit':10000.0,
            'credit_limit':10000.0,
            'language':'en',
            'state':'active'}
    req = http.post(CUSTOMERS_URL, data=cust_data)

    user_data = {'user_id':USER_ID,
                'password':'secret',
                'interfaces':'',
                'features':'',
                'mobile_phone':'375290000000',
                'first_name':'fn',
                'last_name':'ln',
                'company':'com',
                'occupation':'oc',
                'email':'u@m.c',
                'country':'cou',
                'language':'en',
                'state':'active'}
    req = http.post(USERS_URL, data=user_data)

    def fin():
        print ("finalizing...")
        http.delete(USERS_URL+'/'+USER_ID)
        http.delete(CUSTOMERS_URL+'/'+CUSTOMER_UUID)

    request.addfinalizer(fin)
    return http

def test_add_customer_s_interfaces_to_user_succ(http):
    req_data = {'interfaces':'soap;mm;email'}
    req = http.put(USERS_URL+'/'+USER_ID, data=req_data)
    assert req.status_code == 200
    assert req.json()['interfaces'] == ['soap','mm','email']

def test_remove_user_s_interface_succ(http):
    req_data = {'interfaces':'soap;email'}
    req = http.put(USERS_URL+'/'+USER_ID, data=req_data)
    assert req.status_code == 200
    assert req.json()['interfaces'] == ['soap','email']

def test_add_non_customer_s_oneapi_interface_to_user_fail(http):
    req_data = {'interfaces':'oneapi'}
    req = http.put(USERS_URL+'/'+USER_ID, data=req_data)
    assert req.status_code == 400

def test_delete_interface_from_customer_should_delete_from_user_succ(http):
    test_add_customer_s_interfaces_to_user_succ(http)

    req_data = {'interfaces':'soap;mm'}
    req = http.put(CUSTOMERS_URL+'/'+CUSTOMER_UUID, data=req_data)
    assert req.status_code == 200
    assert req.json()['interfaces'] == ['soap','mm']

    req2 = http.get(USERS_URL+'/'+USER_ID)
    assert req2.status_code == 200
    assert req.json()['interfaces'] == ['soap','mm']
