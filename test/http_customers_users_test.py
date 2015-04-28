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

CUSTOMER_UUID = '493b3678-9dc8-11e2-8cce-00269e42f7a5'
BAD_CUSTOMER_UUID = 'bad_customer'

USER_ID = 'user'
BAD_USER_ID = 'bad_user'

BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/customers'
BASE_USERS_URL = BASE_URL+'/'+CUSTOMER_UUID+'/users'

@pytest.fixture(scope="module")
def http(request):
    http = requests

    data = {'customer_uuid':CUSTOMER_UUID,
            'customer_id':'0',
            'name':'name',
            'priority':1,
            'rps':1000,
            'receipts_allowed':True,
            'no_retry':False,
            'default_validity':'000003000000000R',
            'max_validity':259200,
            'default_provider_id':'0a89542c-5270-11e1-bf27-001d0947ec73',
            'network_map_id':'befa8b7c-c4a3-11e3-b670-00269e42f7a5',
            'pay_type':'postpaid',
            'credit':10000.0,
            'credit_limit':10000.0,
            'language':'en',
            'state':'active'}
    req = http.post(BASE_URL, data=data)

    def fin():
        print ("finalizing...")
        http.delete(BASE_URL+'/'+CUSTOMER_UUID)

    request.addfinalizer(fin)
    return http

def test_create_user_succ(http):
    req_data = {'id':USER_ID,
                'password':'secret',
                'connection_types':'transmitter;receiver',
                'mobile_phone':'375290000000',
                'first_name':'fn',
                'last_name':'ln',
                'company':'com',
                'occupation':'oc',
                'email':'u@m.c',
                'country':'cou',
                'language':'en',
                'state':'active'}
    req = http.post(BASE_USERS_URL, data=req_data)
    assert req.status_code == 201
    resp_data = req.json()
    # add/remove some fields expected in response
    req_data['connection_types'] = ['transmitter', 'receiver']
    req_data['features'] = []
    del req_data['password']
    assert resp_data == req_data

def test_update_user_succ(http):
    req_data = {'connection_types':'transmitter',
                'password':'secret2',
                'mobile_phone':'375290000001',
                'first_name':'fn1',
                'last_name':'ln1',
                'company':'com1',
                'occupation':'oc1',
                'email':'u@m.d',
                'country':'cou1',
                'language':'fr',
                'state':'blocked'}
    req = http.put(BASE_USERS_URL+'/'+USER_ID, data=req_data)
    assert req.status_code == 200
    resp_data = req.json()
    # add/remove some fields expected in response
    req_data['id'] = USER_ID
    req_data['connection_types'] = ['transmitter']
    req_data['features'] = []
    del req_data['password']
    assert resp_data == req_data

def test_read_bad_user_succ(http):
    req = http.get(BASE_USERS_URL+'/'+BAD_USER_ID)
    assert req.status_code == 404

def test_read_user_succ(http):
    req = http.get(BASE_USERS_URL+'/'+USER_ID)
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = {'id':USER_ID,
                'connection_types':['transmitter'],
                'mobile_phone':'375290000001',
                'first_name':'fn1',
                'last_name':'ln1',
                'company':'com1',
                'occupation':'oc1',
                'email':'u@m.d',
                'country':'cou1',
                'language':'fr',
                'state':'blocked',
                'features':[]}
    assert resp_data == exp_data

def test_delete_user_succ(http):
    req = http.delete(BASE_USERS_URL+'/'+USER_ID)
    assert req.status_code == 204
