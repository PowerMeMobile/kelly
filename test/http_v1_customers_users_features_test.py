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
USER_ID = 'user'

BAD_CUSTOMER_UUID = 'bad_customer'
BAD_USER_ID = 'bad_user'

BASE_CUSTOMERS_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/v1/customers'
BASE_FEATURES_URL = BASE_CUSTOMERS_URL+'/'+CUSTOMER_UUID+'/users/'+USER_ID+'/features'
BAD_CUSTOMER_FEATURES_URL = BASE_CUSTOMERS_URL+'/'+BAD_CUSTOMER_UUID+'/users/'+USER_ID+'/features'
BAD_USER_FEATURES_URL = BASE_CUSTOMERS_URL+'/'+CUSTOMER_UUID+'/users/'+BAD_USER_ID+'/features'

@pytest.fixture(scope="function")
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
    req = http.post(BASE_CUSTOMERS_URL, data=data)

    data = {'id':'user',
            'password':'secret',
            'interfaces':'soap',
            'state':'active'}
    req = http.post(BASE_CUSTOMERS_URL+'/'+CUSTOMER_UUID+'/users', data=data)

    def fin():
        print ("finalizing...")
        req = http.get(BASE_FEATURES_URL)
        if req.status_code == 200:
            for data in req.json():
                http.delete(BASE_FEATURES_URL+'/'+data['name'])
        http.delete(BASE_CUSTOMERS_URL+'/'+CUSTOMER_UUID+'/users/user')
        http.delete(BASE_CUSTOMERS_URL+'/'+CUSTOMER_UUID)

    request.addfinalizer(fin)
    return http

def test_bad_customer_fail(http):
    req = http.get(BAD_CUSTOMER_FEATURES_URL)
    assert req.status_code == 404

def test_bad_user_fail(http):
    req = http.get(BAD_USER_FEATURES_URL)
    assert req.status_code == 404

def test_get_all_empty_succ(http):
    req = http.get(BASE_FEATURES_URL)
    assert req.status_code == 200
    assert req.json() == []

def test_get_non_existing_fail(http):
    req = http.get(BASE_FEATURES_URL+'/override_originator')
    assert req.status_code == 404

def test_post_new_succ(http):
    req = http.post(BASE_FEATURES_URL, data={'name':'override_originator', 'value':'empty'})
    assert req.status_code == 200
    assert len(req.json()) == 1
    assert req.json() == [{'name':'override_originator', 'value':'empty'}]

def test_post_existing_fail(http):
    http.post(BASE_FEATURES_URL, data={'name':'override_originator', 'value':'empty'})

    req = http.post(BASE_FEATURES_URL, data={'name':'override_originator', 'value':'empty'})
    assert req.status_code == 400

def test_post_another_succ(http):
    http.post(BASE_FEATURES_URL, data={'name':'override_originator', 'value':'empty'})

    req = http.post(BASE_FEATURES_URL, data={'name':'inbox', 'value':'false'})
    assert req.status_code == 200
    assert len(req.json()) == 2
    assert req.json() == [{'name':'override_originator', 'value':'empty'}, {'name':'inbox', 'value':'false'}]

def test_get_all_succ(http):
    http.post(BASE_FEATURES_URL, data={'name':'override_originator', 'value':'empty'})
    http.post(BASE_FEATURES_URL, data={'name':'inbox', 'value':'false'})

    req = http.get(BASE_FEATURES_URL)
    assert req.status_code == 200
    assert len(req.json()) == 2
    assert req.json() == [{'name':'override_originator', 'value':'empty'}, {'name':'inbox', 'value':'false'}]

def test_get_existing_succ(http):
    http.post(BASE_FEATURES_URL, data={'name':'override_originator', 'value':'empty'})
    http.post(BASE_FEATURES_URL, data={'name':'inbox', 'value':'false'})

    req = http.get(BASE_FEATURES_URL+'/inbox')
    assert req.json() == {'name':'inbox', 'value':'false'}

def test_put_existing_succ(http):
    http.post(BASE_FEATURES_URL, data={'name':'override_originator', 'value':'empty'})

    req = http.put(BASE_FEATURES_URL+'/override_originator?value=any')
    assert req.status_code == 200
    assert req.json() == [{'name':'override_originator', 'value':'any'}]

def test_put_non_existing_fail(http):
    req = http.put(BASE_FEATURES_URL+'/override_originator?value=any')
    assert req.status_code == 404

def test_delete_existing_succ(http):
    http.post(BASE_FEATURES_URL, data={'name':'override_originator', 'value':'empty'})
    http.post(BASE_FEATURES_URL, data={'name':'inbox', 'value':'false'})

    req = http.delete(BASE_FEATURES_URL+'/override_originator')
    assert req.status_code == 200
    assert req.json() == [{'name':'inbox', 'value':'false'}]

def test_delete_non_existing_succ(http):
    req = http.delete(BASE_FEATURES_URL+'/unknown')
    assert req.status_code == 200
    assert req.json() == []
