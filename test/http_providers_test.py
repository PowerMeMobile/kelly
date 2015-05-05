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

PROVIDER_ID = 'c973e494-c4a4-11e3-8684-00269e42f7a5'

BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT
PROVIDERS_URL = BASE_URL+'/providers'

@pytest.fixture(scope="module")
def http(request):
    http = requests

    def fin():
        print ("finalizing...")
        http.delete(PROVIDERS_URL+'/'+PROVIDER_ID)

    request.addfinalizer(fin)
    return http

def test_create_provider_succ(http):
    req_data = {'id':PROVIDER_ID,
                'name':'provider',
                'description':'description',
                'gateway_id':'7dc235d0-c938-4b66-8f8c-c9037c7eace7',
                'bulk_gateway_id':'7dc235d0-c938-4b66-8f8c-c9037c7eace7',
                'receipts_supported':True,
                'sms_add_points':0.0}
    req = http.post(PROVIDERS_URL, data=req_data)
    assert req.status_code == 201
    resp_data = req.json()
    assert resp_data == req_data

def test_update_provider_succ(http):
    req_data = {'id':PROVIDER_ID,
                'name':'new provider',
                'description':'new description',
                'gateway_id':'7dc235d0-c938-4b66-8f8c-c9037c7eace6',
                'bulk_gateway_id':'7dc235d0-c938-4b66-8f8c-c9037c7eace6',
                'receipts_supported':False,
                'sms_add_points':1.0}
    req = http.put(PROVIDERS_URL+'/'+PROVIDER_ID, data=req_data)
    assert req.status_code == 200
    resp_data = req.json()
    assert resp_data == req_data

def test_read_provider_succ(http):
    req = http.get(PROVIDERS_URL+'/'+PROVIDER_ID)
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = {'id':PROVIDER_ID,
                'name':'new provider',
                'description':'new description',
                'gateway_id':'7dc235d0-c938-4b66-8f8c-c9037c7eace6',
                'bulk_gateway_id':'7dc235d0-c938-4b66-8f8c-c9037c7eace6',
                'receipts_supported':False,
                'sms_add_points':1.0}
    assert resp_data == exp_data

def test_delete_provider_succ(http):
    req = http.delete(PROVIDERS_URL+'/'+PROVIDER_ID)
    assert req.status_code == 204
