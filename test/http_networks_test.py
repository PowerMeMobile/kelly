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

NETWORK_ID = '920a009a-5270-11e1-b961-001d0947ec73'

BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT
NETWORKS_URL = BASE_URL+'/networks'

@pytest.fixture(scope="module")
def http(request):
    http = requests

    def fin():
        print ("finalizing...")
        http.delete(NETWORKS_URL+'/'+NETWORK_ID)

    request.addfinalizer(fin)
    return http

def test_create_network_succ(http):
    req_data = {'id':NETWORK_ID,
                'name':'network',
                'country':'country',
                'hex_code':'FF',
                'country_code':'375',
                'number_len':12,
                'prefixes':'29;33;44',
                'gmt_diff':'+2',
                'dst':'5,7,3',
                'provider_id':'0a89542c-5270-11e1-bf27-001d0947ec73',
                'is_home':True,
                'sms_points':1.0,
                'sms_mult_points':1.0}
    req = http.post(NETWORKS_URL, data=req_data)
    assert req.status_code == 201
    resp_data = req.json()
    # change some fields expected in response
    req_data['prefixes'] = ['29', '33', '44']
    assert resp_data == req_data

def test_update_network_succ(http):
    req_data = {'id':NETWORK_ID,
                'name':'new network',
                'country':'new country',
                'hex_code':'AA',
                'country_code':'376',
                'number_len':13,
                'prefixes':'33;44',
                'gmt_diff':'-2',
                'dst':'6,5,2',
                'provider_id':'0a89542c-5270-11e1-bf27-001d0947ec74',
                'is_home':False,
                'sms_points':0.0,
                'sms_mult_points':0.0}
    req = http.put(NETWORKS_URL+'/'+NETWORK_ID, data=req_data)
    assert req.status_code == 200
    resp_data = req.json()
    # change some fields expected in response
    req_data['prefixes'] = ['33', '44']
    assert resp_data == req_data

def test_update_network_2_succ(http):
    req_data = {'id':NETWORK_ID,
                'is_home':True}
    req = http.put(NETWORKS_URL+'/'+NETWORK_ID, data=req_data)
    assert req.status_code == 200
    resp_data = req.json()
    assert resp_data['is_home'] == True

def test_read_network_succ(http):
    req = http.get(NETWORKS_URL+'/'+NETWORK_ID)
    assert req.status_code == 200
    resp_data = req.json()
    exp_data = {'id':NETWORK_ID,
                'name':'new network',
                'country':'new country',
                'hex_code':'AA',
                'country_code':'376',
                'number_len':13,
                'prefixes':['33','44'],
                'gmt_diff':'-2',
                'dst':'6,5,2',
                'provider_id':'0a89542c-5270-11e1-bf27-001d0947ec74',
                'is_home':True,
                'sms_points':0.0,
                'sms_mult_points':0.0}
    assert resp_data == exp_data

def test_delete_network_succ(http):
    req = http.delete(NETWORKS_URL+'/'+NETWORK_ID)
    assert req.status_code == 204
