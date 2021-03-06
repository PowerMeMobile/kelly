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

ENTRY_ID = '62ce045e-c4b3-11e3-9d4c-00269e42f7a5'
BAD_ENTRY_ID = 'bad_entry_id'

BASE_URL = 'http://'+KELLY_HOST+':'+KELLY_PORT+'/network_maps'

@pytest.fixture(scope="function")
def http(request):
    http = requests

    def fin():
        print ("finalizing...")
        http.delete(BASE_URL+'/'+ENTRY_ID)

    request.addfinalizer(fin)
    return http

def test_create_network_map_wo_network_ids_fail(http):
    req = http.post(BASE_URL, data={'id':ENTRY_ID,
                                    'name':'country'})
    assert req.status_code == 400
    json = req.json()
    assert json['request_error']['service_exception']['message_id'] == 'SVC0005'
    assert json['request_error']['service_exception']['variables'] == ['network_ids']

def test_create_network_map_succ(http):
    req = http.post(BASE_URL, data={
        'id':ENTRY_ID,
        'name':'country',
        'network_ids':'80755f2a-c4b3-11e3-b7a4-00269e42f7a5a;977ffa0e-c4b3-11e3-a01f-00269e42f7a5'})
    assert req.status_code == 201
    json = req.json()
    assert json['id'] == ENTRY_ID
    assert json['name'] == 'country'
    assert json['network_ids'] == ['80755f2a-c4b3-11e3-b7a4-00269e42f7a5a','977ffa0e-c4b3-11e3-a01f-00269e42f7a5']

def test_get_network_map_succ(http):
    req = http.post(BASE_URL, data={
        'id':ENTRY_ID,
        'name':'country',
        'network_ids':'80755f2a-c4b3-11e3-b7a4-00269e42f7a5a;977ffa0e-c4b3-11e3-a01f-00269e42f7a5'})
    assert req.status_code == 201

    req = http.get(BASE_URL+'/'+ENTRY_ID)
    assert req.status_code == 200
    json = req.json()
    assert json['id'] == ENTRY_ID
    assert json['name'] == 'country'
    assert json['network_ids'] == ['80755f2a-c4b3-11e3-b7a4-00269e42f7a5a','977ffa0e-c4b3-11e3-a01f-00269e42f7a5']

def test_get_network_map_fail(http):
    req = http.get(BASE_URL+'/'+BAD_ENTRY_ID)
    assert req.status_code == 404

def test_update_network_map_succ(http):
    req = http.post(BASE_URL, data={
        'id':ENTRY_ID,
        'name':'country',
        'network_ids':'80755f2a-c4b3-11e3-b7a4-00269e42f7a5a;977ffa0e-c4b3-11e3-a01f-00269e42f7a5'})
    assert req.status_code == 201

    req = http.put(BASE_URL+'/'+ENTRY_ID, data={
        'name':'new_country',
        'network_ids':'80755f2a-c4b3-11e3-b7a4-00269e42f7a5a'})
    assert req.status_code == 200
    json = req.json()
    assert json['id'] == ENTRY_ID
    assert json['name'] == 'new_country'
    assert json['network_ids'] == ['80755f2a-c4b3-11e3-b7a4-00269e42f7a5a']

def test_update_network_map_fail(http):
    req = http.post(BASE_URL, data={
        'id':ENTRY_ID,
        'name':'country',
        'network_ids':'80755f2a-c4b3-11e3-b7a4-00269e42f7a5a;977ffa0e-c4b3-11e3-a01f-00269e42f7a5'})
    assert req.status_code == 201

    req = http.put(BASE_URL+'/'+BAD_ENTRY_ID, data={
        'name':'new_country',
        'network_ids':'80755f2a-c4b3-11e3-b7a4-00269e42f7a5a'})
    assert req.status_code == 404

def test_delete_network_map_succ(http):
    req = http.post(BASE_URL, data={
        'id':ENTRY_ID,
        'name':'country',
        'network_ids':'80755f2a-c4b3-11e3-b7a4-00269e42f7a5a;977ffa0e-c4b3-11e3-a01f-00269e42f7a5'})
    assert req.status_code == 201

    req = http.delete(BASE_URL+'/'+ENTRY_ID)
    assert req.status_code == 204
