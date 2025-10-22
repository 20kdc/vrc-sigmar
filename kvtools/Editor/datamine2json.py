#!/usr/bin/env python3
import json

dfile = open("datamine.txt", "r")

data = {}
data_externs = {}
data_types = {}
data_events = {}
data["externs"] = data_externs
data["types"] = data_types
data["events"] = data_events

while True:
	decltype = dfile.readline().strip()
	if decltype == "END":
		break
	elif decltype == "EXTERN":
		extern_id = dfile.readline().strip()
		extern_type = dfile.readline().strip()
		extern_id_type = extern_id[:extern_id.find(".")]
		extern_id_signature = extern_id[extern_id.find(".") + 1:]
		extern_parameters = []
		for _ in range(int(dfile.readline().strip())):
			p_name = dfile.readline().strip()
			p_type = dfile.readline().strip()
			p_dir = dfile.readline().strip()
			extern_parameters.append({
				"name": p_name,
				"type": p_type,
				"dir": p_dir,
			})
		data_externs[extern_id] = {
			"id": extern_id,
			"id_type": extern_id_type,
			"id_signature": extern_id_signature,
			"type": extern_type,
			"parameters": extern_parameters
		}
		if not (extern_id_type in data_types):
			# fake replacable static type
			data_types[extern_id_type] = {
				"static": True,
				"name": extern_id_type,
				"netname": "",
				"base": "",
				"interfaces": []
			}
	elif decltype == "EVENT":
		event_name = dfile.readline().strip()
		event_entrypoint = dfile.readline().strip()
		event_parameters = []
		for _ in range(int(dfile.readline().strip())):
			p_name = dfile.readline().strip()
			p_type = dfile.readline().strip()
			event_parameters.append({
				"name": p_name,
				"type": p_type
			})
		data_events[event_entrypoint] = {
			"name": event_name,
			"entrypoint": event_entrypoint,
			"parameters": event_parameters
		}
	elif decltype == "TYPE":
		type_name = dfile.readline().strip()
		type_netname = dfile.readline().strip()
		type_base = dfile.readline().strip()
		type_interfaces = []
		for _ in range(int(dfile.readline().strip())):
			type_interfaces.append(dfile.readline().strip())
		data_types[type_name] = {
			"static": False,
			"name": type_name,
			"netname": type_netname,
			"base": type_base,
			"interfaces": type_interfaces
		}
		if type_base == "SystemEnum":
			data_types[type_name]["enumValues"] = {}
	elif decltype == "EVAL":
		eval_type = dfile.readline().strip()
		eval_name = dfile.readline().strip()
		eval_value = int(dfile.readline().strip())
		data_types[eval_type]["enumValues"][eval_name] = eval_value
	else:
		raise Exception("Unknown decltype " + decltype)

json.dump(data, open("api.json", "w"), indent="\t")
