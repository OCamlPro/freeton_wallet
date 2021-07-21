all: build
	ft contract import !{name}Debot.sol

build: !{name}Debot.tvm

!{name}Debot.tvm: !{name}Debot.sol lib/*.sol
	ft contract build !{name}Debot.sol -f

deploy:
	ft contract deploy !{name}Debot --create debot_!{name} -f
	ft call debot_!{name} setABI '{ "dabi": "%{hex:read:contract:abi:!{name}Debot}" }'
	ft call debot_!{name} setIcon '{ "icon": "%{hex:string:data:image/png;base64,%{base64:file:!{name}Debot.png}}" }'

run:
	ft client -- debot fetch %{account:address:debot_!{name}}

invoke:
	ft client -- debot invoke %{account:address:debot_!{name}} '%{account:in-message:debot_!{name}:0:other_entry:{ "x": 3 \\}}'
